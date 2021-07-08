use crate::db::AnalyzerDb;
use crate::errors;
use crate::impl_intern_key;
use crate::namespace::types;
use fe_common::diagnostics::{Diagnostic, Label};
use fe_common::files::SourceFileId;
use fe_parser::ast;
use fe_parser::node::Node;
use indexmap::IndexMap;
use semver::{Version, VersionReq};
use std::rc::Rc;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Module {
    pub ast: ast::Module,
    pub file: SourceFileId,
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub struct ModuleId(pub(crate) u32);
impl_intern_key!(ModuleId);
impl ModuleId {
    pub fn data(&self, db: &dyn AnalyzerDb) -> Rc<Module> {
        db.lookup_intern_module(*self)
    }

    pub fn type_defs(&self, db: &dyn AnalyzerDb) -> Rc<IndexMap<String, TypeDefId>> {
        db.module_type_defs(*self)
    }

    pub fn resolve_type(&self, db: &dyn AnalyzerDb, name: &str) -> Option<Rc<types::Type>> {
        db.module_resolve_type(*self, name.into())
    }

    pub fn diagnostics(&self, db: &dyn AnalyzerDb) -> Vec<Diagnostic> {
        let mut diags = vec![];
        let ast::Module { body } = &self.data(db).ast;
        for stmt in body {
            match stmt {
                ast::ModuleStmt::Pragma(inner) => {
                    diags.extend(check_pragma_version(inner).into_iter())
                }
                ast::ModuleStmt::Import(inner) => {
                    diags.push(errors::not_yet_implemented("import", inner.span))
                }
                _ => {} // everything else is a type def, handled below.
            }
        }

        // XXX need to check for duplicate names
        for type_def in self.type_defs(db).values() {
            diags.extend(type_def.diagnostics(db).iter().cloned())
        }
        diags
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum TypeDefId {
    Alias(TypeAliasId),
    Struct(StructId),
    Contract(ContractId),
    // Event(EventDefId),
}
impl TypeDefId {
    pub fn typ(&self, db: &dyn AnalyzerDb) -> Rc<types::Type> {
        match self {
            TypeDefId::Alias(id) => id.typ(db),
            TypeDefId::Struct(id) => Rc::new(types::Type::Struct(id.typ(db).as_ref().clone())),
            TypeDefId::Contract(id) => Rc::new(types::Type::Contract(id.typ(db).as_ref().clone())),
        }
    }

    pub fn diagnostics(&self, db: &dyn AnalyzerDb) -> Vec<Diagnostic> {
        match self {
            TypeDefId::Alias(id) => id.diagnostics(db).to_vec(),
            TypeDefId::Struct(id) => id.diagnostics(db),
            TypeDefId::Contract(id) => id.diagnostics(db),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct TypeAlias {
    pub ast: Node<ast::TypeAlias>,
    pub module: ModuleId,
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub struct TypeAliasId(pub(crate) u32);
impl_intern_key!(TypeAliasId);

impl TypeAliasId {
    pub fn data(&self, db: &dyn AnalyzerDb) -> Rc<TypeAlias> {
        db.lookup_intern_type_alias(*self)
    }
    pub fn typ(&self, db: &dyn AnalyzerDb) -> Rc<types::Type> {
        db.type_alias_type(*self).value
    }
    pub fn diagnostics(&self, db: &dyn AnalyzerDb) -> Vec<Diagnostic> {
        db.type_alias_type(*self).diagnostics.to_vec()
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Contract {
    pub ast: Node<ast::Contract>,
    pub module: ModuleId,
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub struct ContractId(pub(crate) u32);
impl_intern_key!(ContractId);
impl ContractId {
    pub fn name(&self, db: &dyn AnalyzerDb) -> String {
        self.data(db).ast.kind.name.kind.clone()
    }
    pub fn data(&self, db: &dyn AnalyzerDb) -> Rc<Contract> {
        db.lookup_intern_contract(*self)
    }
    pub fn module(&self, db: &dyn AnalyzerDb) -> ModuleId {
        self.data(db).module
    }

    pub fn typ(&self, db: &dyn AnalyzerDb) -> Rc<types::Contract> {
        db.contract_type(*self).value
    }

    pub fn field(&self, db: &dyn AnalyzerDb, name: &str) -> Option<(Rc<types::Type>, usize)> {
        let fields = db.contract_fields(*self).value;
        let (index, _, typ) = fields.get_full(name)?;
        Some((Rc::clone(&typ), index))
    }

    /// All functions, including duplicates
    pub fn functions(&self, db: &dyn AnalyzerDb) -> Rc<Vec<FunctionId>> {
        db.contract_functions(*self)
    }

    /// All events, including duplicates
    pub fn events(&self, db: &dyn AnalyzerDb) -> Rc<Vec<EventId>> {
        db.contract_events(*self)
    }

    pub fn diagnostics(&self, db: &dyn AnalyzerDb) -> Vec<Diagnostic> {
        let mut diags = vec![];
        diags.extend(db.contract_type(*self).diagnostics.iter().cloned());
        diags.extend(db.contract_fields(*self).diagnostics.iter().cloned());
        for id in self.functions(db).iter() {
            diags.extend(id.diagnostics(db).iter().cloned());
        }
        for id in self.events(db).iter() {
            diags.extend(id.diagnostics(db).iter().cloned());
        }
        diags
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Function {
    pub ast: Node<ast::Function>,
    pub contract: ContractId,
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub struct FunctionId(pub(crate) u32);
impl_intern_key!(FunctionId);
impl FunctionId {
    pub fn data(&self, db: &dyn AnalyzerDb) -> Rc<Function> {
        db.lookup_intern_function(*self)
    }

    pub fn contract(&self, db: &dyn AnalyzerDb) -> ContractId {
        self.data(db).contract
    }

    pub fn module(&self, db: &dyn AnalyzerDb) -> ModuleId {
        self.contract(db).module(db)
    }

    pub fn signature(&self, db: &dyn AnalyzerDb) -> Rc<types::FunctionSignature> {
        db.function_signature(*self).value
    }

    pub fn param(&self, db: &dyn AnalyzerDb, name: &str) -> Option<types::FixedSize> {
        self.signature(db)
            .params
            .iter()
            .find_map(|(pname, typ)| (pname == name).then(|| typ.clone()))
    }

    pub fn diagnostics(&self, db: &dyn AnalyzerDb) -> Vec<Diagnostic> {
        let mut diags = vec![];
        diags.extend(db.function_signature(*self).diagnostics.iter().cloned());
        diags.extend(db.function_body(*self).diagnostics.iter().cloned());
        diags
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Struct {
    pub ast: Node<ast::Struct>,
    pub module: ModuleId,
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub struct StructId(pub(crate) u32);
impl_intern_key!(StructId);
impl StructId {
    pub fn data(&self, db: &dyn AnalyzerDb) -> Rc<Struct> {
        db.lookup_intern_struct(*self)
    }
    pub fn typ(&self, db: &dyn AnalyzerDb) -> Rc<types::Struct> {
        db.struct_type(*self).value
    }
    pub fn diagnostics(&self, db: &dyn AnalyzerDb) -> Vec<Diagnostic> {
        db.struct_type(*self).diagnostics.as_ref().clone()
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Event {
    pub ast: Node<ast::Event>,
    pub contract: ContractId,
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub struct EventId(pub(crate) u32);
impl_intern_key!(EventId);

impl EventId {
    pub fn diagnostics(&self, db: &dyn AnalyzerDb) -> Vec<Diagnostic> {
        // XXX
        vec![]
    }
    // namespace::events::EventDef
}

// XXX: move this
fn check_pragma_version(stmt: &Node<ast::Pragma>) -> Option<Diagnostic> {
    let version_requirement = &stmt.kind.version_requirement;
    // This can't fail because the parser already validated it
    let requirement =
        VersionReq::parse(&version_requirement.kind).expect("Invalid version requirement");
    let actual_version =
        Version::parse(env!("CARGO_PKG_VERSION")).expect("Missing package version");

    if !requirement.matches(&actual_version) {
        Some(errors::fancy_error(
            format!(
                "The current compiler version {} doesn't match the specified requirement",
                actual_version
            ),
            vec![Label::primary(
                version_requirement.span,
                "The specified version requirement",
            )],
            vec![format!(
                "Note: Use `pragma {}` to make the code compile",
                actual_version
            )],
        ))
    } else {
        None
    }
}
