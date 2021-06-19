use crate::context::FunctionAttributes;
use crate::db::AnalyzerDb;
use crate::impl_intern_key;
use crate::namespace::types;
use fe_common::diagnostics::Diagnostic;
use fe_common::files::SourceFileId;
use fe_parser::ast;
use fe_parser::node::Node;
use indexmap::IndexMap;
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

    pub fn diagnostics(&self, db: &dyn AnalyzerDb) -> Rc<Vec<Diagnostic>> {
        todo!() //db.module_diagnostics(*self)
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
        let fields = db.contract_fields(*self);
        let (index, _, typ) = fields.get_full(name)?;
        Some((Rc::clone(&typ), index))
    }
    pub fn functions(&self, db: &dyn AnalyzerDb) -> Rc<IndexMap<String, FunctionId>> {
        db.contract_functions(*self)
    }
    pub fn events(&self, db: &dyn AnalyzerDb) -> Rc<IndexMap<String, EventId>> {
        db.contract_events(*self)
    }

    pub fn diagnostics(&self, db: &dyn AnalyzerDb) -> Rc<Vec<Diagnostic>> {
        // XXX: merge with context diags
        // db.contract_type_query(*self).diagnostics
        todo!()
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

    pub fn typ(&self, db: &dyn AnalyzerDb) -> Rc<FunctionAttributes> {
        todo!() //self.data(db).contract.
    }

    pub fn param(&self, db: &dyn AnalyzerDb, name: &str) -> Option<types::FixedSize> {
        self.typ(db)
            .params
            .iter()
            .find_map(|(pname, typ)| (pname == name).then(|| typ.clone()))
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

    // pub fn typ(&self, db: &dyn AnalyzerDb) ->
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
    // namespace::events::EventDef
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

    // pub fn typ(&self, db: &dyn AnalyzerDb) ->
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
            _ => todo!(),
            // TypeDefId::Struct(id) => Rc::new(types::Type::Struct(*id.typ(db))),
            // TypeDefId::Contract(id) => Rc::new(types::Type::Contract(*id.typ(db))),
        }
    }
}
