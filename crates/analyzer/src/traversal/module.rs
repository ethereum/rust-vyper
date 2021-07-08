use crate::db::AnalyzerDb;
use crate::errors::{self, AlreadyDefined, FatalError};
use crate::namespace::items::TypeDefId;
use crate::namespace::scopes::{ModuleScope, Scope, Shared};
use crate::namespace::types::Type;
use crate::traversal::{contracts, module, structs, types};
use crate::Context;
use fe_common::diagnostics::Label;
use fe_parser::ast as fe;
use fe_parser::node::Node;
use indexmap::IndexMap;
use semver::{Version, VersionReq};
use std::collections::{BTreeSet, HashMap};
use std::rc::Rc;

/// Gather context information for a module and check for type errors.
pub fn module(context: &mut Context, module: &fe::Module) -> Result<(), FatalError> {
    let scope = ModuleScope::new();

    let mut contracts = vec![];

    let module = module.data(context.db);
    for stmt in module.body.iter() {
        match &stmt {
            fe::ModuleStmt::TypeAlias(id) => type_alias(context, Rc::clone(&scope), *id)?,
            fe::ModuleStmt::Pragma(inner) => pragma_stmt(context, inner),
            fe::ModuleStmt::Struct(id) => structs::struct_def(context, Rc::clone(&scope), *id)?,
            fe::ModuleStmt::Contract(inner) => {
                // Collect contract statements and the scope that we create for them. After we
                // have walked all contracts once, we walk over them again for a
                // more detailed inspection.
                let contract_scope = contracts::contract_def(Rc::clone(&scope), context, inner)?;
                contracts.push((inner, contract_scope))
            }
            fe::ModuleStmt::Import(inner) => context.not_yet_implemented("import", inner.span),
        }
    }

    for (contract, scope) in contracts.iter() {
        contracts::contract_body(Rc::clone(&scope), context, contract)?
    }

    context.set_module(scope.into());

    Ok(())
}

pub fn resolve_type_query(db: &dyn AnalyzerDb, name: &str, module: ModuleId) -> Option<Rc<Type>> {
    let defs = db.type_defs(module);
    db.type_def_type(defs.get(name)?)
}

pub fn type_def_type_query(db: &dyn AnalyzerDb, typedef: TypeDefId) -> Rc<Type> {
    match typedef {
        TypeDefId::Alias(id) => db.alias_type(id),
        TypeDefId::Struct(id) => db.struct_type(id),
        TypeDefId::Contract(id) => db.contract_type(id),
    }
}

fn type_alias(
    context: &mut Context,
    scope: Shared<ModuleScope>,
    type_alias: &Node<fe::TypeAlias>,
) -> Result<(), FatalError> {
    let fe::TypeAlias { name, typ } = &type_alias.kind;
    let typ = types::type_desc(&Scope::Module(Rc::clone(&scope)), context, &typ)?;

    if let Err(AlreadyDefined) = scope.borrow_mut().add_type_def(&name.kind, typ) {
        context.fancy_error(
            "a type definition with the same name already exists",
            // TODO: figure out how to include the previously defined definition
            vec![Label::primary(
                type_alias.span,
                format!("Conflicting definition of `{}`", name.kind),
            )],
            vec![format!(
                "Note: Give one of the `{}` definitions a different name",
                name.kind
            )],
        )
    }
    Ok(())
}
