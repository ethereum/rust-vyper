use crate::db::AnalyzerDb;
use crate::namespace::items::{Contract, ModuleId, Struct, TypeAlias, TypeDefId};
use crate::namespace::types;
use fe_common::diagnostics::Diagnostic;
use fe_parser::ast;
use fe_parser::node::Node;
use indexmap::IndexMap;
use std::rc::Rc;

pub fn module_type_defs(db: &dyn AnalyzerDb, module: ModuleId) -> Rc<IndexMap<String, TypeDefId>> {
    let ast::Module { body } = &module.data(db).ast;
    Rc::new(
        body.iter()
            .filter_map(|stmt| match stmt {
                ast::ModuleStmt::TypeAlias(node) => Some((
                    node.kind.name.kind.clone(),
                    TypeDefId::Alias(db.intern_type_alias(Rc::new(TypeAlias {
                        ast: node.clone(),
                        module,
                    }))),
                )),
                ast::ModuleStmt::Contract(node) => Some((
                    node.kind.name.kind.clone(),
                    TypeDefId::Contract(db.intern_contract(Rc::new(Contract {
                        ast: node.clone(),
                        module,
                    }))),
                )),
                ast::ModuleStmt::Struct(node) => Some((
                    node.kind.name.kind.clone(),
                    TypeDefId::Struct(db.intern_struct(Rc::new(Struct {
                        ast: node.clone(),
                        module,
                    }))),
                )),
                _ => None,
            })
            .collect(),
    )
}

pub fn module_resolve_type(
    db: &dyn AnalyzerDb,
    module: ModuleId,
    name: String,
) -> Option<Rc<types::Type>> {
    let type_defs = module.type_defs(db);
    Some(type_defs.get(&name)?.typ(db))
}

pub(crate) fn module_diagnostics(db: &dyn AnalyzerDb, module: ModuleId) -> Rc<Vec<Diagnostic>> {
    todo!()
    // let mut diags = vec![];
    // for stmt in module.data(db).body {
    //     match stmt {
    //         ast::ModuleStmt::Pragma(inner) => diags.extend(pragma_stmt(context, inner).into()),
    //         ast::ModuleStmt::Import(inner) => {
    //             diags.push(errors::not_yet_implemented("import", inner.span))
    //         }
    //         _ => {} // everything else is handled below
    //     }
    // }

    // for module.type_defs(db)
}
