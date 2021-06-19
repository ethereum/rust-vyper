use crate::context::AnalyzerContext;
use crate::db::{Analysis, AnalyzerDb};
use crate::namespace::items::{self, ContractId, EventId, FunctionId, ModuleId};
use crate::namespace::types;
use fe_common::diagnostics::Diagnostic;
use fe_parser::ast;
use fe_parser::node::Node;
use indexmap::IndexMap;
use std::rc::Rc;

struct Ctx<'a> {
    db: &'a dyn AnalyzerDb,
    module: ModuleId,
    diagnostics: Vec<Diagnostic>,
}
impl<'a> AnalyzerContext for Ctx<'a> {
    fn resolve_type(&self, name: &str) -> Option<Rc<types::Type>> {
        self.module.resolve_type(self.db, name)
    }
    fn add_diagnostic(&mut self, diag: Diagnostic) {
        self.diagnostics.push(diag)
    }
}

pub fn contract_functions(
    db: &dyn AnalyzerDb,
    contract: ContractId,
) -> Rc<IndexMap<String, FunctionId>> {
    Rc::new(
        contract
            .data(db)
            .ast
            .kind
            .body
            .iter()
            .filter_map(|stmt| match stmt {
                ast::ContractStmt::Event(_) => None,
                ast::ContractStmt::Function(node) => Some((
                    node.kind.name.kind.clone(),
                    db.intern_function(Rc::new(items::Function {
                        ast: node.clone(),
                        contract,
                    })),
                )),
            })
            .collect(),
    )
}

pub fn contract_events(db: &dyn AnalyzerDb, contract: ContractId) -> Rc<IndexMap<String, EventId>> {
    Rc::new(
        contract
            .data(db)
            .ast
            .kind
            .body
            .iter()
            .filter_map(|stmt| match stmt {
                ast::ContractStmt::Function(_) => None,
                ast::ContractStmt::Event(node) => Some((
                    node.kind.name.kind.clone(),
                    db.intern_event(Rc::new(items::Event {
                        ast: node.clone(),
                        contract,
                    })),
                )),
            })
            .collect(),
    )
}

pub fn contract_fields(
    db: &dyn AnalyzerDb,
    contract: ContractId,
) -> Rc<IndexMap<String, Rc<types::Type>>> {
    todo!()
}

pub fn contract_type(db: &dyn AnalyzerDb, contract: ContractId) -> Analysis<Rc<types::Contract>> {
    let mut ctx = Ctx {
        db,
        module: contract.module(db),
        diagnostics: vec![],
    };

    // context.fancy_error(
    //     "a function with the same name already exists",
    //     // TODO: figure out how to include the previously defined function
    //     vec![Label::primary(
    //         def.span,
    //         format!("Conflicting definition of contract `{}`", name),
    //     )],
    //     vec![format!(
    //         "Note: Give one of the `{}` functions a different name",
    //         name
    //     )],
    // )

    // for func in contract.functions(db) {
    //     let func.data(db).ast
    // }

    // let functions = body.iter().filter_map(|stmt| match stmt {
    //     fe::ContractStmt::EventDef(_) => None,
    //     fe::ContractStmt::FuncDef(func) =>
    // })

    // Analysis {
    //     value: Rc::new(types::Contract {
    //         name: name.clone(),
    //         functions,
    //     }),
    //     diagnostics,
    // }
    todo!()
}
