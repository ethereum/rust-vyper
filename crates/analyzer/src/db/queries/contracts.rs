use crate::context::AnalyzerContext;
use crate::db::{Analysis, AnalyzerDb};
use crate::errors;
use crate::namespace::items::{self, ContractId, EventId, FunctionId, ModuleId};
use crate::namespace::scopes::ItemScope;
use crate::namespace::types;
use crate::traversal::types::type_desc;
use fe_common::diagnostics::{Diagnostic, Label};
use fe_parser::ast;
use fe_parser::node::Node;
use indexmap::{map::Entry, IndexMap};
use std::collections::HashSet;
use std::rc::Rc;

pub fn contract_functions(db: &dyn AnalyzerDb, contract: ContractId) -> Rc<Vec<FunctionId>> {
    let body = &contract.data(db).ast.kind.body;
    Rc::new(
        body.iter()
            .filter_map(|stmt| match stmt {
                ast::ContractStmt::Event(_) => None,
                ast::ContractStmt::Function(node) => {
                    Some(db.intern_function(Rc::new(items::Function {
                        ast: node.clone(),
                        contract,
                    })))
                }
            })
            .collect(),
    )
}

pub fn contract_events(db: &dyn AnalyzerDb, contract: ContractId) -> Rc<Vec<EventId>> {
    let body = &contract.data(db).ast.kind.body;
    Rc::new(
        body.iter()
            .filter_map(|stmt| match stmt {
                ast::ContractStmt::Function(_) => None,
                ast::ContractStmt::Event(node) => Some(db.intern_event(Rc::new(items::Event {
                    ast: node.clone(),
                    contract,
                }))),
            })
            .collect(),
    )
}

pub fn contract_fields(
    db: &dyn AnalyzerDb,
    contract: ContractId,
) -> Analysis<Rc<IndexMap<String, Rc<types::Type>>>> {
    let mut scope = ItemScope::new(db, contract.module(db));
    let mut fields = IndexMap::new();

    let contract_def = &contract.data(db).ast.kind;
    for field in &contract_def.fields {
        let name = field.kind.name.kind.clone();

        if field.kind.is_pub {
            scope.not_yet_implemented("contract `pub` fields", field.span);
        }
        if field.kind.is_const {
            scope.not_yet_implemented("contract `const` fields", field.span);
        }
        if let Some(node) = &field.kind.value {
            scope.not_yet_implemented("contract field initial value assignment", node.span);
        }

        match fields.entry(name) {
            Entry::Occupied(entry) => {
                scope.fancy_error(
                    &format!(
                        "duplicate field names in `contract {}`",
                        contract_def.name.kind,
                    ),
                    vec![
                        Label::primary(
                            contract_def.field_span(entry.key()).unwrap(),
                            format!("`{}` first defined here", entry.key()),
                        ),
                        Label::secondary(field.span, format!("`{}` redefined here", entry.key())),
                    ],
                    vec![],
                );
            }
            Entry::Vacant(entry) => {
                entry.insert(Rc::new(type_desc(&mut scope, &field.kind.typ)));
            }
        }
    }

    Analysis {
        value: Rc::new(fields),
        diagnostics: Rc::new(scope.diagnostics),
    }
}

pub fn contract_type(db: &dyn AnalyzerDb, contract: ContractId) -> Analysis<Rc<types::Contract>> {
    let mut contract_typ = types::Contract {
        name: contract.name(db),
        functions: IndexMap::new(),
    };
    let mut diagnostics = vec![];

    for func in contract.functions(db).iter() {
        let def = &func.data(db).ast;
        let name = def.kind.name.kind.clone();

        match contract_typ.functions.entry(name) {
            Entry::Occupied(entry) => {
                diagnostics.push(errors::fancy_error(
                    format!(
                        "duplicate function names in `contract {}`",
                        contract_typ.name,
                    ),
                    vec![
                        Label::primary(
                            entry.get().id.data(db).ast.span,
                            format!("`{}` first defined here", entry.key()),
                        ),
                        Label::secondary(def.span, format!("`{}` redefined here", entry.key())),
                    ],
                    vec![],
                ));
            }
            Entry::Vacant(entry) => {
                let mut signature = func.signature(db);
                let mut is_public = def.kind.is_pub;

                if entry.key() == "__init__" {
                    // `__init__` must be `pub`.
                    if !is_public {
                        diagnostics.push(errors::fancy_error(
                            "`__init__` function is not public",
                            vec![Label::primary(
                                def.span,
                                "`__init__` function must be public",
                            )],
                            vec![
                                "Hint: Add the `pub` modifier.".to_string(),
                                "Example: `pub def __init__():`".to_string(),
                            ],
                        ));
                        is_public = true;
                    }
                }

                entry.insert(Rc::new(types::ContractFunction {
                    is_public,
                    id: *func,
                    signature,
                }));
            }
        }
    }

    Analysis {
        value: Rc::new(contract_typ),
        diagnostics: Rc::new(diagnostics),
    }
}
