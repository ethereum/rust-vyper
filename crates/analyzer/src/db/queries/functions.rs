use crate::context::{AnalyzerContext, CallType, ExpressionAttributes, FunctionBody};
use crate::db::{Analysis, AnalyzerDb};
use crate::errors;
use crate::namespace::items::{self, ContractId, EventId, FunctionId, ModuleId};
use crate::namespace::scopes::{BlockScope, BlockScopeType, FunctionScope};
use crate::namespace::types::{self, FixedSize, Type};
use crate::traversal::functions::traverse_statements;
use crate::traversal::types::type_desc;
use fe_common::diagnostics::{Diagnostic, Label};
use fe_parser::ast;
use fe_parser::node::{Node, NodeId};
use std::collections::{BTreeMap, HashMap};
use std::convert::TryFrom;
use std::rc::Rc;

/// Gather context information for a function definition and check for type
/// errors. Does not inspect the function body.
pub fn function_signature(
    db: &dyn AnalyzerDb,
    function: FunctionId,
) -> Analysis<Rc<types::FunctionSignature>> {
    let def = &function.data(db).ast.kind;

    let mut context = BasicContext::new(db, function.module(db));

    let mut names = HashMap::new();
    let params = def
        .args
        .iter()
        .enumerate()
        .filter_map(|(index, arg)| {
            let ast::FunctionArg { name, typ } = &arg.kind;
            let typ = match FixedSize::try_from(type_desc(&mut context, typ)) {
                Ok(typ) => typ,
                Err(_) => {
                    context.error(
                        "function parameter types must have fixed size",
                        typ.span,
                        "`Map` type can't be used as a function parameter",
                    );
                    FixedSize::unknown()
                }
            };

            if let Some(dup_idx) = names.get(&name.kind) {
                let dup_arg: &Node<ast::FunctionArg> = &def.args[*dup_idx];
                context.add_diagnostic(errors::fancy_error(
                    format!("duplicate parameter names in function `{}`", def.name.kind),
                    vec![
                        Label::primary(
                            dup_arg.span,
                            format!("first use of the name `{}`", name.kind),
                        ),
                        Label::secondary(
                            arg.span,
                            format!("second use of the name `{}`", name.kind),
                        ),
                    ],
                    vec![],
                ));
                None
            } else {
                names.insert(&name.kind, index);
                Some((name.kind.clone(), typ))
            }
        })
        .collect();

    let return_type = def
        .return_type
        .as_ref()
        .map(|type_node| {
            if def.name.kind == "__init__" {
                // `__init__` must not return any type other than `()`.
                if type_node.kind != ast::TypeDesc::Unit {
                    context.add_diagnostic(errors::fancy_error(
                        "`__init__` function has incorrect return type",
                        vec![Label::primary(type_node.span, "return type should be `()`")],
                        vec![
                            "Hint: Remove the return type specification.".to_string(),
                            "Example: `pub def __init__():`".to_string(),
                        ],
                    ));
                }
                FixedSize::unit()
            } else {
                match FixedSize::try_from(type_desc(&mut context, &type_node)) {
                    Ok(typ) => typ,
                    Err(_) => {
                        context.error(
                            "function return type must have a fixed size".into(),
                            type_node.span,
                            "this can't be returned from a function".into(),
                        );
                        FixedSize::unknown()
                    }
                }
            }
        })
        .unwrap_or_else(FixedSize::unit);

    Analysis {
        value: Rc::new(types::FunctionSignature {
            params,
            return_type,
        }),
        diagnostics: Rc::new(context.diagnostics),
    }
}

/// Gather context information for a function body and check for type errors.
pub fn function_body(db: &dyn AnalyzerDb, function: FunctionId) -> Analysis<Rc<FunctionBody>> {
    let def = &function.data(db).ast.kind;
    let scope = FunctionScope::new(db, function);

    // If the return type is unit, explicit return or no return (implicit) is valid,
    // so no scanning is necessary.
    // If the return type is anything else, we need to ensure that all code paths
    // return or revert.
    let return_type = &function.signature(db).return_type;
    if !return_type.is_unit() && !all_paths_return_or_revert(&def.body) {
        scope.add_diagnostic(errors::fancy_error(
            "function body is missing a return or revert statement",
            vec![
                Label::primary(
                    def.name.span,
                    "all paths of this function must `return` or `revert`",
                ),
                Label::secondary(
                    def.return_type.as_ref().unwrap().span,
                    format!("expected function to return `{}`", return_type),
                ),
            ],
            vec![],
        ));
    }

    match traverse_statements(
        &mut BlockScope::new(&scope, BlockScopeType::Function),
        &def.body,
    ) {
        Ok(()) => {}
        Err(_) => {} // XXX
    }
    Analysis {
        value: Rc::new(scope.body.into_inner()),
        diagnostics: Rc::new(scope.diagnostics.into_inner()),
    }
}

fn all_paths_return_or_revert(block: &[Node<ast::FuncStmt>]) -> bool {
    for statement in block.iter().rev() {
        match &statement.kind {
            ast::FuncStmt::Return { .. } => return true,
            ast::FuncStmt::Revert { .. } => return true,
            ast::FuncStmt::If {
                test: _,
                body,
                or_else,
            } => {
                let body_returns = all_paths_return_or_revert(&body);
                let or_else_returns = or_else.is_empty() || all_paths_return_or_revert(&or_else);
                if body_returns && or_else_returns {
                    return true;
                }
            }
            _ => {}
        }
    }

    false
}

struct BasicContext<'a> {
    db: &'a dyn AnalyzerDb,
    module: ModuleId,
    diagnostics: Vec<Diagnostic>,
}
impl<'a> BasicContext<'a> {
    pub fn new(db: &'a dyn AnalyzerDb, module: ModuleId) -> Self {
        Self {
            db,
            module,
            diagnostics: Vec::new(),
        }
    }
}

impl<'a> AnalyzerContext for BasicContext<'a> {
    fn resolve_type(&self, name: &str) -> Option<Rc<Type>> {
        self.module.resolve_type(self.db, name)
    }
    fn add_diagnostic(&mut self, diag: Diagnostic) {
        self.diagnostics.push(diag)
    }
}
