use crate::context::{
    AnalyzerContext, CallType, ExpressionAttributes, FunctionAttributes, FunctionBody,
};
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
pub fn function_type(
    db: &dyn AnalyzerDb,
    function: FunctionId,
) -> Analysis<Rc<FunctionAttributes>> {
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
                        "this type can't be used as a function parameter",
                    );
                    FixedSize::unknown()
                }
            };

            if let Some(dup_idx) = names.get(&name.kind) {
                let dup_arg: &Node<ast::FunctionArg> = &def.args[*dup_idx];
                context.add_diagnostic(errors::fancy_error(
                    "duplicate function parameter names",
                    vec![
                        Label::primary(arg.span, "duplicate name"),
                        Label::secondary(dup_arg.span, "previously defined here"),
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

    let mut return_type = def
        .return_type
        .as_ref()
        .map(
            |typ| match FixedSize::try_from(type_desc(&mut context, &typ)) {
                Ok(typ) => typ,
                Err(_) => {
                    context.error(
                        "function return type must have a fixed size".into(),
                        typ.span,
                        "this can't be returned from a function".into(),
                    );
                    FixedSize::unknown()
                }
            },
        )
        .unwrap_or_else(FixedSize::unit);

    let mut is_public = def.is_pub;

    if def.name.kind == "__init__" {
        // `__init__` must not return any type other than `()`.
        if !return_type.is_unit() {
            context.add_diagnostic(errors::fancy_error(
                "`__init__` function has incorrect return type",
                vec![Label::primary(
                    def.return_type.as_ref().unwrap().span,
                    "return type should be `()`",
                )],
                vec![
                    "Hint: Remove the return type specification.".to_string(),
                    "Example: `pub def __init__():`".to_string(),
                ],
            ));
            return_type = FixedSize::unit();
        }

        // `__init__` must be `pub`.
        if !is_public {
            context.add_diagnostic(errors::fancy_error(
                "`__init__` function is not public",
                vec![Label::primary(
                    def.name.span,
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

    Analysis {
        value: Rc::new(FunctionAttributes {
            is_public,
            name: def.name.kind.clone(),
            params,
            return_type,
        }),
        diagnostics: context.diagnostics,
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
    let return_type = &function.typ(db).return_type;
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
        Err(_) => {}
    }
    todo!()
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
