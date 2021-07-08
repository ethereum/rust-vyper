use crate::context::AnalyzerContext;
use crate::errors::{AlreadyDefined2, FatalError};
use crate::namespace::scopes::BlockScope;
use crate::namespace::types::{Base, FixedSize};
use crate::traversal::{expressions, types};
use fe_common::diagnostics::Label;
use fe_parser::ast as fe;
use fe_parser::node::Node;
use std::convert::TryFrom;
use std::rc::Rc;

/// Gather context information for var declarations and check for type errors.
pub fn var_decl(scope: &mut BlockScope, stmt: &Node<fe::FuncStmt>) -> Result<(), FatalError> {
    if let fe::FuncStmt::VarDecl { target, typ, value } = &stmt.kind {
        let declared_type = match FixedSize::try_from(types::type_desc(scope, &typ)) {
            Ok(typ) if typ.is_mysterious() => return Err(FatalError),
            Ok(typ) => typ,
            Err(_) => {
                // If this conversion fails, the type must be a map (for now at least)
                scope.error(
                    "invalid variable type".into(),
                    typ.span,
                    "`Map` type can only be used as a contract field".into(),
                );
                return Err(FatalError);
            }
        };

        if let Some(value) = value {
            let value_attributes =
                expressions::assignable_expr(scope, value, Some(&declared_type.clone().into()))?;

            if declared_type != value_attributes.typ {
                scope.type_error(
                    "type mismatch".into(),
                    value.span,
                    &declared_type,
                    &value_attributes.typ,
                );
            }
        }

        add_var(scope, &target, declared_type.clone())?;
        return Ok(());
    }

    unreachable!()
}

/// Add declared variables to the scope.
fn add_var(
    scope: &mut BlockScope,
    target: &Node<fe::VarDeclTarget>,
    typ: FixedSize,
) -> Result<(), FatalError> {
    match (&target.kind, typ) {
        (fe::VarDeclTarget::Name(name), typ) => {
            if let Err(AlreadyDefined2(prev_span)) = scope.add_var(&name, typ, target.span) {
                scope.fancy_error(
                    "duplicate variable definition",
                    // TODO: figure out how to include the previously defined var
                    vec![
                        Label::primary(prev_span, &format!("`{}` first defined here", name)),
                        Label::secondary(target.span, &format!("`{}` redefined here", name)),
                    ],
                    vec![],
                )
            }
            Ok(())
        }
        (fe::VarDeclTarget::Tuple(items), FixedSize::Tuple(items_ty)) => {
            let items_ty = items_ty.items;
            if items.len() != items_ty.as_vec().len() {
                return Err(FatalError);
            }
            for (item, item_ty) in items.iter().zip(items_ty.into_iter()) {
                add_var(scope, item, item_ty)?;
            }
            Ok(())
        }
        _ => Err(FatalError),
    }
}
