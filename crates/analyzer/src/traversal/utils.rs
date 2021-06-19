use fe_common::diagnostics::Label;
use fe_parser::ast as fe;
use fe_parser::node::Node;

use crate::context::AnalyzerContext;
use crate::errors::{BinaryOperationError, TypeError};
use crate::namespace::types::{FixedSize, Type};
use std::convert::TryInto;

pub fn types_to_fixed_sizes(sizes: &[Type]) -> Result<Vec<FixedSize>, TypeError> {
    sizes.iter().map(|param| param.clone().try_into()).collect()
}

pub fn add_bin_operations_errors(
    context: &mut dyn AnalyzerContext,
    left: &Node<fe::Expr>,
    right: &Node<fe::Expr>,
    error: BinaryOperationError,
) {
    match error {
        BinaryOperationError::NotEqualAndUnsigned => context.fancy_error(
            "The types for this operation must be equal and unsigned",
            vec![
                Label::primary(left.span, ""),
                Label::primary(right.span, ""),
            ],
            vec![],
        ),
        BinaryOperationError::RightIsSigned => context.fancy_error(
            "The right hand side of this operation must be unsigned",
            vec![Label::primary(right.span, "incompatible signed numeric")],
            vec![],
        ),
        BinaryOperationError::RightTooLarge => context.fancy_error(
            "The right hand side of this operation must fit into the left",
            vec![Label::primary(right.span, "size too large")],
            vec![],
        ),
        BinaryOperationError::TypesNotEqual => context.fancy_error(
            "The types for this operation must be equal",
            vec![
                Label::primary(left.span, ""),
                Label::primary(right.span, ""),
            ],
            vec![],
        ),
        BinaryOperationError::TypesNotNumeric => context.fancy_error(
            "The types for this operation must be numeric",
            vec![
                Label::primary(left.span, ""),
                Label::primary(right.span, ""),
            ],
            vec![],
        ),
    }
}
