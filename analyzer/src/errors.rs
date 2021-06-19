//! Semantic errors.

use fe_common::diagnostics::{Diagnostic, Label, Severity};
use fe_common::Span;
use std::fmt::Display;

/// Error to be returned from APIs that should reject duplicate definitions
#[derive(Debug)]
pub struct AlreadyDefined;

#[derive(Debug)]
pub struct AlreadyDefined2<T>(pub T); // XXX

/// Error to be returned when otherwise no meaningful information can be returned
#[derive(Debug)]
pub struct FatalError;

/// Error indicating that a value can not move between memory and storage
#[derive(Debug)]
pub struct CannotMove;

/// Error indicating that a value has the wrong type
#[derive(Debug)]
pub struct TypeError;

/// Errors that can result from indexing
#[derive(Debug, PartialEq)]
pub enum IndexingError {
    WrongIndexType,
    NotSubscriptable,
}

/// Errors that can result from a binary operation
#[derive(Debug, PartialEq)]
pub enum BinaryOperationError {
    TypesNotEqual,
    TypesNotNumeric,
    RightTooLarge,
    RightIsSigned,
    NotEqualAndUnsigned,
}

#[derive(Debug)]
pub struct AnalyzerError(pub Vec<Diagnostic>);

pub fn error(message: impl Into<String>, label_span: Span, label: impl Into<String>) -> Diagnostic {
    fancy_error(message, vec![Label::primary(label_span, label)], vec![])
}

pub fn type_error(
    message: impl Into<String>,
    span: Span,
    expected: impl Display,
    actual: impl Display,
) -> Diagnostic {
    error(
        message,
        span,
        format!("this has type `{}`; expected type `{}`", actual, expected),
    )
}

pub fn not_yet_implemented(feature: impl Display, span: Span) -> Diagnostic {
    error(
        "feature not yet implemented",
        span,
        format!("{} is not yet implemented", feature),
    )
}

pub fn fancy_error(
    message: impl Into<String>,
    labels: Vec<Label>,
    notes: Vec<String>,
) -> Diagnostic {
    Diagnostic {
        severity: Severity::Error,
        message: message.into(),
        labels,
        notes,
    }
}
