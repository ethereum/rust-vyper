use crate::builtins::GlobalMethod;
use crate::db::AnalyzerDb;
use crate::errors::{self, CannotMove};
use crate::namespace::events::EventDef;
use crate::namespace::items::EventId;
use crate::namespace::items::FunctionId;
use crate::namespace::types::{Array, Contract, FixedSize, Struct, Tuple, Type};
pub use fe_common::diagnostics::Label;
use fe_common::diagnostics::{Diagnostic, Severity};
use fe_common::Span;
use fe_parser::ast as fe;
use fe_parser::node::{Node, NodeId};

use std::collections::{BTreeMap, BTreeSet};
use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::rc::Rc;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Analysis<T> {
    pub value: T,
    pub diagnostics: Rc<Vec<Diagnostic>>,
}

pub trait AnalyzerContext {
    fn resolve_type(&self, name: &str) -> Option<Rc<Type>>;
    fn add_diagnostic(&mut self, diag: Diagnostic);

    fn error(&mut self, message: &str, label_span: Span, label: &str) {
        self.fancy_error(message, vec![Label::primary(label_span, label)], vec![])
    }

    fn type_error(
        &mut self,
        message: &str,
        span: Span,
        expected: &dyn Display,
        actual: &dyn Display,
    ) {
        self.error(
            message,
            span,
            &format!("this has type `{}`; expected type `{}`", actual, expected),
        )
    }

    fn not_yet_implemented(&mut self, feature: &str, span: Span) {
        self.error(
            &format!("feature not yet implemented: {}", feature),
            span,
            "not yet implemented".into(),
        )
    }

    fn fancy_error(&mut self, message: &str, labels: Vec<Label>, notes: Vec<String>) {
        self.add_diagnostic(Diagnostic {
            severity: Severity::Error,
            message: message.into(),
            labels,
            notes,
        })
    }
}

/// Indicates where an expression is stored.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Location {
    /// A storage value may not have a nonce known at compile time, so it is
    /// optional.
    Storage {
        nonce: Option<usize>,
    },
    Memory,
    Value,
}

impl Location {
    /// The expected location of a value with the given type when being
    /// assigned, returned, or passed.
    pub fn assign_location(typ: &FixedSize) -> Self {
        match typ {
            FixedSize::Base(_) => Location::Value,
            FixedSize::Contract(_) => Location::Value,
            FixedSize::Array(_) => Location::Memory,
            FixedSize::Tuple(_) => Location::Memory,
            FixedSize::String(_) => Location::Memory,
            FixedSize::Struct(_) => Location::Memory,
        }
    }
}

#[derive(Default, Clone, Debug, PartialEq, Eq)]
pub struct FunctionBody {
    pub expressions: BTreeMap<NodeId, ExpressionAttributes>,
    pub emits: BTreeMap<NodeId, EventId>,
    pub declarations: BTreeMap<NodeId, FixedSize>,
    pub calls: BTreeMap<NodeId, CallType>,
    pub type_descs: BTreeMap<NodeId, Type>,
}

/// Contains contextual information relating to an expression AST node.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ExpressionAttributes {
    pub typ: Type,
    pub location: Location,
    pub move_location: Option<Location>,
}

impl ExpressionAttributes {
    pub fn new(typ: Type, location: Location) -> Self {
        Self {
            typ,
            location,
            move_location: None,
        }
    }

    /// Adds a move to memory.
    pub fn into_cloned(mut self) -> Self {
        self.move_location = Some(Location::Memory);
        self
    }

    /// Adds a move to value, if it is in storage or memory.
    pub fn into_loaded(mut self) -> Result<Self, CannotMove> {
        match self.typ {
            Type::Base(_) => {}
            Type::Contract(_) => {}
            _ => return Err(CannotMove),
        }

        if self.location != Location::Value {
            self.move_location = Some(Location::Value);
        }

        Ok(self)
    }

    /// The final location of an expression after a possible move.
    pub fn final_location(&self) -> Location {
        if let Some(location) = self.move_location {
            return location;
        }
        self.location
    }
}

/// The type of a function call.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CallType {
    BuiltinFunction { func: GlobalMethod },
    TypeConstructor { typ: Type },
    SelfAttribute { func_name: String },
    ValueAttribute,
    TypeAttribute { typ: Type, func_name: String },
}
