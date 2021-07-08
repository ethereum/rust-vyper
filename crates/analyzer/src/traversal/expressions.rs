use crate::context::{AnalyzerContext, CallType, ExpressionAttributes, Location};
use crate::errors::{FatalError, IndexingError, TypeError};
use crate::namespace::scopes::BlockScope;
use crate::namespace::types::{
    Array, Base, Contract, FeString, FixedSize, Integer, Struct, Tuple, Type, TypeDowncast, U256,
};
use crate::operations;
use crate::traversal::utils::{add_bin_operations_errors, types_to_fixed_sizes};

use crate::builtins::{
    BlockField, ChainField, ContractTypeMethod, GlobalMethod, MsgField, Object, SelfField, TxField,
    ValueMethod,
};
use fe_common::numeric;
use fe_common::{diagnostics::Label, utils::humanize::pluralize_conditionally};
use fe_common::{Span, Spanned};
use fe_parser::ast as fe;
use fe_parser::ast::UnaryOperator;
use fe_parser::node::Node;
use num_bigint::BigInt;
use std::convert::TryFrom;
use std::rc::Rc;
use std::str::FromStr;
use vec1::Vec1;

/// Gather context information for expressions and check for type errors.
pub fn expr(
    scope: &mut BlockScope,
    exp: &Node<fe::Expr>,
    expected_type: Option<&Type>,
) -> Result<ExpressionAttributes, FatalError> {
    match &exp.kind {
        fe::Expr::Name(_) => expr_name(scope, exp, expected_type),
        fe::Expr::Num(_) => Ok(expr_num(scope, exp, expected_type.as_int())),
        fe::Expr::Bool(_) => expr_bool(exp),
        fe::Expr::Subscript { .. } => expr_subscript(scope, exp),
        fe::Expr::Attribute { .. } => expr_attribute(scope, exp),
        fe::Expr::Ternary { .. } => expr_ternary(scope, exp),
        fe::Expr::BoolOperation { .. } => expr_bool_operation(scope, exp),
        fe::Expr::BinOperation { .. } => expr_bin_operation(scope, exp, expected_type.as_int()),
        fe::Expr::UnaryOperation { .. } => expr_unary_operation(scope, exp, expected_type),
        fe::Expr::CompOperation { .. } => expr_comp_operation(scope, exp),
        fe::Expr::Call { .. } => expr_call(scope, exp),
        fe::Expr::List { elts } => expr_list(scope, elts, expected_type.as_array()),
        fe::Expr::Tuple { .. } => expr_tuple(scope, exp, expected_type.as_tuple()),
        fe::Expr::Str(_) => expr_str(exp),
        fe::Expr::Unit => Ok(ExpressionAttributes::new(Type::unit(), Location::Value)),
    }
}

pub fn expr_list(
    scope: &mut BlockScope,
    elts: &[Node<fe::Expr>],
    expected_type: Option<&Array>,
) -> Result<ExpressionAttributes, FatalError> {
    if elts.is_empty() {
        return Ok(ExpressionAttributes {
            typ: Type::Array(Array {
                size: 0,
                inner: expected_type.map_or(Base::Unit, |arr| arr.inner),
            }),
            location: Location::Memory,
            move_location: None,
        });
    }

    let inner_type = if let Some(expected) = expected_type {
        for elt in elts {
            let attr = expr(scope, elt, Some(&Type::Base(expected.inner)))?;
            if attr.typ != Type::Base(expected.inner) {
                scope.type_error("type mismatch".into(), elt.span, &expected.inner, &attr.typ);
            }
        }
        expected.inner
    } else {
        let first_attr = expr(scope, &elts[0], None)?;
        let inner = match first_attr.typ {
            Type::Base(base) => base,
            _ => {
                scope.error(
                    "arrays can only hold primitive types",
                    elts[0].span,
                    &format!(
                        "this has type `{}`; expected a primitive type",
                        first_attr.typ
                    ),
                );
                return Err(FatalError);
            }
        };

        // Assuming every element attribute should match the attribute of 0th element
        // of list.
        for elt in &elts[1..] {
            let attr = expr(scope, elt, Some(&first_attr.typ))?;
            if attr.typ != first_attr.typ {
                scope.fancy_error(
                    "array elements must have same type".into(),
                    vec![
                        Label::primary(elts[0].span, format!("this has type `{}`", first_attr.typ)),
                        Label::secondary(elt.span, format!("this has type `{}`", attr.typ)),
                    ],
                    vec![],
                );
            }
        }
        inner
    };

    // TODO: Right now we are only supporting Base type arrays
    // Potential we can support the tuples as well.
    let array_typ = Array {
        size: elts.len(),
        inner: inner_type,
    };

    Ok(ExpressionAttributes {
        typ: Type::Array(array_typ),
        location: Location::Memory,
        move_location: None,
    })
}

/// Gather context information for expressions and check for type errors.
///
/// Also ensures that the expression is on the stack.
pub fn value_expr(
    scope: &mut BlockScope,
    exp: &Node<fe::Expr>,
    expected_type: Option<&Type>,
) -> Result<ExpressionAttributes, FatalError> {
    let original_attributes = expr(scope, exp, expected_type)?;
    let attributes = original_attributes.clone().into_loaded().map_err(|_| {
        scope.fancy_error(
            "can't move value onto stack".into(),
            vec![Label::primary(exp.span, "Value to be moved")],
            vec![format!(
                "Note: Can't move `{}` types on the stack",
                original_attributes.typ
            )],
        );
        FatalError
    })?;

    // XXX scope.update_expression(exp, attributes.clone());

    Ok(attributes)
}

/// Gather context information for expressions and check for type errors.
///
/// Also ensures that the expression is in the type's assigment location.
pub fn assignable_expr(
    scope: &mut BlockScope,
    exp: &Node<fe::Expr>,
    expected_type: Option<&Type>,
) -> Result<ExpressionAttributes, FatalError> {
    use Type::*;

    let mut attributes = expr(scope, exp, expected_type)?;
    match &attributes.typ {
        Base(_) | Contract(_) => {
            if attributes.location != Location::Value {
                attributes.move_location = Some(Location::Value);
            }
        }
        Array(_) | Tuple(_) | String(_) | Struct(_) => {
            if attributes.final_location() != Location::Memory {
                scope.fancy_error(
                    "value must be copied to memory".into(),
                    vec![Label::primary(exp.span, "this value is in storage")],
                    vec!["Hint: values located in storage can be copied to memory using the `to_mem` function.".into(),
                         "Example: `self.my_array.to_mem()`".into(),
                    ],
                );
                attributes.move_location = Some(Location::Memory);
            }
        }
        Map(_) => {
            scope.error(
                "`Map` type cannot reside in memory".into(),
                exp.span,
                "this type can only be used in a contract field".into(),
            );
            return Err(FatalError);
        }
    };
    // XXX scope.update_expression(exp, attributes.clone());

    Ok(attributes)
}

fn expr_tuple(
    scope: &mut BlockScope,
    exp: &Node<fe::Expr>,
    expected_type: Option<&Tuple>,
) -> Result<ExpressionAttributes, FatalError> {
    if let fe::Expr::Tuple { elts } = &exp.kind {
        let types = elts
            .iter()
            .enumerate()
            .map(|(idx, elt)| {
                let exp_type = expected_type
                    .and_then(|tup| tup.items.get(idx))
                    .map(|fixed| fixed.clone().into());
                assignable_expr(scope, elt, exp_type.as_ref()).map(|attributes| attributes.typ)
            })
            .collect::<Result<Vec<_>, _>>()?;

        let tuple_types = match types_to_fixed_sizes(&types) {
            Err(TypeError) => {
                scope.error(
                    "variable size types can not be part of tuples".into(),
                    exp.span,
                    "".into(),
                );
                return Err(FatalError);
            }
            Ok(val) => val,
        };

        let tuple = Tuple {
            items: Vec1::try_from_vec(tuple_types).expect("tuple is empty"),
        };

        Ok(ExpressionAttributes::new(
            Type::Tuple(tuple),
            Location::Memory,
        ))
    } else {
        unreachable!()
    }
}

fn expr_name(
    scope: &mut BlockScope,
    exp: &Node<fe::Expr>,
    expected_type: Option<&Type>,
) -> Result<ExpressionAttributes, FatalError> {
    if let fe::Expr::Name(name) = &exp.kind {
        return match scope.var_type(name) {
            Some(FixedSize::Base(base)) => {
                Ok(ExpressionAttributes::new(Type::Base(base), Location::Value))
            }
            Some(FixedSize::Contract(contract)) => Ok(ExpressionAttributes::new(
                Type::Contract(contract),
                Location::Value,
            )),
            Some(FixedSize::Array(array)) => Ok(ExpressionAttributes::new(
                Type::Array(array),
                Location::Memory,
            )),
            Some(FixedSize::String(string)) => Ok(ExpressionAttributes::new(
                Type::String(string),
                Location::Memory,
            )),
            Some(FixedSize::Tuple(tuple)) => Ok(ExpressionAttributes::new(
                Type::Tuple(tuple),
                Location::Memory,
            )),
            Some(FixedSize::Struct(val)) => Ok(ExpressionAttributes::new(
                Type::Struct(val),
                Location::Memory,
            )),
            None => {
                scope.error(
                    &format!("cannot find value `{}` in this scope", name),
                    exp.span,
                    "undefined".into(),
                );
                match expected_type {
                    Some(typ) => {
                        let fixed_size = match FixedSize::try_from(typ.clone()) {
                            Err(TypeError) => {
                                scope.error(
                                    &format!("Expected type with fixed size but got `{}`", typ),
                                    exp.span,
                                    "wrong type".into(),
                                );
                                return Err(FatalError);
                            }
                            Ok(val) => val,
                        };

                        Ok(ExpressionAttributes::new(
                            typ.clone(),
                            Location::assign_location(&fixed_size),
                        ))
                    }
                    None => Err(FatalError),
                }
            }
        };
    }

    unreachable!()
}

fn expr_str(exp: &Node<fe::Expr>) -> Result<ExpressionAttributes, FatalError> {
    if let fe::Expr::Str(string) = &exp.kind {
        return Ok(ExpressionAttributes::new(
            Type::String(FeString {
                max_size: string.len(),
            }),
            Location::Memory,
        ));
    }

    unreachable!()
}

fn expr_bool(exp: &Node<fe::Expr>) -> Result<ExpressionAttributes, FatalError> {
    if let fe::Expr::Bool(_) = &exp.kind {
        return Ok(ExpressionAttributes::new(
            Type::Base(Base::Bool),
            Location::Value,
        ));
    }

    unreachable!()
}

fn expr_num(
    scope: &mut BlockScope,
    exp: &Node<fe::Expr>,
    expected_type: Option<Integer>,
) -> ExpressionAttributes {
    if let fe::Expr::Num(num) = &exp.kind {
        let int_typ = expected_type.unwrap_or(Integer::U256);
        let num = to_bigint(num);
        validate_numeric_literal_fits_type(scope, num, exp.span, int_typ);
        return ExpressionAttributes::new(Type::int(int_typ), Location::Value);
    }

    unreachable!()
}

fn expr_subscript(
    scope: &mut BlockScope,
    exp: &Node<fe::Expr>,
) -> Result<ExpressionAttributes, FatalError> {
    if let fe::Expr::Subscript { value, index } = &exp.kind {
        let value_attributes = expr(scope, value, None)?;
        let index_attributes = value_expr(scope, index, None)?;

        // performs type checking
        let typ =
            match operations::index(value_attributes.typ.clone(), index_attributes.typ.clone()) {
                Err(IndexingError::NotSubscriptable) => {
                    scope.fancy_error(
                        &format!("`{}` type is not subscriptable", value_attributes.typ),
                        vec![Label::primary(value.span, "unsubscriptable type")],
                        vec!["Note: Only arrays and maps are subscriptable".into()],
                    );
                    return Err(FatalError);
                }
                Err(IndexingError::WrongIndexType) => {
                    scope.fancy_error(
                        &format!(
                            "can not subscript {} with type {}",
                            value_attributes.typ, index_attributes.typ
                        ),
                        vec![Label::primary(index.span, "wrong index type")],
                        vec![],
                    );
                    return Err(FatalError);
                }
                Ok(val) => val,
            };

        let location = match value_attributes.location {
            Location::Storage { .. } => Location::Storage { nonce: None },
            Location::Memory => Location::Memory,
            // neither maps or arrays can be stored as values, so this is unreachable
            Location::Value => unreachable!(),
        };

        return Ok(ExpressionAttributes::new(typ, location));
    }

    unreachable!()
}

fn expr_attribute(
    scope: &mut BlockScope,
    exp: &Node<fe::Expr>,
) -> Result<ExpressionAttributes, FatalError> {
    if let fe::Expr::Attribute { value, attr } = &exp.kind {
        let base_type = |typ| Ok(ExpressionAttributes::new(Type::Base(typ), Location::Value));
        let array_type = |array| {
            Ok(ExpressionAttributes::new(
                Type::Array(array),
                Location::Value,
            ))
        };
        let fatal_err = Err(FatalError);

        // If the value is a name, check if it is a builtin object and attribute.
        if let fe::Expr::Name(name) = &value.kind {
            match Object::from_str(name) {
                Ok(Object::Self_) => return expr_attribute_self(scope, attr),
                Ok(Object::Block) => {
                    return match BlockField::from_str(&attr.kind) {
                        Ok(BlockField::Coinbase) => base_type(Base::Address),
                        Ok(BlockField::Difficulty) => base_type(U256),
                        Ok(BlockField::Number) => base_type(U256),
                        Ok(BlockField::Timestamp) => base_type(U256),
                        Err(_) => {
                            scope.fancy_error(
                                "Not a block field".into(),
                                vec![
                                    Label::primary(
                                        attr.span,
                                        "",
                                    ),
                                ],
                                vec!["Note: Only `coinbase`, `difficulty`, `number` and `timestamp` can be accessed on `block`.".into()],
                            );
                            fatal_err
                        }
                    }
                }
                Ok(Object::Chain) => {
                    return match ChainField::from_str(&attr.kind) {
                        Ok(ChainField::Id) => base_type(U256),
                        Err(_) => {
                            scope.fancy_error(
                                "Not a chain field".into(),
                                vec![Label::primary(attr.span, "")],
                                vec!["Note: Only `id` can be accessed on `chain`.".into()],
                            );
                            fatal_err
                        }
                    }
                }
                Ok(Object::Msg) => {
                    return match MsgField::from_str(&attr.kind) {
                        Ok(MsgField::Sender) => base_type(Base::Address),
                        Ok(MsgField::Sig) => array_type(Array {
                            size: 32,
                            inner: Base::Byte,
                        }),
                        Ok(MsgField::Value) => base_type(U256),
                        Err(_) => {
                            scope.fancy_error(
                                "Not a `msg` field".into(),
                                vec![
                                    Label::primary(
                                        attr.span,
                                        "",
                                    ),
                                ],
                                vec!["Note: Only `sender`, `sig` and `value` can be accessed on `msg`.".into()],
                            );
                            fatal_err
                        }
                    }
                }
                Ok(Object::Tx) => {
                    return match TxField::from_str(&attr.kind) {
                        Ok(TxField::GasPrice) => base_type(U256),
                        Ok(TxField::Origin) => base_type(Base::Address),
                        Err(_) => {
                            scope.fancy_error(
                                "Not a `tx` field".into(),
                                vec![Label::primary(attr.span, "")],
                                vec![
                                    "Note: Only `gas_price` and `origin` can be accessed on `tx`."
                                        .into(),
                                ],
                            );
                            fatal_err
                        }
                    }
                }
                Err(_) => {}
            }
        }

        // We attempt to analyze the value as an expression. If this is successful, we
        // build a new set of attributes from the value attributes.
        let expression_attributes = expr(scope, value, None)?;
        return match expression_attributes {
            // If the value is a struct, we return the type of the attribute. The location stays the
            // same and can be memory or storage.
            ExpressionAttributes {
                typ: Type::Struct(struct_),
                location,
                ..
            } => {
                if let Some(typ) = struct_.get_field_type(&attr.kind) {
                    Ok(ExpressionAttributes::new(typ.to_owned().into(), location))
                } else {
                    scope.fancy_error(
                        &format!(
                            "No field `{}` exists on struct `{}`",
                            &attr.kind, struct_.name
                        ),
                        vec![Label::primary(attr.span, "undefined field")],
                        vec![],
                    );
                    fatal_err
                }
            }
            ExpressionAttributes {
                typ: Type::Tuple(tuple),
                location,
                ..
            } => {
                let item_index = match tuple_item_index(&attr.kind) {
                    Some(index) => index,
                    None => {
                        scope.fancy_error(
                            &format!("No field `{}` exists on this tuple", &attr.kind),
                            vec![
                                Label::primary(
                                    attr.span,
                                    "undefined field",
                                )
                            ],
                            vec!["Note: Tuple values are accessed via `itemN` properties such as `item0` or `item1`".into()],
                        );
                        return fatal_err;
                    }
                };

                if let Some(typ) = tuple.items.get(item_index) {
                    Ok(ExpressionAttributes::new(typ.to_owned().into(), location))
                } else {
                    scope.fancy_error(
                        &format!("No field `item{}` exists on this tuple", item_index),
                        vec![Label::primary(attr.span, "unknown field")],
                        vec![format!(
                            "Note: The highest possible field for this tuple is `item{}`",
                            tuple.items.len() - 1
                        )],
                    );
                    fatal_err
                }
            }
            _ => {
                scope.fancy_error(
                    &format!(
                        "No field `{}` exists on type {}",
                        &attr.kind, expression_attributes.typ
                    ),
                    vec![Label::primary(attr.span, "unknown field")],
                    vec![],
                );
                fatal_err
            }
        };
    }

    unreachable!()
}

/// Pull the item index from the attribute string (e.g. "item4" -> "4").
fn tuple_item_index(item: &str) -> Option<usize> {
    if item.len() < 5 || &item[..4] != "item" {
        None
    } else {
        item[4..].parse::<usize>().ok()
    }
}

fn expr_attribute_self(
    scope: &mut BlockScope,
    attr: &Node<String>,
) -> Result<ExpressionAttributes, FatalError> {
    if let Ok(SelfField::Address) = SelfField::from_str(&attr.kind) {
        return Ok(ExpressionAttributes::new(
            Type::Base(Base::Address),
            Location::Value,
        ));
    }

    match scope.contract_field(&attr.kind) {
        Some((typ, nonce)) => Ok(ExpressionAttributes::new(
            typ.as_ref().clone(),
            Location::Storage { nonce: Some(nonce) },
        )),
        None => {
            scope.fancy_error(
                &format!("No field `{}` exists on this contract", &attr.kind),
                vec![Label::primary(attr.span, "undefined field")],
                vec![],
            );
            Err(FatalError)
        }
    }
}

fn expr_bin_operation(
    scope: &mut BlockScope,
    exp: &Node<fe::Expr>,
    expected_type: Option<Integer>,
) -> Result<ExpressionAttributes, FatalError> {
    if let fe::Expr::BinOperation { left, op, right } = &exp.kind {
        let expected = expected_type.map(Type::int);
        let left_attributes = value_expr(scope, left, expected.as_ref())?;
        let right_attributes = value_expr(scope, right, expected.as_ref())?;

        let typ = match operations::bin(&left_attributes.typ, &op.kind, &right_attributes.typ) {
            Err(err) => {
                add_bin_operations_errors(scope, left, right, err);
                return Err(FatalError);
            }
            Ok(val) => val,
        };

        return Ok(ExpressionAttributes::new(typ, Location::Value));
    }

    unreachable!()
}

fn expr_unary_operation(
    scope: &mut BlockScope,
    exp: &Node<fe::Expr>,
    expected_type: Option<&Type>,
) -> Result<ExpressionAttributes, FatalError> {
    if let fe::Expr::UnaryOperation { op, operand } = &exp.kind {
        let operand_attributes = value_expr(scope, operand, None)?;

        let emit_err = |scope: &mut BlockScope, expected| {
            scope.error(
                &format!(
                    "cannot apply unary operator `{}` to type `{}`",
                    op.kind, operand_attributes.typ
                ),
                operand.span,
                &format!(
                    "this has type `{}`; expected {}",
                    operand_attributes.typ, expected
                ),
            )
        };

        return match op.kind {
            fe::UnaryOperator::USub => {
                let int_type = expected_type.as_int().unwrap_or(Integer::I256);
                match operand_attributes.typ {
                    Type::Base(Base::Numeric(_)) => {
                        if let fe::Expr::Num(num_str) = &operand.kind {
                            let num = -to_bigint(num_str);
                            validate_numeric_literal_fits_type(scope, num, exp.span, int_type);
                        }
                    }
                    _ => emit_err(scope, "a numeric type"),
                }
                Ok(ExpressionAttributes::new(
                    Type::int(int_type),
                    Location::Value,
                ))
            }
            fe::UnaryOperator::Not => {
                if !matches!(operand_attributes.typ, Type::Base(Base::Bool)) {
                    emit_err(scope, "type `bool`");
                }
                Ok(ExpressionAttributes::new(
                    Type::Base(Base::Bool),
                    Location::Value,
                ))
            }
            UnaryOperator::Invert => {
                scope.not_yet_implemented("unary invert", exp.span);
                Ok(ExpressionAttributes::new(Type::unit(), Location::Value))
            }
        };
    }

    unreachable!()
}

fn expr_call(
    scope: &mut BlockScope,
    exp: &Node<fe::Expr>,
) -> Result<ExpressionAttributes, FatalError> {
    if let fe::Expr::Call {
        func,
        generic_args,
        args,
    } = &exp.kind
    {
        return match expr_call_type(scope, func, generic_args.as_ref())? {
            CallType::BuiltinFunction { func: builtin } => {
                expr_call_builtin_function(scope, builtin, func.span, args)
            }
            CallType::TypeConstructor { typ } => {
                expr_call_type_constructor(scope, func.span, typ, args)
            }
            CallType::SelfAttribute { func_name } => {
                expr_call_self_attribute(scope, &func_name, func.span, args)
            }
            CallType::ValueAttribute => expr_call_value_attribute(scope, func, args),
            CallType::TypeAttribute { typ, func_name } => {
                expr_call_type_attribute(scope, typ, &func_name, func.span, args)
            }
        };
    }

    unreachable!()
}

fn expr_call_builtin_function(
    scope: &mut BlockScope,
    typ: GlobalMethod,
    name_span: Span,
    args: &Node<Vec<Node<fe::CallArg>>>,
) -> Result<ExpressionAttributes, FatalError> {
    let argument_attributes = expr_call_args(scope, args)?;
    match typ {
        GlobalMethod::Keccak256 => {
            validate_arg_count(scope, typ.into(), name_span, args, 1);
            validate_arg_labels(scope, args, &[None]);

            // We only need to return with a FatalError here because the error is already reported
            // as part of a different check on function arguments.
            let arg_typ = argument_attributes
                .first()
                .map(|attr| &attr.typ)
                .ok_or(FatalError)?;

            if !matches!(
                arg_typ,
                Type::Array(Array {
                    inner: Base::Byte,
                    ..
                })
            ) {
                scope.fancy_error(
                    &format!(
                        "`{}` can not be used as a parameter to `keccak(..)`",
                        arg_typ
                    ),
                    vec![Label::primary(args.span, "wrong type")],
                    vec!["Note: keccak(..) expects a byte array as parameter".into()],
                );
            }
            Ok(ExpressionAttributes::new(Type::Base(U256), Location::Value))
        }
    }
}

pub fn validate_arg_count(
    context: &mut dyn AnalyzerContext,
    name: &str,
    name_span: Span,
    args: &Node<Vec<impl Spanned>>,
    param_count: usize,
) {
    if args.kind.len() != param_count {
        let mut labels = vec![Label::primary(
            name_span,
            format!(
                "expects {} {}",
                param_count,
                pluralize_conditionally("argument", param_count)
            ),
        )];
        if args.kind.is_empty() {
            labels.push(Label::secondary(args.span, "supplied 0 arguments"));
        } else {
            for arg in &args.kind {
                labels.push(Label::secondary(arg.span(), ""));
            }
            labels.last_mut().unwrap().message = format!(
                "supplied {} {}",
                args.kind.len(),
                pluralize_conditionally("argument", args.kind.len())
            );
        }

        context.fancy_error(
            &format!(
                "`{}` expects {} {}, but {} {} provided",
                name,
                param_count,
                pluralize_conditionally("argument", param_count),
                args.kind.len(),
                pluralize_conditionally(("was", "were"), args.kind.len())
            ),
            labels,
            vec![],
        );
        // TODO: add `defined here` label (need span for definition)
    }
}

pub fn validate_arg_labels(
    scope: &mut BlockScope,
    args: &Node<Vec<Node<fe::CallArg>>>,
    labels: &[Option<&str>],
) {
    for (expected_label, arg) in labels.iter().zip(args.kind.iter()) {
        let arg_val = &arg.kind.value;
        match (expected_label, &arg.kind.label) {
            (Some(expected_label), Some(actual_label)) => {
                if *expected_label != actual_label.kind {
                    let notes = if labels
                        .iter()
                        .any(|nm| nm == &Some(actual_label.kind.as_str()))
                    {
                        vec!["Note: arguments must be provided in order.".into()]
                    } else {
                        vec![]
                    };
                    scope.fancy_error(
                        "argument label mismatch".into(),
                        vec![Label::primary(
                            actual_label.span,
                            format!("expected `{}`", expected_label),
                        )],
                        notes,
                    );
                }
            }
            (Some(expected_label), None) => match &arg_val.kind {
                fe::Expr::Name(var_name) if var_name == *expected_label => {}
                _ => {
                    scope.fancy_error(
                        "missing argument label".into(),
                        vec![Label::primary(
                            Span::new(arg_val.span.start, arg_val.span.start),
                            format!("add `{}=` here", expected_label),
                        )],
                        vec![format!(
                            "Note: this label is optional if the argument is a variable named `{}`.",
                            expected_label
                        )],
                    );
                }
            },
            (None, Some(actual_label)) => {
                scope.error(
                    "argument should not be labeled".into(),
                    actual_label.span,
                    "remove this label".into(),
                );
            }
            (None, None) => {}
        }
    }
}

pub fn validate_arg_types(
    scope: &mut BlockScope,
    name: &str,
    args: &Node<Vec<Node<fe::CallArg>>>,
    params: &[(String, FixedSize)],
) -> Result<(), FatalError> {
    for ((label, param_type), arg) in params.iter().zip(args.kind.iter()) {
        let val_attrs = assignable_expr(scope, &arg.kind.value, Some(&param_type.clone().into()))?;
        if param_type != &val_attrs.typ {
            scope.type_error(
                &format!("incorrect type for `{}` argument `{}`", name, label),
                arg.kind.value.span,
                param_type,
                &val_attrs.typ,
            );
        }
    }
    Ok(())
}

pub fn validate_named_args(
    scope: &mut BlockScope,
    name: &str,
    name_span: Span,
    args: &Node<Vec<Node<fe::CallArg>>>,
    params: &[(String, FixedSize)],
) -> Result<(), FatalError> {
    validate_arg_count(scope, name, name_span, args, params.len());
    validate_arg_labels(
        scope,
        args,
        &params
            .iter()
            .map(|(nm, _)| Some(nm.as_str()))
            .collect::<Vec<_>>(),
    );
    validate_arg_types(scope, name, args, params)?;
    Ok(())
}

fn expr_call_struct_constructor(
    scope: &mut BlockScope,
    name_span: Span,
    typ: Struct,
    args: &Node<Vec<Node<fe::CallArg>>>,
) -> Result<ExpressionAttributes, FatalError> {
    validate_named_args(scope, &typ.name, name_span, &args, &typ.fields)?;

    Ok(ExpressionAttributes::new(
        Type::Struct(typ),
        Location::Memory,
    ))
}

fn expr_call_type_constructor(
    scope: &mut BlockScope,
    name_span: Span,
    typ: Type,
    args: &Node<Vec<Node<fe::CallArg>>>,
) -> Result<ExpressionAttributes, FatalError> {
    if let Type::Struct(struct_type) = typ {
        return expr_call_struct_constructor(scope, name_span, struct_type, args);
    }

    // These all expect 1 arg, for now.
    validate_arg_count(scope, &format!("{}", typ), name_span, args, 1);
    validate_arg_labels(scope, args, &[None]);

    match &typ {
        Type::String(string_type) => {
            if let Some(arg) = args.kind.first() {
                assignable_expr(scope, &arg.kind.value, None)?;
                validate_str_literal_fits_type(scope, &arg.kind.value, string_type);
            }
            Ok(ExpressionAttributes::new(typ, Location::Memory))
        }
        Type::Contract(_) => {
            if let Some(arg) = args.kind.first() {
                let arg_attr = assignable_expr(scope, &arg.kind.value, None)?;
                if arg_attr.typ != Type::Base(Base::Address) {
                    scope.type_error("type mismatch".into(), arg.span, &Base::Address, &typ);
                }
            }
            Ok(ExpressionAttributes::new(typ, Location::Value))
        }
        Type::Base(Base::Numeric(_)) => {
            if let Some(arg) = args.kind.first() {
                // This will check if the literal fits inside int_type.
                assignable_expr(scope, &arg.kind.value, Some(&typ))?;
                validate_is_numeric_literal(scope, &arg.kind.value);
            }
            Ok(ExpressionAttributes::new(typ, Location::Value))
        }
        Type::Base(Base::Address) => {
            if let Some(arg) = args.kind.first() {
                let arg_attr = assignable_expr(scope, &arg.kind.value, None)?;
                match arg_attr.typ {
                    Type::Contract(_)
                    | Type::Base(Base::Numeric(_))
                    | Type::Base(Base::Address) => {}
                    _ => {
                        scope.fancy_error(
                            &format!("`{}` can not be used as a parameter to `address(..)`", arg_attr.typ),
                            vec![Label::primary(arg.span, "wrong type")],
                            vec!["Note: address(..) expects a parameter of a contract type, numeric or address".into()],
                        );
                    }
                }
            };
            Ok(ExpressionAttributes::new(
                Type::Base(Base::Address),
                Location::Value,
            ))
        }
        _ => Err(FatalError),
    }
}

fn expr_call_args(
    scope: &mut BlockScope,
    args: &Node<Vec<Node<fe::CallArg>>>,
) -> Result<Vec<ExpressionAttributes>, FatalError> {
    args.kind
        .iter()
        .map(|arg| assignable_expr(scope, &arg.kind.value, None))
        .collect::<Result<Vec<_>, _>>()
}

fn expr_call_self_attribute(
    scope: &mut BlockScope,
    func_name: &str,
    name_span: Span,
    args: &Node<Vec<Node<fe::CallArg>>>,
) -> Result<ExpressionAttributes, FatalError> {
    if let Some(func) = scope.contract_function(func_name) {
        let sig = &func.signature;
        validate_arg_count(scope, func_name, name_span, args, sig.params.len());
        validate_arg_types(scope, func_name, args, &sig.params)?;

        let return_location = match &sig.return_type {
            FixedSize::Base(_) => Location::Value,
            _ => Location::Memory,
        };
        Ok(ExpressionAttributes::new(
            sig.return_type.clone().into(),
            return_location,
        ))
    } else {
        scope.fancy_error(
            &format!("no function `{}` exists this contract", func_name),
            vec![Label::primary(name_span, "undefined function")],
            vec![],
        );
        Err(FatalError)
    }
}

fn expr_call_value_attribute(
    scope: &mut BlockScope,
    func: &Node<fe::Expr>,
    args: &Node<Vec<Node<fe::CallArg>>>,
) -> Result<ExpressionAttributes, FatalError> {
    if let fe::Expr::Attribute { value, attr } = &func.kind {
        let value_attributes = expr(scope, &value, None)?;

        if let Type::Contract(contract) = &value_attributes.typ {
            // We must ensure the expression is loaded onto the stack.
            let expression = value_attributes.clone().into_loaded().map_err(|_| {
                // TODO: Add test code that triggers this
                scope.fancy_error(
                    "can't move value onto stack".into(),
                    vec![Label::primary(value.span, "Value to be moved")],
                    vec![format!(
                        "Note: Can't move `{}` types on the stack",
                        value_attributes.typ
                    )],
                );
                FatalError
            })?;

            // XXX scope.update_expression(value, expression);
            return expr_call_contract_attribute(
                scope,
                contract.to_owned(),
                &attr.kind,
                attr.span,
                args,
            );
        }

        // for now all of these function expect 0 arguments
        validate_arg_count(scope, &attr.kind, attr.span, args, 0);

        return match ValueMethod::from_str(&attr.kind) {
            Err(_) => {
                scope.fancy_error(
                    &format!(
                        "No function `{}` exists on type `{}`",
                        &attr.kind, &value_attributes.typ
                    ),
                    vec![Label::primary(attr.span, "undefined function")],
                    vec![],
                );
                return Err(FatalError);
            }
            Ok(ValueMethod::Clone) => {
                match value_attributes.location {
                    Location::Storage { .. } => {
                        scope.fancy_error(
                            "`clone()` called on value in storage".into(),
                            vec![
                                Label::primary(value.span, "this value is in storage"),
                                Label::secondary(attr.span, "hint: try `to_mem()` here"),
                            ],
                            vec![],
                        );
                    }
                    Location::Value => {
                        scope.fancy_error(
                            "`clone()` called on primitive type".into(),
                            vec![
                                Label::primary(value.span, "this value does not need to be cloned"),
                                Label::secondary(attr.span, "hint: remove `.clone()`"),
                            ],
                            vec![],
                        );
                    }
                    Location::Memory => {}
                }
                Ok(value_attributes.into_cloned())
            }
            Ok(ValueMethod::ToMem) => {
                match value_attributes.location {
                    Location::Storage { .. } => {}
                    Location::Value => {
                        scope.fancy_error(
                            "`to_mem()` called on primitive type".into(),
                            vec![
                                Label::primary(
                                    value.span,
                                    "this value does not need to be copied to memory",
                                ),
                                Label::secondary(attr.span, "hint: remove `.to_mem()`"),
                            ],
                            vec![],
                        );
                    }
                    Location::Memory => {
                        scope.fancy_error(
                            "`to_mem()` called on value in memory".into(),
                            vec![
                                Label::primary(value.span, "this value is in storage"),
                                Label::secondary(
                                    attr.span,
                                    "hint: to make a copy, use `.to_mem()` here",
                                ),
                            ],
                            vec![],
                        );
                    }
                }
                Ok(value_attributes.into_cloned())
            }
            Ok(ValueMethod::AbiEncode) => match &value_attributes.typ {
                Type::Struct(struct_) => {
                    if value_attributes.final_location() != Location::Memory {
                        scope.fancy_error(
                            "value must be copied to memory".into(),
                            vec![Label::primary(value.span, "this value is in storage")],
                            vec!["Hint: values located in storage can be copied to memory using the `to_mem` function.".into(),
                                 "Example: `self.my_array.to_mem().abi_encode()`".into(),
                            ],
                        );
                    }

                    Ok(ExpressionAttributes::new(
                        Type::Array(Array {
                            inner: Base::Byte,
                            size: struct_.fields.len() * 32,
                        }),
                        Location::Memory,
                    ))
                }
                Type::Tuple(tuple) => {
                    if value_attributes.final_location() != Location::Memory {
                        scope.fancy_error(
                            "value must be copied to memory".into(),
                            vec![Label::primary(value.span, "this value is in storage")],
                            vec!["Hint: values located in storage can be copied to memory using the `to_mem` function.".into(),
                                 "Example: `self.my_array.to_mem().abi_encode()`".into(),
                            ],
                        );
                    }

                    Ok(ExpressionAttributes::new(
                        Type::Array(Array {
                            inner: Base::Byte,
                            size: tuple.items.len() * 32,
                        }),
                        Location::Memory,
                    ))
                }
                _ => {
                    scope.fancy_error(
                        &format!(
                            "value of type {} does not support `abi_encode()`",
                            value_attributes.typ
                        ),
                        vec![Label::primary(
                            value.span,
                            "this value cannot be encoded using `abi_encode()`",
                        )],
                        vec![
                            "Hint: struct and tuple values can be encoded.".into(),
                            "Example: `(42,).abi_encode()`".into(),
                        ],
                    );

                    Ok(ExpressionAttributes::new(Type::unit(), Location::Value))
                }
            },
        };
    }

    unreachable!()
}

fn expr_call_type_attribute(
    scope: &mut BlockScope,
    typ: Type,
    func_name: &str,
    name_span: Span,
    args: &Node<Vec<Node<fe::CallArg>>>,
) -> Result<ExpressionAttributes, FatalError> {
    let arg_attributes = expr_call_args(scope, args)?;
    let contract_name = scope.contract_name();

    let report_circular_dependency = |scope: &mut BlockScope, method: String| {
        scope.fancy_error(
            &format!("`{contract}.{}(...)` called within `{contract}` creates an illegal circular dependency", method, contract=contract_name),
            vec![Label::primary(name_span, "Contract creation")],
            vec![format!("Note: Consider using a dedicated factory contract to create instances of `{}`", contract_name)]);
    };

    match (typ.clone(), ContractTypeMethod::from_str(func_name)) {
        (Type::Contract(contract), Ok(ContractTypeMethod::Create2)) => {
            validate_arg_count(scope, func_name, name_span, args, 2);

            if contract_name == contract.name {
                report_circular_dependency(scope, ContractTypeMethod::Create2.to_string());
            }

            if !matches!(
                (&arg_attributes[0].typ, &arg_attributes[1].typ),
                (Type::Base(Base::Numeric(_)), Type::Base(Base::Numeric(_)))
            ) {
                scope.fancy_error(
                    "function `create2` expects numeric parameters".into(),
                    vec![Label::primary(args.span, "invalid argument")],
                    vec![],
                );
            }
            Ok(ExpressionAttributes::new(
                Type::Contract(contract),
                Location::Value,
            ))
        }
        (Type::Contract(contract), Ok(ContractTypeMethod::Create)) => {
            validate_arg_count(scope, func_name, name_span, args, 1);

            if contract_name == contract.name {
                report_circular_dependency(scope, ContractTypeMethod::Create.to_string());
            }

            if !matches!(&arg_attributes[0].typ, Type::Base(Base::Numeric(_))) {
                scope.fancy_error(
                    "function `create` expects numeric parameter".into(),
                    vec![Label::primary(args.span, "invalid argument")],
                    vec![],
                );
            }

            Ok(ExpressionAttributes::new(
                Type::Contract(contract),
                Location::Value,
            ))
        }
        _ => {
            scope.fancy_error(
                &format!("No function `{}` exists on type `{}`", func_name, &typ),
                vec![Label::primary(name_span, "undefined function")],
                vec![],
            );
            Err(FatalError)
        }
    }
}

fn expr_call_contract_attribute(
    scope: &mut BlockScope,
    contract: Contract,
    func_name: &str,
    name_span: Span,
    args: &Node<Vec<Node<fe::CallArg>>>,
) -> Result<ExpressionAttributes, FatalError> {
    if let Some(function) = contract.functions.get(func_name) {
        let sig = &function.signature;
        let return_type = sig.return_type.to_owned();

        validate_arg_count(scope, func_name, name_span, args, sig.params.len());
        validate_arg_types(scope, func_name, args, &sig.params)?;

        Ok(ExpressionAttributes::new(
            return_type.clone().into(),
            Location::assign_location(&return_type),
        ))
    } else {
        scope.fancy_error(
            &format!(
                "No function `{}` exists on contract `{}`",
                func_name, contract.name
            ),
            vec![Label::primary(name_span, "undefined function")],
            vec![],
        );
        Err(FatalError)
    }
}

fn expr_call_type(
    scope: &mut BlockScope,
    func: &Node<fe::Expr>,
    generic_args: Option<&Node<Vec<fe::GenericArg>>>,
) -> Result<CallType, FatalError> {
    let call_type = match &func.kind {
        fe::Expr::Name(name) => expr_name_call_type(scope, name, func.span, generic_args),
        fe::Expr::Attribute { .. } => expr_attribute_call_type(scope, func),
        _ => {
            let expression = expr(scope, func, None)?;
            scope.fancy_error(
                &format!("the {} type is not callable", expression.typ),
                vec![Label::primary(
                    func.span,
                    format!("this has type {}", expression.typ),
                )],
                vec![],
            );
            Err(FatalError)
        }
    }?;

    // XXX scope.add_call(func, call_type.clone());
    Ok(call_type)
}

fn expr_name_call_type(
    scope: &mut BlockScope,
    name: &str,
    name_span: Span,
    generic_args: Option<&Node<Vec<fe::GenericArg>>>,
) -> Result<CallType, FatalError> {
    match (name, generic_args) {
        ("keccak256", _) => Ok(CallType::BuiltinFunction {
            func: GlobalMethod::Keccak256,
        }),
        ("address", _) => Ok(CallType::TypeConstructor {
            typ: Type::Base(Base::Address),
        }),
        ("u256", _) => Ok(CallType::TypeConstructor {
            typ: Type::Base(Base::Numeric(Integer::U256)),
        }),
        ("u128", _) => Ok(CallType::TypeConstructor {
            typ: Type::Base(Base::Numeric(Integer::U128)),
        }),
        ("u64", _) => Ok(CallType::TypeConstructor {
            typ: Type::Base(Base::Numeric(Integer::U64)),
        }),
        ("u32", _) => Ok(CallType::TypeConstructor {
            typ: Type::Base(Base::Numeric(Integer::U32)),
        }),
        ("u16", _) => Ok(CallType::TypeConstructor {
            typ: Type::Base(Base::Numeric(Integer::U16)),
        }),
        ("u8", _) => Ok(CallType::TypeConstructor {
            typ: Type::Base(Base::Numeric(Integer::U8)),
        }),
        ("i256", _) => Ok(CallType::TypeConstructor {
            typ: Type::Base(Base::Numeric(Integer::I256)),
        }),
        ("i128", _) => Ok(CallType::TypeConstructor {
            typ: Type::Base(Base::Numeric(Integer::I128)),
        }),
        ("i64", _) => Ok(CallType::TypeConstructor {
            typ: Type::Base(Base::Numeric(Integer::I64)),
        }),
        ("i32", _) => Ok(CallType::TypeConstructor {
            typ: Type::Base(Base::Numeric(Integer::I32)),
        }),
        ("i16", _) => Ok(CallType::TypeConstructor {
            typ: Type::Base(Base::Numeric(Integer::I16)),
        }),
        ("i8", _) => Ok(CallType::TypeConstructor {
            typ: Type::Base(Base::Numeric(Integer::I8)),
        }),
        ("String", Some(Node { kind: args, .. })) => match &args[..] {
            [fe::GenericArg::Int(len)] => Ok(CallType::TypeConstructor {
                typ: Type::String(FeString { max_size: len.kind }),
            }),
            _ => Err(FatalError),
        },
        (value, _) => {
            if let Some(typ) = scope.resolve_type(value) {
                Ok(CallType::TypeConstructor {
                    typ: typ.as_ref().clone(),
                })
            } else {
                scope.error("undefined function".into(), name_span, "undefined".into());
                Err(FatalError)
            }
        }
    }
}

fn expr_attribute_call_type(
    scope: &mut BlockScope,
    exp: &Node<fe::Expr>,
) -> Result<CallType, FatalError> {
    if let fe::Expr::Attribute { value, attr } = &exp.kind {
        if let fe::Expr::Name(name) = &value.kind {
            match Object::from_str(&name) {
                Ok(Object::Block) | Ok(Object::Chain) | Ok(Object::Msg) | Ok(Object::Tx) => {
                    scope.fancy_error(
                        &format!("no function `{}` exists on builtin `{}`", &attr.kind, &name),
                        vec![Label::primary(exp.span, "undefined function")],
                        vec![],
                    );

                    return Err(FatalError);
                }
                Ok(Object::Self_) => {
                    return Ok(CallType::SelfAttribute {
                        func_name: attr.kind.to_string(),
                    })
                }
                Err(_) => {}
            }

            if let Some(typ) = scope.resolve_type(&name) {
                return Ok(CallType::TypeAttribute {
                    typ: typ.as_ref().clone(),
                    func_name: attr.kind.to_string(),
                });
            }
        };

        return Ok(CallType::ValueAttribute);
    }

    unreachable!()
}

fn validate_is_numeric_literal(scope: &mut BlockScope, value: &Node<fe::Expr>) -> Option<BigInt> {
    if let fe::Expr::UnaryOperation { operand, op: _ } = &value.kind {
        if let fe::Expr::Num(num) = &operand.kind {
            return Some(-to_bigint(num));
        }
    } else if let fe::Expr::Num(num) = &value.kind {
        return Some(to_bigint(num));
    }
    scope.error(
        "type mismatch".into(),
        value.span,
        "expected a number literal".into(),
    );
    None
}

fn validate_numeric_literal_fits_type(
    scope: &mut BlockScope,
    num: BigInt,
    span: Span,
    int_type: Integer,
) {
    if !int_type.fits(num) {
        scope.error(
            &format!("literal out of range for `{}`", int_type),
            span,
            &format!("does not fit into type `{}`", int_type),
        );
    }
}

fn validate_str_literal_fits_type(
    scope: &mut BlockScope,
    arg_val: &Node<fe::Expr>,
    typ: &FeString,
) {
    if let fe::Expr::Str(string) = &arg_val.kind {
        if string.len() > typ.max_size {
            scope.error(
                "string capacity exceeded".into(),
                arg_val.span,
                &format!(
                    "this string has length {}; expected length <= {}",
                    string.len(),
                    typ.max_size
                ),
            );
        }
    } else {
        scope.error(
            "type mismatch".into(),
            arg_val.span,
            "expected a string literal".into(),
        );
    }
}

fn expr_comp_operation(
    scope: &mut BlockScope,
    exp: &Node<fe::Expr>,
) -> Result<ExpressionAttributes, FatalError> {
    if let fe::Expr::CompOperation { left, op, right } = &exp.kind {
        // comparison operands should be moved to the stack
        let left_attr = value_expr(scope, left, None)?;
        let right_attr = value_expr(scope, right, Some(&left_attr.typ))?;

        if left_attr.typ != right_attr.typ {
            scope.fancy_error(
                &format!("`{}` operands must have the same type", op.kind),
                vec![
                    Label::primary(left.span, format!("this has type `{}`", left_attr.typ)),
                    Label::secondary(
                        right.span,
                        format!("this has incompatible type `{}`", right_attr.typ),
                    ),
                ],
                vec![],
            );
        }

        // for now we assume these are the only possible attributes
        return Ok(ExpressionAttributes::new(
            Type::Base(Base::Bool),
            Location::Value,
        ));
    }

    unreachable!()
}

fn expr_ternary(
    scope: &mut BlockScope,
    exp: &Node<fe::Expr>,
) -> Result<ExpressionAttributes, FatalError> {
    if let fe::Expr::Ternary {
        if_expr,
        test,
        else_expr,
    } = &exp.kind
    {
        // test attributes should be stored as a value
        let test_attributes = value_expr(scope, test, None)?;
        // the return expressions should be stored in their default locations
        //
        // If, for example, one of the expressions is stored in memory and the other is
        // stored in storage, it's necessary that we move them to the same location.
        // This could be memory or the stack, depending on the type.
        let if_expr_attributes = assignable_expr(scope, if_expr, None)?;
        let else_expr_attributes =
            assignable_expr(scope, else_expr, Some(&if_expr_attributes.typ))?;

        // Make sure the `test_attributes` is a boolean type.
        if Type::Base(Base::Bool) != test_attributes.typ {
            scope.error(
                "`if` test expression must be a `bool`".into(),
                test.span,
                &format!("this has type `{}`; expected `bool`", test_attributes.typ),
            );
        }
        // Should have the same return Type
        if if_expr_attributes.typ != else_expr_attributes.typ {
            scope.fancy_error(
                "`if` and `else` values must have same type".into(),
                vec![
                    Label::primary(
                        if_expr.span,
                        format!("this has type `{}`", if_expr_attributes.typ),
                    ),
                    Label::secondary(
                        else_expr.span,
                        format!("this has type `{}`", else_expr_attributes.typ),
                    ),
                ],
                vec![],
            );
        }

        let loc = if_expr_attributes.final_location();
        return Ok(ExpressionAttributes::new(if_expr_attributes.typ, loc));
    }
    unreachable!()
}

fn expr_bool_operation(
    scope: &mut BlockScope,
    exp: &Node<fe::Expr>,
) -> Result<ExpressionAttributes, FatalError> {
    if let fe::Expr::BoolOperation { left, op, right } = &exp.kind {
        for operand in &[left, right] {
            let attributes = value_expr(scope, operand, None)?;
            if attributes.typ != Type::Base(Base::Bool) {
                scope.error(
                    &format!("binary op `{}` operands must have type `bool`", op.kind),
                    operand.span,
                    &format!("this has type `{}`; expected `bool`", attributes.typ),
                );
            }
        }
        return Ok(ExpressionAttributes::new(
            Type::Base(Base::Bool),
            Location::Value,
        ));
    }

    unreachable!()
}

/// Converts a input string to `BigInt`.
///
/// # Panics
/// Panics if `num` contains invalid digit.
fn to_bigint(num: &str) -> BigInt {
    numeric::Literal::new(num)
        .parse::<BigInt>()
        .expect("the numeric literal contains a invalid digit")
}
