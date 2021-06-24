use crate::errors::{BinaryOperationError, IndexingError};
use crate::namespace::types::{Array, Base, Map, Type, U256};

use fe_parser::ast as fe;

/// Finds the type of an index operation and checks types.
///
/// e.g. `foo[42]`
pub fn index(value: Type, index: Type) -> Result<Type, IndexingError> {
    match value {
        Type::Array(array) => index_array(array, index),
        Type::Map(map) => index_map(map, index),
        Type::Base(Base::Unknown) => Ok(Type::unknown()),
        Type::Base(_) => Err(IndexingError::NotSubscriptable),
        Type::Tuple(_) => Err(IndexingError::NotSubscriptable),
        Type::String(_) => Err(IndexingError::NotSubscriptable),
        Type::Contract(_) => Err(IndexingError::NotSubscriptable),
        Type::Struct(_) => Err(IndexingError::NotSubscriptable),
    }
}

fn index_array(array: Array, index: Type) -> Result<Type, IndexingError> {
    if index != Type::Base(U256) {
        return Err(IndexingError::WrongIndexType);
    }

    Ok(Type::Base(array.inner))
}

fn index_map(map: Map, index: Type) -> Result<Type, IndexingError> {
    if index != Type::Base(map.key) {
        return Err(IndexingError::WrongIndexType);
    }

    Ok(*map.value)
}

/// Finds the type of a binary operation and checks types.
pub fn bin(left: &Type, op: &fe::BinOperator, right: &Type) -> Result<Type, BinaryOperationError> {
    match op {
        fe::BinOperator::Add
        | fe::BinOperator::Sub
        | fe::BinOperator::Mult
        | fe::BinOperator::Div
        | fe::BinOperator::Mod => bin_arithmetic(left, right),
        fe::BinOperator::Pow => bin_pow(left, right),
        fe::BinOperator::LShift | fe::BinOperator::RShift => bin_bit_shift(left, right),
        fe::BinOperator::BitOr | fe::BinOperator::BitXor | fe::BinOperator::BitAnd => {
            bin_bit(left, right)
        }
    }
}

fn bin_arithmetic(left: &Type, right: &Type) -> Result<Type, BinaryOperationError> {
    if let (Type::Base(Base::Numeric(left)), Type::Base(Base::Numeric(right))) = (left, right) {
        if left == right {
            // For now, we require that the types be numeric and equal. In the future, we
            // will want to loosen this up such that arithmetic operations can be performed
            // on different types.
            //
            // The rules should be:
            // - Any combination of numeric types can be operated on.
            // - If either number is signed, we return a signed type.
            // - The larger type is returned.
            Ok(Type::Base(Base::Numeric(left.to_owned())))
        } else {
            // The types are not equal. Again, there is no need to be this strict.
            Err(BinaryOperationError::TypesNotEqual)
        }
    } else {
        Err(BinaryOperationError::TypesNotNumeric)
    }
}

fn bin_pow(left: &Type, right: &Type) -> Result<Type, BinaryOperationError> {
    if let (Type::Base(Base::Numeric(left)), Type::Base(Base::Numeric(right))) = (left, right) {
        // The exponent is not allowed to be a signed integer. To allow calculations
        // such as -2 ** 3 we allow the right hand side to be an unsigned integer
        // even if the left side is a signed integer. It is allowed as long as the
        // right side is the same size or smaller than the left side (e.g. i16 ** u16
        // but not i16 ** u32). The type of the result will be the type of the left
        // side and under/overflow checks are based on that type.
        if right.is_signed() {
            Err(BinaryOperationError::RightIsSigned)
        } else if left.can_hold(&right) {
            Ok(Type::Base(Base::Numeric(left.to_owned())))
        } else {
            Err(BinaryOperationError::RightTooLarge)
        }
    } else {
        Err(BinaryOperationError::TypesNotNumeric)
    }
}

fn bin_bit_shift(left: &Type, right: &Type) -> Result<Type, BinaryOperationError> {
    if let (Type::Base(Base::Numeric(left)), Type::Base(Base::Numeric(right))) = (left, right) {
        // The right side must be unsigned.
        if !right.is_signed() {
            Ok(Type::Base(Base::Numeric(left.to_owned())))
        } else {
            Err(BinaryOperationError::RightIsSigned)
        }
    } else {
        Err(BinaryOperationError::TypesNotNumeric)
    }
}

fn bin_bit(left: &Type, right: &Type) -> Result<Type, BinaryOperationError> {
    if let (Type::Base(Base::Numeric(left)), Type::Base(Base::Numeric(right))) = (left, right) {
        // We require that both numbers be unsigned and equal in size.
        if !left.is_signed() && left == right {
            Ok(Type::Base(Base::Numeric(left.to_owned())))
        } else {
            Err(BinaryOperationError::NotEqualAndUnsigned)
        }
    } else {
        Err(BinaryOperationError::TypesNotNumeric)
    }
}

#[cfg(test)]
mod tests {
    use crate::errors::IndexingError;
    use crate::namespace::types::{Array, Base, Map, Type, U256};
    use crate::operations;
    use rstest::rstest;

    const U256_ARRAY_TYPE: Type = Type::Array(Array {
        inner: U256,
        size: 100,
    });
    const U256_TYPE: Type = Type::Base(U256);
    const BOOL_TYPE: Type = Type::Base(Base::Bool);

    fn u256_bool_map() -> Type {
        Type::Map(Map {
            key: U256,
            value: Box::new(Type::Base(Base::Bool)),
        })
    }

    #[rstest(
        value,
        index,
        expected,
        case(U256_ARRAY_TYPE, U256_TYPE, U256_TYPE),
        case(u256_bool_map(), U256_TYPE, BOOL_TYPE)
    )]
    fn basic_index(value: Type, index: Type, expected: Type) {
        let actual = operations::index(value, index).expect("failed to get expected type");
        assert_eq!(actual, expected)
    }

    #[rstest(
        value,
        index,
        case(U256_ARRAY_TYPE, BOOL_TYPE),
        case(u256_bool_map(), BOOL_TYPE),
        case(u256_bool_map(), U256_ARRAY_TYPE)
    )]
    fn type_error_index(value: Type, index: Type) {
        let actual = operations::index(value, index).expect_err("didn't fail");
        assert_eq!(actual, IndexingError::WrongIndexType)
    }
}
