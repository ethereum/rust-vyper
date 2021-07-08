use crate::errors::{AlreadyDefined, TypeError};
use crate::namespace::items::FunctionId;

use indexmap::IndexMap;
use num_bigint::BigInt;
use num_traits::ToPrimitive;
use std::convert::TryFrom;
use std::fmt;
use std::rc::Rc;
use strum::IntoStaticStr;
use vec1::Vec1;

pub fn u256_min() -> BigInt {
    BigInt::from(0)
}

pub fn u256_max() -> BigInt {
    BigInt::from(2).pow(256) - 1
}

pub fn i256_max() -> BigInt {
    BigInt::from(2).pow(255) - 1
}

pub fn i256_min() -> BigInt {
    BigInt::from(-2).pow(255)
}

/// The type has a constant size known to the compiler.
pub trait FeSized {
    /// Constant size of the type.
    fn size(&self) -> usize;
}

/// The size of uint element in the ABI encoding.
///
/// Example: The values inside of a byte array have a padded size of 1 byte and
/// a data size of 1 byte, whereas the values inside of an address array have
/// a padded size of 32 bytes and a data size of 20 bytes. These sizes are
/// needed by our encoding/decoding functions to properly read and write data.
#[derive(Clone, Debug, PartialEq, PartialOrd, Ord, Eq)]
pub struct AbiUintSize {
    pub data_size: usize,
    pub padded_size: usize,
}

/// The size of an array.
///
/// Can either be statically-sized with a fixed value known by the compiler
/// or a dynamically-sized with the value not being known until runtime.
#[derive(Clone, Debug, PartialEq, PartialOrd, Ord, Eq)]
pub enum AbiArraySize {
    Static { size: usize },
    Dynamic,
}

/// The type of an element in terms of the ABI spec.
#[derive(Clone, Debug, PartialEq, PartialOrd, Ord, Eq)]
pub enum AbiType {
    /// Array elements consist of a dynamically- or statically-sized set of
    /// uints.
    Array {
        inner: Box<AbiType>,
        size: AbiArraySize,
    },
    Tuple {
        elems: Vec<AbiType>,
    },
    /// All elements are encoded as a uint or set of uints.
    Uint {
        size: AbiUintSize,
    },
}

/// Data can be decoded from memory or calldata.
#[derive(Clone, Debug, PartialEq, PartialOrd, Ord, Eq)]
pub enum AbiDecodeLocation {
    Calldata,
    Memory,
}

/// Single component of a tuple.
#[derive(Clone, Debug, PartialEq, PartialOrd, Ord, Eq)]
pub struct AbiComponent {
    pub name: String,
    pub typ: String,
    /// The subcomponents of the component.
    pub components: Vec<AbiComponent>,
}

/// Information relevant to ABI encoding.
pub trait AbiEncoding: SafeNames {
    /// Name of the type as it appears in the Json ABI.
    fn abi_json_name(&self) -> String;

    /// Name of the type as it appears in the selector preimage.
    fn abi_selector_name(&self) -> String;

    /// The components of an ABI tuple.
    fn abi_components(&self) -> Vec<AbiComponent>;

    /// The ABI type of a Fe type.
    fn abi_type(&self) -> AbiType;
}

/// Names that can be used to build identifiers without collision.
pub trait SafeNames {
    /// Name in the lower snake format (e.g. lower_snake_case).
    fn lower_snake(&self) -> String;
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Base(Base),
    Array(Array),
    Map(Map),
    Tuple(Tuple),
    String(FeString),
    Contract(Contract),
    Struct(Struct),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FixedSize {
    Base(Base),
    Array(Array),
    Tuple(Tuple),
    String(FeString),
    Contract(Contract),
    Struct(Struct),
}

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd, Ord, Eq)]
pub enum Base {
    Numeric(Integer),
    Bool,
    Byte,
    Address,
    Unit,
    Unknown,
}

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd, Ord, Eq, IntoStaticStr)]
pub enum Integer {
    U256,
    U128,
    U64,
    U32,
    U16,
    U8,
    I256,
    I128,
    I64,
    I32,
    I16,
    I8,
}

pub const U256: Base = Base::Numeric(Integer::U256);

#[derive(Clone, Debug, PartialEq, PartialOrd, Ord, Eq)]
pub struct Array {
    pub size: usize,
    pub inner: Base,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Map {
    pub key: Base,
    pub value: Box<Type>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Tuple {
    pub items: Vec1<FixedSize>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Struct {
    pub name: String,
    pub fields: Vec<(String, FixedSize)>,
}

#[derive(Clone, Debug, PartialEq, PartialOrd, Ord, Eq)]
pub struct FeString {
    pub max_size: usize,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Contract {
    pub name: String,
    pub functions: IndexMap<String, Rc<ContractFunction>>,
}

impl Map {
    fn is_mysterious(&self) -> bool {
        self.key == Base::Unknown || self.value.is_mysterious()
    }
}

impl Tuple {
    fn is_mysterious(&self) -> bool {
        self.items.iter().any(|typ| typ.is_mysterious())
    }
}

impl Struct {
    pub fn new(name: &str) -> Struct {
        Struct {
            name: name.to_string(),
            fields: vec![],
        }
    }

    /// Return `true` if the struct does not have any fields, otherwise return `false`
    pub fn is_empty(&self) -> bool {
        self.fields.is_empty()
    }

    /// Add a field to the struct
    pub fn add_field(&mut self, name: &str, value: &FixedSize) -> Result<(), AlreadyDefined> {
        if self.fields.iter().any(|(fname, _)| fname == name) {
            Err(AlreadyDefined)
        } else {
            self.fields.push((name.to_string(), value.clone()));
            Ok(())
        }
    }

    /// Return the type of the given field name
    pub fn get_field_type(&self, name: &str) -> Option<&FixedSize> {
        self.fields
            .iter()
            .find(|(nm, _)| nm == name)
            .map(|(_, typ)| typ)
    }

    /// Return the types of all fields
    pub fn get_field_types(&self) -> Vec<FixedSize> {
        self.fields.iter().cloned().map(|(_, typ)| typ).collect()
    }

    /// Return the index of the given field name
    pub fn get_field_index(&self, name: &str) -> Option<usize> {
        self.fields.iter().position(|(field, _)| field == name)
    }

    fn is_mysterious(&self) -> bool {
        self.fields.iter().any(|(_, typ)| typ.is_mysterious())
    }
}

impl Integer {
    /// Returns `true` if the integer is signed, otherwise `false`
    pub fn is_signed(&self) -> bool {
        matches!(
            self,
            Integer::I256
                | Integer::I128
                | Integer::I64
                | Integer::I32
                | Integer::I16
                | Integer::I8
        )
    }

    /// Returns `true` if the integer is at least the same size (or larger) than
    /// `other`
    pub fn can_hold(&self, other: &Integer) -> bool {
        self.size() >= other.size()
    }

    /// Returns `true` if `num` represents a number that fits the type
    pub fn fits(&self, num: BigInt) -> bool {
        match self {
            Integer::U8 => num.to_u8().is_some(),
            Integer::U16 => num.to_u16().is_some(),
            Integer::U32 => num.to_u32().is_some(),
            Integer::U64 => num.to_u64().is_some(),
            Integer::U128 => num.to_u128().is_some(),
            Integer::I8 => num.to_i8().is_some(),
            Integer::I16 => num.to_i16().is_some(),
            Integer::I32 => num.to_i32().is_some(),
            Integer::I64 => num.to_i64().is_some(),
            Integer::I128 => num.to_i128().is_some(),
            Integer::U256 => num >= u256_min() && num <= u256_max(),
            Integer::I256 => num >= i256_min() && num <= i256_max(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ContractFunction {
    pub is_public: bool,
    // XXX id is only used to get the span. we just get it via the contract's ast.
    pub id: FunctionId,
    pub signature: Rc<FunctionSignature>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FunctionSignature {
    pub params: Vec<(String, FixedSize)>,
    pub return_type: FixedSize,
}

impl FunctionSignature {
    pub fn param_types(&self) -> Vec<FixedSize> {
        self.params.iter().map(|(_, typ)| typ.to_owned()).collect()
    }

    // XXX: is this used?
    pub fn param_names(&self) -> Vec<String> {
        self.params
            .iter()
            .map(|(name, _)| name.to_owned())
            .collect()
    }
}

impl Type {
    pub fn is_signed_integer(&self) -> bool {
        if let Type::Base(Base::Numeric(integer)) = &self {
            return integer.is_signed();
        }
        false
    }

    pub fn unit() -> Self {
        Type::Base(Base::Unit)
    }

    pub fn unknown() -> Self {
        Type::Base(Base::Unknown)
    }

    pub fn int(int_type: Integer) -> Self {
        Type::Base(Base::Numeric(int_type))
    }

    // XXX blah
    pub fn is_mysterious(&self) -> bool {
        match self {
            Type::Array(array) => array.inner == Base::Unknown,
            Type::Base(base) => *base == Base::Unknown,
            Type::Tuple(tuple) => tuple.is_mysterious(),
            Type::String(string) => false,
            Type::Contract(contract) => false,
            Type::Struct(strct) => strct.is_mysterious(),
            Type::Map(map) => map.is_mysterious(),
        }
    }
}

pub trait TypeDowncast {
    fn as_array(&self) -> Option<&Array>;
    fn as_tuple(&self) -> Option<&Tuple>;
    fn as_string(&self) -> Option<&FeString>;
    fn as_map(&self) -> Option<&Map>;
    fn as_int(&self) -> Option<Integer>;
}

impl TypeDowncast for Type {
    fn as_array(&self) -> Option<&Array> {
        match self {
            Type::Array(inner) => Some(inner),
            _ => None,
        }
    }
    fn as_tuple(&self) -> Option<&Tuple> {
        match self {
            Type::Tuple(inner) => Some(inner),
            _ => None,
        }
    }
    fn as_string(&self) -> Option<&FeString> {
        match self {
            Type::String(inner) => Some(inner),
            _ => None,
        }
    }
    fn as_map(&self) -> Option<&Map> {
        match self {
            Type::Map(inner) => Some(inner),
            _ => None,
        }
    }
    fn as_int(&self) -> Option<Integer> {
        match self {
            Type::Base(Base::Numeric(int)) => Some(*int),
            _ => None,
        }
    }
}

impl TypeDowncast for Option<&Type> {
    fn as_array(&self) -> Option<&Array> {
        self.and_then(|t| t.as_array())
    }
    fn as_tuple(&self) -> Option<&Tuple> {
        self.and_then(|t| t.as_tuple())
    }
    fn as_string(&self) -> Option<&FeString> {
        self.and_then(|t| t.as_string())
    }
    fn as_map(&self) -> Option<&Map> {
        self.and_then(|t| t.as_map())
    }
    fn as_int(&self) -> Option<Integer> {
        self.and_then(|t| t.as_int())
    }
}

impl From<FixedSize> for Type {
    fn from(value: FixedSize) -> Self {
        match value {
            FixedSize::Array(array) => Type::Array(array),
            FixedSize::Base(base) => Type::Base(base),
            FixedSize::Tuple(tuple) => Type::Tuple(tuple),
            FixedSize::String(string) => Type::String(string),
            FixedSize::Contract(contract) => Type::Contract(contract),
            FixedSize::Struct(val) => Type::Struct(val),
        }
    }
}

impl From<Base> for Type {
    fn from(value: Base) -> Self {
        Type::Base(value)
    }
}

impl FixedSize {
    /// Returns true if the type is `()`.
    pub fn is_unit(&self) -> bool {
        self == &Self::Base(Base::Unit)
    }

    pub fn is_mysterious(&self) -> bool {
        match self {
            FixedSize::Array(array) => array.inner == Base::Unknown,
            FixedSize::Base(base) => *base == Base::Unknown,
            FixedSize::Tuple(tuple) => tuple.is_mysterious(),
            FixedSize::String(string) => false,
            FixedSize::Contract(contract) => false,
            FixedSize::Struct(strct) => strct.is_mysterious(),
        }
    }

    /// Creates an instance of bool.
    pub fn bool() -> Self {
        FixedSize::Base(Base::Bool)
    }

    /// Creates an instance of `()`.
    pub fn unit() -> Self {
        FixedSize::Base(Base::Unit)
    }

    pub fn unknown() -> Self {
        FixedSize::Base(Base::Unknown)
    }
}

impl PartialEq<Type> for FixedSize {
    fn eq(&self, other: &Type) -> bool {
        match (self, other) {
            (FixedSize::Array(in1), Type::Array(in2)) => in1 == in2,
            (FixedSize::Base(in1), Type::Base(in2)) => in1 == in2,
            (FixedSize::Tuple(in1), Type::Tuple(in2)) => in1 == in2,
            (FixedSize::String(in1), Type::String(in2)) => in1 == in2,
            (FixedSize::Contract(in1), Type::Contract(in2)) => in1 == in2,
            (FixedSize::Struct(in1), Type::Struct(in2)) => in1 == in2,
            _ => false,
        }
    }
}

impl From<Base> for FixedSize {
    fn from(value: Base) -> Self {
        FixedSize::Base(value)
    }
}

impl From<FeString> for FixedSize {
    fn from(value: FeString) -> Self {
        FixedSize::String(value)
    }
}

impl TryFrom<Type> for FixedSize {
    type Error = TypeError;

    fn try_from(value: Type) -> Result<Self, TypeError> {
        match value {
            Type::Array(array) => Ok(FixedSize::Array(array)),
            Type::Base(base) => Ok(FixedSize::Base(base)),
            Type::Tuple(tuple) => Ok(FixedSize::Tuple(tuple)),
            Type::String(string) => Ok(FixedSize::String(string)),
            Type::Struct(val) => Ok(FixedSize::Struct(val)),
            Type::Map(_) => Err(TypeError),
            Type::Contract(contract) => Ok(FixedSize::Contract(contract)),
        }
    }
}

impl From<Tuple> for FixedSize {
    fn from(value: Tuple) -> Self {
        FixedSize::Tuple(value)
    }
}

impl FeSized for FixedSize {
    fn size(&self) -> usize {
        match self {
            FixedSize::Base(base) => base.size(),
            FixedSize::Array(array) => array.size(),
            FixedSize::Tuple(tuple) => tuple.size(),
            FixedSize::String(string) => string.size(),
            FixedSize::Contract(contract) => contract.size(),
            FixedSize::Struct(val) => val.size(),
        }
    }
}

impl FeSized for Integer {
    fn size(&self) -> usize {
        match self {
            Integer::U8 => 1,
            Integer::U16 => 2,
            Integer::U32 => 4,
            Integer::U64 => 8,
            Integer::U128 => 16,
            Integer::U256 => 32,
            Integer::I8 => 1,
            Integer::I16 => 2,
            Integer::I32 => 4,
            Integer::I64 => 8,
            Integer::I128 => 16,
            Integer::I256 => 32,
        }
    }
}

impl FeSized for Base {
    fn size(&self) -> usize {
        match self {
            Base::Numeric(integer) => integer.size(),
            Base::Bool => 1,
            Base::Byte => 1,
            Base::Address => 32,
            Base::Unit => 0,
            Base::Unknown => 0,
        }
    }
}

impl FeSized for Array {
    fn size(&self) -> usize {
        self.size * self.inner.size()
    }
}

impl FeSized for Tuple {
    fn size(&self) -> usize {
        self.items.iter().map(|typ| typ.size()).sum()
    }
}

impl FeSized for Struct {
    fn size(&self) -> usize {
        self.fields.len() * 32
    }
}

impl FeSized for FeString {
    fn size(&self) -> usize {
        self.max_size + 32
    }
}

impl FeSized for Contract {
    fn size(&self) -> usize {
        32
    }
}

impl AbiEncoding for FixedSize {
    fn abi_json_name(&self) -> String {
        match self {
            FixedSize::Array(array) => array.abi_json_name(),
            FixedSize::Base(base) => base.abi_json_name(),
            FixedSize::Tuple(tuple) => tuple.abi_json_name(),
            FixedSize::String(string) => string.abi_json_name(),
            FixedSize::Contract(contract) => contract.abi_json_name(),
            FixedSize::Struct(val) => val.abi_json_name(),
        }
    }

    fn abi_selector_name(&self) -> String {
        match self {
            FixedSize::Array(array) => array.abi_selector_name(),
            FixedSize::Base(base) => base.abi_selector_name(),
            FixedSize::Tuple(tuple) => tuple.abi_selector_name(),
            FixedSize::String(string) => string.abi_selector_name(),
            FixedSize::Contract(contract) => contract.abi_selector_name(),
            FixedSize::Struct(val) => val.abi_selector_name(),
        }
    }

    fn abi_components(&self) -> Vec<AbiComponent> {
        match self {
            FixedSize::Array(array) => array.abi_components(),
            FixedSize::Base(base) => base.abi_components(),
            FixedSize::Tuple(tuple) => tuple.abi_components(),
            FixedSize::String(string) => string.abi_components(),
            FixedSize::Contract(contract) => contract.abi_components(),
            FixedSize::Struct(val) => val.abi_components(),
        }
    }

    fn abi_type(&self) -> AbiType {
        match self {
            FixedSize::Base(base) => base.abi_type(),
            FixedSize::Array(array) => array.abi_type(),
            FixedSize::Tuple(tuple) => tuple.abi_type(),
            FixedSize::String(string) => string.abi_type(),
            FixedSize::Contract(contract) => contract.abi_type(),
            FixedSize::Struct(val) => val.abi_type(),
        }
    }
}

impl AbiEncoding for Base {
    fn abi_json_name(&self) -> String {
        match self {
            Base::Numeric(Integer::U256) => "uint256".to_string(),
            Base::Numeric(Integer::U128) => "uint128".to_string(),
            Base::Numeric(Integer::U64) => "uint64".to_string(),
            Base::Numeric(Integer::U32) => "uint32".to_string(),
            Base::Numeric(Integer::U16) => "uint16".to_string(),
            Base::Numeric(Integer::U8) => "uint8".to_string(),
            Base::Numeric(Integer::I256) => "int256".to_string(),
            Base::Numeric(Integer::I128) => "int128".to_string(),
            Base::Numeric(Integer::I64) => "int64".to_string(),
            Base::Numeric(Integer::I32) => "int32".to_string(),
            Base::Numeric(Integer::I16) => "int16".to_string(),
            Base::Numeric(Integer::I8) => "int8".to_string(),
            Base::Address => "address".to_string(),
            Base::Byte => "byte".to_string(),
            Base::Bool => "bool".to_string(),
            Base::Unit => panic!("unit type is not abi encodable"),
            Base::Unknown => panic!("unknown type is not abi encodable"),
        }
    }

    fn abi_selector_name(&self) -> String {
        self.abi_json_name()
    }

    fn abi_components(&self) -> Vec<AbiComponent> {
        vec![]
    }

    fn abi_type(&self) -> AbiType {
        let (data_size, padded_size) = match self {
            Base::Bool => (1, 32),
            Base::Numeric(size) => match size {
                Integer::U256 => (32, 32),
                Integer::U128 => (16, 32),
                Integer::U64 => (8, 32),
                Integer::U32 => (4, 32),
                Integer::U16 => (2, 32),
                Integer::U8 => (1, 32),
                Integer::I256 => (32, 32),
                Integer::I128 => (16, 32),
                Integer::I64 => (8, 32),
                Integer::I32 => (4, 32),
                Integer::I16 => (2, 32),
                Integer::I8 => (1, 32),
            },
            Base::Address => (32, 32),
            Base::Byte => (1, 1),
            Base::Unit => panic!("unit type is not abi encodable"),
            Base::Unknown => panic!("unknown type is not abi encodable"),
        };
        AbiType::Uint {
            size: AbiUintSize {
                data_size,
                padded_size,
            },
        }
    }
}

impl AbiEncoding for Array {
    fn abi_json_name(&self) -> String {
        if self.inner == Base::Byte {
            return format!("bytes{}", self.size);
        }

        format!("{}[{}]", self.inner.abi_json_name(), self.size)
    }

    fn abi_selector_name(&self) -> String {
        self.abi_json_name()
    }

    fn abi_components(&self) -> Vec<AbiComponent> {
        vec![]
    }

    fn abi_type(&self) -> AbiType {
        AbiType::Array {
            inner: Box::new(self.inner.abi_type()),
            size: AbiArraySize::Static { size: self.size },
        }
    }
}

impl AbiEncoding for Struct {
    fn abi_json_name(&self) -> String {
        "tuple".to_string()
    }

    fn abi_selector_name(&self) -> String {
        let field_names = self
            .fields
            .iter()
            .map(|(_, typ)| typ.abi_json_name())
            .collect::<Vec<String>>();
        let joined_names = field_names.join(",");
        format!("({})", joined_names)
    }

    fn abi_components(&self) -> Vec<AbiComponent> {
        self.fields
            .iter()
            .map(|(name, typ)| AbiComponent {
                name: name.to_owned(),
                typ: typ.abi_json_name(),
                components: vec![],
            })
            .collect()
    }

    fn abi_type(&self) -> AbiType {
        AbiType::Tuple {
            elems: self.fields.iter().map(|(_, typ)| typ.abi_type()).collect(),
        }
    }
}

impl AbiEncoding for Tuple {
    fn abi_json_name(&self) -> String {
        "tuple".to_string()
    }

    fn abi_selector_name(&self) -> String {
        let field_names = self
            .items
            .iter()
            .map(|typ| typ.abi_json_name())
            .collect::<Vec<String>>();
        let joined_names = field_names.join(",");
        format!("({})", joined_names)
    }

    fn abi_components(&self) -> Vec<AbiComponent> {
        self.items
            .iter()
            .enumerate()
            .map(|(index, item)| AbiComponent {
                name: format!("item{}", index),
                typ: item.abi_json_name(),
                components: vec![],
            })
            .collect()
    }

    fn abi_type(&self) -> AbiType {
        AbiType::Tuple {
            elems: self.items.iter().map(|typ| typ.abi_type()).collect(),
        }
    }
}

impl AbiEncoding for Contract {
    fn abi_json_name(&self) -> String {
        unimplemented!();
    }

    fn abi_selector_name(&self) -> String {
        unimplemented!()
    }

    fn abi_components(&self) -> Vec<AbiComponent> {
        unimplemented!()
    }

    fn abi_type(&self) -> AbiType {
        unimplemented!();
    }
}

impl AbiEncoding for FeString {
    fn abi_json_name(&self) -> String {
        "string".to_string()
    }

    fn abi_selector_name(&self) -> String {
        "string".to_string()
    }

    fn abi_components(&self) -> Vec<AbiComponent> {
        vec![]
    }

    fn abi_type(&self) -> AbiType {
        AbiType::Array {
            inner: Box::new(AbiType::Uint {
                size: AbiUintSize {
                    data_size: 1,
                    padded_size: 1,
                },
            }),
            size: AbiArraySize::Dynamic,
        }
    }
}

impl SafeNames for FixedSize {
    fn lower_snake(&self) -> String {
        match self {
            FixedSize::Array(array) => array.lower_snake(),
            FixedSize::Base(base) => base.lower_snake(),
            FixedSize::Tuple(tuple) => tuple.lower_snake(),
            FixedSize::String(string) => string.lower_snake(),
            FixedSize::Contract(contract) => contract.lower_snake(),
            FixedSize::Struct(val) => val.lower_snake(),
        }
    }
}

impl SafeNames for Base {
    fn lower_snake(&self) -> String {
        match self {
            Base::Numeric(Integer::U256) => "u256".to_string(),
            Base::Numeric(Integer::U128) => "u128".to_string(),
            Base::Numeric(Integer::U64) => "u64".to_string(),
            Base::Numeric(Integer::U32) => "u32".to_string(),
            Base::Numeric(Integer::U16) => "u16".to_string(),
            Base::Numeric(Integer::U8) => "u8".to_string(),
            Base::Numeric(Integer::I256) => "i256".to_string(),
            Base::Numeric(Integer::I128) => "i128".to_string(),
            Base::Numeric(Integer::I64) => "i64".to_string(),
            Base::Numeric(Integer::I32) => "i32".to_string(),
            Base::Numeric(Integer::I16) => "i16".to_string(),
            Base::Numeric(Integer::I8) => "i8".to_string(),
            Base::Address => "address".to_string(),
            Base::Byte => "byte".to_string(),
            Base::Bool => "bool".to_string(),
            Base::Unit => "unit".to_string(),
            Base::Unknown => unreachable!(),
        }
    }
}

impl SafeNames for Array {
    fn lower_snake(&self) -> String {
        format!("array_{}_{}", self.inner.lower_snake(), self.size)
    }
}

impl SafeNames for Struct {
    fn lower_snake(&self) -> String {
        format!("struct_{}", self.name)
    }
}

impl SafeNames for Tuple {
    fn lower_snake(&self) -> String {
        let field_names = self
            .items
            .iter()
            .map(|typ| typ.lower_snake())
            .collect::<Vec<String>>();
        let joined_names = field_names.join("_");

        // The trailing `_` denotes the end of the tuple, to differentiate between
        // different tuple nestings. Eg
        // (A, (B, C), D) => tuple_A_tuple_B_C__D_
        // (A, (B, C, D)) => tuple_A_tuple_B_C_D__
        // Conceptually, each paren and comma is replaced with an underscore.
        format!("tuple_{}_", joined_names)
    }
}

impl SafeNames for Contract {
    fn lower_snake(&self) -> String {
        unimplemented!();
    }
}

impl SafeNames for FeString {
    fn lower_snake(&self) -> String {
        format!("string_{}", self.max_size)
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Base(inner) => inner.fmt(f),
            Type::Array(inner) => inner.fmt(f),
            Type::Map(inner) => inner.fmt(f),
            Type::Tuple(inner) => inner.fmt(f),
            Type::String(inner) => inner.fmt(f),
            Type::Contract(inner) => inner.fmt(f),
            Type::Struct(inner) => inner.fmt(f),
        }
    }
}

impl fmt::Display for FixedSize {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FixedSize::Base(inner) => inner.fmt(f),
            FixedSize::Array(inner) => inner.fmt(f),
            FixedSize::Tuple(inner) => inner.fmt(f),
            FixedSize::String(inner) => inner.fmt(f),
            FixedSize::Contract(inner) => inner.fmt(f),
            FixedSize::Struct(inner) => inner.fmt(f),
        }
    }
}

impl fmt::Display for Base {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = match self {
            Base::Numeric(int) => return int.fmt(f),
            Base::Bool => "bool",
            Base::Byte => "byte",
            Base::Address => "address",
            Base::Unit => "()",
            Base::Unknown => "unknown",
        };
        write!(f, "{}", name)
    }
}

impl fmt::Display for Integer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = match self {
            Integer::U256 => "u256",
            Integer::U128 => "u128",
            Integer::U64 => "u64",
            Integer::U32 => "u32",
            Integer::U16 => "u16",
            Integer::U8 => "u8",
            Integer::I256 => "i256",
            Integer::I128 => "i128",
            Integer::I64 => "i64",
            Integer::I32 => "i32",
            Integer::I16 => "i16",
            Integer::I8 => "i8",
        };
        write!(f, "{}", name)
    }
}

impl fmt::Display for Array {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}[{}]", self.inner, self.size)
    }
}

impl fmt::Display for Map {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Map<{}, {}>", self.key, self.value)
    }
}

impl fmt::Display for Tuple {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(")?;
        let mut delim = "";
        for item in &self.items {
            write!(f, "{}{}", delim, item)?;
            delim = ", ";
        }
        write!(f, ")")
    }
}

impl fmt::Display for FeString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "String<{}>", self.max_size)
    }
}

impl fmt::Display for Contract {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl fmt::Display for Struct {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}
