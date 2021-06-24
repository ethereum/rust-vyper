use strum::{EnumString, IntoStaticStr};

#[derive(Debug, PartialEq, EnumString)]
#[strum(serialize_all = "snake_case")]
pub enum ValueMethod {
    Clone,
    ToMem,
    AbiEncode,
}

#[derive(Clone, Debug, PartialEq, Eq, EnumString, IntoStaticStr, Hash)]
#[strum(serialize_all = "snake_case")]
pub enum GlobalMethod {
    Keccak256,
}

#[derive(strum::ToString, Debug, PartialEq, EnumString)]
#[strum(serialize_all = "snake_case")]
pub enum ContractTypeMethod {
    Create,
    Create2,
}

#[derive(strum::ToString, Debug, PartialEq, EnumString)]
#[strum(serialize_all = "lowercase")]
pub enum Object {
    Block,
    Chain,
    Msg,
    Tx,
    #[strum(serialize = "self")]
    Self_,
}

#[derive(Debug, PartialEq, EnumString)]
#[strum(serialize_all = "snake_case")]
pub enum BlockField {
    Coinbase,
    Difficulty,
    Number,
    Timestamp,
}

#[derive(Debug, PartialEq, EnumString)]
#[strum(serialize_all = "snake_case")]
pub enum ChainField {
    Id,
}

#[derive(Debug, PartialEq, EnumString)]
#[strum(serialize_all = "snake_case")]
pub enum MsgField {
    Sender,
    Sig,
    Value,
}

#[derive(Debug, PartialEq, EnumString)]
#[strum(serialize_all = "snake_case")]
pub enum TxField {
    GasPrice,
    Origin,
}

#[derive(Debug, PartialEq, EnumString)]
#[strum(serialize_all = "snake_case")]
pub enum SelfField {
    Address,
}
