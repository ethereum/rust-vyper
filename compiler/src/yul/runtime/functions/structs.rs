use crate::yul::names;
use fe_analyzer::namespace::types::{FeSized, Struct};
use yultsur::*;

/// Generate a YUL function that can be used to create an instance of
/// `struct_type`
pub fn generate_new_fn(struct_type: &Struct) -> yul::Statement {
    let function_name = names::struct_new_call(&struct_type.name);

    if struct_type.is_empty() {
        // We return 0 here because it is safe to assume that we never write to an empty
        // struct. If we end up writing to an empty struct that's an actual Fe
        // bug.
        return function_definition! {
            function [function_name]() -> return_val {
                 (return_val := 0)
            }
        };
    }

    let params = struct_type
        .fields
        .iter()
        .map(|(name, _)| {
            identifier! {(name)}
        })
        .collect::<Vec<_>>();

    let body = struct_type
        .fields
        .iter()
        .enumerate()
        .map(|(index, (key, _))| {
            if index == 0 {
                let param_identifier_exp = identifier_expression! {(key)};
                statements! {
                    (return_val := alloc(32))
                    (mstore(return_val, [param_identifier_exp]))
                }
            } else {
                let ptr_identifier = format!("{}_ptr", key);
                let ptr_identifier = identifier! {(ptr_identifier)};
                let ptr_identifier_exp = identifier_expression! {(ptr_identifier)};
                let param_identifier_exp = identifier_expression! {(key)};
                statements! {
                    (let [ptr_identifier] := alloc(32))
                    (mstore([ptr_identifier_exp], [param_identifier_exp]))
                }
            }
        })
        .flatten()
        .collect::<Vec<_>>();

    function_definition! {
        function [function_name]([params...]) -> return_val {
            [body...]
        }
    }
}

/// Generate a YUL function that can be used to read a property of `struct_type`
pub fn generate_get_fn(struct_type: &Struct, field_name: &str) -> yul::Statement {
    let function_name = names::struct_getter_call(&struct_type.name, field_name);
    let field_index = struct_type
        .get_field_index(field_name)
        .unwrap_or_else(|| panic!("No field {} in {}", field_name, struct_type.name));
    let field_type = struct_type
        .get_field_type(field_name)
        .unwrap_or_else(|| panic!("No field {} in {}", field_name, struct_type.name));
    // The value of each field occupies 32 bytes. This includes values with sizes
    // less than 32 bytes. So, when we get the pointer to the value of a struct
    // field, we must take into consideration the left-padding. The left-padding is
    // equal to the difference between the value's size and 32 bytes, so we end up
    // adding the word offset and the byte offset.
    let field_offset = if field_type.size() < 32 {
        field_index * 32 + (32 - field_type.size())
    } else {
        field_index * field_type.size()
    };

    let offset = literal_expression! { (field_offset) };
    function_definition! {
        function [function_name](ptr) -> return_val {
             (return_val := add(ptr, [offset]))
        }
    }
}

/// Builds a set of functions used to interact with structs used in a contract
pub fn struct_apis(struct_type: Struct) -> Vec<yul::Statement> {
    [
        vec![generate_new_fn(&struct_type)],
        struct_type
            .fields
            .iter()
            .map(|(name, _)| generate_get_fn(&struct_type, &name))
            .collect(),
    ]
    .concat()
}
