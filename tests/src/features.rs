//! Simple contract tests that narrowly test a given feature

#![cfg(feature = "solc-backend")]
use evm_runtime::Handler;
use primitive_types::{H160, U256};
use rstest::rstest;
use std::collections::BTreeMap;
use std::iter;

use fe_common::utils::keccak;
use fe_compiler_test_utils::*;
use fe_compiler_test_utils::{self as test_utils};

pub fn deploy_contract(
    executor: &mut Executor,
    fixture: &str,
    contract_name: &str,
    init_params: &[ethabi::Token],
) -> ContractHarness {
    test_utils::deploy_contract(
        executor,
        &format!("fixtures/features/{}", fixture),
        contract_name,
        init_params,
    )
}

pub fn load_contract(address: H160, fixture: &str, contract_name: &str) -> ContractHarness {
    test_utils::load_contract(
        address,
        &format!("fixtures/features/{}", fixture),
        contract_name,
    )
}

#[test]
fn test_to_2s_complement() {
    let minus_three = U256::from_dec_str(
        "115792089237316195423570985008687907853269984665640564039457584007913129639933",
    )
    .unwrap();
    assert_eq!(minus_three, to_2s_complement(-3));
    assert_eq!(U256::from(3), to_2s_complement(3));
}

#[test]
fn evm_sanity() {
    with_executor(&|mut executor| {
        let address = H160::zero();
        let amount = U256::from(1000);

        executor.state_mut().deposit(address, amount);
        assert_eq!(executor.balance(address), amount);
    })
}

#[test]
fn test_revert() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "revert.fe", "Foo", &[]);

        let exit = harness.capture_call(&mut executor, "bar", &[]);

        assert!(matches!(
            exit,
            evm::Capture::Exit((evm::ExitReason::Revert(_), _))
        ))
    })
}

#[test]
fn test_assert() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "assert.fe", "Foo", &[]);

        let exit1 = harness.capture_call(&mut executor, "bar", &[uint_token(4)]);

        match exit1 {
            evm::Capture::Exit((evm::ExitReason::Revert(_), output)) => assert_eq!(output.len(), 0),
            _ => panic!("Did not revert correctly"),
        }

        let exit2 = harness.capture_call(&mut executor, "bar", &[uint_token(42)]);

        assert!(matches!(
            exit2,
            evm::Capture::Exit((evm::ExitReason::Succeed(_), _))
        ));

        let exit3 =
            harness.capture_call(&mut executor, "revert_with_static_string", &[uint_token(4)]);

        match exit3 {
            evm::Capture::Exit((evm::ExitReason::Revert(_), output)) => {
                assert_eq!(output, encode_error_reason("Must be greater than five"))
            }
            _ => panic!("Did not revert correctly"),
        }

        let reason = "A very looooooooooooooong reason that consumes multiple words";
        let exit4 = harness.capture_call(
            &mut executor,
            "revert_with",
            &[uint_token(4), string_token(&reason)],
        );

        match exit4 {
            evm::Capture::Exit((evm::ExitReason::Revert(_), output)) => {
                assert_eq!(output, encode_error_reason(reason))
            }
            _ => panic!("Did not revert correctly"),
        }
    })
}

#[rstest(fixture_file, input, expected,
    case("for_loop_with_static_array.fe", &[], uint_token(30)),
    case("for_loop_with_break.fe", &[], uint_token(15)),
    case("for_loop_with_continue.fe", &[], uint_token(17)),
    case("while_loop_with_continue.fe", &[], uint_token(1)),
    case("while_loop.fe", &[], uint_token(3)),
    case("while_loop_with_break.fe", &[], uint_token(1)),
    case("while_loop_with_break_2.fe", &[], uint_token(1)),
    case("if_statement.fe", &[uint_token(6)], uint_token(1)),
    case("if_statement.fe", &[uint_token(4)], uint_token(0)),
    case("if_statement_2.fe", &[uint_token(6)], uint_token(1)),
    case("if_statement_with_block_declaration.fe", &[], uint_token(1)),
    case("ternary_expression.fe", &[uint_token(6)], uint_token(1)),
    case("ternary_expression.fe", &[uint_token(4)], uint_token(0)),
    case("call_statement_without_args.fe", &[], uint_token(100)),
    case("call_statement_with_args.fe", &[], uint_token(100)),
    case("call_statement_with_args_2.fe", &[], uint_token(100)),
    case("return_bool_true.fe", &[], bool_token(true)),
    case("return_bool_false.fe", &[], bool_token(false)),
    case("return_bool_inverted.fe", &[bool_token(true)], bool_token(false)),
    case("return_bool_inverted.fe", &[bool_token(false)], bool_token(true)),
    case("return_u256_from_called_fn_with_args.fe", &[], uint_token(200)),
    case("return_u256_from_called_fn.fe", &[], uint_token(42)),
    case("return_u256.fe", &[], uint_token(42)),
    case("return_i256.fe", &[], int_token(-3)),
    case("return_identity_u256.fe", &[uint_token(42)], uint_token(42)),
    case("return_identity_u128.fe", &[uint_token(42)], uint_token(42)),
    case("return_identity_u64.fe", &[uint_token(42)], uint_token(42)),
    case("return_identity_u32.fe", &[uint_token(42)], uint_token(42)),
    case("return_identity_u16.fe", &[uint_token(42)], uint_token(42)),
    case("return_identity_u8.fe", &[uint_token(42)], uint_token(42)),
    case("return_u128_cast.fe", &[], uint_token(42)),
    case("return_i128_cast.fe", &[], int_token(-3)),
    case("return_msg_sig.fe", &[], bytes32_token("febb0f7e")),
    case("return_sum_list_expression_1.fe", &[], uint_token(210)),
    case("return_sum_list_expression_2.fe", &[], uint_token(210)),
    // binary operators
    case("return_addition_u256.fe", &[uint_token(42), uint_token(42)], uint_token(84)),
    case("return_addition_i256.fe", &[int_token(-42), int_token(-42)], int_token(-84)),
    case("return_addition_i256.fe", &[int_token(-42), int_token(42)], int_token(0)),
    case("return_addition_u128.fe", &[uint_token(42), uint_token(42)], uint_token(84)),
    case("return_subtraction_u256.fe", &[uint_token(42), uint_token(42)], uint_token(0)),
    case("return_subtraction_i256.fe", &[int_token(-42), int_token(-42)], int_token(0)),
    case("return_subtraction_i256.fe", &[int_token(-42), int_token(42)], int_token(-84)),
    case("return_multiplication_u256.fe", &[uint_token(42), uint_token(42)], uint_token(1764)),
    case("return_multiplication_i256.fe", &[int_token(-42), int_token(-42)], int_token(1764)),
    case("return_multiplication_i256.fe", &[int_token(-42), int_token(42)], int_token(-1764)),
    case("return_division_u256.fe", &[uint_token(42), uint_token(42)], uint_token(1)),
    case("return_division_i256.fe", &[int_token(-42), int_token(-42)], int_token(1)),
    case("return_division_i256.fe", &[int_token(-1), int_token(1)], int_token(-1)),
    case("return_division_i256.fe", &[int_token(-42), int_token(42)], int_token(-1)),
    case("return_pow_u256.fe", &[uint_token(2), uint_token(0)], uint_token(1)),
    case("return_pow_u256.fe", &[uint_token(2), uint_token(4)], uint_token(16)),
    case("return_pow_i256.fe", &[int_token(-2), uint_token(3)], int_token(-8)),
    case("return_mod_u256.fe", &[uint_token(5), uint_token(2)], uint_token(1)),
    case("return_mod_u256.fe", &[uint_token(5), uint_token(3)], uint_token(2)),
    case("return_mod_u256.fe", &[uint_token(5), uint_token(5)], uint_token(0)),
    case("return_mod_i256.fe", &[int_token(5), int_token(2)], int_token(1)),
    case("return_mod_i256.fe", &[int_token(5), int_token(3)], int_token(2)),
    case("return_mod_i256.fe", &[int_token(5), int_token(5)], int_token(0)),
    case("return_bitwiseand_u256.fe", &[uint_token(12), uint_token(25)], uint_token(8)),
    case("return_bitwiseand_u128.fe", &[uint_token(12), uint_token(25)], uint_token(8)),
    case("return_bitwiseor_u256.fe", &[uint_token(12), uint_token(25)], uint_token(29)),
    case("return_bitwisexor_u256.fe", &[uint_token(12), uint_token(25)], uint_token(21)),
    case("return_bitwiseshl_u256.fe", &[uint_token(212), uint_token(0)], uint_token(212)),
    case("return_bitwiseshl_u256.fe", &[uint_token(212), uint_token(1)], uint_token(424)),
    case("return_bitwiseshr_u256.fe", &[uint_token(212), uint_token(0)], uint_token(212)),
    case("return_bitwiseshr_u256.fe", &[uint_token(212), uint_token(1)], uint_token(106)),
    case("return_bitwiseshr_i256.fe", &[int_token(212), uint_token(0)], int_token(212)),
    case("return_bitwiseshr_i256.fe", &[int_token(212), uint_token(1)], int_token(106)),
    // comparison operators
    case("return_eq_u256.fe", &[uint_token(1), uint_token(1)], bool_token(true)),
    case("return_eq_u256.fe", &[uint_token(1), uint_token(2)], bool_token(false)),
    case("return_noteq_u256.fe", &[uint_token(1), uint_token(1)], bool_token(false)),
    case("return_noteq_u256.fe", &[uint_token(1), uint_token(2)], bool_token(true)),
    case("return_lt_u256.fe", &[uint_token(1), uint_token(2)], bool_token(true)),
    case("return_lt_u256.fe", &[uint_token(1), uint_token(1)], bool_token(false)),
    case("return_lt_u256.fe", &[uint_token(2), uint_token(1)], bool_token(false)),
    case("return_lt_u128.fe", &[uint_token(1), uint_token(2)], bool_token(true)),
    // lt_i256 with positive and negative numbers
    case("return_lt_i256.fe", &[int_token(1), int_token(2)], bool_token(true)),
    case("return_lt_i256.fe", &[int_token(1), int_token(1)], bool_token(false)),
    case("return_lt_i256.fe", &[int_token(2), int_token(1)], bool_token(false)),
    case("return_lt_i256.fe", &[int_token(-2), int_token(-1)], bool_token(true)),
    case("return_lt_i256.fe", &[int_token(-1), int_token(-1)], bool_token(false)),
    case("return_lt_i256.fe", &[int_token(-1), int_token(-2)], bool_token(false)),
    case("return_lte_u256.fe", &[uint_token(1), uint_token(2)], bool_token(true)),
    case("return_lte_u256.fe", &[uint_token(1), uint_token(1)], bool_token(true)),
    // lte_i256 with positive and negative numbers
    case("return_lte_u256.fe", &[uint_token(2), uint_token(1)], bool_token(false)),
    case("return_lte_i256.fe", &[int_token(1), int_token(2)], bool_token(true)),
    case("return_lte_i256.fe", &[int_token(1), int_token(1)], bool_token(true)),
    case("return_lte_i256.fe", &[int_token(2), int_token(1)], bool_token(false)),
    case("return_lte_i256.fe", &[int_token(-2), int_token(-1)], bool_token(true)),
    case("return_lte_i256.fe", &[int_token(-1), int_token(-1)], bool_token(true)),
    case("return_lte_i256.fe", &[int_token(-1), int_token(-2)], bool_token(false)),
    case("return_gt_u256.fe", &[uint_token(2), uint_token(1)], bool_token(true)),
    case("return_gt_u256.fe", &[uint_token(1), uint_token(1)], bool_token(false)),
    case("return_gt_u256.fe", &[uint_token(1), uint_token(2)], bool_token(false)),
    // gt_i256 with positive and negative numbers
    case("return_gt_i256.fe", &[int_token(2), int_token(1)], bool_token(true)),
    case("return_gt_i256.fe", &[int_token(1), int_token(1)], bool_token(false)),
    case("return_gt_i256.fe", &[int_token(1), int_token(2)], bool_token(false)),
    case("return_gt_i256.fe", &[int_token(-1), int_token(-2)], bool_token(true)),
    case("return_gt_i256.fe", &[int_token(-1), int_token(-1)], bool_token(false)),
    case("return_gt_i256.fe", &[int_token(-2), int_token(-1)], bool_token(false)),
    case("return_gte_u256.fe", &[uint_token(2), uint_token(1)], bool_token(true)),
    case("return_gte_u256.fe", &[uint_token(1), uint_token(1)], bool_token(true)),
    case("return_gte_u256.fe", &[uint_token(1), uint_token(2)], bool_token(false)),
    // gte_i256 with positive and negative numbers
    case("return_gte_i256.fe", &[int_token(2), int_token(1)], bool_token(true)),
    case("return_gte_i256.fe", &[int_token(1), int_token(1)], bool_token(true)),
    case("return_gte_i256.fe", &[int_token(1), int_token(2)], bool_token(false)),
    case("return_gte_i256.fe", &[int_token(-1), int_token(-2)], bool_token(true)),
    case("return_gte_i256.fe", &[int_token(-1), int_token(-1)], bool_token(true)),
    case("return_gte_i256.fe", &[int_token(-2), int_token(-1)], bool_token(false)),
    // `and` bool operation
    case("return_bool_op_and.fe", &[bool_token(true), bool_token(true)], bool_token(true)),
    case("return_bool_op_and.fe", &[bool_token(true), bool_token(false)], bool_token(false)),
    case("return_bool_op_and.fe", &[bool_token(false), bool_token(true)], bool_token(false)),
    case("return_bool_op_and.fe", &[bool_token(false), bool_token(false)], bool_token(false)),
    // `or` bool operation
    case("return_bool_op_or.fe", &[bool_token(true), bool_token(true)], bool_token(true)),
    case("return_bool_op_or.fe", &[bool_token(true), bool_token(false)], bool_token(true)),
    case("return_bool_op_or.fe", &[bool_token(false), bool_token(true)], bool_token(true)),
    case("return_bool_op_or.fe", &[bool_token(false), bool_token(false)], bool_token(false)),
    // radix
    case("radix_hex.fe", &[], uint_token(0xfe)),
    case("radix_octal.fe", &[], uint_token(0o70)),
    case("radix_binary.fe", &[], uint_token(0b10)),
    case::map_tuple("map_tuple.fe", &[uint_token(1234)], uint_token(1234)),
    case::int_literal_coercion("int_literal_coercion.fe", &[], uint_token(300)),
)]
fn test_method_return(fixture_file: &str, input: &[ethabi::Token], expected: ethabi::Token) {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, fixture_file, "Foo", &[]);
        harness.test_function(&mut executor, "bar", input, Some(&expected))
    })
}

#[test]
fn return_array() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "return_array.fe", "Foo", &[]);

        harness.test_function(
            &mut executor,
            "bar",
            &[uint_token(42)],
            Some(&u256_array_token(&[0, 0, 0, 42, 0])),
        )
    })
}

#[test]
fn multi_param() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "multi_param.fe", "Foo", &[]);

        harness.test_function(
            &mut executor,
            "bar",
            &[uint_token(4), uint_token(42), uint_token(420)],
            Some(&u256_array_token(&[4, 42, 420])),
        )
    })
}

#[rstest(
    fixture_file,
    case("u256_u256_map.fe"),
    case("u128_u128_map.fe"),
    case("u64_u64_map.fe"),
    case("u32_u32_map.fe"),
    case("u16_u16_map.fe"),
    case("u8_u8_map.fe")
)]
fn test_map(fixture_file: &str) {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, fixture_file, "Foo", &[]);

        harness.test_function(
            &mut executor,
            "write_bar",
            &[uint_token(4), uint_token(42)],
            None,
        );

        harness.test_function(
            &mut executor,
            "write_bar",
            &[uint_token(420), uint_token(12)],
            None,
        );

        harness.test_function(
            &mut executor,
            "read_bar",
            &[uint_token(4)],
            Some(&uint_token(42)),
        );

        harness.test_function(
            &mut executor,
            "read_bar",
            &[uint_token(420)],
            Some(&uint_token(12)),
        );
    })
}

#[test]
fn address_bytes10_map() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "address_bytes10_map.fe", "Foo", &[]);

        let address1 = address_token("0000000000000000000000000000000000000001");
        let bytes1 = bytes_token("ten bytes1");

        let address2 = address_token("0000000000000000000000000000000000000002");
        let bytes2 = bytes_token("ten bytes2");

        harness.test_function(
            &mut executor,
            "write_bar",
            &[address1.clone(), bytes1.clone()],
            None,
        );

        harness.test_function(
            &mut executor,
            "write_bar",
            &[address2.clone(), bytes2.clone()],
            None,
        );

        harness.test_function(&mut executor, "read_bar", &[address1], Some(&bytes1));
        harness.test_function(&mut executor, "read_bar", &[address2], Some(&bytes2));
    })
}

#[test]
fn return_builtin_attributes() {
    let gas_price = 123;
    let origin = address_token("0000000000000000000000000000000000000001");
    let chain_id = 42;
    let block_number = 5;
    let block_coinbase = address_token("0000000000000000000000000000000000000002");
    let block_timestamp = 1234567890;
    let block_difficulty = 12345;

    let vicinity = evm::backend::MemoryVicinity {
        gas_price: U256::from(gas_price),
        origin: origin.clone().into_address().unwrap(),
        chain_id: U256::from(chain_id),
        block_hashes: Vec::new(),
        block_number: U256::from(block_number),
        block_coinbase: block_coinbase.clone().into_address().unwrap(),
        block_timestamp: U256::from(block_timestamp),
        block_difficulty: U256::from(block_difficulty),
        block_gas_limit: primitive_types::U256::MAX,
    };

    let backend = evm::backend::MemoryBackend::new(&vicinity, BTreeMap::new());

    with_executor_backend(backend, &|mut executor| {
        let mut harness =
            deploy_contract(&mut executor, "return_builtin_attributes.fe", "Foo", &[]);
        let sender = address_token("1234000000000000000000000000000000005678");
        harness.caller = sender.clone().into_address().unwrap();
        let value = 55555;
        harness.value = U256::from(value);
        harness.test_function(&mut executor, "coinbase", &[], Some(&block_coinbase));
        harness.test_function(
            &mut executor,
            "difficulty",
            &[],
            Some(&uint_token(block_difficulty)),
        );
        harness.test_function(
            &mut executor,
            "number",
            &[],
            Some(&uint_token(block_number)),
        );
        harness.test_function(
            &mut executor,
            "timestamp",
            &[],
            Some(&uint_token(block_timestamp)),
        );
        harness.test_function(&mut executor, "chainid", &[], Some(&uint_token(chain_id)));
        harness.test_function(&mut executor, "sender", &[], Some(&sender));
        harness.test_function(&mut executor, "value", &[], Some(&uint_token(value)));
        harness.test_function(&mut executor, "origin", &[], Some(&origin));
        harness.test_function(
            &mut executor,
            "gas_price",
            &[],
            Some(&uint_token(gas_price)),
        );
    })
}

#[test]
fn nested_map() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "nested_map.fe", "Foo", &[]);

        let address1 = address_token("1000000000000000000000000000000000000001");
        let address2 = address_token("2000000000000000000000000000000000000002");
        let address3 = address_token("3000000000000000000000000000000000000003");

        // write bar (address -> address -> u256)
        harness.test_function(
            &mut executor,
            "write_bar",
            &[address1.clone(), address2.clone(), uint_token(12)],
            None,
        );
        harness.test_function(
            &mut executor,
            "write_bar",
            &[address1.clone(), address3.clone(), uint_token(13)],
            None,
        );
        harness.test_function(
            &mut executor,
            "write_bar",
            &[address2.clone(), address1.clone(), uint_token(21)],
            None,
        );

        // write baz (address -> u256 -> bool)
        harness.test_function(
            &mut executor,
            "write_baz",
            &[address1.clone(), uint_token(26), bool_token(true)],
            None,
        );
        harness.test_function(
            &mut executor,
            "write_baz",
            &[address2.clone(), uint_token(42), bool_token(true)],
            None,
        );
        harness.test_function(
            &mut executor,
            "write_baz",
            &[address2.clone(), uint_token(100), bool_token(false)],
            None,
        );

        // read bar
        harness.test_function(
            &mut executor,
            "read_bar",
            &[address1.clone(), address2.clone()],
            Some(&uint_token(12)),
        );
        harness.test_function(
            &mut executor,
            "read_bar",
            &[address1.clone(), address3],
            Some(&uint_token(13)),
        );
        harness.test_function(
            &mut executor,
            "read_bar",
            &[address2.clone(), address1.clone()],
            Some(&uint_token(21)),
        );

        // read baz
        harness.test_function(
            &mut executor,
            "read_baz",
            &[address1, uint_token(26)],
            Some(&bool_token(true)),
        );
        harness.test_function(
            &mut executor,
            "read_baz",
            &[address2.clone(), uint_token(42)],
            Some(&bool_token(true)),
        );
        harness.test_function(
            &mut executor,
            "read_baz",
            &[address2, uint_token(100)],
            Some(&bool_token(false)),
        );
    })
}

#[test]
fn events() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "events.fe", "Foo", &[]);

        let addr1 = address_token("1234000000000000000000000000000000005678");
        let addr2 = address_token("9123000000000000000000000000000000004567");
        let addr_array = ethabi::Token::FixedArray(vec![addr1.clone(), addr2.clone()]);
        let bytes = bytes_token(
            iter::repeat("ten bytes.")
                .take(10)
                .collect::<String>()
                .as_str(),
        );

        harness.test_function(&mut executor, "emit_nums", &[], None);
        harness.test_function(&mut executor, "emit_bases", &[addr1.clone()], None);
        harness.test_function(
            &mut executor,
            "emit_mix",
            &[addr1.clone(), bytes.clone()],
            None,
        );
        harness.test_function(
            &mut executor,
            "emit_addresses",
            &[addr1.clone(), addr2],
            None,
        );

        harness.events_emitted(
            executor,
            &[
                ("Nums", &[uint_token(26), uint_token(42)]),
                ("Bases", &[uint_token(26), addr1.clone()]),
                ("Mix", &[uint_token(26), addr1, uint_token(42), bytes]),
                ("Addresses", &[addr_array]),
            ],
        );
    })
}

#[test]
fn constructor() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(
            &mut executor,
            "constructor.fe",
            "Foo",
            &[uint_token(26), uint_token(42)],
        );

        harness.test_function(&mut executor, "read_bar", &[], Some(&uint_token(68)));
    })
}

#[test]
fn strings() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(
            &mut executor,
            "strings.fe",
            "Foo",
            &[
                string_token("string 1"),
                address_token("1000000000000000000000000000000000000001"),
                string_token("string 2"),
                uint_token(42),
                string_token("string 3"),
            ],
        );

        harness.test_function(
            &mut executor,
            "bar",
            &[string_token("string 4"), string_token("string 5")],
            Some(&string_token("string 5")),
        );

        harness.test_function(
            &mut executor,
            "return_static_string",
            &[],
            Some(&string_token("The quick brown fox jumps over the lazy dog")),
        );

        harness.test_function(
            &mut executor,
            "return_casted_static_string",
            &[],
            Some(&string_token("foo")),
        );

        harness.events_emitted(
            executor,
            &[(
                "MyEvent",
                &[
                    string_token("string 2"),
                    uint_token(42),
                    string_token("string 1"),
                    string_token("string 3"),
                    address_token("1000000000000000000000000000000000000001"),
                    string_token("static string"),
                    string_token("foo"),
                ],
            )],
        );
    });
}

#[test]
fn test_numeric_sizes() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "numeric_sizes.fe", "Foo", &[]);

        for config in NumericAbiTokenBounds::get_all().iter() {
            harness.test_function(
                &mut executor,
                &format!("get_u{}_min", config.size),
                &[],
                Some(&config.u_min.clone()),
            );
            harness.test_function(
                &mut executor,
                &format!("get_u{}_max", config.size),
                &[],
                Some(&config.u_max.clone()),
            );
            harness.test_function(
                &mut executor,
                &format!("get_i{}_min", config.size),
                &[],
                Some(&config.i_min.clone()),
            );
            harness.test_function(
                &mut executor,
                &format!("get_i{}_max", config.size),
                &[],
                Some(&config.i_max.clone()),
            );
        }
    })
}

#[test]
fn sized_vals_in_sto() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "sized_vals_in_sto.fe", "Foo", &[]);

        let num = uint_token(68);
        let nums = u256_array_token(&(0..42).into_iter().collect::<Vec<_>>());
        let string = string_token("there are 26 protons in fe");

        harness.test_function(&mut executor, "write_num", &[num.clone()], None);
        harness.test_function(&mut executor, "read_num", &[], Some(&num));

        harness.test_function(&mut executor, "write_nums", &[nums.clone()], None);
        harness.test_function(&mut executor, "read_nums", &[], Some(&nums));

        harness.test_function(&mut executor, "write_str", &[string.clone()], None);
        harness.test_function(&mut executor, "read_str", &[], Some(&string));

        harness.test_function(&mut executor, "emit_event", &[], None);
        harness.events_emitted(executor, &[("MyEvent", &[num, nums, string])]);
    });
}

#[test]
fn checked_arithmetic() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(
            &mut executor,
            "checked_arithmetic.fe",
            "CheckedArithmetic",
            &[],
        );

        for config in NumericAbiTokenBounds::get_all().iter() {
            // ADDITION

            // unsigned: max_value + 1 fails
            harness.test_function_reverts(
                &mut executor,
                &format!("add_u{}", config.size),
                &[config.u_max.clone(), uint_token(1)],
            );

            // unsigned: max_value + 0 works
            harness.test_function(
                &mut executor,
                &format!("add_u{}", config.size),
                &[config.u_max.clone(), uint_token(0)],
                Some(&config.u_max),
            );

            // signed: max_value + 1 fails
            harness.test_function_reverts(
                &mut executor,
                &format!("add_i{}", config.size),
                &[config.i_max.clone(), int_token(1)],
            );

            // signed: max_value + 0 works
            harness.test_function(
                &mut executor,
                &format!("add_i{}", config.size),
                &[config.i_max.clone(), int_token(0)],
                Some(&config.i_max),
            );

            // signed: min_value + -1 fails
            harness.test_function_reverts(
                &mut executor,
                &format!("add_i{}", config.size),
                &[config.i_min.clone(), int_token(-1)],
            );

            // signed: min_value + 0 works
            harness.test_function(
                &mut executor,
                &format!("add_i{}", config.size),
                &[config.i_min.clone(), int_token(0)],
                Some(&config.i_min),
            );

            // SUBTRACTION
            // unsigned: min_value - 1 fails
            harness.test_function_reverts(
                &mut executor,
                &format!("sub_u{}", config.size),
                &[config.u_min.clone(), uint_token(1)],
            );

            // unsigned: min_value - 0 works
            harness.test_function(
                &mut executor,
                &format!("sub_u{}", config.size),
                &[config.u_min.clone(), uint_token(0)],
                Some(&config.u_min),
            );

            // signed: min_value - 1 fails
            harness.test_function_reverts(
                &mut executor,
                &format!("sub_i{}", config.size),
                &[config.i_min.clone(), int_token(1)],
            );

            // signed: min_value - 0 works
            harness.test_function(
                &mut executor,
                &format!("sub_i{}", config.size),
                &[config.i_min.clone(), int_token(0)],
                Some(&config.i_min),
            );

            // signed: max_value - -1 fails
            harness.test_function_reverts(
                &mut executor,
                &format!("sub_i{}", config.size),
                &[config.i_max.clone(), int_token(-1)],
            );

            // signed: max_value - -0 works
            harness.test_function(
                &mut executor,
                &format!("sub_i{}", config.size),
                &[config.i_max.clone(), int_token(-0)],
                Some(&config.i_max),
            );

            // DIVISON
            // unsigned: anything / 0 fails
            harness.test_function_reverts(
                &mut executor,
                &format!("div_u{}", config.size),
                &[config.u_max.clone(), uint_token(0)],
            );

            // unsigned: 3 / 2 works
            harness.test_function(
                &mut executor,
                &format!("div_u{}", config.size),
                &[uint_token(3), uint_token(2)],
                Some(&uint_token(1)),
            );

            // signed: anything / 0 fails
            harness.test_function_reverts(
                &mut executor,
                &format!("div_i{}", config.size),
                &[config.i_max.clone(), int_token(0)],
            );

            // signed: min_value / -1 fails
            harness.test_function_reverts(
                &mut executor,
                &format!("div_i{}", config.size),
                &[config.i_min.clone(), int_token(-1)],
            );

            // signed: 3 / -2 works
            harness.test_function(
                &mut executor,
                &format!("div_i{}", config.size),
                &[int_token(3), int_token(-2)],
                Some(&int_token(-1)),
            );

            // EXPONENTIATION
            // unsigned: max ** 2 fails
            harness.test_function_reverts(
                &mut executor,
                &format!("pow_u{}", config.size),
                &[config.u_max.clone(), uint_token(2)],
            );

            // unsigned: 2 ** (bit_len-1) works
            harness.test_function(
                &mut executor,
                &format!("pow_u{}", config.size),
                &[uint_token(2), uint_token(config.size - 1)],
                Some(&ethabi::Token::Uint(
                    U256::from(2).pow(U256::from(config.size - 1)),
                )),
            );

            // signed: max ** 2 fails (overflow)
            harness.test_function_reverts(
                &mut executor,
                &format!("pow_i{}", config.size),
                &[config.i_max.clone(), uint_token(2)],
            );

            // signed: min ** 3 fails (underflow)
            harness.test_function_reverts(
                &mut executor,
                &format!("pow_i{}", config.size),
                &[config.i_min.clone(), uint_token(3)],
            );

            // signed: 2 ** (bit_len-2) works
            harness.test_function(
                &mut executor,
                &format!("pow_i{}", config.size),
                &[int_token(2), uint_token(config.size - 2)],
                Some(&ethabi::Token::Int(
                    U256::from(2).pow(U256::from(config.size - 2)),
                )),
            );

            // signed: -2 ** (bit_len-1) works
            harness.test_function(
                &mut executor,
                &format!("pow_i{}", config.size),
                &[int_token(-2), uint_token(config.size - 1)],
                Some(&ethabi::Token::Int(get_2s_complement_for_negative(
                    U256::from(2).pow(U256::from(config.size - 1)),
                ))),
            );

            // MODULO
            // unsigned: anything % 0 fails
            harness.test_function_reverts(
                &mut executor,
                &format!("mod_u{}", config.size),
                &[config.u_max.clone(), uint_token(0)],
            );

            // unsigned: max_value % 2 works
            harness.test_function(
                &mut executor,
                &format!("mod_u{}", config.size),
                &[config.u_max.clone(), uint_token(2)],
                Some(&uint_token(1)),
            );

            // signed: anything % 0 fails
            harness.test_function_reverts(
                &mut executor,
                &format!("mod_i{}", config.size),
                &[config.i_max.clone(), int_token(0)],
            );

            // unsigned: max_value % 2 works
            harness.test_function(
                &mut executor,
                &format!("mod_i{}", config.size),
                &[config.i_max.clone(), int_token(2)],
                Some(&int_token(1)),
            );

            // signed: 13 % -3 works
            harness.test_function(
                &mut executor,
                &format!("mod_i{}", config.size),
                &[int_token(13), int_token(-3)],
                Some(&int_token(1)),
            );

            // signed: -13 % 3 works
            harness.test_function(
                &mut executor,
                &format!("mod_i{}", config.size),
                &[int_token(-13), int_token(3)],
                Some(&int_token(-1)),
            );

            // MULTIPLICATION
            // unsigned: max_value * 2 fails
            harness.test_function_reverts(
                &mut executor,
                &format!("mul_u{}", config.size),
                &[config.u_max.clone(), uint_token(2)],
            );

            // unsigned: max_value * 1 works
            harness.test_function(
                &mut executor,
                &format!("mul_u{}", config.size),
                &[config.u_max.clone(), uint_token(1)],
                Some(&config.u_max),
            );

            // signed: max_value * 2 fails
            harness.test_function_reverts(
                &mut executor,
                &format!("mul_i{}", config.size),
                &[config.i_max.clone(), int_token(2)],
            );

            // signed: max_value * 1 works
            harness.test_function(
                &mut executor,
                &format!("mul_i{}", config.size),
                &[config.i_max.clone(), int_token(1)],
                Some(&config.i_max),
            );

            // signed: max_value * -2 fails
            harness.test_function_reverts(
                &mut executor,
                &format!("mul_i{}", config.size),
                &[config.i_max.clone(), int_token(-2)],
            );

            // signed: min_value * -2 fails
            harness.test_function_reverts(
                &mut executor,
                &format!("mul_i{}", config.size),
                &[config.i_min.clone(), int_token(-2)],
            );

            harness.test_function(
                &mut executor,
                &format!("mul_i{}", config.size),
                &[config.i_min.clone(), int_token(1)],
                Some(&config.i_min),
            );
        }
    });
}

#[test]
fn structs() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "structs.fe", "Foo", &[]);

        harness.test_function(&mut executor, "create_house", &[], None);
        harness.test_function(&mut executor, "bar", &[], Some(&uint_token(2)));

        let encoded_house = [
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 1, 44, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 1, 244, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
        ];
        let token = ethabi::Token::FixedBytes(ethabi::FixedBytes::from(encoded_house));
        harness.test_function(&mut executor, "encode_house", &[], Some(&token));

        harness.test_function(
            &mut executor,
            "hashed_house",
            &[],
            Some(&uint_token_from_dec_str(
                "46276961562062403346660092841258592376337652487249021183958956662511039738107",
            )),
        );
    });
}

#[test]
fn keccak() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "keccak.fe", "Keccak", &[]);
        // The expected value in clear text is on purpose for added clarity. All other
        // tests use get_keccak256 to calculate the expected results on the fly.
        harness.test_function(
            &mut executor,
            "return_hash_from_u256",
            &[ethabi::Token::FixedBytes(
                U256::from(1).to_be_bytes().to_vec(),
            )],
            Some(&uint_token_from_dec_str(
                "80084422859880547211683076133703299733277748156566366325829078699459944778998",
            )),
        );

        harness.test_function(
            &mut executor,
            "return_hash_from_u256",
            &[ethabi::Token::FixedBytes(
                U256::from(1).to_be_bytes().to_vec(),
            )],
            Some(&ethabi::Token::Uint(
                keccak::full_as_bytes(&U256::from(1).to_be_bytes()).into(),
            )),
        );

        harness.test_function(
            &mut executor,
            "return_hash_from_u8",
            &[ethabi::Token::FixedBytes([1].into())],
            Some(&ethabi::Token::Uint(
                keccak::full_as_bytes(&1u8.to_be_bytes()).into(),
            )),
        );

        harness.test_function(
            &mut executor,
            "return_hash_from_u8",
            &[ethabi::Token::FixedBytes([0].into())],
            Some(&ethabi::Token::Uint(
                keccak::full_as_bytes(&0u8.to_be_bytes()).into(),
            )),
        );

        harness.test_function(
            &mut executor,
            "return_hash_from_foo",
            &[bytes_token("foo")],
            Some(&ethabi::Token::Uint(
                keccak::full_as_bytes(&"foo".as_bytes()).into(),
            )),
        );
    });
}

#[test]
fn math() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "math.fe", "Math", &[]);
        harness.test_function(
            &mut executor,
            "sqrt",
            &[uint_token(16000)],
            Some(&uint_token(126)),
        );

        harness.test_function(
            &mut executor,
            "min",
            &[uint_token(1), uint_token(2)],
            Some(&uint_token(1)),
        );
    });
}

#[test]
fn two_contracts() {
    with_executor(&|mut executor| {
        let foo_harness = deploy_contract(&mut executor, "two_contracts.fe", "Foo", &[]);
        let bar_harness = deploy_contract(&mut executor, "two_contracts.fe", "Bar", &[]);

        foo_harness.test_function(&mut executor, "foo", &[], Some(&uint_token(42)));

        bar_harness.test_function(&mut executor, "bar", &[], Some(&uint_token(26)));
    })
}

#[test]
fn external_contract() {
    with_executor(&|mut executor| {
        let foo_harness = deploy_contract(&mut executor, "external_contract.fe", "Foo", &[]);
        let proxy_harness = deploy_contract(&mut executor, "external_contract.fe", "FooProxy", &[]);

        let foo_address = ethabi::Token::Address(foo_harness.address);
        let my_num = uint_token(26);
        let my_addrs = address_array_token(&["0", "1", "42", "3", "4"]);
        let my_string = string_token("hello world");

        proxy_harness.test_function(
            &mut executor,
            "call_emit_event",
            &[
                foo_address.clone(),
                my_num.clone(),
                my_addrs.clone(),
                my_string.clone(),
            ],
            None,
        );

        let a = 26;
        let b = 42;
        let c = 26 * 42;

        proxy_harness.test_function(
            &mut executor,
            "call_build_array",
            &[foo_address, uint_token(a), uint_token(b)],
            Some(&u256_array_token(&[a, c, b])),
        );

        foo_harness.events_emitted(executor, &[("MyEvent", &[my_num, my_addrs, my_string])]);
    })
}

#[test]
fn create2_contract() {
    with_executor(&|mut executor| {
        let factory_harness =
            deploy_contract(&mut executor, "create2_contract.fe", "FooFactory", &[]);

        let foo_address = factory_harness
            .call_function(&mut executor, "create2_foo", &[])
            .expect("factory did not return an address")
            .into_address()
            .expect("not an address");

        let foo_harness = load_contract(foo_address, "create2_contract.fe", "Foo");

        foo_harness.test_function(&mut executor, "get_my_num", &[], Some(&uint_token(42)));
    })
}

#[test]
fn create_contract() {
    with_executor(&|mut executor| {
        let factory_harness =
            deploy_contract(&mut executor, "create_contract.fe", "FooFactory", &[]);

        let foo_address = factory_harness
            .call_function(&mut executor, "create_foo", &[])
            .expect("factory did not return an address")
            .into_address()
            .expect("not an address");

        let foo_harness = load_contract(foo_address, "create_contract.fe", "Foo");

        foo_harness.test_function(&mut executor, "get_my_num", &[], Some(&uint_token(42)));
    })
}

#[test]
fn create_contract_from_init() {
    with_executor(&|mut executor| {
        let factory_harness = deploy_contract(
            &mut executor,
            "create_contract_from_init.fe",
            "FooFactory",
            &[],
        );

        let foo_address = factory_harness
            .call_function(&mut executor, "get_foo_addr", &[])
            .expect("factory did not return an address")
            .into_address()
            .expect("not an address");

        let foo_harness = load_contract(foo_address, "create_contract_from_init.fe", "Foo");

        foo_harness.test_function(&mut executor, "get_my_num", &[], Some(&uint_token(42)));
    })
}

#[test]
fn struct_with_string() {
    with_executor(&|mut executor| {
        let string_harness = deploy_contract(&mut executor, "struct_with_string.fe", "Foo", &[]);

        string_harness.test_function(
            &mut executor,
            "get_some_string",
            &[],
            Some(&string_token("Fooooo")),
        );
    })
}

#[test]
fn nested_struct() {
    with_executor(&|mut executor| {
        let string_harness = deploy_contract(&mut executor, "nested_struct.fe", "Foo", &[]);

        string_harness.test_function(
            &mut executor,
            "get_some_string",
            &[],
            Some(&string_token("Fooooo")),
        );
    })
}

#[test]
fn nested_tuple() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "nested_tuple.fe", "Foo", &[]);

        harness.test_function(&mut executor, "bar", &[], Some(&uint_token(1)));
        harness.test_function(&mut executor, "bar2", &[], Some(&uint_token(2)));
    })
}

#[rstest(
    fixture_file,
    contract_name,
    case("ownable.fe", "Ownable"),
    case("empty.fe", "Empty"),
    case("return_unit.fe", "Foo")
)]
fn can_deploy_fixture(fixture_file: &str, contract_name: &str) {
    with_executor(&|mut executor| {
        deploy_contract(&mut executor, fixture_file, contract_name, &[]);
    })
}

#[test]
fn self_address() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "self_address.fe", "Foo", &[]);
        harness.test_function(
            &mut executor,
            "my_address",
            &[],
            Some(&ethabi::Token::Address(harness.address)),
        );
    });
}

#[rstest(
    target,
    op,
    value,
    expected,
    case(2, "add", 5, 7),
    case(42, "sub", 26, 16),
    case(10, "mul", 42, 420),
    case(43, "div", 5, 8),
    case(43, "mod", 5, 3),
    case(3, "pow", 5, 243),
    case(1, "lshift", 7, 128),
    case(128, "rshift", 7, 1),
    case(26, "bit_or", 42, 58),
    case(26, "bit_xor", 42, 48),
    case(26, "bit_and", 42, 10),
    case(2, "add_from_sto", 5, 7),
    case(2, "add_from_mem", 5, 7)
)]
fn aug_assign(target: usize, op: &str, value: usize, expected: usize) {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "aug_assign.fe", "Foo", &[]);
        harness.test_function(
            &mut executor,
            op,
            &[uint_token(target), uint_token(value)],
            Some(&uint_token(expected)),
        );
    });
}

#[test]
fn base_tuple() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "base_tuple.fe", "Foo", &[]);
        harness.test_function(
            &mut executor,
            "bar",
            &[uint_token(42), bool_token(true)],
            Some(&tuple_token(&[uint_token(42), bool_token(true)])),
        );
    });
}

#[test]
fn tuple_destructuring() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "tuple_destructuring.fe", "Foo", &[]);
        harness.test_function(&mut executor, "bar", &[], Some(&uint_token(42)));
        harness.test_function(
            &mut executor,
            "baz",
            &[uint_token(1), bool_token(false)],
            Some(&uint_token(1)),
        );
    });
}
