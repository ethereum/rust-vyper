#![allow(unused_imports, unused_variables, dead_code)]

use fe_analyzer::errors::AnalyzerError;
use fe_analyzer::AnalyzerDb;
use fe_common::diagnostics::cs;
use fe_common::diagnostics::{diagnostics_string, print_diagnostics, Diagnostic, Label};
use fe_common::files::{FileStore, SourceFileId};
use fe_parser::node::Span;
use insta::assert_snapshot;
use std::collections::hash_map::DefaultHasher;
use std::fmt::Debug;
use std::hash::{Hash, Hasher};

macro_rules! test_analysis {
    ($name:ident, $path:expr) => {
        #[test]
        // #[wasm_bindgen_test]
        fn $name() {
            // let mut files = FileStore::new();
            // let src = test_files::fixture($path);
            // let id = files.add_file($path, src);
            // let fe_module = match fe_parser::parse_file(&src, id) {
            //     Ok((module, _)) => module,
            //     Err(diags) => {
            //         print_diagnostics(&diags, &files);
            //         panic!("parsing failed");
            //     }
            // };
            // XXX
        }
    };
}

/* XXX
test_analysis! { erc20_token, "demos/erc20_token.fe"}
test_analysis! { guest_book, "demos/guest_book.fe"}
test_analysis! { uniswap, "demos/uniswap.fe"}
test_analysis! { address_bytes10_map, "features/address_bytes10_map.fe"}
test_analysis! { assert, "features/assert.fe"}
test_analysis! { aug_assign, "features/aug_assign.fe"}
test_analysis! { base_tuple, "features/base_tuple.fe"}
test_analysis! { call_statement_with_args, "features/call_statement_with_args.fe"}
test_analysis! { call_statement_with_args_2, "features/call_statement_with_args_2.fe"}
test_analysis! { call_statement_without_args, "features/call_statement_without_args.fe"}
test_analysis! { checked_arithmetic, "features/checked_arithmetic.fe"}
test_analysis! { constructor, "features/constructor.fe"}
test_analysis! { create2_contract, "features/create2_contract.fe"}
test_analysis! { create_contract, "features/create_contract.fe"}
test_analysis! { create_contract_from_init, "features/create_contract_from_init.fe"}
test_analysis! { empty, "features/empty.fe"}
test_analysis! { events, "features/events.fe"}
test_analysis! { external_contract, "features/external_contract.fe"}
test_analysis! { for_loop_with_break, "features/for_loop_with_break.fe"}
test_analysis! { for_loop_with_continue, "features/for_loop_with_continue.fe"}
test_analysis! { for_loop_with_static_array, "features/for_loop_with_static_array.fe"}
test_analysis! { if_statement, "features/if_statement.fe"}
test_analysis! { if_statement_2, "features/if_statement_2.fe"}
test_analysis! { if_statement_with_block_declaration, "features/if_statement_with_block_declaration.fe"}
test_analysis! { keccak, "features/keccak.fe"}
test_analysis! { math, "features/math.fe"}
test_analysis! { multi_param, "features/multi_param.fe"}
test_analysis! { nested_map, "features/nested_map.fe"}
test_analysis! { numeric_sizes, "features/numeric_sizes.fe"}
test_analysis! { ownable, "features/ownable.fe"}
test_analysis! { return_addition_i256, "features/return_addition_i256.fe"}
test_analysis! { return_addition_u128, "features/return_addition_u128.fe"}
test_analysis! { return_addition_u256, "features/return_addition_u256.fe"}
test_analysis! { return_array, "features/return_array.fe"}
test_analysis! { return_bitwiseand_u128, "features/return_bitwiseand_u128.fe"}
test_analysis! { return_bitwiseand_u256, "features/return_bitwiseand_u256.fe"}
test_analysis! { return_bitwiseor_u256, "features/return_bitwiseor_u256.fe"}
test_analysis! { return_bitwiseshl_u256, "features/return_bitwiseshl_u256.fe"}
test_analysis! { return_bitwiseshr_i256, "features/return_bitwiseshr_i256.fe"}
test_analysis! { return_bitwiseshr_u256, "features/return_bitwiseshr_u256.fe"}
test_analysis! { return_bitwisexor_u256, "features/return_bitwisexor_u256.fe"}
test_analysis! { return_bool_false, "features/return_bool_false.fe"}
test_analysis! { return_bool_inverted, "features/return_bool_inverted.fe"}
test_analysis! { return_bool_op_and, "features/return_bool_op_and.fe"}
test_analysis! { return_bool_op_or, "features/return_bool_op_or.fe"}
test_analysis! { return_bool_true, "features/return_bool_true.fe"}
test_analysis! { return_builtin_attributes, "features/return_builtin_attributes.fe"}
test_analysis! { return_division_i256, "features/return_division_i256.fe"}
test_analysis! { return_division_u256, "features/return_division_u256.fe"}
test_analysis! { return_empty_tuple, "features/return_unit.fe"}
test_analysis! { return_eq_u256, "features/return_eq_u256.fe"}
test_analysis! { return_gt_i256, "features/return_gt_i256.fe"}
test_analysis! { return_gt_u256, "features/return_gt_u256.fe"}
test_analysis! { return_gte_i256, "features/return_gte_i256.fe"}
test_analysis! { return_gte_u256, "features/return_gte_u256.fe"}
test_analysis! { return_i128_cast, "features/return_i128_cast.fe"}
test_analysis! { return_i256, "features/return_i256.fe"}
test_analysis! { return_identity_u8, "features/return_identity_u8.fe"}
test_analysis! { return_identity_u16, "features/return_identity_u16.fe"}
test_analysis! { return_identity_u32, "features/return_identity_u32.fe"}
test_analysis! { return_identity_u64, "features/return_identity_u64.fe"}
test_analysis! { return_identity_u128, "features/return_identity_u128.fe"}
test_analysis! { return_identity_u256, "features/return_identity_u256.fe"}
test_analysis! { return_lt_i256, "features/return_lt_i256.fe"}
test_analysis! { return_lt_u128, "features/return_lt_u128.fe"}
test_analysis! { return_lt_u256, "features/return_lt_u256.fe"}
test_analysis! { return_lte_i256, "features/return_lte_i256.fe"}
test_analysis! { return_lte_u256, "features/return_lte_u256.fe"}
test_analysis! { return_mod_i256, "features/return_mod_i256.fe"}
test_analysis! { return_mod_u256, "features/return_mod_u256.fe"}
test_analysis! { return_msg_sig, "features/return_msg_sig.fe"}
test_analysis! { return_multiplication_i256, "features/return_multiplication_i256.fe"}
test_analysis! { return_multiplication_u256, "features/return_multiplication_u256.fe"}
test_analysis! { return_noteq_u256, "features/return_noteq_u256.fe"}
test_analysis! { return_pow_i256, "features/return_pow_i256.fe"}
test_analysis! { return_pow_u256, "features/return_pow_u256.fe"}
test_analysis! { return_subtraction_i256, "features/return_subtraction_i256.fe"}
test_analysis! { return_subtraction_u256, "features/return_subtraction_u256.fe"}
test_analysis! { return_u128_cast, "features/return_u128_cast.fe"}
test_analysis! { return_u256, "features/return_u256.fe"}
test_analysis! { return_u256_from_called_fn, "features/return_u256_from_called_fn.fe"}
test_analysis! { return_u256_from_called_fn_with_args, "features/return_u256_from_called_fn_with_args.fe"}
test_analysis! { revert, "features/revert.fe"}
test_analysis! { self_address, "features/self_address.fe"}
test_analysis! { sized_vals_in_sto, "features/sized_vals_in_sto.fe"}
test_analysis! { strings, "features/strings.fe"}
test_analysis! { structs, "features/structs.fe"}
test_analysis! { ternary_expression, "features/ternary_expression.fe"}
test_analysis! { two_contracts, "features/two_contracts.fe"}
test_analysis! { u8_u8_map, "features/u8_u8_map.fe"}
test_analysis! { u16_u16_map, "features/u16_u16_map.fe"}
test_analysis! { u32_u32_map, "features/u32_u32_map.fe"}
test_analysis! { u64_u64_map, "features/u64_u64_map.fe"}
test_analysis! { u128_u128_map, "features/u128_u128_map.fe"}
test_analysis! { u256_u256_map, "features/u256_u256_map.fe"}
test_analysis! { while_loop, "features/while_loop.fe"}
test_analysis! { while_loop_with_break, "features/while_loop_with_break.fe"}
test_analysis! { while_loop_with_break_2, "features/while_loop_with_break_2.fe"}
test_analysis! { while_loop_with_continue, "features/while_loop_with_continue.fe"}
test_analysis! { abi_encoding_stress, "stress/abi_encoding_stress.fe"}
test_analysis! { data_copying_stress, "stress/data_copying_stress.fe"}
test_analysis! { tuple_stress, "stress/tuple_stress.fe"}
 */

fn build_snapshot(path: &str, src: &str, db: &dyn AnalyzerDb) -> String {
    todo!()
    // let mut file_store = FileStore::new();
    // let id = file_store.add_file(path, src);

    // let diagnostics = [
    //     build_diagnostics(id, &context.get_spanned_expressions()),
    //     build_diagnostics(id, &context.get_spanned_emits()),
    //     build_diagnostics(id, &context.get_spanned_functions()),
    //     build_diagnostics(id, &context.get_spanned_declarations()),
    //     build_diagnostics(id, &context.get_spanned_contracts()),
    //     build_diagnostics(id, &context.get_spanned_calls()),
    //     build_diagnostics(id, &context.get_spanned_events()),
    //     build_diagnostics(id, &context.get_spanned_type_descs()),
    // ]
    // .concat();

    // format!(
    //     "{:#?}\n\n{}",
    //     context
    //         .get_module()
    //         .expect("context is missing module attributes"),
    //     diagnostics_string(&diagnostics, &file_store)
    // )
}

fn build_diagnostics<T: Hash + Debug>(
    file_id: SourceFileId,
    spanned_attributes: &[(Span, T)],
) -> Vec<cs::Diagnostic<SourceFileId>> {
    spanned_attributes
        .iter()
        .map(|(span, attributes)| build_attributes_diagnostic(file_id, span, attributes))
        .collect::<Vec<_>>()
}

fn build_attributes_diagnostic<T: Hash + Debug>(
    file_id: SourceFileId,
    span: &Span,
    attributes: &T,
) -> cs::Diagnostic<SourceFileId> {
    // Hash the attributes and label the span with it.
    let label = cs::Label::primary(file_id, span.start..span.end)
        .with_message(format!("attributes hash: {}", hash(attributes)));
    cs::Diagnostic::note()
        .with_labels(vec![label])
        .with_notes(vec![format!("{:#?}", attributes)])
}

fn hash<T: Hash>(item: &T) -> u64 {
    let mut s = DefaultHasher::new();
    item.hash(&mut s);
    s.finish()
}
