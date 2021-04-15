use std::collections::{
    BTreeMap,
    HashMap,
};
use std::str::FromStr;

use wasm_bindgen::prelude::*;

use evm_runtime::{
    ExitReason,
    Handler,
};
use primitive_types::{
    self,
    H160,
    U256,
};
use stringreader::StringReader;

#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

const DEFAULT_CALLER: &str = "1000000000000000000000000000000000000001";

#[derive(serde::Serialize, serde::Deserialize)]
struct CompiledContract {
    json_abi: String,
    bytecode: String,
}

#[wasm_bindgen(module = "/js/solc.js")]
extern "C" {
    #[wasm_bindgen(catch, js_name = "compileContract")]
    fn compile_contract(name: &str, value: String) -> Result<String, JsValue>;

}

#[wasm_bindgen]
pub fn compile(input: &str) -> Result<JsValue, JsValue> {
    console_error_panic_hook::set_once();

    let named_contracts =
        fe_compiler::compile(input, false, false).map_err(|e| format!("{}", e))?;

    let mut named_compiled_contracts = HashMap::new();
    for (name, contract) in named_contracts.contracts {
        let compiled_contract = CompiledContract {
            json_abi: contract.json_abi,
            bytecode: compile_contract(&name, contract.yul)?,
        };

        named_compiled_contracts.insert(name, compiled_contract);
    }

    Ok(JsValue::from_serde(&named_compiled_contracts).unwrap())
}

#[wasm_bindgen]
pub fn run(
    named_contracts: JsValue,
    contract_name: String,
    function_name: String,
    init_args: Vec<JsValue>,
    function_args: Vec<JsValue>,
) -> Result<String, JsValue> {
    console_error_panic_hook::set_once();

    if !init_args.is_empty() || !function_args.is_empty() {
        return Err("args is not supported yet".into());
    }

    let named_contracts: HashMap<String, CompiledContract> =
        named_contracts.into_serde().map_err(|e| e.to_string())?;
    let contract = named_contracts
        .get(&contract_name)
        .ok_or_else(|| format!("{} is not included in passed contracts", contract_name))?;
    let abi =
        ethabi::Contract::load(StringReader::new(&contract.json_abi)).map_err(|e| e.to_string())?;
    let bytecode = hex::decode(&contract.bytecode).map_err(|e| e.to_string())?;
    if abi.constructor.is_some() {
        return Err("contract with constructor is not supported yet".into());
    }

    let vicinity = evm::backend::MemoryVicinity {
        gas_price: U256::zero(),
        origin: H160::zero(),
        chain_id: U256::zero(),
        block_hashes: Vec::new(),
        block_number: U256::zero(),
        block_coinbase: H160::zero(),
        block_timestamp: U256::zero(),
        block_difficulty: U256::zero(),
        block_gas_limit: U256::MAX,
    };
    //let state: BTreeMap<primitive_types::H160, evm::backend::MemoryAccount> =
    // BTreeMap::new();
    let state = BTreeMap::new();
    let backend = evm::backend::MemoryBackend::new(&vicinity, state);
    let config = evm::Config::istanbul();
    let mut executor = evm::executor::StackExecutor::new(&backend, usize::max_value(), &config);

    let harness = match executor.create(
        address(DEFAULT_CALLER),
        evm_runtime::CreateScheme::Legacy {
            caller: address(DEFAULT_CALLER),
        },
        U256::zero(),
        bytecode,
        None,
    ) {
        evm::Capture::Exit(exit) => {
            ContractHarness::new(exit.1.expect("Unable to retrieve contract address"), abi)
        }
        else_ => return Err(format!("{:?}", else_).into()),
    };

    Ok(harness
        .call_function(&mut executor, &function_name, &[])
        .map_or_else(|| "()".to_string(), |abi| format!("{:?}", abi)))
}

struct ContractHarness {
    address: H160,
    abi: ethabi::Contract,
    caller: H160,
    value: U256,
}

type Executor<'a> = evm::executor::StackExecutor<'a, 'a, evm::backend::MemoryBackend<'a>>;

impl ContractHarness {
    fn new(contract_address: H160, abi: ethabi::Contract) -> Self {
        let caller = address(DEFAULT_CALLER);

        ContractHarness {
            address: contract_address,
            abi,
            caller,
            value: U256::zero(),
        }
    }

    fn capture_call(
        &self,
        executor: &mut Executor,
        name: &str,
        input: &[ethabi::Token],
    ) -> evm::Capture<(evm::ExitReason, Vec<u8>), std::convert::Infallible> {
        let function = &self.abi.functions[name][0];

        let context = evm::Context {
            address: self.address,
            caller: self.caller,
            apparent_value: self.value,
        };

        let input = function
            .encode_input(input)
            .expect("Unable to encode input");

        executor.call(self.address, None, input, None, false, context)
    }

    fn call_function(
        &self,
        executor: &mut Executor,
        name: &str,
        input: &[ethabi::Token],
    ) -> Option<ethabi::Token> {
        let function = &self.abi.functions[name][0];

        match self.capture_call(executor, name, &input) {
            evm::Capture::Exit((ExitReason::Succeed(_), output)) => {
                let output = function
                    .decode_output(&output)
                    .expect(&format!("unable to decode output: {:?}", &output))
                    .pop();
                return output;
            }
            evm::Capture::Exit((reason, _)) => panic!("failed to run \"{}\": {:?}", name, reason),
            _ => panic!("trap"),
        }
    }
}

#[allow(dead_code)]
fn address(s: &str) -> H160 {
    H160::from_str(s).expect(&format!("couldn't create address from: {}", s))
}
