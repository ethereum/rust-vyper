use crate::errors::CompileError;
use crate::yul::{base, constructor, selectors};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use vyper_parser::ast as vyp;
use yultsur::yul;

pub type Shared<T> = Rc<RefCell<T>>;

#[allow(dead_code)]
pub struct ModuleScope<'a> {
    type_defs: HashMap<&'a str, &'a vyp::TypeDesc<'a>>,
}

#[allow(dead_code)]
pub struct ContractScope<'a> {
    parent: Shared<ModuleScope<'a>>,
}

#[allow(dead_code)]
pub struct FunctionScope<'a> {
    parent: Shared<ContractScope<'a>>,
}

#[allow(dead_code)]
pub enum Scope<'a> {
    Module(Shared<ModuleScope<'a>>),
    Contract(Shared<ContractScope<'a>>),
    Function(Shared<FunctionScope<'a>>),
}

impl<'a> ModuleScope<'a> {
    fn new() -> Shared<Self> {
        Rc::new(RefCell::new(ModuleScope {
            type_defs: HashMap::new(),
        }))
    }
}

impl<'a> ContractScope<'a> {
    fn new(parent: Shared<ModuleScope<'a>>) -> Shared<Self> {
        Rc::new(RefCell::new(ContractScope { parent }))
    }
}

impl<'a> FunctionScope<'a> {
    fn new(parent: Shared<ContractScope<'a>>) -> Shared<Self> {
        Rc::new(RefCell::new(FunctionScope { parent }))
    }
}

/// Builds a vector of Yul contracts from a Vyper module.
pub fn module<'a>(module: &'a vyp::Module<'a>) -> Result<Vec<yul::Object>, CompileError> {
    let scope = ModuleScope::new();

    Ok(module
        .body
        .iter()
        .map(|stmt| module_stmt(Rc::clone(&scope), &stmt.node))
        .collect::<Result<Vec<Option<yul::Statement>>, CompileError>>()?
        .into_iter()
        .filter_map(|statement| {
            if let Some(yul::Statement::Object(object)) = statement {
                return Some(object);
            }

            None
        })
        .collect::<Vec<yul::Object>>())
}

/// Builds a Yul statement from a Vyper module statement.
pub fn module_stmt<'a>(
    scope: Shared<ModuleScope<'a>>,
    stmt: &'a vyp::ModuleStmt<'a>,
) -> Result<Option<yul::Statement>, CompileError> {
    match stmt {
        vyp::ModuleStmt::TypeDef { .. } => {
            type_def(scope, stmt)?;
            Ok(None)
        }
        vyp::ModuleStmt::ContractDef { .. } => {
            contract_def(scope, stmt).map(|object| Some(yul::Statement::Object(object)))
        }
        _ => Err(CompileError::static_str(
            "Unable to translate module statement.",
        )),
    }
}

/// Builds a Yul object from a Vyper contract.
pub fn contract_def<'a>(
    scope: Shared<ModuleScope<'a>>,
    stmt: &'a vyp::ModuleStmt<'a>,
) -> Result<yul::Object, CompileError> {
    let new_scope = ContractScope::new(scope);

    if let vyp::ModuleStmt::ContractDef { name: _, body } = stmt {
        let mut statements = body
            .iter()
            .map(|stmt| contract_stmt(Rc::clone(&new_scope), &stmt.node))
            .collect::<Result<Vec<yul::Statement>, CompileError>>()?;

        // TODO: Use functions from actual contract ABI once the builder is merged.
        statements.push(yul::Statement::Switch(selectors::switch(vec![])?));

        return Ok(yul::Object {
            name: base::untyped_identifier("Contract"),
            code: constructor::code(),
            objects: vec![yul::Object {
                name: base::untyped_identifier("runtime"),
                code: yul::Code {
                    block: yul::Block { statements },
                },
                objects: vec![],
            }],
        });
    }

    Err(CompileError::static_str(
        "Contract definition translation requires ContractDef.",
    ))
}

/// Builds a Yul statement from a Vyper contract statement.
pub fn contract_stmt<'a>(
    scope: Shared<ContractScope<'a>>,
    stmt: &'a vyp::ContractStmt<'a>,
) -> Result<yul::Statement, CompileError> {
    match stmt {
        vyp::ContractStmt::FuncDef { .. } => {
            func_def(scope, stmt).map(|definition| yul::Statement::FunctionDefinition(definition))
        }
        _ => Err(CompileError::static_str(
            "Unable to translate module statement.",
        )),
    }
}

/// Adds the custom type def to the module scope.
fn type_def<'a>(
    scope: Shared<ModuleScope<'a>>,
    stmt: &'a vyp::ModuleStmt<'a>,
) -> Result<(), CompileError> {
    if let vyp::ModuleStmt::TypeDef { name, typ } = stmt {
        scope.borrow_mut().type_defs.insert(name.node, &typ.node);
        return Ok(());
    }

    Err(CompileError::static_str(
        "Type definition translation requires TypeDef.",
    ))
}

/// Builds a Yul function definition from a Vyper function definition.
pub fn func_def<'a>(
    scope: Shared<ContractScope<'a>>,
    stmt: &'a vyp::ContractStmt<'a>,
) -> Result<yul::FunctionDefinition, CompileError> {
    if let vyp::ContractStmt::FuncDef {
        qual: _,
        name,
        args,
        return_type,
        body,
    } = stmt
    {
        let new_scope = FunctionScope::new(scope);

        let parameters = args
            .iter()
            .map(|arg| func_def_arg(Rc::clone(&new_scope), &arg.node))
            .collect::<Result<Vec<yul::Identifier>, CompileError>>()?;

        let returns = if return_type.is_some() {
            vec![base::untyped_identifier("return_val")]
        } else {
            Vec::new()
        };

        let statements: Vec<yul::Statement> = body
            .iter()
            .map(|stmt| func_stmt(Rc::clone(&new_scope), &stmt.node))
            .collect::<Result<Vec<yul::Statement>, CompileError>>()?;

        return Ok(yul::FunctionDefinition {
            name: base::untyped_identifier(name.node),
            parameters,
            returns,
            block: yul::Block { statements },
        });
    }

    Err(CompileError::static_str(
        "Function definition translation requires FuncDef.",
    ))
}

/// Builds a Yul identifier from a Vyper function argument.
pub fn func_def_arg<'a>(
    _scope: Shared<FunctionScope<'a>>,
    arg: &'a vyp::FuncDefArg<'a>,
) -> Result<yul::Identifier, CompileError> {
    Ok(base::untyped_identifier(arg.name.node))
}

/// Builds a Yul statement from a function statement.
pub fn func_stmt<'a>(
    scope: Shared<FunctionScope<'a>>,
    stmt: &'a vyp::FuncStmt<'a>,
) -> Result<yul::Statement, CompileError> {
    match stmt {
        vyp::FuncStmt::Return { .. } => func_return(scope, stmt),
        _ => Err(CompileError::static_str(
            "Unable to translate function statement",
        )),
    }
}

/// Builds a Yul return statement from a Vyper return statement.
fn func_return<'a>(
    scope: Shared<FunctionScope<'a>>,
    stmt: &'a vyp::FuncStmt<'a>,
) -> Result<yul::Statement, CompileError> {
    if let vyp::FuncStmt::Return { value: Some(value) } = stmt {
        let return_value = expr(scope, &value.node)?;

        return Ok(yul::Statement::Assignment(yul::Assignment {
            identifiers: vec![base::untyped_identifier("return_val")],
            expression: return_value,
        }));
    }

    Err(CompileError::static_str(
        "Function return translation requires Return parameter.",
    ))
}

/// Builds a Yul expression from a Vyper expression.
fn expr<'a>(
    _scope: Shared<FunctionScope<'a>>,
    expr: &'a vyp::Expr<'a>,
) -> Result<yul::Expression, CompileError> {
    match expr {
        vyp::Expr::Name(name) => Ok(base::untyped_literal_expr(name)),
        _ => Err(CompileError::static_str("Unable to translate expression")),
    }
}

#[cfg(test)]
mod tests {
    use crate::yul::ast_builder::{contract_def, func_def, type_def, ContractScope, ModuleScope};
    use std::rc::Rc;
    use vyper_parser::ast::TypeDesc;
    use vyper_parser::parsers;

    #[test]
    fn test_compile_type_def() {
        let src = "type Foo = u256";
        let toks = vyper_parser::get_parse_tokens(src).unwrap();
        let stmt = parsers::type_def(&toks[..]).unwrap().1.node;
        let scope = ModuleScope::new();

        type_def(Rc::clone(&scope), &stmt).expect("Unable to handle type def");

        assert_eq!(
            scope.borrow().type_defs["Foo"],
            &TypeDesc::Base { base: "u256" },
            "Compilation of type definition failed."
        );
    }

    #[test]
    fn test_compile_func_def() {
        let src = "def foo(x: u256) -> u256:\n   return x";
        let toks = vyper_parser::get_parse_tokens(src).unwrap();
        let stmt = parsers::func_def(&toks[..]).unwrap().1.node;
        let scope = ContractScope::new(ModuleScope::new());

        let result = func_def(Rc::clone(&scope), &stmt).expect("Unable to build func def");

        assert_eq!(
            result.to_string(),
            "function foo(x) -> return_val { return_val := x }",
            "Compilation of function definition failed."
        );
    }

    #[test]
    fn test_contract_def() {
        let src = "contract Foo:\
                   \n  pub def bar(x: u256) -> u256:\
                   \n    return x";
        let toks = vyper_parser::get_parse_tokens(src).unwrap();
        let stmt = parsers::contract_def(&toks[..]).unwrap().1.node;
        let scope = ModuleScope::new();

        let result = contract_def(scope, &stmt).expect("Unable to handle contract def");

        assert_eq!(
            result.to_string(),
            r#"object "Contract" { code { let size := datasize("runtime") datacopy(0, dataoffset("runtime"), size) return(0, size) } object "runtime" { code { function bar(x) -> return_val { return_val := x } switch shr(224, calldataload(0))  }  } }"#,
            "Compilation of contract definition failed."
        );
    }
}