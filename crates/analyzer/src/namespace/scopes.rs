#![allow(unstable_name_collisions)] // expect_none, which ain't gonna be stabilized

use crate::context::{
    AnalyzerContext, CallType, ExpressionAttributes, FunctionAttributes, FunctionBody,
};
use crate::db::AnalyzerDb;
use crate::errors::AlreadyDefined2;
use crate::namespace::events::EventDef;
use crate::namespace::items::{EventId, FunctionId, ModuleId};
use crate::namespace::types::{Array, FixedSize, Tuple, Type};
use fe_common::diagnostics::Diagnostic;
use fe_common::Span;
use fe_parser::ast;
use fe_parser::node::{Node, NodeId};
use std::cell::RefCell;
use std::collections::btree_map::Entry;
use std::collections::BTreeMap;
use std::rc::Rc;

pub type Shared<T> = Rc<RefCell<T>>;

pub struct ItemScope<'a> {
    db: &'a dyn AnalyzerDb,
    module: ModuleId,
    diagnostics: Vec<Diagnostic>,
}

impl<'a> AnalyzerContext for ItemScope<'a> {
    fn resolve_type(&self, name: &str) -> Option<Rc<Type>> {
        self.module.resolve_type(self.db, name)
    }
    fn add_diagnostic(&mut self, diag: Diagnostic) {
        self.diagnostics.push(diag)
    }
}

pub struct FunctionScope<'a> {
    pub db: &'a dyn AnalyzerDb,
    pub function: FunctionId,

    pub body: RefCell<FunctionBody>,
    pub diagnostics: RefCell<Vec<Diagnostic>>,
}

impl<'a> FunctionScope<'a> {
    pub fn new(db: &'a dyn AnalyzerDb, function: FunctionId) -> Self {
        Self {
            db,
            function,
            body: RefCell::new(FunctionBody::default()),
            diagnostics: RefCell::new(vec![]),
        }
    }

    pub fn add_diagnostic(&self, diag: Diagnostic) {
        self.diagnostics.borrow_mut().push(diag)
    }

    fn resolve_type(&self, name: &str) -> Option<Rc<Type>> {
        self.function.module(self.db).resolve_type(self.db, name)
    }

    pub fn var_type(&self, name: &str) -> Option<FixedSize> {
        self.function.param(self.db, name)
    }

    pub fn contract_field(&self, name: &str) -> Option<(Rc<Type>, usize)> {
        self.function.contract(self.db).field(self.db, name)
    }

    pub fn contract_function(&self, name: &str) -> Option<Rc<FunctionAttributes>> {
        self.function
            .contract(self.db)
            .functions(self.db)
            .get(name)
            .map(|id| id.typ(self.db))
    }

    pub fn function_return_type(&self) -> FixedSize {
        self.function.typ(self.db).return_type.clone()
    }
    /// Attribute contextual information to an expression node.
    ///
    /// # Panics
    ///
    /// Panics if an entry already exists for the node id.
    pub fn add_expression(&mut self, node: &Node<ast::Expr>, attributes: ExpressionAttributes) {
        // self.add_node(node);
        self.body
            .borrow_mut()
            .expressions
            .insert(node.id, attributes)
            .expect_none("expression attributes already exist");
    }

    /// Update the expression attributes.
    ///
    /// # Panics
    ///
    /// Panics if an entry does not already exist for the node id.
    pub fn update_expression(&mut self, node: &Node<ast::Expr>, attributes: ExpressionAttributes) {
        self.body
            .borrow_mut()
            .expressions
            .insert(node.id, attributes)
            .expect("expression attributes do not exist");
    }
    /// Attribute contextual information to an emit statement node.
    ///
    /// # Panics
    ///
    /// Panics if an entry already exists for the node id.
    pub fn add_emit(&mut self, node: &Node<ast::FuncStmt>, event: EventId) {
        // self.add_node(node);
        self.body
            .borrow_mut()
            .emits
            .insert(node.id, event)
            .expect_none("emit statement attributes already exist");
    }
    /// Attribute contextual information to a declaration node.
    ///
    /// # Panics
    ///
    /// Panics if an entry already exists for the node id.
    pub fn add_declaration(&mut self, node: &Node<ast::FuncStmt>, typ: FixedSize) {
        self.body
            .borrow_mut()
            .declarations
            .insert(node.id, typ)
            .expect_none("declaration attributes already exist");
    }
    /// Attribute contextual information to a call expression node.
    ///
    /// # Panics
    ///
    /// Panics if an entry already exists for the node id.
    pub fn add_call(&mut self, node: &Node<ast::Expr>, call_type: CallType) {
        // self.add_node(node);
        self.body
            .borrow_mut()
            .calls
            .insert(node.id, call_type)
            .expect_none("call attributes already exist");
    }
    /// Attribute contextual information to a type description node.
    ///
    /// # Panics
    ///
    /// Panics if an entry already exists for the node id.
    pub fn add_type_desc(&mut self, node: &Node<ast::TypeDesc>, typ: Type) {
        // self.add_node(node);
        self.body
            .borrow_mut()
            .type_descs
            .insert(node.id, typ)
            .expect_none("type desc attributes already exist");
    }
}

// impl<'a> AnalyzerContext for FunctionScope<'a> {
//     fn add_diagnostic(&mut self, diag: Diagnostic) {
//         self.diagnostics.push(diag)
//     }
// }

pub struct BlockScope<'a, 'b> {
    root: &'a FunctionScope<'b>,
    pub parent: Option<&'a BlockScope<'a, 'b>>,
    pub variable_defs: BTreeMap<String, (FixedSize, Span)>,
    pub typ: BlockScopeType,
}

#[derive(Clone, Debug, PartialEq)]
pub enum BlockScopeType {
    Function,
    IfElse,
    Loop,
}

impl AnalyzerContext for BlockScope<'_, '_> {
    fn resolve_type(&self, name: &str) -> Option<Rc<Type>> {
        self.root.resolve_type(name)
    }
    fn add_diagnostic(&mut self, diag: Diagnostic) {
        self.root.add_diagnostic(diag)
    }
}

impl<'a, 'b> BlockScope<'a, 'b> {
    pub fn new(root: &'a FunctionScope<'b>, typ: BlockScopeType) -> Self {
        BlockScope {
            root,
            parent: None,
            variable_defs: BTreeMap::new(),
            typ,
        }
    }

    pub fn new_child(&'a self, typ: BlockScopeType) -> Self {
        BlockScope {
            root: self.root,
            parent: Some(self),
            variable_defs: BTreeMap::new(),
            typ,
        }
    }

    // /// Return the contract scope and its immediate block scope child
    // fn find_scope_boundary(&self) -> (Shared<ContractScope>, Shared<BlockScope>) {
    //     let mut parent = self.parent.clone();
    //     let mut last_block_scope = Rc::new(RefCell::new(self.clone()));
    //     loop {
    //         parent = match parent {
    //             BlockScopeParent::Block(ref scope) => {
    //                 last_block_scope = Rc::clone(&scope);
    //                 scope.borrow().parent.clone()
    //             }
    //             BlockScopeParent::Contract(ref scope) => {
    //                 return (Rc::clone(&scope), last_block_scope)
    //             }
    //         }
    //     }
    // }

    // /// Return the block scope that is associated with the function block
    // pub fn function_scope(&self) -> Shared<BlockScope> {
    //     let (_, function_scope) = self.find_scope_boundary();
    //     function_scope
    // }

    // /// Lookup an event definition on the inherited contract scope
    // pub fn contract_event_def(&self, name: &str) -> Option<EventDef> {
    //     self.contract_scope().borrow().event_def(name)
    // }

    /// Lookup a field definition on the inherited contract scope
    pub fn contract_field(&self, name: &str) -> Option<(Rc<Type>, usize)> {
        self.root.contract_field(name)
    }

    /// Lookup a function definition on the inherited contract scope.
    pub fn contract_function(&self, name: &str) -> Option<Rc<FunctionAttributes>> {
        self.root.contract_function(name)
    }

    // /// Lookup the function definition for the current block scope on the
    // /// inherited contract scope.
    // pub fn current_function_def(&self) -> Option<ContractFunctionDef> {
    //     self.contract_function_def(&self.function_scope().borrow().name)
    // }

    pub fn function_return_type(&self) -> FixedSize {
        self.root.function_return_type()
    }

    /// Lookup a definition in current or inherited block scope
    pub fn var_type(&self, name: &str) -> Option<FixedSize> {
        self.variable_defs
            .get(name)
            .map(|(typ, _)| (*typ).clone())
            .or_else(|| self.parent?.var_type(name))
            .or_else(|| self.root.var_type(name))
    }

    /// Add a variable to the block scope.
    pub fn add_var(
        &mut self,
        name: &str,
        typ: FixedSize,
        span: Span,
    ) -> Result<(), AlreadyDefined2<Span>> {
        match self.variable_defs.entry(name.to_owned()) {
            Entry::Occupied(e) => Err(AlreadyDefined2(e.get().1)),
            Entry::Vacant(e) => {
                e.insert((typ, span));
                Ok(())
            }
        }
    }

    /// Return true if the scope or any of its parents is of the given type
    pub fn inherits_type(&self, typ: BlockScopeType) -> bool {
        self.typ == typ || self.parent.map_or(false, |scope| scope.inherits_type(typ))
    }

    // /// Filter module scope for type definitions that match the given predicate
    // pub fn get_module_type_defs<B, F: FnMut(&Type) -> Option<B>>(&self, predicate: F) -> Vec<B> {
    //     self.module_scope().borrow().get_type_defs(predicate)
    // }

    // /// Gets a type definition by name.
    // pub fn get_module_type_def(&self, name: &str) -> Option<Type> {
    //     self.module_scope().borrow().get_type_def(name)
    // }
}

// #[derive(Clone, Debug, PartialEq)]
// pub struct ContractFunctionDef {
//     pub is_public: bool,
//     pub name: String,
//     pub params: Vec<(String, FixedSize)>,
//     pub return_type: FixedSize,
//     pub scope: Shared<BlockScope>,
// }

// #[derive(Clone, Debug, PartialEq)]
// pub struct ModuleScope {
//     /// Type definitions in a module.
//     pub type_defs: BTreeMap<String, Type>,
//     /// Tuples that were used inside of a module.
//     ///
//     /// BTreeSet is used for ordering, this way items are retrieved in the same order every time.
//     pub tuples_used: BTreeSet<Tuple>,
// }

// #[derive(Clone, Debug, PartialEq)]
// pub struct ContractScope {
//     pub name: String,
//     pub parent: Shared<ModuleScope>,
//     pub interface: Vec<String>,
//     pub event_defs: BTreeMap<String, EventDef>,
//     pub field_defs: BTreeMap<String, ContractFieldDef>,
//     pub function_defs: BTreeMap<String, ContractFunctionDef>,
//     pub list_expressions: BTreeSet<Array>,
//     pub string_defs: BTreeSet<String>,
//     pub created_contracts: BTreeSet<String>,
//     num_fields: usize,
// }

// #[derive(Clone, Debug, PartialEq)]
// pub enum Scope {
//     Module(Shared<ModuleScope>),
//     Contract(Shared<ContractScope>),
//     Block(Shared<BlockScope>),
// }

// impl Scope {
//     pub fn module_scope(&self) -> Shared<ModuleScope> {
//         match self {
//             Scope::Module(scope) => Rc::clone(scope),
//             Scope::Contract(scope) => Rc::clone(&scope.borrow().parent),
//             Scope::Block(scope) => Rc::clone(&scope.borrow().contract_scope().borrow().parent),
//         }
//     }
// }

// impl ModuleScope {
//     pub fn new() -> Shared<Self> {
//         Rc::new(RefCell::new(ModuleScope {
//             type_defs: BTreeMap::new(),
//             tuples_used: BTreeSet::new(),
//         }))
//     }

//     /// Add a type definiton to the scope
//     pub fn add_type_def(&mut self, name: &str, typ: Type) -> Result<(), AlreadyDefined> {
//         match self.type_defs.entry(name.to_owned()) {
//             Entry::Occupied(_) => Err(AlreadyDefined),
//             Entry::Vacant(entry) => Ok(entry.insert(typ)).map(|_| ()),
//         }
//     }

//     /// Filter module scope for type definitions that match the given predicate
//     pub fn get_type_defs<B, F: FnMut(&Type) -> Option<B>>(&self, predicate: F) -> Vec<B> {
//         self.type_defs.values().filter_map(predicate).collect()
//     }

//     /// Gets a type definition by name.
//     pub fn get_type_def(&self, name: &str) -> Option<Type> {
//         self.type_defs.get(name).map(|typ| typ.to_owned())
//     }
// }

// impl ContractScope {
//     pub fn new(name: &str, parent: Shared<ModuleScope>) -> Shared<Self> {
//         Rc::new(RefCell::new(ContractScope {
//             name: name.to_owned(),
//             parent,
//             function_defs: BTreeMap::new(),
//             event_defs: BTreeMap::new(),
//             field_defs: BTreeMap::new(),
//             string_defs: BTreeSet::new(),
//             interface: vec![],
//             created_contracts: BTreeSet::new(),
//             list_expressions: BTreeSet::new(),
//             num_fields: 0,
//         }))
//     }

//     /// Return the module scope that the contract scope inherits from
//     pub fn module_scope(&self) -> Shared<ModuleScope> {
//         Rc::clone(&self.parent)
//     }

//     // these should be on ModuleId:
//     //
//     // /// Filter module scope for type definitions that match the given predicate
//     // pub fn get_module_type_defs<B, F: FnMut(&Type) -> Option<B>>(&self, predicate: F) -> Vec<B> {
//     //     self.module_scope().borrow().get_type_defs(predicate)
//     // }
//     // /// Lookup contract event definition by its name.
//     // pub fn event_def(&self, name: &str) -> Option<EventDef> {
//     //     self.event_defs.get(name).map(|def| (*def).clone())
//     // }
//     // /// Lookup contract field definition by its name.
//     // pub fn field_def(&self, name: &str) -> Option<ContractFieldDef> {
//     //     self.field_defs.get(name).map(|def| (*def).clone())
//     // }
//     // /// Lookup contract function definition by its name.
//     // pub fn function_def(&self, name: &str) -> Option<ContractFunctionDef> {
//     //     self.function_defs.get(name).map(|def| (*def).clone())
//     // }

//     // these should be moved to the lowering/yulgen stages:
//     //
//     // /// Add a static string definition to the scope.
//     // pub fn add_string(&mut self, value: &str) {
//     //     self.string_defs.insert(value.to_owned());
//     // }
//     // /// Add the name of another contract that has been created within this
//     // /// contract.
//     // pub fn add_created_contract(&mut self, name: &str) {
//     //     self.created_contracts.insert(name.to_owned());
//     // }
//     // /// Add the array type of a list expression that was used within the contract.
//     // pub fn add_used_list_expression(&mut self, typ: Array) {
//     //     self.list_expressions.insert(typ);
//     // }
// }

/// temporary helper until `BTreeMap::try_insert` is stabilized

trait OptionExt {
    fn expect_none(self, msg: &str);
}

impl<T> OptionExt for Option<T> {
    fn expect_none(self, msg: &str) {
        if self.is_some() {
            panic!("{}", msg)
        }
    }
}
