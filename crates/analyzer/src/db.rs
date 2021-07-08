use crate::context::{Analysis, FunctionBody};
use crate::namespace::items::{
    self, ContractId, EventId, FunctionId, ModuleId, StructId, TypeAliasId, TypeDefId,
};
use crate::namespace::{events, types};
use fe_common::diagnostics::Diagnostic;
use fe_parser::node::NodeId;
use indexmap::IndexMap;
use std::collections::BTreeMap;
use std::rc::Rc;

mod queries;

// Given an item (intern) id, you should be able to get the item's parent.

// Plan:
//
// "items" = {module, contract, struct, event, function}
//
// Items are interned, and referred to by their InternId in the analyzer and compiler.
//
// Analyzer does the interning. Ast will stay Node<T>s for now. This avoids breaking
// the parser and lowering tests. We can also create a new db for the lowering analysis,
// fwiw.
//
// Separate context object for each item, which collects diagnostics.
//
// Each item will be marked Incomplete if the analysis reaches a fatal error.
// If a future analysis references the incomplete item in an erroneous/unverifiable way,
// that analysis will stop with a fatal error.
//
// (Parent) item analysis will also analyze all child items.
//
// Compiler should use db queries for item-level compilation,
// and use NodeId maps for fn bodies. I don't think there's a need to map item NodeIds
// to InternIds.
//
// fn expr_attributes_query(db, func: FuncId, expr: NodeId) -> Rc<ExprAttr>:
//   db.func_context(func).expressions.get(expr)

// from rust-analyzer {
#[macro_export]
macro_rules! impl_intern_key {
    ($name:ident) => {
        impl salsa::InternKey for $name {
            fn from_intern_id(v: salsa::InternId) -> Self {
                $name(v.as_u32())
            }
            fn as_intern_id(&self) -> salsa::InternId {
                salsa::InternId::from(self.0)
            }
        }
    };
}
// } from rust-analyzer

#[salsa::query_group(AnalyzerDbStorage)]
pub trait AnalyzerDb {
    #[salsa::interned]
    fn intern_module(&self, data: Rc<items::Module>) -> ModuleId;
    #[salsa::interned]
    fn intern_struct(&self, data: Rc<items::Struct>) -> StructId;
    #[salsa::interned]
    fn intern_type_alias(&self, data: Rc<items::TypeAlias>) -> TypeAliasId;
    #[salsa::interned]
    fn intern_contract(&self, data: Rc<items::Contract>) -> ContractId;
    #[salsa::interned]
    fn intern_function(&self, data: Rc<items::Function>) -> FunctionId;
    #[salsa::interned]
    fn intern_event(&self, data: Rc<items::Event>) -> EventId;

    #[salsa::invoke(queries::module::module_type_defs)]
    fn module_type_defs(&self, module: ModuleId) -> Rc<IndexMap<String, TypeDefId>>;
    #[salsa::invoke(queries::module::module_resolve_type)]
    fn module_resolve_type(&self, module: ModuleId, name: String) -> Option<Rc<types::Type>>;

    #[salsa::invoke(queries::contracts::contract_type)]
    fn contract_type(&self, contract: ContractId) -> Analysis<Rc<types::Contract>>;
    #[salsa::invoke(queries::contracts::contract_functions)]
    fn contract_functions(&self, contract: ContractId) -> Rc<Vec<FunctionId>>;
    #[salsa::invoke(queries::contracts::contract_events)]
    fn contract_events(&self, contract: ContractId) -> Rc<Vec<EventId>>;
    #[salsa::invoke(queries::contracts::contract_fields)]
    fn contract_fields(
        &self,
        contract: ContractId,
    ) -> Analysis<Rc<IndexMap<String, Rc<types::Type>>>>;

    #[salsa::invoke(queries::functions::function_signature)]
    fn function_signature(&self, id: FunctionId) -> Analysis<Rc<types::FunctionSignature>>;
    #[salsa::invoke(queries::functions::function_body)]
    fn function_body(&self, id: FunctionId) -> Analysis<Rc<FunctionBody>>;

    #[salsa::invoke(queries::structs::struct_type)]
    fn struct_type(&self, id: StructId) -> Analysis<Rc<types::Struct>>;

    #[salsa::invoke(queries::events::event_type)]
    fn event_type(&self, event: EventId) -> Analysis<Rc<events::EventDef>>;

    #[salsa::invoke(queries::types::type_alias_type)]
    fn type_alias_type(&self, id: TypeAliasId) -> Analysis<Rc<types::Type>>;

    // #[salsa::invoke(queries::contracts::contract_field_type)]
    // fn contract_field_type(&self, contract: ContractId, name: String) -> Option<(Rc<types::Type>, usize)>;
}

#[salsa::database(AnalyzerDbStorage)]
#[derive(Default)]
pub struct TestDb {
    storage: salsa::Storage<TestDb>,
}
impl salsa::Database for TestDb {}

// struct ItemLoc<N: ItemTreeNode>
//   container: ModuleId
//   id: ItemTreeId<N>

// InternDatabase
//  fn intern_function(&self, loc: FunctionLoc) -> FunctionId
//  fn intern_struct
//  fn intern_block

// pub struct FunctionId(salsa::InternId);
// type FunctionLoc = AssocItemLoc<Function>;
// impl_intern!(FunctionId, FunctionLoc, intern_function, lookup_intern_function);

// Module
//  fn scope(self, db, visible_from: Option<Module>) -> Vec<(Name, ScopeDef)>
//

// fn struct_defs(db: &dyn AnalyzerDb) -> Rc<HashMap<String, NodeId>> {
//     Rc::new(
//         db.module()
//             .body
//             .iter()
//             .filter_map(|stmt| match &stmt.kind {
//                 ast::ModuleStmt::StructDef { name, .. } => Some((name.kind.clone(), stmt.id)),
//                 _ => None,
//             })
//             .collect(),
//     )
// }

// fn resolve_type(db: &dyn AnalyzerDb, name: &str) -> Option<Type> {
//     match db.type_names.get(name)? {
//         TypeDefId::Alias(alias_id) -> module::type_alias_2(alias_id.data(db))
//         Alias(TypeAliasId),
//             Struct(StructDefId),
//             // Contract(ContractDefId),
//             // Event(EventDefId),
//         }
//     }
// }
