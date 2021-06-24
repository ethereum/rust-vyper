use crate::context::{Context, ModuleContext};
use crate::mappers::{contracts, types};
use crate::names;
use crate::utils::ZeroSpanNode;
use fe_analyzer::db::AnalyzerDb;
use fe_analyzer::namespace::types::{Base, FixedSize, Tuple};
use fe_parser::ast as fe;
use fe_parser::node::{Node, Span};

/// Lowers a module.
pub fn module(db: &AnalyzerDb, module: fe::Module) -> fe::Module {
    let mut module_context = ModuleContext::new(db);

    let lowered_body = module
        .body
        .into_iter()
        .map(|stmt| match stmt {
            fe::ModuleStmt::Pragma(_) => stmt,
            fe::ModuleStmt::TypeAlias(inner) => fe::ModuleStmt::TypeAlias(Node::new(
                fe::TypeAlias {
                    name: inner.kind.name,
                    typ: types::type_desc(&mut Context::new(&mut module_context), inner.kind.typ),
                },
                inner.span,
            )),
            fe::ModuleStmt::Struct(_) => stmt,
            fe::ModuleStmt::Import(_) => stmt,
            fe::ModuleStmt::Contract(inner) => fe::ModuleStmt::Contract(contracts::contract_def(
                &mut Context::new(&mut module_context),
                inner,
            )),
        })
        .collect::<Vec<_>>();

    let struct_defs_from_tuples = module_context
        .tuples
        .iter()
        .map(|typ| {
            fe::ModuleStmt::Struct(Node::new(
                build_tuple_struct(&module_context, typ),
                Span::zero(),
            ))
        })
        .collect::<Vec<fe::ModuleStmt>>();

    fe::Module {
        body: [struct_defs_from_tuples, lowered_body].concat(),
    }
}

fn build_tuple_struct(module: &ModuleContext, tuple: &Tuple) -> fe::Struct {
    let fields = tuple
        .items
        .iter()
        .enumerate()
        .map(|(index, typ)| {
            Node::new(
                build_struct_field(format!("item{}", index), build_type_desc(module, typ)),
                Span::zero(),
            )
        })
        .collect();

    fe::Struct {
        name: Node::new(names::tuple_struct_name(tuple), Span::zero()),
        fields,
    }
}

fn build_struct_field(name: String, type_desc: fe::TypeDesc) -> fe::Field {
    fe::Field {
        is_pub: false,
        is_const: false,
        name: Node::new(name, Span::zero()),
        typ: Node::new(type_desc, Span::zero()),
        value: None,
    }
}

fn build_type_desc(module: &ModuleContext, typ: &FixedSize) -> fe::TypeDesc {
    match typ {
        FixedSize::Base(Base::Unit) => fe::TypeDesc::Unit,
        FixedSize::Base(base) => fe::TypeDesc::Base {
            base: names::base_type_name(base),
        },
        FixedSize::Array(array) => fe::TypeDesc::Array {
            dimension: array.size,
            typ: build_type_desc(module, &array.inner.into()).into_boxed_node(),
        },
        FixedSize::Tuple(tuple) => fe::TypeDesc::Base {
            base: names::tuple_struct_name(&tuple),
        },
        FixedSize::String(string) => fe::TypeDesc::Generic {
            base: Node::new("String".to_string(), Span::zero()),
            args: Node::new(
                vec![fe::GenericArg::Int(Node::new(
                    string.max_size,
                    Span::zero(),
                ))],
                Span::zero(),
            ),
        },
        FixedSize::Contract(contract) => fe::TypeDesc::Base {
            base: contract.name.clone(),
        },
        FixedSize::Struct(strukt) => fe::TypeDesc::Base {
            base: strukt.name.clone(),
        },
    }
}
