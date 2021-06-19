use fe_common::diagnostics::Label;
use fe_parser::ast as fe;
use fe_parser::node::Node;

use crate::db::AnalyzerDb;
use crate::errors::{AlreadyDefined, FatalError};
use crate::namespace::scopes::{ModuleScope, Scope, Shared};
use crate::namespace::types::{FixedSize, Struct, Type};
use crate::traversal::types::type_desc;
use std::rc::Rc;

pub fn struct_def(
    context: &mut Context,
    module_scope: Shared<ModuleScope>,
    struct_def: &Node<fe::Struct>,
) -> Result<(), FatalError> {
    let fe::Struct { name, fields } = &struct_def.kind;
    let mut val = Struct::new(&name.kind);
    for field in fields {
        let fe::Field { name, typ, .. } = &field.kind;
        let field_type = type_desc(&Scope::Module(Rc::clone(&module_scope)), context, &typ)?;
        if let Type::Base(base_typ) = field_type {
            if let Err(AlreadyDefined) = val.add_field(&name.kind, &FixedSize::Base(base_typ)) {
                let first_definition = fields
                    .iter()
                    .find(|val| {
                        let fe::Field {
                            name: inner_name, ..
                        } = &val.kind;
                        inner_name.kind == name.kind && val.span != field.span
                    })
                    .expect("Missing field");

                context.fancy_error(
                    "a struct field with the same name already exists",
                    vec![
                        Label::primary(
                            first_definition.span,
                            format!("First definition of field `{}`", name.kind),
                        ),
                        Label::primary(
                            field.span,
                            format!("Conflicting definition of field `{}`", name.kind),
                        ),
                    ],
                    vec![format!(
                        "Note: Give one of the `{}` fields a different name",
                        name.kind
                    )],
                )
            }
        } else {
            context.not_yet_implemented("non-base type struct fields", field.span)
        }
    }
    // XXX: this should be done at the module analysis level
    // if let Err(AlreadyDefined) = module_scope
    //     .borrow_mut()
    //     .add_type_def(&name.kind, Type::Struct(val))
    // {
    //     context.add_diagnostic(errors::fancy_error(
    //         "a struct with the same name already exists",
    //         // TODO: figure out how to include the previously defined struct
    //         vec![Label::primary(
    //             struct_def.span,
    //             format!("Conflicting definition of struct `{}`", name.kind),
    //         )],
    //         vec![format!(
    //             "Note: Give one of the `{}` structs a different name",
    //             name.kind
    //         )],
    //     ))
    // }

    Ok(())
}

// pub fn struct_type_query(
//     db: &dyn AnalyzerDb,
//     module: ModuleId,
//     struct_def: StructDefId,
// ) -> Result<(), SemanticError> {
//     let struct_def = struct_def.data(context.db);
//     let struct_name = &struct_def.name.kind;
//     let mut val = Struct::new(struct_name);
//     for field in &struct_def.fields {
//         let Field { name, typ, .. } = &field.kind;
//         let field_type = type_desc(&Scope::Module(Rc::clone(&module_scope)), context, &typ)?;
//         if let Type::Base(base_typ) = field_type {
//             val.add_field(&name.kind, &FixedSize::Base(base_typ))?;
//         } else {
//             context.not_yet_implemented("non-base type struct fields", field.span)
//         }
//     }
//     module_scope
//         .borrow_mut()
//         .add_type_def(struct_name, Type::Struct(val))?;
//     Ok(())
// }
