use crate::context::AnalyzerContext;
use crate::db::Analysis;
use crate::namespace::items::StructId;
use crate::namespace::scopes::ItemScope;
use crate::namespace::types;
use crate::traversal::types::type_desc;
use crate::AnalyzerDb;
use fe_parser::ast;
use std::rc::Rc;

pub fn struct_type(db: &dyn AnalyzerDb, struct_id: StructId) -> Analysis<Rc<types::Struct>> {
    let mut scope = ItemScope::new(db, struct_id.data(db).module);

    let ast::Struct { name, fields } = &struct_id.data(db).ast.kind;

    let fields = fields
        .iter()
        .map(|node| {
            let ast::Field {
                is_pub,
                is_const,
                name: field_name,
                typ,
                value,
            } = &node.kind;

            if *is_pub {
                scope.not_yet_implemented("struct `pub` fields", node.span);
            }
            if *is_const {
                scope.not_yet_implemented("struct `const` fields", node.span);
            }
            if let Some(node) = value {
                scope.not_yet_implemented("struct field initial value assignment", node.span);
            }
            let typ = match type_desc(&mut scope, typ) {
                types::Type::Base(base) => types::FixedSize::Base(base),
                _ => {
                    scope.not_yet_implemented("non-primitive type struct fields", node.span);
                    types::FixedSize::unknown()
                }
            };
            (field_name.kind.clone(), typ)
        })
        .collect();

    Analysis {
        value: Rc::new(types::Struct {
            name: name.kind.clone(),
            fields,
        }),
        diagnostics: Rc::new(scope.diagnostics),
    }
}
