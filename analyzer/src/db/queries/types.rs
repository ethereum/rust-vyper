use crate::db::Analysis;
use crate::namespace::items::TypeAliasId;
use crate::namespace::types;
use crate::AnalyzerDb;
use fe_parser::ast;
use std::rc::Rc;

pub fn type_alias_type(db: &dyn AnalyzerDb, alias: TypeAliasId) -> Analysis<Rc<types::Type>> {
    todo!()
    // let mut field_types = vec![];

    // let ast::StructDef { name, fields } = strukt.data(db);
    // for field in fields {
    //     let Field {
    //         name: field_name,
    //         typ,
    //         ..
    //     } = &field.kind;
    // }
}
