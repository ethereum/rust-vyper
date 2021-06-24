use crate::db::Analysis;
use crate::namespace::items::StructId;
use crate::namespace::types;
use crate::AnalyzerDb;
use fe_parser::ast;
use std::rc::Rc;

pub fn struct_type(db: &dyn AnalyzerDb, struct_id: StructId) -> Analysis<Rc<types::Struct>> {
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
