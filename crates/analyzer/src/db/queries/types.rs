use crate::db::Analysis;
use crate::namespace::items::TypeAliasId;
use crate::namespace::scopes::ItemScope;
use crate::namespace::types;
use crate::traversal::types::type_desc;
use crate::AnalyzerDb;
use fe_parser::ast;
use std::rc::Rc;

pub fn type_alias_type(db: &dyn AnalyzerDb, alias: TypeAliasId) -> Analysis<Rc<types::Type>> {
    let mut scope = ItemScope::new(db, alias.data(db).module);
    let typ = type_desc(&mut scope, &alias.data(db).ast.kind.typ);

    Analysis {
        value: Rc::new(typ),
        diagnostics: Rc::new(scope.diagnostics),
    }
}
