//! Fe Lowering.

use fe_analyzer::db::AnalyzerDb;
use fe_parser::ast;

mod context;
mod mappers;
mod names;
mod utils;

/// Lowers the Fe source AST to a Fe HIR AST.
pub fn lower(db: &dyn AnalyzerDb, module: ast::Module) -> ast::Module {
    mappers::module::module(db, module)
}
