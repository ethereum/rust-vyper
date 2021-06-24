use fe_analyzer::db::AnalyzerDb;
use fe_analyzer::namespace::types::{Array, Tuple};
use indexmap::IndexSet;
use std::collections::BTreeSet;

pub struct ModuleContext<'a> {
    pub db: &'a dyn AnalyzerDb,

    /// Tuples that were used inside of a module,
    /// and the generated name of the resulting struct.
    pub tuples: IndexSet<Tuple>,

    /// Holds fresh id for [`ModuleContext::make_unique_name`]
    fresh_id: u64,
}

impl<'a> ModuleContext<'a> {
    pub fn new(db: &'a dyn AnalyzerDb) -> Self {
        Self {
            analysis,
            tuples: IndexSet::new(),
            fresh_id: 0,
        }
    }

    /// Makes a unique name from the given name, keeping it as readable as possible.
    pub fn make_unique_name(&mut self, name: &str) -> String {
        let id = self.fresh_id;
        self.fresh_id += 1;
        format!("${}_{}", name, id)
    }
}

// This is contract context, but it's used all over so it has a short name.
pub struct Context<'a, 'b> {
    pub module: &'a mut ModuleContext<'b>,
    /// List expressions that the contract uses
    pub list_expressions: BTreeSet<Array>,
}

impl<'a, 'b> Context<'a, 'b> {
    pub fn new(module: &'a mut ModuleContext<'b>) -> Self {
        Self {
            module,
            list_expressions: BTreeSet::new(),
        }
    }
}
