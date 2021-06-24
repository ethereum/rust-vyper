use crate::db::Analysis;
use crate::namespace::events::EventDef;
use crate::namespace::items::EventId;
use crate::AnalyzerDb;
use fe_parser::ast;
use std::rc::Rc;

pub fn event_type(db: &dyn AnalyzerDb, event: EventId) -> Analysis<Rc<EventDef>> {
    todo!()
}
