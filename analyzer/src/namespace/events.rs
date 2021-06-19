use crate::namespace::types::{AbiEncoding, FixedSize};
use fe_common::utils::keccak;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct EventDef {
    pub name: String,
    pub topic: String,
    pub fields: Vec<(String, FixedSize)>,
    pub indexed_fields: Vec<usize>,
}

impl EventDef {
    pub fn new(name: &str, fields: Vec<(String, FixedSize)>, indexed_fields: Vec<usize>) -> Self {
        let abi_fields = fields
            .iter()
            .map(|(_, typ)| typ.abi_selector_name())
            .collect::<Vec<String>>();
        let topic = build_event_topic(name, abi_fields);

        Self {
            name: name.to_string(),
            topic,
            fields,
            indexed_fields,
        }
    }

    /// The event's indexed fields.
    ///
    /// These should be logged as additional topics.
    pub fn indexed_field_types_with_index(&self) -> Vec<(usize, FixedSize)> {
        self.indexed_fields
            .to_owned()
            .into_iter()
            .map(|index| (index, self.fields[index].1.to_owned()))
            .collect()
    }

    /// The event's non-indexed fields.
    ///
    /// These should be logged in the data section.
    pub fn non_indexed_field_types_with_index(&self) -> Vec<(usize, FixedSize)> {
        self.fields
            .to_owned()
            .into_iter()
            .enumerate()
            .filter(|(index, _)| !self.indexed_fields.contains(index))
            .map(|(index, (_, typ))| (index, typ))
            .collect()
    }

    /// The event's non-indexed field types.
    pub fn non_indexed_field_types(&self) -> Vec<FixedSize> {
        self.non_indexed_field_types_with_index()
            .into_iter()
            .map(|(_, typ)| typ)
            .collect()
    }

    /// The event's field types.
    pub fn iter_field_types(&self) -> impl Iterator<Item = &FixedSize> + '_ {
        self.fields.iter().map(|(_, typ)| typ)
    }

    pub fn has_field(&self, field_name: &str) -> bool {
        self.fields.iter().any(|(name, _)| name == field_name)
    }
}

fn build_event_topic(name: &str, fields: Vec<String>) -> String {
    let signature = format!("{}({})", name, fields.join(","));
    keccak::full(signature.as_bytes())
}

#[cfg(test)]
mod tests {
    use crate::namespace::events::EventDef;
    use crate::namespace::types::{Base, FixedSize};

    #[test]
    fn test_new_event() {
        let event = EventDef::new(
            "MyEvent",
            vec![
                ("my_addr".to_string(), FixedSize::Base(Base::Address)),
                ("my_addr2".to_string(), FixedSize::Base(Base::Address)),
                ("my_bool".to_string(), FixedSize::bool()),
            ],
            vec![1],
        );

        assert_eq!(
            event.iter_field_types().collect::<Vec<_>>(),
            vec![
                &FixedSize::Base(Base::Address),
                &FixedSize::Base(Base::Address),
                &FixedSize::bool(),
            ],
        );

        assert_eq!(
            event.non_indexed_field_types_with_index(),
            vec![(0, FixedSize::Base(Base::Address)), (2, FixedSize::bool())]
        );

        assert_eq!(
            event.indexed_field_types_with_index(),
            vec![(1, FixedSize::Base(Base::Address))]
        );
    }
}
