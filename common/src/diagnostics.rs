#![allow(unused_variables, unused_mut)] // XXX
use crate::files::{FileStore, SourceFileId};
use crate::Span;
use codespan_reporting::diagnostic as cs;
use codespan_reporting::term;
pub use cs::Severity;
use term::termcolor::{BufferWriter, ColorChoice};

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Diagnostic {
    pub severity: Severity,
    pub message: String,
    pub labels: Vec<Label>,
    pub notes: Vec<String>,
}
impl Diagnostic {
    pub fn into_cs(self, file_id: SourceFileId) -> cs::Diagnostic<SourceFileId> {
        cs::Diagnostic {
            severity: self.severity,
            code: None,
            message: self.message,
            labels: self
                .labels
                .into_iter()
                .map(|label| label.into_cs_label(file_id))
                .collect(),
            notes: self.notes,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum LabelStyle {
    Primary,
    Secondary,
}
impl Into<cs::LabelStyle> for LabelStyle {
    fn into(self) -> cs::LabelStyle {
        match self {
            LabelStyle::Primary => cs::LabelStyle::Primary,
            LabelStyle::Secondary => cs::LabelStyle::Secondary,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Label {
    pub style: LabelStyle,
    pub span: Span,
    pub message: String,
}
impl Label {
    /// Create a primary label with the given message. This will underline the
    /// given span with carets (`^^^^`).
    pub fn primary<S: Into<String>>(span: Span, message: S) -> Self {
        Label {
            style: LabelStyle::Primary,
            span,
            message: message.into(),
        }
    }

    /// Create a secondary label with the given message. This will underline the
    /// given span with hyphens (`----`).
    pub fn secondary<S: Into<String>>(span: Span, message: S) -> Self {
        Label {
            style: LabelStyle::Secondary,
            span,
            message: message.into(),
        }
    }

    /// Convert into a [`codespan_reporting::Diagnostic::Label`]
    pub fn into_cs_label(self, file_id: SourceFileId) -> cs::Label<SourceFileId> {
        cs::Label {
            style: self.style.into(),
            file_id,
            range: self.span.into(),
            message: self.message,
        }
    }
}

/// Print the given diagnostics to stderr.
pub fn print_diagnostics(diagnostics: &[Diagnostic], files: &FileStore) {
    let writer = BufferWriter::stderr(ColorChoice::Auto);
    let mut buffer = writer.buffer();
    let config = term::Config::default();

    for diag in diagnostics {
        todo!()
        // term::emit(&mut buffer, &config, files, &diag).unwrap();
    }
    // If we use `writer` here, the output won't be captured by rust's test system.
    eprintln!("{}", std::str::from_utf8(buffer.as_slice()).unwrap());
}

/// Format the given diagnostics as a string.
pub fn diagnostics_string(diagnostics: &[Diagnostic], files: &FileStore) -> String {
    let writer = BufferWriter::stderr(ColorChoice::Never);
    let mut buffer = writer.buffer();
    let config = term::Config::default();

    for diag in diagnostics {
        todo!()
        // term::emit(&mut buffer, &config, files, &diag).expect("failed to emit diagnostic");
    }
    std::str::from_utf8(buffer.as_slice()).unwrap().to_string()
}
