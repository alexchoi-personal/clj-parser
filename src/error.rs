use thiserror::Error;

use crate::span::Span;

#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum ErrorKind {
    #[error("unexpected character: '{0}'")]
    UnexpectedChar(char),

    #[error("unexpected end of file")]
    UnexpectedEof,

    #[error("unterminated string")]
    UnterminatedString,

    #[error("unterminated regex")]
    UnterminatedRegex,

    #[error("unterminated {0}")]
    UnterminatedCollection(&'static str),

    #[error("invalid number: {0}")]
    InvalidNumber(String),

    #[error("invalid character literal: {0}")]
    InvalidCharLiteral(String),

    #[error("invalid radix: {0}")]
    InvalidRadix(u32),

    #[error("invalid escape character: '{0}'")]
    InvalidEscape(char),

    #[error("invalid symbol: {0}")]
    InvalidSymbol(String),

    #[error("invalid keyword: {0}")]
    InvalidKeyword(String),

    #[error("invalid tagged literal: {0}")]
    InvalidTaggedLiteral(String),

    #[error("invalid reader conditional: {0}")]
    InvalidReaderConditional(String),

    #[error("map literal must contain an even number of forms")]
    OddMapEntries,

    #[error("duplicate set entry")]
    DuplicateSetEntry,

    #[error("mismatched delimiter: expected '{expected}', found '{found}'")]
    MismatchedDelimiter { expected: char, found: char },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    pub kind: ErrorKind,
    pub span: Span,
    pub source: String,
}

impl ParseError {
    pub fn new(kind: ErrorKind, span: Span, source: impl Into<String>) -> Self {
        Self {
            kind,
            span,
            source: source.into(),
        }
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} at byte {}..{}: '{}'",
            self.kind, self.span.start, self.span.end, self.source
        )
    }
}

impl std::error::Error for ParseError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        Some(&self.kind)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum ExpandErrorKind {
    #[error("unquote-splice (~@) not valid outside of list context")]
    UnquoteSpliceNotInList,
    #[error("unquote (~) not valid outside of syntax-quote")]
    UnquoteOutsideSyntaxQuote,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpandError {
    pub kind: ExpandErrorKind,
    pub span: Option<crate::span::Span>,
}

impl ExpandError {
    pub fn new(kind: ExpandErrorKind) -> Self {
        Self { kind, span: None }
    }

    pub fn with_span(kind: ExpandErrorKind, span: crate::span::Span) -> Self {
        Self { kind, span: Some(span) }
    }
}

impl std::fmt::Display for ExpandError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.span {
            Some(span) => write!(f, "{} at {}..{}", self.kind, span.start, span.end),
            None => write!(f, "{}", self.kind),
        }
    }
}

impl std::error::Error for ExpandError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_error_display() {
        let error = ParseError::new(ErrorKind::UnexpectedChar('x'), Span::new(0, 1), "x");
        let display = format!("{}", error);
        assert!(display.contains("unexpected character"));
        assert!(display.contains("0..1"));
    }

    #[test]
    fn test_parse_error_source() {
        use std::error::Error;
        let error = ParseError::new(ErrorKind::UnexpectedEof, Span::new(0, 0), "");
        assert!(error.source().is_some());
    }

    #[test]
    fn test_error_kind_display() {
        assert!(format!("{}", ErrorKind::InvalidNumber("bad".into())).contains("invalid number"));
        assert!(format!("{}", ErrorKind::InvalidRadix(99)).contains("invalid radix"));
        assert!(
            format!("{}", ErrorKind::InvalidTaggedLiteral("x".into())).contains("tagged literal")
        );
        assert!(format!("{}", ErrorKind::DuplicateSetEntry).contains("duplicate set"));
    }
}
