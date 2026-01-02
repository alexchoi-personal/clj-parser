#![allow(clippy::missing_errors_doc)]

mod ast;
mod error;
mod lexer;
mod parser;
mod span;

pub use ast::{Form, ParseOpts, Platform, ReadCondBehavior, SymbolicVal};
pub use error::{ErrorKind, ParseError};
pub use lexer::{Lexer, Token};
pub use parser::Parser;
pub use span::{Span, Spanned};

impl Default for ParseOpts {
    fn default() -> Self {
        Self {
            platform: Platform::Default,
            read_cond: ReadCondBehavior::Allow,
        }
    }
}

pub fn parse(source: &str) -> Result<Vec<Spanned<Form>>, ParseError> {
    Parser::new(source).parse()
}

pub fn parse_with_opts(source: &str, opts: ParseOpts) -> Result<Vec<Spanned<Form>>, ParseError> {
    Parser::with_opts(source, opts).parse()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        let forms = parse("(+ 1 2)").unwrap();
        assert_eq!(forms.len(), 1);
        assert!(matches!(forms[0].value, Form::List(_)));
    }

    #[test]
    fn test_parse_with_opts() {
        let opts = ParseOpts {
            platform: Platform::Clj,
            read_cond: ReadCondBehavior::Allow,
        };
        let forms = parse_with_opts("#?(:clj 1 :cljs 2)", opts).unwrap();
        assert_eq!(forms.len(), 1);
        assert!(matches!(forms[0].value, Form::Int(1)));
    }

    #[test]
    fn test_default_parse_opts() {
        let opts = ParseOpts::default();
        assert!(matches!(opts.platform, Platform::Default));
        assert!(matches!(opts.read_cond, ReadCondBehavior::Allow));
    }
}
