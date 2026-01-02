#![allow(clippy::missing_errors_doc)]

mod ast;
mod error;
mod expander;
mod lexer;
mod parser;
mod printer;
mod span;

pub use ast::{Form, ParseOpts, Platform, ReadCondBehavior, StringValue, SymbolicVal};
pub use bumpalo::Bump;
pub use error::{ErrorKind, ExpandError, ExpandErrorKind, ParseError};
pub use expander::Expander;
pub use lexer::{Lexer, Token};
pub use parser::Parser;
pub use printer::{print_form, print_forms};
pub use span::{Span, Spanned};

impl Default for ParseOpts {
    fn default() -> Self {
        Self {
            platform: Platform::Default,
            read_cond: ReadCondBehavior::Allow,
        }
    }
}

pub fn parse<'a>(
    source: &'a str,
    bump: &'a Bump,
) -> Result<bumpalo::collections::Vec<'a, Spanned<Form<'a>>>, ParseError> {
    Parser::new(source, bump).parse()
}

pub fn parse_with_opts<'a>(
    source: &'a str,
    opts: ParseOpts,
    bump: &'a Bump,
) -> Result<bumpalo::collections::Vec<'a, Spanned<Form<'a>>>, ParseError> {
    Parser::with_opts(source, opts, bump).parse()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        let bump = Bump::new();
        let forms = parse("(+ 1 2)", &bump).unwrap();
        assert_eq!(forms.len(), 1);
        assert!(matches!(forms[0].value, Form::List(_)));
    }

    #[test]
    fn test_parse_with_opts() {
        let bump = Bump::new();
        let opts = ParseOpts {
            platform: Platform::Clj,
            read_cond: ReadCondBehavior::Allow,
        };
        let forms = parse_with_opts("#?(:clj 1 :cljs 2)", opts, &bump).unwrap();
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
