#![allow(clippy::missing_errors_doc, clippy::must_use_candidate)]

use crate::ast::{Form, ParseOpts, Platform, ReadCondBehavior, SymbolicVal};
use crate::error::{ErrorKind, ParseError};
use crate::lexer::{Lexer, Token};
use crate::span::{Span, Spanned};

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    source: &'a str,
    current: Option<(Token<'a>, Span)>,
    opts: ParseOpts,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        Self::with_opts(source, ParseOpts::default())
    }

    pub fn with_opts(source: &'a str, opts: ParseOpts) -> Self {
        Self {
            lexer: Lexer::new(source),
            source,
            current: None,
            opts,
        }
    }

    fn advance(&mut self) -> Result<(Token<'a>, Span), ParseError> {
        if let Some(tok) = self.current.take() {
            Ok(tok)
        } else {
            self.lexer.next_token()
        }
    }

    fn peek(&mut self) -> Result<(Token<'a>, Span), ParseError> {
        if self.current.is_none() {
            self.current = Some(self.lexer.next_token()?);
        }
        Ok(self.current.clone().unwrap())
    }

    fn make_error(&self, kind: ErrorKind, span: Span) -> ParseError {
        let source = if span.end <= self.source.len() {
            &self.source[span.start..span.end]
        } else {
            ""
        };
        ParseError::new(kind, span, source)
    }

    pub fn parse(&mut self) -> Result<Vec<Spanned<Form>>, ParseError> {
        let mut forms = Vec::new();
        loop {
            let (tok, _) = self.peek()?;
            if matches!(tok, Token::Eof) {
                break;
            }
            match self.parse_form() {
                Ok(form) => forms.push(form),
                Err(e) => return Err(e),
            }
        }
        Ok(forms)
    }

    pub fn parse_form(&mut self) -> Result<Spanned<Form>, ParseError> {
        loop {
            let (tok, span) = self.advance()?;
            let start_span = span;

            let form = match tok {
                Token::Eof => {
                    return Err(self.make_error(ErrorKind::UnexpectedEof, span));
                }
                Token::LParen => self.parse_list()?,
                Token::LBracket => self.parse_vector()?,
                Token::LBrace => self.parse_map()?,
                Token::RParen => {
                    return Err(self.make_error(
                        ErrorKind::MismatchedDelimiter {
                            expected: '(',
                            found: ')',
                        },
                        span,
                    ));
                }
                Token::RBracket => {
                    return Err(self.make_error(
                        ErrorKind::MismatchedDelimiter {
                            expected: '[',
                            found: ']',
                        },
                        span,
                    ));
                }
                Token::RBrace => {
                    return Err(self.make_error(
                        ErrorKind::MismatchedDelimiter {
                            expected: '{',
                            found: '}',
                        },
                        span,
                    ));
                }
                Token::Quote => self.parse_quote()?,
                Token::Backtick => self.parse_syntax_quote()?,
                Token::Tilde => self.parse_unquote()?,
                Token::TildeAt => self.parse_unquote_splice()?,
                Token::At => self.parse_deref()?,
                Token::Caret => self.parse_meta()?,
                Token::Hash => return Err(self.make_error(ErrorKind::UnexpectedEof, span)),
                Token::HashBrace => self.parse_set()?,
                Token::HashParen => self.parse_anon_fn()?,
                Token::HashQuote => self.parse_var()?,
                Token::HashUnderscore => {
                    self.parse_discard()?;
                    continue;
                }
                Token::HashQuestion => self.parse_reader_cond(false)?,
                Token::HashQuestionAt => self.parse_reader_cond(true)?,
                Token::HashHash => self.parse_symbolic()?,
                Token::Number(s) => Self::parse_number(s),
                Token::String(s) => Form::String(s),
                Token::Char(c) => Form::Char(c),
                Token::Symbol(s) => Self::parse_symbol(s),
                Token::Keyword(s) => Self::parse_keyword(s),
                Token::Regex(s) => Form::Regex(s),
                Token::TagSymbol(tag) => self.parse_tagged(tag)?,
            };

            let end_span = self.current_end_span(start_span);
            return Ok(Spanned::new(
                form,
                Span::new(start_span.start, end_span.end),
            ));
        }
    }

    fn current_end_span(&self, fallback: Span) -> Span {
        if let Some((_, span)) = &self.current {
            Span::new(fallback.start, span.start)
        } else {
            fallback
        }
    }

    fn parse_list(&mut self) -> Result<Form, ParseError> {
        let mut forms = Vec::new();
        loop {
            let (tok, span) = self.peek()?;
            match tok {
                Token::RParen => {
                    self.advance()?;
                    return Ok(Form::List(forms));
                }
                Token::Eof => {
                    return Err(self.make_error(ErrorKind::UnterminatedCollection("list"), span));
                }
                Token::RBracket => {
                    return Err(self.make_error(
                        ErrorKind::MismatchedDelimiter {
                            expected: ')',
                            found: ']',
                        },
                        span,
                    ));
                }
                Token::RBrace => {
                    return Err(self.make_error(
                        ErrorKind::MismatchedDelimiter {
                            expected: ')',
                            found: '}',
                        },
                        span,
                    ));
                }
                Token::HashUnderscore => {
                    self.advance()?;
                    self.parse_discard()?;
                }
                _ => {
                    let spanned = self.parse_form()?;
                    forms.push(spanned.value);
                }
            }
        }
    }

    fn parse_vector(&mut self) -> Result<Form, ParseError> {
        let mut forms = Vec::new();
        loop {
            let (tok, span) = self.peek()?;
            match tok {
                Token::RBracket => {
                    self.advance()?;
                    return Ok(Form::Vector(forms));
                }
                Token::Eof => {
                    return Err(self.make_error(ErrorKind::UnterminatedCollection("vector"), span));
                }
                Token::RParen => {
                    return Err(self.make_error(
                        ErrorKind::MismatchedDelimiter {
                            expected: ']',
                            found: ')',
                        },
                        span,
                    ));
                }
                Token::RBrace => {
                    return Err(self.make_error(
                        ErrorKind::MismatchedDelimiter {
                            expected: ']',
                            found: '}',
                        },
                        span,
                    ));
                }
                Token::HashUnderscore => {
                    self.advance()?;
                    self.parse_discard()?;
                }
                _ => {
                    let spanned = self.parse_form()?;
                    forms.push(spanned.value);
                }
            }
        }
    }

    fn parse_map(&mut self) -> Result<Form, ParseError> {
        let mut entries = Vec::new();
        loop {
            let (tok, span) = self.peek()?;
            match tok {
                Token::RBrace => {
                    self.advance()?;
                    return Ok(Form::Map(entries));
                }
                Token::Eof => {
                    return Err(self.make_error(ErrorKind::UnterminatedCollection("map"), span));
                }
                Token::RParen => {
                    return Err(self.make_error(
                        ErrorKind::MismatchedDelimiter {
                            expected: '}',
                            found: ')',
                        },
                        span,
                    ));
                }
                Token::RBracket => {
                    return Err(self.make_error(
                        ErrorKind::MismatchedDelimiter {
                            expected: '}',
                            found: ']',
                        },
                        span,
                    ));
                }
                Token::HashUnderscore => {
                    self.advance()?;
                    self.parse_discard()?;
                }
                _ => {
                    let key = self.parse_form()?;

                    loop {
                        let (tok, span) = self.peek()?;
                        match tok {
                            Token::RBrace | Token::Eof => {
                                return Err(self.make_error(ErrorKind::OddMapEntries, span));
                            }
                            Token::HashUnderscore => {
                                self.advance()?;
                                self.parse_discard()?;
                            }
                            _ => break,
                        }
                    }

                    let value = self.parse_form()?;
                    entries.push((key.value, value.value));
                }
            }
        }
    }

    fn parse_set(&mut self) -> Result<Form, ParseError> {
        let mut forms = Vec::new();
        loop {
            let (tok, span) = self.peek()?;
            match tok {
                Token::RBrace => {
                    self.advance()?;
                    return Ok(Form::Set(forms));
                }
                Token::Eof => {
                    return Err(self.make_error(ErrorKind::UnterminatedCollection("set"), span));
                }
                Token::RParen => {
                    return Err(self.make_error(
                        ErrorKind::MismatchedDelimiter {
                            expected: '}',
                            found: ')',
                        },
                        span,
                    ));
                }
                Token::RBracket => {
                    return Err(self.make_error(
                        ErrorKind::MismatchedDelimiter {
                            expected: '}',
                            found: ']',
                        },
                        span,
                    ));
                }
                Token::HashUnderscore => {
                    self.advance()?;
                    self.parse_discard()?;
                }
                _ => {
                    let spanned = self.parse_form()?;
                    forms.push(spanned.value);
                }
            }
        }
    }

    fn parse_quote(&mut self) -> Result<Form, ParseError> {
        let form = self.parse_form()?;
        Ok(Form::Quote(Box::new(form.value)))
    }

    fn parse_syntax_quote(&mut self) -> Result<Form, ParseError> {
        let form = self.parse_form()?;
        Ok(Form::SyntaxQuote(Box::new(form.value)))
    }

    fn parse_unquote(&mut self) -> Result<Form, ParseError> {
        let form = self.parse_form()?;
        Ok(Form::Unquote(Box::new(form.value)))
    }

    fn parse_unquote_splice(&mut self) -> Result<Form, ParseError> {
        let form = self.parse_form()?;
        Ok(Form::UnquoteSplice(Box::new(form.value)))
    }

    fn parse_deref(&mut self) -> Result<Form, ParseError> {
        let form = self.parse_form()?;
        Ok(Form::Deref(Box::new(form.value)))
    }

    fn parse_var(&mut self) -> Result<Form, ParseError> {
        let form = self.parse_form()?;
        Ok(Form::Var(Box::new(form.value)))
    }

    fn parse_meta(&mut self) -> Result<Form, ParseError> {
        let meta_form = self.parse_form()?;
        let form = self.parse_form()?;
        Ok(Form::Meta {
            meta: Box::new(meta_form.value),
            form: Box::new(form.value),
        })
    }

    fn parse_anon_fn(&mut self) -> Result<Form, ParseError> {
        let mut forms = Vec::new();
        loop {
            let (tok, span) = self.peek()?;
            match tok {
                Token::RParen => {
                    self.advance()?;
                    return Ok(Form::AnonFn(forms));
                }
                Token::Eof => {
                    return Err(self.make_error(
                        ErrorKind::UnterminatedCollection("anonymous function"),
                        span,
                    ));
                }
                Token::RBracket => {
                    return Err(self.make_error(
                        ErrorKind::MismatchedDelimiter {
                            expected: ')',
                            found: ']',
                        },
                        span,
                    ));
                }
                Token::RBrace => {
                    return Err(self.make_error(
                        ErrorKind::MismatchedDelimiter {
                            expected: ')',
                            found: '}',
                        },
                        span,
                    ));
                }
                Token::HashUnderscore => {
                    self.advance()?;
                    self.parse_discard()?;
                }
                _ => {
                    let spanned = self.parse_form()?;
                    forms.push(spanned.value);
                }
            }
        }
    }

    fn parse_discard(&mut self) -> Result<(), ParseError> {
        self.parse_form()?;
        Ok(())
    }

    fn parse_tagged(&mut self, tag: &str) -> Result<Form, ParseError> {
        let form = self.parse_form()?;
        Ok(Form::Tagged {
            tag: tag.to_string(),
            form: Box::new(form.value),
        })
    }

    fn parse_reader_cond(&mut self, splicing: bool) -> Result<Form, ParseError> {
        match self.opts.read_cond {
            ReadCondBehavior::Error => {
                let (_, span) = self.peek()?;
                return Err(self.make_error(
                    ErrorKind::InvalidReaderConditional("reader conditionals not allowed".into()),
                    span,
                ));
            }
            ReadCondBehavior::Preserve => {
                return self.parse_reader_cond_preserve(splicing);
            }
            ReadCondBehavior::Allow => {}
        }

        let (tok, span) = self.advance()?;
        if !matches!(tok, Token::LParen) {
            return Err(self.make_error(
                ErrorKind::InvalidReaderConditional("expected '(' after #?".into()),
                span,
            ));
        }

        let mut branches = Vec::new();
        let mut selected_form: Option<Form> = None;

        loop {
            let (tok, span) = self.peek()?;
            match tok {
                Token::RParen => {
                    self.advance()?;
                    break;
                }
                Token::Eof => {
                    return Err(self.make_error(
                        ErrorKind::UnterminatedCollection("reader conditional"),
                        span,
                    ));
                }
                _ => {
                    let platform_form = self.parse_form()?;
                    let platform_key = match &platform_form.value {
                        Form::Keyword {
                            name,
                            ns: None,
                            auto_resolve: false,
                        } => name.clone(),
                        _ => {
                            return Err(self.make_error(
                                ErrorKind::InvalidReaderConditional(
                                    "reader conditional branch must be a keyword".into(),
                                ),
                                platform_form.span,
                            ));
                        }
                    };

                    let (tok, span) = self.peek()?;
                    if matches!(tok, Token::RParen | Token::Eof) {
                        return Err(self.make_error(
                            ErrorKind::InvalidReaderConditional(
                                "reader conditional must have even number of forms".into(),
                            ),
                            span,
                        ));
                    }

                    let value_form = self.parse_form()?;

                    if selected_form.is_none() {
                        let platform_matches = matches!(
                            (&self.opts.platform, platform_key.as_str()),
                            (Platform::Clj, "clj")
                                | (Platform::Cljs, "cljs")
                                | (Platform::Cljr, "cljr")
                                | (Platform::Default | _, "default")
                        );
                        if platform_matches {
                            selected_form = Some(value_form.value.clone());
                        }
                    }

                    branches.push((platform_form.value, value_form.value));
                }
            }
        }

        if let Some(form) = selected_form {
            Ok(form)
        } else {
            Ok(Form::ReaderCond { splicing, branches })
        }
    }

    fn parse_reader_cond_preserve(&mut self, splicing: bool) -> Result<Form, ParseError> {
        let (tok, span) = self.advance()?;
        if !matches!(tok, Token::LParen) {
            return Err(self.make_error(
                ErrorKind::InvalidReaderConditional("expected '(' after #?".into()),
                span,
            ));
        }

        let mut branches = Vec::new();

        loop {
            let (tok, span) = self.peek()?;
            match tok {
                Token::RParen => {
                    self.advance()?;
                    break;
                }
                Token::Eof => {
                    return Err(self.make_error(
                        ErrorKind::UnterminatedCollection("reader conditional"),
                        span,
                    ));
                }
                _ => {
                    let platform_form = self.parse_form()?;
                    let (tok, span) = self.peek()?;
                    if matches!(tok, Token::RParen | Token::Eof) {
                        return Err(self.make_error(
                            ErrorKind::InvalidReaderConditional(
                                "reader conditional must have even number of forms".into(),
                            ),
                            span,
                        ));
                    }
                    let value_form = self.parse_form()?;
                    branches.push((platform_form.value, value_form.value));
                }
            }
        }

        Ok(Form::ReaderCond { splicing, branches })
    }

    fn parse_symbolic(&mut self) -> Result<Form, ParseError> {
        let (tok, span) = self.advance()?;
        match tok {
            Token::Symbol(s) => match s {
                "Inf" => Ok(Form::SymbolicVal(SymbolicVal::Inf)),
                "-Inf" => Ok(Form::SymbolicVal(SymbolicVal::NegInf)),
                "NaN" => Ok(Form::SymbolicVal(SymbolicVal::NaN)),
                _ => Err(self.make_error(
                    ErrorKind::InvalidSymbol(format!("unknown symbolic value: {s}")),
                    span,
                )),
            },
            _ => Err(self.make_error(
                ErrorKind::InvalidSymbol("expected symbolic value after ##".into()),
                span,
            )),
        }
    }

    fn parse_number(s: &str) -> Form {
        if let Some(num_str) = s.strip_suffix('N')
            && num_str
                .chars()
                .all(|c| c.is_ascii_digit() || c == '-' || c == '+')
        {
            return Form::BigInt(num_str.to_string());
        }

        if let Some(num_str) = s.strip_suffix('M') {
            return Form::BigDecimal(num_str.to_string());
        }

        if s.contains('/') {
            let parts: Vec<&str> = s.split('/').collect();
            if parts.len() == 2
                && let (Ok(numer), Ok(denom)) = (parts[0].parse::<i64>(), parts[1].parse::<i64>())
            {
                return Form::Ratio { numer, denom };
            }
        }

        if (s.contains('.') || s.contains('e') || s.contains('E'))
            && let Ok(f) = s.parse::<f64>()
        {
            return Form::Float(f);
        }

        if s.starts_with("0x") || s.starts_with("0X") {
            let hex_str = &s[2..];
            if let Ok(n) = i64::from_str_radix(hex_str, 16) {
                return Form::Int(n);
            }
        }

        if s.starts_with("-0x") || s.starts_with("-0X") {
            let hex_str = &s[3..];
            if let Ok(n) = i64::from_str_radix(hex_str, 16) {
                return Form::Int(-n);
            }
        }

        if s.starts_with("+0x") || s.starts_with("+0X") {
            let hex_str = &s[3..];
            if let Ok(n) = i64::from_str_radix(hex_str, 16) {
                return Form::Int(n);
            }
        }

        if s.contains('r') || s.contains('R') {
            let lowercase = s.to_lowercase();
            if let Some(idx) = lowercase.find('r') {
                let radix_str = &s[..idx];
                let value_str = &s[idx + 1..];

                let (radix_str, negative) = if let Some(stripped) = radix_str.strip_prefix('-') {
                    (stripped, true)
                } else if let Some(stripped) = radix_str.strip_prefix('+') {
                    (stripped, false)
                } else {
                    (radix_str, false)
                };

                if let Ok(radix) = radix_str.parse::<u32>()
                    && (2..=36).contains(&radix)
                    && let Ok(n) = i64::from_str_radix(value_str, radix)
                {
                    return Form::Int(if negative { -n } else { n });
                }
            }
        }

        if s.len() > 1 && s.starts_with('0') && !s.contains('.') {
            let (num_str, negative) = if let Some(stripped) = s.strip_prefix('-') {
                (stripped, true)
            } else if let Some(stripped) = s.strip_prefix('+') {
                (stripped, false)
            } else {
                (s, false)
            };

            if num_str.starts_with('0')
                && num_str.len() > 1
                && num_str.chars().skip(1).all(|c| ('0'..='7').contains(&c))
                && let Ok(n) = i64::from_str_radix(&num_str[1..], 8)
            {
                return Form::Int(if negative { -n } else { n });
            }
        }

        if let Ok(n) = s.parse::<i64>() {
            return Form::Int(n);
        }

        if let Ok(f) = s.parse::<f64>() {
            return Form::Float(f);
        }

        Form::BigInt(s.to_string())
    }

    fn parse_symbol(s: &str) -> Form {
        match s {
            "nil" => Form::Nil,
            "true" => Form::Bool(true),
            "false" => Form::Bool(false),
            _ => {
                if let Some(idx) = s.find('/') {
                    if idx > 0 && idx < s.len() - 1 {
                        let ns = &s[..idx];
                        let name = &s[idx + 1..];
                        Form::Symbol {
                            ns: Some(ns.to_string()),
                            name: name.to_string(),
                        }
                    } else if s == "/" {
                        Form::Symbol {
                            ns: None,
                            name: "/".to_string(),
                        }
                    } else {
                        Form::Symbol {
                            ns: None,
                            name: s.to_string(),
                        }
                    }
                } else {
                    Form::Symbol {
                        ns: None,
                        name: s.to_string(),
                    }
                }
            }
        }
    }

    fn parse_keyword(s: &str) -> Form {
        let s = s.strip_prefix(':').unwrap_or(s);
        let auto_resolve = s.starts_with(':');
        let s = if auto_resolve { &s[1..] } else { s };

        if let Some(idx) = s.find('/') {
            if idx > 0 && idx < s.len() - 1 {
                let ns = &s[..idx];
                let name = &s[idx + 1..];
                Form::Keyword {
                    ns: Some(ns.to_string()),
                    name: name.to_string(),
                    auto_resolve,
                }
            } else {
                Form::Keyword {
                    ns: None,
                    name: s.to_string(),
                    auto_resolve,
                }
            }
        } else {
            Form::Keyword {
                ns: None,
                name: s.to_string(),
                auto_resolve,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(source: &str) -> Vec<Form> {
        let mut parser = Parser::new(source);
        parser
            .parse()
            .unwrap()
            .into_iter()
            .map(|s| s.value)
            .collect()
    }

    fn parse_one(source: &str) -> Form {
        parse(source).into_iter().next().unwrap()
    }

    #[test]
    fn test_nil() {
        assert_eq!(parse_one("nil"), Form::Nil);
    }

    #[test]
    fn test_booleans() {
        assert_eq!(parse_one("true"), Form::Bool(true));
        assert_eq!(parse_one("false"), Form::Bool(false));
    }

    #[test]
    fn test_integers() {
        assert_eq!(parse_one("123"), Form::Int(123));
        assert_eq!(parse_one("-456"), Form::Int(-456));
        assert_eq!(parse_one("+789"), Form::Int(789));
        assert_eq!(parse_one("0"), Form::Int(0));
    }

    #[test]
    fn test_big_integers() {
        assert_eq!(parse_one("123N"), Form::BigInt("123".to_string()));
        assert_eq!(parse_one("-456N"), Form::BigInt("-456".to_string()));
    }

    #[test]
    fn test_floats() {
        assert_eq!(parse_one("3.14"), Form::Float(3.14));
        assert_eq!(parse_one("-2.5"), Form::Float(-2.5));
        assert_eq!(parse_one("1e10"), Form::Float(1e10));
        assert_eq!(parse_one("1.5E-3"), Form::Float(1.5e-3));
    }

    #[test]
    fn test_big_decimals() {
        assert_eq!(parse_one("3.14M"), Form::BigDecimal("3.14".to_string()));
        assert_eq!(parse_one("100M"), Form::BigDecimal("100".to_string()));
    }

    #[test]
    fn test_ratios() {
        assert_eq!(parse_one("1/2"), Form::Ratio { numer: 1, denom: 2 });
        assert_eq!(
            parse_one("-3/4"),
            Form::Ratio {
                numer: -3,
                denom: 4
            }
        );
    }

    #[test]
    fn test_hex_numbers() {
        assert_eq!(parse_one("0xFF"), Form::Int(255));
        assert_eq!(parse_one("0xff"), Form::Int(255));
        assert_eq!(parse_one("0x10"), Form::Int(16));
    }

    #[test]
    fn test_radix_numbers() {
        assert_eq!(parse_one("2r1010"), Form::Int(10));
        assert_eq!(parse_one("16rFF"), Form::Int(255));
        assert_eq!(parse_one("8r17"), Form::Int(15));
    }

    #[test]
    fn test_octal_numbers() {
        assert_eq!(parse_one("017"), Form::Int(15));
        assert_eq!(parse_one("010"), Form::Int(8));
    }

    #[test]
    fn test_strings() {
        assert_eq!(parse_one(r#""hello""#), Form::String("hello".to_string()));
        assert_eq!(
            parse_one(r#""hello\nworld""#),
            Form::String("hello\nworld".to_string())
        );
        assert_eq!(parse_one(r#""""#), Form::String("".to_string()));
    }

    #[test]
    fn test_chars() {
        assert_eq!(parse_one(r"\a"), Form::Char('a'));
        assert_eq!(parse_one(r"\newline"), Form::Char('\n'));
        assert_eq!(parse_one(r"\space"), Form::Char(' '));
    }

    #[test]
    fn test_symbols() {
        assert_eq!(
            parse_one("foo"),
            Form::Symbol {
                ns: None,
                name: "foo".to_string()
            }
        );
        assert_eq!(
            parse_one("bar/baz"),
            Form::Symbol {
                ns: Some("bar".to_string()),
                name: "baz".to_string()
            }
        );
        assert_eq!(
            parse_one("+"),
            Form::Symbol {
                ns: None,
                name: "+".to_string()
            }
        );
        assert_eq!(
            parse_one("/"),
            Form::Symbol {
                ns: None,
                name: "/".to_string()
            }
        );
    }

    #[test]
    fn test_keywords() {
        assert_eq!(
            parse_one(":foo"),
            Form::Keyword {
                ns: None,
                name: "foo".to_string(),
                auto_resolve: false
            }
        );
        assert_eq!(
            parse_one("::bar"),
            Form::Keyword {
                ns: None,
                name: "bar".to_string(),
                auto_resolve: true
            }
        );
        assert_eq!(
            parse_one(":ns/key"),
            Form::Keyword {
                ns: Some("ns".to_string()),
                name: "key".to_string(),
                auto_resolve: false
            }
        );
    }

    #[test]
    fn test_regex() {
        assert_eq!(
            parse_one(r#"#"hello\d+""#),
            Form::Regex(r"hello\d+".to_string())
        );
    }

    #[test]
    fn test_list() {
        assert_eq!(parse_one("()"), Form::List(vec![]));
        assert_eq!(
            parse_one("(1 2 3)"),
            Form::List(vec![Form::Int(1), Form::Int(2), Form::Int(3)])
        );
        assert_eq!(
            parse_one("(+ 1 2)"),
            Form::List(vec![
                Form::Symbol {
                    ns: None,
                    name: "+".to_string()
                },
                Form::Int(1),
                Form::Int(2)
            ])
        );
    }

    #[test]
    fn test_vector() {
        assert_eq!(parse_one("[]"), Form::Vector(vec![]));
        assert_eq!(
            parse_one("[1 2 3]"),
            Form::Vector(vec![Form::Int(1), Form::Int(2), Form::Int(3)])
        );
    }

    #[test]
    fn test_map() {
        assert_eq!(parse_one("{}"), Form::Map(vec![]));
        assert_eq!(
            parse_one("{:a 1 :b 2}"),
            Form::Map(vec![
                (
                    Form::Keyword {
                        ns: None,
                        name: "a".to_string(),
                        auto_resolve: false
                    },
                    Form::Int(1)
                ),
                (
                    Form::Keyword {
                        ns: None,
                        name: "b".to_string(),
                        auto_resolve: false
                    },
                    Form::Int(2)
                )
            ])
        );
    }

    #[test]
    fn test_set() {
        assert_eq!(parse_one("#{}"), Form::Set(vec![]));
        assert_eq!(
            parse_one("#{1 2 3}"),
            Form::Set(vec![Form::Int(1), Form::Int(2), Form::Int(3)])
        );
    }

    #[test]
    fn test_quote() {
        assert_eq!(
            parse_one("'foo"),
            Form::Quote(Box::new(Form::Symbol {
                ns: None,
                name: "foo".to_string()
            }))
        );
        assert_eq!(
            parse_one("'(1 2 3)"),
            Form::Quote(Box::new(Form::List(vec![
                Form::Int(1),
                Form::Int(2),
                Form::Int(3)
            ])))
        );
    }

    #[test]
    fn test_syntax_quote() {
        assert_eq!(
            parse_one("`foo"),
            Form::SyntaxQuote(Box::new(Form::Symbol {
                ns: None,
                name: "foo".to_string()
            }))
        );
    }

    #[test]
    fn test_unquote() {
        assert_eq!(
            parse_one("~foo"),
            Form::Unquote(Box::new(Form::Symbol {
                ns: None,
                name: "foo".to_string()
            }))
        );
    }

    #[test]
    fn test_unquote_splice() {
        assert_eq!(
            parse_one("~@foo"),
            Form::UnquoteSplice(Box::new(Form::Symbol {
                ns: None,
                name: "foo".to_string()
            }))
        );
    }

    #[test]
    fn test_deref() {
        assert_eq!(
            parse_one("@foo"),
            Form::Deref(Box::new(Form::Symbol {
                ns: None,
                name: "foo".to_string()
            }))
        );
    }

    #[test]
    fn test_var() {
        assert_eq!(
            parse_one("#'foo"),
            Form::Var(Box::new(Form::Symbol {
                ns: None,
                name: "foo".to_string()
            }))
        );
    }

    #[test]
    fn test_meta() {
        assert_eq!(
            parse_one("^:foo bar"),
            Form::Meta {
                meta: Box::new(Form::Keyword {
                    ns: None,
                    name: "foo".to_string(),
                    auto_resolve: false
                }),
                form: Box::new(Form::Symbol {
                    ns: None,
                    name: "bar".to_string()
                })
            }
        );
    }

    #[test]
    fn test_anon_fn() {
        assert_eq!(
            parse_one("#(+ % 1)"),
            Form::AnonFn(vec![
                Form::Symbol {
                    ns: None,
                    name: "+".to_string()
                },
                Form::Symbol {
                    ns: None,
                    name: "%".to_string()
                },
                Form::Int(1)
            ])
        );
    }

    #[test]
    fn test_discard() {
        assert_eq!(parse("1 #_2 3"), vec![Form::Int(1), Form::Int(3)]);
        assert_eq!(
            parse("#_foo bar"),
            vec![Form::Symbol {
                ns: None,
                name: "bar".to_string()
            }]
        );
        assert_eq!(parse("#_(1 2 3) 4"), vec![Form::Int(4)]);
    }

    #[test]
    fn test_tagged() {
        assert_eq!(
            parse_one("#inst \"2023-01-01\""),
            Form::Tagged {
                tag: "inst".to_string(),
                form: Box::new(Form::String("2023-01-01".to_string()))
            }
        );
        assert_eq!(
            parse_one("#uuid \"123e4567-e89b-12d3-a456-426614174000\""),
            Form::Tagged {
                tag: "uuid".to_string(),
                form: Box::new(Form::String(
                    "123e4567-e89b-12d3-a456-426614174000".to_string()
                ))
            }
        );
    }

    #[test]
    fn test_symbolic_values() {
        assert_eq!(parse_one("##Inf"), Form::SymbolicVal(SymbolicVal::Inf));
        assert_eq!(parse_one("##-Inf"), Form::SymbolicVal(SymbolicVal::NegInf));
        assert_eq!(parse_one("##NaN"), Form::SymbolicVal(SymbolicVal::NaN));
    }

    #[test]
    fn test_reader_cond_clj() {
        let mut parser = Parser::with_opts(
            "#?(:clj 1 :cljs 2)",
            ParseOpts {
                platform: Platform::Clj,
                read_cond: ReadCondBehavior::Allow,
            },
        );
        let forms: Vec<Form> = parser
            .parse()
            .unwrap()
            .into_iter()
            .map(|s| s.value)
            .collect();
        assert_eq!(forms, vec![Form::Int(1)]);
    }

    #[test]
    fn test_reader_cond_cljs() {
        let mut parser = Parser::with_opts(
            "#?(:clj 1 :cljs 2)",
            ParseOpts {
                platform: Platform::Cljs,
                read_cond: ReadCondBehavior::Allow,
            },
        );
        let forms: Vec<Form> = parser
            .parse()
            .unwrap()
            .into_iter()
            .map(|s| s.value)
            .collect();
        assert_eq!(forms, vec![Form::Int(2)]);
    }

    #[test]
    fn test_reader_cond_default() {
        let mut parser = Parser::with_opts(
            "#?(:clj 1 :default 3)",
            ParseOpts {
                platform: Platform::Cljs,
                read_cond: ReadCondBehavior::Allow,
            },
        );
        let forms: Vec<Form> = parser
            .parse()
            .unwrap()
            .into_iter()
            .map(|s| s.value)
            .collect();
        assert_eq!(forms, vec![Form::Int(3)]);
    }

    #[test]
    fn test_reader_cond_preserve() {
        let mut parser = Parser::with_opts(
            "#?(:clj 1 :cljs 2)",
            ParseOpts {
                platform: Platform::Clj,
                read_cond: ReadCondBehavior::Preserve,
            },
        );
        let forms: Vec<Form> = parser
            .parse()
            .unwrap()
            .into_iter()
            .map(|s| s.value)
            .collect();
        assert_eq!(
            forms,
            vec![Form::ReaderCond {
                splicing: false,
                branches: vec![
                    (
                        Form::Keyword {
                            ns: None,
                            name: "clj".to_string(),
                            auto_resolve: false
                        },
                        Form::Int(1)
                    ),
                    (
                        Form::Keyword {
                            ns: None,
                            name: "cljs".to_string(),
                            auto_resolve: false
                        },
                        Form::Int(2)
                    )
                ]
            }]
        );
    }

    #[test]
    fn test_reader_cond_splicing() {
        let mut parser = Parser::with_opts(
            "#?@(:clj [1 2] :cljs [3 4])",
            ParseOpts {
                platform: Platform::Clj,
                read_cond: ReadCondBehavior::Preserve,
            },
        );
        let forms: Vec<Form> = parser
            .parse()
            .unwrap()
            .into_iter()
            .map(|s| s.value)
            .collect();
        assert_eq!(
            forms,
            vec![Form::ReaderCond {
                splicing: true,
                branches: vec![
                    (
                        Form::Keyword {
                            ns: None,
                            name: "clj".to_string(),
                            auto_resolve: false
                        },
                        Form::Vector(vec![Form::Int(1), Form::Int(2)])
                    ),
                    (
                        Form::Keyword {
                            ns: None,
                            name: "cljs".to_string(),
                            auto_resolve: false
                        },
                        Form::Vector(vec![Form::Int(3), Form::Int(4)])
                    )
                ]
            }]
        );
    }

    #[test]
    fn test_reader_cond_error() {
        let mut parser = Parser::with_opts(
            "#?(:clj 1)",
            ParseOpts {
                platform: Platform::Clj,
                read_cond: ReadCondBehavior::Error,
            },
        );
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_nested_collections() {
        assert_eq!(
            parse_one("[[1 2] [3 4]]"),
            Form::Vector(vec![
                Form::Vector(vec![Form::Int(1), Form::Int(2)]),
                Form::Vector(vec![Form::Int(3), Form::Int(4)])
            ])
        );
        assert_eq!(
            parse_one("{:a {:b 1}}"),
            Form::Map(vec![(
                Form::Keyword {
                    ns: None,
                    name: "a".to_string(),
                    auto_resolve: false
                },
                Form::Map(vec![(
                    Form::Keyword {
                        ns: None,
                        name: "b".to_string(),
                        auto_resolve: false
                    },
                    Form::Int(1)
                )])
            )])
        );
    }

    #[test]
    fn test_multiple_forms() {
        let forms = parse("1 2 3");
        assert_eq!(forms, vec![Form::Int(1), Form::Int(2), Form::Int(3)]);
    }

    #[test]
    fn test_empty_input() {
        let forms = parse("");
        assert_eq!(forms, vec![]);
    }

    #[test]
    fn test_whitespace_only() {
        let forms = parse("   \n\t  ");
        assert_eq!(forms, vec![]);
    }

    #[test]
    fn test_comments() {
        let forms = parse("1 ; comment\n 2");
        assert_eq!(forms, vec![Form::Int(1), Form::Int(2)]);
    }

    #[test]
    fn test_unterminated_list() {
        let mut parser = Parser::new("(1 2 3");
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_unterminated_vector() {
        let mut parser = Parser::new("[1 2 3");
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_unterminated_map() {
        let mut parser = Parser::new("{:a 1");
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_unterminated_set() {
        let mut parser = Parser::new("#{1 2");
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_odd_map_entries() {
        let mut parser = Parser::new("{:a 1 :b}");
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_mismatched_delimiters() {
        let mut parser = Parser::new("(1 2]");
        assert!(parser.parse().is_err());

        let mut parser = Parser::new("[1 2)");
        assert!(parser.parse().is_err());

        let mut parser = Parser::new("{:a 1)");
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_complex_expression() {
        let source = "(defn greet [name] (str \"Hello, \" name \"!\"))";
        let form = parse_one(source);
        assert!(matches!(form, Form::List(_)));
    }

    #[test]
    fn test_span_tracking() {
        let mut parser = Parser::new("foo");
        let form = parser.parse_form().unwrap();
        assert_eq!(form.span.start, 0);
        assert_eq!(form.span.end, 3);
    }

    #[test]
    fn test_negative_hex() {
        assert_eq!(parse_one("-0xFF"), Form::Int(-255));
    }

    #[test]
    fn test_negative_radix() {
        assert_eq!(parse_one("-2r1010"), Form::Int(-10));
    }

    #[test]
    fn test_multiple_discards() {
        assert_eq!(parse("#_1 #_2 3"), vec![Form::Int(3)]);
    }

    #[test]
    fn test_discard_in_collection() {
        assert_eq!(
            parse_one("[1 #_2 3]"),
            Form::Vector(vec![Form::Int(1), Form::Int(3)])
        );
    }

    #[test]
    fn test_nested_discard() {
        assert_eq!(parse("#_#_1 2 3"), vec![Form::Int(3)]);
    }

    #[test]
    fn test_triple_discard() {
        assert_eq!(parse("#_#_#_1 2 3 4"), vec![Form::Int(4)]);
    }

    #[test]
    fn test_discard_at_end_of_list() {
        assert_eq!(
            parse_one("(a #_b)"),
            Form::List(vec![Form::Symbol {
                ns: None,
                name: "a".to_string()
            }])
        );
    }

    #[test]
    fn test_standalone_rparen() {
        let mut parser = Parser::new(")");
        assert!(parser.parse_form().is_err());
    }

    #[test]
    fn test_standalone_rbracket() {
        let mut parser = Parser::new("]");
        assert!(parser.parse_form().is_err());
    }

    #[test]
    fn test_standalone_rbrace() {
        let mut parser = Parser::new("}");
        assert!(parser.parse_form().is_err());
    }

    #[test]
    fn test_list_mismatched_rbracket() {
        let mut parser = Parser::new("(1 2]");
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_list_mismatched_rbrace() {
        let mut parser = Parser::new("(1 2}");
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_vector_mismatched_rparen() {
        let mut parser = Parser::new("[1 2)");
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_vector_mismatched_rbrace() {
        let mut parser = Parser::new("[1 2}");
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_map_mismatched_rparen() {
        let mut parser = Parser::new("{:a 1)");
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_map_mismatched_rbracket() {
        let mut parser = Parser::new("{:a 1]");
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_set_mismatched_rparen() {
        let mut parser = Parser::new("#{1 2)");
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_set_mismatched_rbracket() {
        let mut parser = Parser::new("#{1 2]");
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_anon_fn_mismatched_rbracket() {
        let mut parser = Parser::new("#(+ 1]");
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_anon_fn_mismatched_rbrace() {
        let mut parser = Parser::new("#(+ 1}");
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_unterminated_anon_fn() {
        let mut parser = Parser::new("#(+ 1 2");
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_hash_eof() {
        let mut parser = Parser::new("#");
        assert!(parser.parse_form().is_err());
    }

    #[test]
    fn test_reader_cond_not_paren() {
        let mut parser = Parser::with_opts(
            "#?[1 2]",
            ParseOpts {
                platform: Platform::Clj,
                read_cond: ReadCondBehavior::Allow,
            },
        );
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_reader_cond_odd_forms() {
        let mut parser = Parser::with_opts(
            "#?(:clj)",
            ParseOpts {
                platform: Platform::Clj,
                read_cond: ReadCondBehavior::Allow,
            },
        );
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_reader_cond_non_keyword_branch() {
        let mut parser = Parser::with_opts(
            "#?(foo 1)",
            ParseOpts {
                platform: Platform::Clj,
                read_cond: ReadCondBehavior::Allow,
            },
        );
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_reader_cond_unterminated() {
        let mut parser = Parser::with_opts(
            "#?(:clj 1",
            ParseOpts {
                platform: Platform::Clj,
                read_cond: ReadCondBehavior::Allow,
            },
        );
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_reader_cond_preserve_not_paren() {
        let mut parser = Parser::with_opts(
            "#?[1 2]",
            ParseOpts {
                platform: Platform::Clj,
                read_cond: ReadCondBehavior::Preserve,
            },
        );
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_reader_cond_preserve_odd_forms() {
        let mut parser = Parser::with_opts(
            "#?(:clj)",
            ParseOpts {
                platform: Platform::Clj,
                read_cond: ReadCondBehavior::Preserve,
            },
        );
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_reader_cond_preserve_unterminated() {
        let mut parser = Parser::with_opts(
            "#?(:clj 1",
            ParseOpts {
                platform: Platform::Clj,
                read_cond: ReadCondBehavior::Preserve,
            },
        );
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_reader_cond_cljr() {
        let mut parser = Parser::with_opts(
            "#?(:cljr 1 :clj 2)",
            ParseOpts {
                platform: Platform::Cljr,
                read_cond: ReadCondBehavior::Allow,
            },
        );
        let forms: Vec<Form> = parser
            .parse()
            .unwrap()
            .into_iter()
            .map(|s| s.value)
            .collect();
        assert_eq!(forms, vec![Form::Int(1)]);
    }

    #[test]
    fn test_reader_cond_platform_default() {
        let mut parser = Parser::with_opts(
            "#?(:default 99)",
            ParseOpts {
                platform: Platform::Default,
                read_cond: ReadCondBehavior::Allow,
            },
        );
        let forms: Vec<Form> = parser
            .parse()
            .unwrap()
            .into_iter()
            .map(|s| s.value)
            .collect();
        assert_eq!(forms, vec![Form::Int(99)]);
    }

    #[test]
    fn test_reader_cond_no_match() {
        let mut parser = Parser::with_opts(
            "#?(:other 1)",
            ParseOpts {
                platform: Platform::Clj,
                read_cond: ReadCondBehavior::Allow,
            },
        );
        let forms: Vec<Form> = parser
            .parse()
            .unwrap()
            .into_iter()
            .map(|s| s.value)
            .collect();
        assert!(matches!(forms[0], Form::ReaderCond { .. }));
    }

    #[test]
    fn test_symbolic_invalid() {
        let mut parser = Parser::new("##Unknown");
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_symbolic_not_symbol() {
        let mut parser = Parser::new("##123");
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_positive_hex() {
        assert_eq!(parse_one("+0xFF"), Form::Int(255));
    }

    #[test]
    fn test_positive_radix() {
        assert_eq!(parse_one("+2r1010"), Form::Int(10));
    }

    #[test]
    fn test_symbol_slash() {
        assert_eq!(
            parse_one("foo/"),
            Form::Symbol {
                ns: None,
                name: "foo/".to_string()
            }
        );
    }

    #[test]
    fn test_symbol_slash_start() {
        assert_eq!(
            parse_one("/bar"),
            Form::Symbol {
                ns: None,
                name: "/bar".to_string()
            }
        );
    }

    #[test]
    fn test_keyword_slash() {
        assert_eq!(
            parse_one(":foo/"),
            Form::Keyword {
                ns: None,
                name: "foo/".to_string(),
                auto_resolve: false
            }
        );
    }

    #[test]
    fn test_auto_resolve_keyword_ns() {
        assert_eq!(
            parse_one("::ns/key"),
            Form::Keyword {
                ns: Some("ns".to_string()),
                name: "key".to_string(),
                auto_resolve: true
            }
        );
    }

    #[test]
    fn test_fallback_bigint() {
        let result = parse_one("1N1");
        assert!(matches!(result, Form::BigInt(_)));
    }

    #[test]
    fn test_eof_on_parse_form() {
        let mut parser = Parser::new("");
        assert!(parser.parse_form().is_err());
    }

    #[test]
    fn test_invalid_ratio() {
        let result = parse_one("1/abc");
        assert!(matches!(result, Form::BigInt(_)));
    }

    #[test]
    fn test_invalid_hex_0x() {
        let result = parse_one("0xZZZ");
        assert!(matches!(result, Form::BigInt(_)));
    }

    #[test]
    fn test_invalid_hex_neg() {
        let result = parse_one("-0xZZZ");
        assert!(matches!(result, Form::BigInt(_)));
    }

    #[test]
    fn test_invalid_hex_pos() {
        let result = parse_one("+0xZZZ");
        assert!(matches!(result, Form::BigInt(_)));
    }

    #[test]
    fn test_invalid_radix_value() {
        let result = parse_one("2rZZZ");
        assert!(matches!(result, Form::BigInt(_)));
    }

    #[test]
    fn test_invalid_radix_base() {
        let result = parse_one("99r123");
        assert!(matches!(result, Form::BigInt(_)));
    }

    #[test]
    fn test_octal_with_invalid_digits() {
        let result = parse_one("09");
        assert!(matches!(result, Form::Int(9)));
    }

    #[test]
    fn test_positive_octal() {
        assert_eq!(parse_one("+017"), Form::Int(17));
    }

    #[test]
    fn test_negative_octal() {
        assert_eq!(parse_one("-017"), Form::Int(-17));
    }

    #[test]
    fn test_float_fallback() {
        assert!(matches!(parse_one("1e999999"), Form::Float(_)));
    }
}
