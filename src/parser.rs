#![allow(clippy::missing_errors_doc, clippy::must_use_candidate)]

use bumpalo::collections::Vec as BumpVec;
use bumpalo::Bump;

use crate::ast::{Form, ParseOpts, Platform, ReadCondBehavior, SymbolicVal};
use crate::error::{ErrorKind, ParseError};
use crate::lexer::{Lexer, Token};
use crate::span::{Span, Spanned};

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    source: &'a str,
    current: Option<(Token<'a>, Span)>,
    opts: ParseOpts,
    bump: &'a Bump,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str, bump: &'a Bump) -> Self {
        Self::with_opts(source, ParseOpts::default(), bump)
    }

    pub fn with_opts(source: &'a str, opts: ParseOpts, bump: &'a Bump) -> Self {
        Self {
            lexer: Lexer::new(source, bump),
            source,
            current: None,
            opts,
            bump,
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

    #[inline]
    fn alloc<T>(&self, val: T) -> &'a T {
        self.bump.alloc(val)
    }

    pub fn parse(&mut self) -> Result<BumpVec<'a, Spanned<Form<'a>>>, ParseError> {
        let mut forms = BumpVec::new_in(self.bump);
        loop {
            let (tok, _) = self.peek()?;
            match tok {
                Token::Eof => break,
                Token::HashUnderscore => {
                    self.advance()?;
                    self.parse_discard()?;
                }
                _ => {
                    forms.push(self.parse_form()?);
                }
            }
        }
        Ok(forms)
    }

    pub fn parse_form(&mut self) -> Result<Spanned<Form<'a>>, ParseError> {
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
                Token::Number(s) => self.parse_number(s),
                Token::String(s) => Form::String(s),
                Token::Char(c) => Form::Char(c),
                Token::Symbol(s) => self.parse_symbol(s),
                Token::Keyword(s) => self.parse_keyword(s),
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

    fn parse_list(&mut self) -> Result<Form<'a>, ParseError> {
        let mut forms = BumpVec::new_in(self.bump);
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

    fn parse_vector(&mut self) -> Result<Form<'a>, ParseError> {
        let mut forms = BumpVec::new_in(self.bump);
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

    fn parse_map(&mut self) -> Result<Form<'a>, ParseError> {
        let mut entries = BumpVec::new_in(self.bump);
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

    fn parse_set(&mut self) -> Result<Form<'a>, ParseError> {
        let mut forms = BumpVec::new_in(self.bump);
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

    fn parse_quote(&mut self) -> Result<Form<'a>, ParseError> {
        let form = self.parse_form()?;
        Ok(Form::Quote(self.alloc(form.value)))
    }

    fn parse_syntax_quote(&mut self) -> Result<Form<'a>, ParseError> {
        let form = self.parse_form()?;
        Ok(Form::SyntaxQuote(self.alloc(form.value)))
    }

    fn parse_unquote(&mut self) -> Result<Form<'a>, ParseError> {
        let form = self.parse_form()?;
        Ok(Form::Unquote(self.alloc(form.value)))
    }

    fn parse_unquote_splice(&mut self) -> Result<Form<'a>, ParseError> {
        let form = self.parse_form()?;
        Ok(Form::UnquoteSplice(self.alloc(form.value)))
    }

    fn parse_deref(&mut self) -> Result<Form<'a>, ParseError> {
        let form = self.parse_form()?;
        Ok(Form::Deref(self.alloc(form.value)))
    }

    fn parse_var(&mut self) -> Result<Form<'a>, ParseError> {
        let form = self.parse_form()?;
        Ok(Form::Var(self.alloc(form.value)))
    }

    fn parse_meta(&mut self) -> Result<Form<'a>, ParseError> {
        let meta_form = self.parse_form()?;
        let form = self.parse_form()?;
        Ok(Form::Meta {
            meta: self.alloc(meta_form.value),
            form: self.alloc(form.value),
        })
    }

    fn parse_anon_fn(&mut self) -> Result<Form<'a>, ParseError> {
        let mut forms = BumpVec::new_in(self.bump);
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

    fn parse_tagged(&mut self, tag: &'a str) -> Result<Form<'a>, ParseError> {
        let form = self.parse_form()?;
        Ok(Form::Tagged {
            tag,
            form: self.alloc(form.value),
        })
    }

    fn parse_reader_cond(&mut self, splicing: bool) -> Result<Form<'a>, ParseError> {
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

        let mut branches = BumpVec::new_in(self.bump);
        let mut selected_form: Option<Form<'a>> = None;

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
                        } => *name,
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
                            (&self.opts.platform, platform_key),
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

    fn parse_reader_cond_preserve(&mut self, splicing: bool) -> Result<Form<'a>, ParseError> {
        let (tok, span) = self.advance()?;
        if !matches!(tok, Token::LParen) {
            return Err(self.make_error(
                ErrorKind::InvalidReaderConditional("expected '(' after #?".into()),
                span,
            ));
        }

        let mut branches = BumpVec::new_in(self.bump);

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

    fn parse_symbolic(&mut self) -> Result<Form<'a>, ParseError> {
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

    fn parse_number(&self, s: &'a str) -> Form<'a> {
        if let Some(num_str) = s.strip_suffix('N')
            && num_str
                .chars()
                .all(|c| c.is_ascii_digit() || c == '-' || c == '+')
        {
            return Form::BigInt(num_str);
        }

        if let Some(num_str) = s.strip_suffix('M') {
            return Form::BigDecimal(num_str);
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

        Form::BigInt(s)
    }

    fn parse_symbol(&self, s: &'a str) -> Form<'a> {
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
                            ns: Some(ns),
                            name,
                        }
                    } else if s == "/" {
                        Form::Symbol {
                            ns: None,
                            name: "/",
                        }
                    } else {
                        Form::Symbol { ns: None, name: s }
                    }
                } else {
                    Form::Symbol { ns: None, name: s }
                }
            }
        }
    }

    fn parse_keyword(&self, s: &'a str) -> Form<'a> {
        let s = s.strip_prefix(':').unwrap_or(s);
        let auto_resolve = s.starts_with(':');
        let s = if auto_resolve { &s[1..] } else { s };

        if let Some(idx) = s.find('/') {
            if idx > 0 && idx < s.len() - 1 {
                let ns = &s[..idx];
                let name = &s[idx + 1..];
                Form::Keyword {
                    ns: Some(ns),
                    name,
                    auto_resolve,
                }
            } else {
                Form::Keyword {
                    ns: None,
                    name: s,
                    auto_resolve,
                }
            }
        } else {
            Form::Keyword {
                ns: None,
                name: s,
                auto_resolve,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(source: &'static str) -> Vec<Form<'static>> {
        let bump: &'static Bump = Box::leak(Box::new(Bump::new()));
        let mut parser = Parser::new(source, bump);
        parser
            .parse()
            .unwrap()
            .into_iter()
            .map(|s| s.value)
            .collect()
    }

    fn parse_one(source: &'static str) -> Form<'static> {
        parse(source).into_iter().next().unwrap()
    }

    #[test]
    fn test_hex_numbers() {
        assert_eq!(parse_one("0xFF"), Form::Int(255));
        assert_eq!(parse_one("0xff"), Form::Int(255));
        assert_eq!(parse_one("0x10"), Form::Int(16));
    }

    #[test]
    fn test_negative_hex() {
        assert_eq!(parse_one("-0xFF"), Form::Int(-255));
    }

    #[test]
    fn test_positive_hex() {
        assert_eq!(parse_one("+0xFF"), Form::Int(255));
    }

    #[test]
    fn test_radix_numbers() {
        assert_eq!(parse_one("2r1010"), Form::Int(10));
        assert_eq!(parse_one("16rFF"), Form::Int(255));
        assert_eq!(parse_one("8r17"), Form::Int(15));
    }

    #[test]
    fn test_negative_radix() {
        assert_eq!(parse_one("-2r1010"), Form::Int(-10));
    }

    #[test]
    fn test_positive_radix() {
        assert_eq!(parse_one("+2r1010"), Form::Int(10));
    }

    #[test]
    fn test_octal_numbers() {
        assert_eq!(parse_one("017"), Form::Int(15));
        assert_eq!(parse_one("010"), Form::Int(8));
    }

    #[test]
    fn test_octal_with_non_octal_digits() {
        let result = parse_one("089");
        assert_eq!(result, Form::Int(89));
    }

    #[test]
    fn test_single_zero_octal() {
        assert_eq!(parse_one("00"), Form::Int(0));
    }

    #[test]
    fn test_signed_number_as_decimal() {
        assert_eq!(parse_one("+017"), Form::Int(17));
        assert_eq!(parse_one("-017"), Form::Int(-17));
    }

    #[test]
    fn test_big_integers() {
        assert_eq!(parse_one("123N"), Form::BigInt("123"));
        assert_eq!(parse_one("-456N"), Form::BigInt("-456"));
    }

    #[test]
    fn test_big_decimals() {
        assert_eq!(parse_one("3.14M"), Form::BigDecimal("3.14"));
        assert_eq!(parse_one("100M"), Form::BigDecimal("100"));
    }

    #[test]
    fn test_bigint_with_sign() {
        assert_eq!(parse_one("+123N"), Form::BigInt("+123"));
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
    fn test_invalid_ratio() {
        let result = parse_one("1/abc");
        assert!(matches!(result, Form::BigInt(_)));
    }

    #[test]
    fn test_invalid_ratio_multiple_slashes() {
        let result = parse_one("1/2/3");
        assert!(matches!(result, Form::BigInt(_)));
    }

    #[test]
    fn test_fallback_bigint() {
        let result = parse_one("1N1");
        assert!(matches!(result, Form::BigInt(_)));
    }

    #[test]
    fn test_float_fallback() {
        assert!(matches!(parse_one("1e999999"), Form::Float(_)));
    }

    #[test]
    fn test_float_with_uppercase_exponent() {
        assert_eq!(parse_one("1E10"), Form::Float(1e10));
    }

    #[test]
    fn test_hex_overflow_to_bigint() {
        let result = parse_one("0xFFFFFFFFFFFFFFFFFFFFFFFF");
        assert!(matches!(result, Form::BigInt(_)));
    }

    #[test]
    fn test_radix_overflow_to_bigint() {
        let result = parse_one("2r11111111111111111111111111111111111111111111111111111111111111111111111111111111");
        assert!(matches!(result, Form::BigInt(_)));
    }

    #[test]
    fn test_invalid_radix_too_large() {
        let result = parse_one("37r10");
        assert!(matches!(result, Form::BigInt(_)));
    }

    #[test]
    fn test_invalid_radix_too_small() {
        let result = parse_one("1r10");
        assert!(matches!(result, Form::BigInt(_)));
    }

    #[test]
    fn test_discard() {
        let bump = Bump::new();
        let mut parser = Parser::new("1 #_2 3", &bump);
        let forms = parser.parse().unwrap();
        assert_eq!(forms.len(), 2);
    }

    #[test]
    fn test_multiple_discards() {
        let bump = Bump::new();
        let mut parser = Parser::new("#_1 #_2 3", &bump);
        let forms = parser.parse().unwrap();
        assert_eq!(forms.len(), 1);
    }

    #[test]
    fn test_nested_discard() {
        let bump = Bump::new();
        let mut parser = Parser::new("#_#_1 2 3", &bump);
        let forms = parser.parse().unwrap();
        assert_eq!(forms.len(), 1);
    }

    #[test]
    fn test_triple_discard() {
        let bump = Bump::new();
        let mut parser = Parser::new("#_#_#_1 2 3 4", &bump);
        let forms = parser.parse().unwrap();
        assert_eq!(forms.len(), 1);
    }

    #[test]
    fn test_discard_at_end_of_file() {
        let bump = Bump::new();
        let mut parser = Parser::new("foo #_(bar baz)", &bump);
        let forms = parser.parse().unwrap();
        assert_eq!(forms.len(), 1);
    }

    #[test]
    fn test_discard_at_end_of_list() {
        let bump = Bump::new();
        let mut parser = Parser::new("(a #_b)", &bump);
        let form = parser.parse_form().unwrap().value;
        if let Form::List(items) = form {
            assert_eq!(items.len(), 1);
        } else {
            panic!("Expected List");
        }
    }

    #[test]
    fn test_discard_in_collection() {
        let bump = Bump::new();
        let mut parser = Parser::new("[1 #_2 3]", &bump);
        let form = parser.parse_form().unwrap().value;
        if let Form::Vector(items) = form {
            assert_eq!(items.len(), 2);
        } else {
            panic!("Expected Vector");
        }
    }

    #[test]
    fn test_only_discard() {
        let bump = Bump::new();
        let mut parser = Parser::new("#_foo", &bump);
        let forms = parser.parse().unwrap();
        assert_eq!(forms.len(), 0);
    }

    #[test]
    fn test_discard_in_set() {
        let bump = Bump::new();
        let mut parser = Parser::new("#{1 #_2 3}", &bump);
        let form = parser.parse_form().unwrap().value;
        if let Form::Set(items) = form {
            assert_eq!(items.len(), 2);
        } else {
            panic!("Expected Set");
        }
    }

    #[test]
    fn test_discard_in_anon_fn() {
        let bump = Bump::new();
        let mut parser = Parser::new("#(+ #_ignored %)", &bump);
        let form = parser.parse_form().unwrap().value;
        if let Form::AnonFn(items) = form {
            assert_eq!(items.len(), 2);
        } else {
            panic!("Expected AnonFn");
        }
    }

    #[test]
    fn test_discard_in_map_value_position() {
        let bump = Bump::new();
        let mut parser = Parser::new("{:a #_ignored 1}", &bump);
        let form = parser.parse_form().unwrap().value;
        if let Form::Map(entries) = form {
            assert_eq!(entries.len(), 1);
        } else {
            panic!("Expected Map");
        }
    }

    #[test]
    fn test_discard_at_map_key_position() {
        let bump = Bump::new();
        let mut parser = Parser::new("{#_ignored :a 1}", &bump);
        let form = parser.parse_form().unwrap().value;
        if let Form::Map(entries) = form {
            assert_eq!(entries.len(), 1);
        } else {
            panic!("Expected Map");
        }
    }

    #[test]
    fn test_discard_causing_odd_map_entries() {
        let bump = Bump::new();
        let mut parser = Parser::new("{:a #_b}", &bump);
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_reader_cond_clj() {
        let bump = Bump::new();
        let mut parser = Parser::with_opts(
            "#?(:clj 1 :cljs 2)",
            ParseOpts {
                platform: Platform::Clj,
                read_cond: ReadCondBehavior::Allow,
            },
            &bump,
        );
        let forms = parser.parse().unwrap();
        assert_eq!(forms.len(), 1);
        assert_eq!(forms[0].value, Form::Int(1));
    }

    #[test]
    fn test_reader_cond_cljs() {
        let bump = Bump::new();
        let mut parser = Parser::with_opts(
            "#?(:clj 1 :cljs 2)",
            ParseOpts {
                platform: Platform::Cljs,
                read_cond: ReadCondBehavior::Allow,
            },
            &bump,
        );
        let forms = parser.parse().unwrap();
        assert_eq!(forms.len(), 1);
        assert_eq!(forms[0].value, Form::Int(2));
    }

    #[test]
    fn test_reader_cond_cljr() {
        let bump = Bump::new();
        let mut parser = Parser::with_opts(
            "#?(:cljr 1 :clj 2)",
            ParseOpts {
                platform: Platform::Cljr,
                read_cond: ReadCondBehavior::Allow,
            },
            &bump,
        );
        let forms = parser.parse().unwrap();
        assert_eq!(forms[0].value, Form::Int(1));
    }

    #[test]
    fn test_reader_cond_default() {
        let bump = Bump::new();
        let mut parser = Parser::with_opts(
            "#?(:clj 1 :default 3)",
            ParseOpts {
                platform: Platform::Cljs,
                read_cond: ReadCondBehavior::Allow,
            },
            &bump,
        );
        let forms = parser.parse().unwrap();
        assert_eq!(forms.len(), 1);
        assert_eq!(forms[0].value, Form::Int(3));
    }

    #[test]
    fn test_reader_cond_platform_default() {
        let bump = Bump::new();
        let mut parser = Parser::with_opts(
            "#?(:default 99)",
            ParseOpts {
                platform: Platform::Default,
                read_cond: ReadCondBehavior::Allow,
            },
            &bump,
        );
        let forms = parser.parse().unwrap();
        assert_eq!(forms[0].value, Form::Int(99));
    }

    #[test]
    fn test_reader_cond_no_match() {
        let bump = Bump::new();
        let mut parser = Parser::with_opts(
            "#?(:other 1)",
            ParseOpts {
                platform: Platform::Clj,
                read_cond: ReadCondBehavior::Allow,
            },
            &bump,
        );
        let forms = parser.parse().unwrap();
        assert!(matches!(forms[0].value, Form::ReaderCond { .. }));
    }

    #[test]
    fn test_reader_cond_splicing() {
        let bump = Bump::new();
        let mut parser = Parser::with_opts(
            "#?@(:clj [1 2] :cljs [3 4])",
            ParseOpts {
                platform: Platform::Clj,
                read_cond: ReadCondBehavior::Preserve,
            },
            &bump,
        );
        let forms = parser.parse().unwrap();
        if let Form::ReaderCond { splicing, .. } = &forms[0].value {
            assert!(*splicing);
        } else {
            panic!("Expected ReaderCond");
        }
    }

    #[test]
    fn test_reader_cond_preserve() {
        let bump = Bump::new();
        let mut parser = Parser::with_opts(
            "#?(:clj 1 :cljs 2)",
            ParseOpts {
                platform: Platform::Clj,
                read_cond: ReadCondBehavior::Preserve,
            },
            &bump,
        );
        let forms = parser.parse().unwrap();
        assert_eq!(forms.len(), 1);
        assert!(matches!(forms[0].value, Form::ReaderCond { .. }));
    }

    #[test]
    fn test_reader_cond_error() {
        let bump = Bump::new();
        let mut parser = Parser::with_opts(
            "#?(:clj 1)",
            ParseOpts {
                platform: Platform::Clj,
                read_cond: ReadCondBehavior::Error,
            },
            &bump,
        );
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_reader_cond_unterminated() {
        let bump = Bump::new();
        let mut parser = Parser::with_opts(
            "#?(:clj 1",
            ParseOpts {
                platform: Platform::Clj,
                read_cond: ReadCondBehavior::Allow,
            },
            &bump,
        );
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_reader_cond_not_paren() {
        let bump = Bump::new();
        let mut parser = Parser::with_opts(
            "#?[1 2]",
            ParseOpts {
                platform: Platform::Clj,
                read_cond: ReadCondBehavior::Allow,
            },
            &bump,
        );
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_reader_cond_odd_forms() {
        let bump = Bump::new();
        let mut parser = Parser::with_opts(
            "#?(:clj)",
            ParseOpts {
                platform: Platform::Clj,
                read_cond: ReadCondBehavior::Allow,
            },
            &bump,
        );
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_reader_cond_non_keyword_branch() {
        let bump = Bump::new();
        let mut parser = Parser::with_opts(
            "#?(foo 1)",
            ParseOpts {
                platform: Platform::Clj,
                read_cond: ReadCondBehavior::Allow,
            },
            &bump,
        );
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_reader_cond_preserve_non_paren() {
        let bump = Bump::new();
        let mut parser = Parser::with_opts(
            "#?[1 2]",
            ParseOpts {
                platform: Platform::Clj,
                read_cond: ReadCondBehavior::Preserve,
            },
            &bump,
        );
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_reader_cond_preserve_unterminated() {
        let bump = Bump::new();
        let mut parser = Parser::with_opts(
            "#?(:clj 1",
            ParseOpts {
                platform: Platform::Clj,
                read_cond: ReadCondBehavior::Preserve,
            },
            &bump,
        );
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_reader_cond_preserve_odd_forms() {
        let bump = Bump::new();
        let mut parser = Parser::with_opts(
            "#?(:clj)",
            ParseOpts {
                platform: Platform::Clj,
                read_cond: ReadCondBehavior::Preserve,
            },
            &bump,
        );
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_reader_cond_splicing_allow() {
        let bump = Bump::new();
        let mut parser = Parser::with_opts(
            "#?@(:clj [1 2])",
            ParseOpts {
                platform: Platform::Clj,
                read_cond: ReadCondBehavior::Allow,
            },
            &bump,
        );
        let forms = parser.parse().unwrap();
        if let Form::Vector(items) = &forms[0].value {
            assert_eq!(items.len(), 2);
        } else {
            panic!("Expected Vector");
        }
    }

    #[test]
    fn test_reader_cond_namespaced_keyword_branch() {
        let bump = Bump::new();
        let mut parser = Parser::with_opts(
            "#?(:ns/clj 1)",
            ParseOpts {
                platform: Platform::Clj,
                read_cond: ReadCondBehavior::Allow,
            },
            &bump,
        );
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_reader_cond_auto_resolve_keyword_branch() {
        let bump = Bump::new();
        let mut parser = Parser::with_opts(
            "#?(::clj 1)",
            ParseOpts {
                platform: Platform::Clj,
                read_cond: ReadCondBehavior::Allow,
            },
            &bump,
        );
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_reader_cond_preserve_splicing_odd_forms() {
        let bump = Bump::new();
        let mut parser = Parser::with_opts(
            "#?@(:clj)",
            ParseOpts {
                platform: Platform::Clj,
                read_cond: ReadCondBehavior::Preserve,
            },
            &bump,
        );
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_reader_cond_error_splicing() {
        let bump = Bump::new();
        let mut parser = Parser::with_opts(
            "#?@(:clj [1])",
            ParseOpts {
                platform: Platform::Clj,
                read_cond: ReadCondBehavior::Error,
            },
            &bump,
        );
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_reader_cond_allow_odd_forms_at_eof() {
        let bump = Bump::new();
        let mut parser = Parser::with_opts(
            "#?(:clj 1 :cljs",
            ParseOpts {
                platform: Platform::Clj,
                read_cond: ReadCondBehavior::Allow,
            },
            &bump,
        );
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_reader_cond_preserve_odd_forms_at_eof() {
        let bump = Bump::new();
        let mut parser = Parser::with_opts(
            "#?(:clj 1 :cljs",
            ParseOpts {
                platform: Platform::Clj,
                read_cond: ReadCondBehavior::Preserve,
            },
            &bump,
        );
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_unterminated_list() {
        let bump = Bump::new();
        let mut parser = Parser::new("(1 2 3", &bump);
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_unterminated_vector() {
        let bump = Bump::new();
        let mut parser = Parser::new("[1 2 3", &bump);
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_unterminated_map() {
        let bump = Bump::new();
        let mut parser = Parser::new("{:a 1", &bump);
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_unterminated_set() {
        let bump = Bump::new();
        let mut parser = Parser::new("#{1 2", &bump);
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_unterminated_anon_fn() {
        let bump = Bump::new();
        let mut parser = Parser::new("#(+ 1", &bump);
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_mismatched_delimiters() {
        let bump = Bump::new();
        let mut parser = Parser::new("(1 2]", &bump);
        assert!(parser.parse().is_err());

        let mut parser = Parser::new("[1 2)", &bump);
        assert!(parser.parse().is_err());

        let mut parser = Parser::new("{:a 1)", &bump);
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_mismatched_list_with_brace() {
        let bump = Bump::new();
        let mut parser = Parser::new("(1 2}", &bump);
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_mismatched_map_paren() {
        let bump = Bump::new();
        let mut parser = Parser::new("{:a 1)", &bump);
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_mismatched_map_bracket() {
        let bump = Bump::new();
        let mut parser = Parser::new("{:a 1]", &bump);
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_mismatched_set_paren() {
        let bump = Bump::new();
        let mut parser = Parser::new("#{1 2)", &bump);
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_mismatched_set_bracket() {
        let bump = Bump::new();
        let mut parser = Parser::new("#{1 2]", &bump);
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_mismatched_anon_fn_bracket() {
        let bump = Bump::new();
        let mut parser = Parser::new("#(+ 1]", &bump);
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_mismatched_anon_fn_brace() {
        let bump = Bump::new();
        let mut parser = Parser::new("#(+ 1}", &bump);
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_odd_map_entries() {
        let bump = Bump::new();
        let mut parser = Parser::new("{:a 1 :b}", &bump);
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_standalone_rparen() {
        let bump = Bump::new();
        let mut parser = Parser::new(")", &bump);
        assert!(parser.parse_form().is_err());
    }

    #[test]
    fn test_standalone_rbracket() {
        let bump = Bump::new();
        let mut parser = Parser::new("]", &bump);
        assert!(parser.parse_form().is_err());
    }

    #[test]
    fn test_standalone_rbrace() {
        let bump = Bump::new();
        let mut parser = Parser::new("}", &bump);
        assert!(parser.parse_form().is_err());
    }

    #[test]
    fn test_eof_on_parse_form() {
        let bump = Bump::new();
        let mut parser = Parser::new("", &bump);
        assert!(parser.parse_form().is_err());
    }

    #[test]
    fn test_hash_eof() {
        let bump = Bump::new();
        let mut parser = Parser::new("#", &bump);
        assert!(parser.parse_form().is_err());
    }

    #[test]
    fn test_symbolic_invalid() {
        let bump = Bump::new();
        let mut parser = Parser::new("##Unknown", &bump);
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_symbolic_not_symbol() {
        let bump = Bump::new();
        let mut parser = Parser::new("##123", &bump);
        assert!(parser.parse().is_err());
    }

    #[test]
    fn test_symbol_slash() {
        assert_eq!(
            parse_one("foo/"),
            Form::Symbol {
                ns: None,
                name: "foo/"
            }
        );
    }

    #[test]
    fn test_symbol_slash_start() {
        assert_eq!(
            parse_one("/bar"),
            Form::Symbol {
                ns: None,
                name: "/bar"
            }
        );
    }

    #[test]
    fn test_keyword_slash() {
        assert_eq!(
            parse_one(":foo/"),
            Form::Keyword {
                ns: None,
                name: "foo/",
                auto_resolve: false
            }
        );
    }

    #[test]
    fn test_keyword_slash_start() {
        assert_eq!(
            parse_one(":/bar"),
            Form::Keyword {
                ns: None,
                name: "/bar",
                auto_resolve: false
            }
        );
    }

    #[test]
    fn test_span_tracking() {
        let bump = Bump::new();
        let mut parser = Parser::new("foo", &bump);
        let form = parser.parse_form().unwrap();
        assert_eq!(form.span.start, 0);
        assert_eq!(form.span.end, 3);
    }

    #[test]
    fn test_parse_multiple_forms_with_spacing() {
        let bump = Bump::new();
        let mut parser = Parser::new("a b c", &bump);
        let forms = parser.parse().unwrap();
        assert_eq!(forms.len(), 3);
        assert_eq!(forms[0].span.end, 1);
        assert!(forms[1].span.start > 1);
    }

    #[test]
    fn test_peek_reuses_cached_token() {
        let bump = Bump::new();
        let mut parser = Parser::new("foo", &bump);
        let (tok1, _) = parser.peek().unwrap();
        let (tok2, _) = parser.peek().unwrap();
        assert_eq!(tok1, tok2);
    }

    #[test]
    fn test_current_end_span_with_current_token() {
        let bump = Bump::new();
        let mut parser = Parser::new("foo bar", &bump);
        let form1 = parser.parse_form().unwrap();
        assert!(form1.span.start == 0);
        assert!(form1.span.end <= 4);
        let form2 = parser.parse_form().unwrap();
        assert!(form2.span.start >= 4);
    }
}
