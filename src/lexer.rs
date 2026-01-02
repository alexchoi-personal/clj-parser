#![allow(
    clippy::missing_errors_doc,
    clippy::missing_panics_doc,
    clippy::must_use_candidate
)]

use bumpalo::Bump;
use memchr::{memchr, memchr2};

use crate::ast::StringValue;
use crate::error::{ErrorKind, ParseError};
use crate::span::Span;

#[derive(Debug, Clone, PartialEq)]
pub enum Token<'a> {
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Quote,
    Backtick,
    Tilde,
    TildeAt,
    At,
    Caret,
    Hash,
    HashBrace,
    HashParen,
    HashQuote,
    HashUnderscore,
    HashQuestion,
    HashQuestionAt,
    HashHash,
    Number(&'a str),
    String(StringValue<'a>),
    Char(char),
    Symbol(&'a str),
    Keyword(&'a str),
    Regex(StringValue<'a>),
    TagSymbol(&'a str),
    Eof,
}

pub struct Lexer<'a> {
    source: &'a str,
    bytes: &'a [u8],
    pos: usize,
    start: usize,
    bump: &'a Bump,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str, bump: &'a Bump) -> Self {
        Self {
            source,
            bytes: source.as_bytes(),
            pos: 0,
            start: 0,
            bump,
        }
    }

    #[inline(always)]
    pub fn peek_char(&self) -> Option<char> {
        if self.pos >= self.bytes.len() {
            return None;
        }
        let b = self.bytes[self.pos];
        if b.is_ascii() {
            Some(b as char)
        } else {
            self.source[self.pos..].chars().next()
        }
    }

    #[inline(always)]
    fn peek_byte(&self) -> Option<u8> {
        self.bytes.get(self.pos).copied()
    }

    #[inline(always)]
    fn peek_byte_offset(&self, offset: usize) -> Option<u8> {
        self.bytes.get(self.pos + offset).copied()
    }

    #[inline(always)]
    pub fn advance(&mut self) -> Option<char> {
        let c = self.peek_char()?;
        self.pos += c.len_utf8();
        Some(c)
    }

    #[inline(always)]
    fn advance_byte(&mut self) {
        self.pos += 1;
    }

    fn make_span(&self) -> Span {
        Span::new(self.start, self.pos)
    }

    fn make_error(&self, kind: ErrorKind) -> ParseError {
        let span = self.make_span();
        let source = &self.source[span.start..span.end];
        ParseError::new(kind, span, source)
    }

    pub fn skip_whitespace_and_comments(&mut self) {
        while self.pos < self.bytes.len() {
            match self.bytes[self.pos] {
                b' ' | b'\t' | b'\n' | b'\r' | b',' => {
                    self.pos += 1;
                }
                b';' => {
                    if let Some(newline_pos) = memchr(b'\n', &self.bytes[self.pos..]) {
                        self.pos += newline_pos;
                    } else {
                        self.pos = self.bytes.len();
                    }
                }
                _ => break,
            }
        }
    }

    pub fn read_string(&mut self) -> Result<StringValue<'a>, ParseError> {
        self.advance_byte();
        let content_start = self.pos;
        let mut has_escape = false;

        loop {
            let remaining = &self.bytes[self.pos..];
            match memchr2(b'"', b'\\', remaining) {
                None => {
                    self.pos = self.bytes.len();
                    return Err(self.make_error(ErrorKind::UnterminatedString));
                }
                Some(idx) => {
                    self.pos += idx;
                    match self.bytes[self.pos] {
                        b'"' => {
                            let content_end = self.pos;
                            self.pos += 1;
                            if has_escape {
                                return self.parse_string_with_escapes(content_start, content_end);
                            } else {
                                return Ok(StringValue::Borrowed(&self.source[content_start..content_end]));
                            }
                        }
                        b'\\' => {
                            has_escape = true;
                            self.pos += 1;
                            if self.pos >= self.bytes.len() {
                                return Err(self.make_error(ErrorKind::UnterminatedString));
                            }
                            let escaped = self.bytes[self.pos];
                            if escaped == b'u' {
                                if self.pos + 5 > self.bytes.len() {
                                    self.pos = self.bytes.len();
                                    return Err(self.make_error(ErrorKind::UnterminatedString));
                                }
                                self.pos += 5;
                            } else {
                                self.pos += 1;
                            }
                        }
                        _ => unreachable!(),
                    }
                }
            }
        }
    }

    fn parse_string_with_escapes(&self, start: usize, end: usize) -> Result<StringValue<'a>, ParseError> {
        let mut result = bumpalo::collections::String::new_in(self.bump);
        let mut pos = start;

        while pos < end {
            match self.bytes[pos] {
                b'\\' => {
                    pos += 1;
                    let escaped = match self.bytes[pos] {
                        b'n' => '\n',
                        b't' => '\t',
                        b'r' => '\r',
                        b'"' => '"',
                        b'\\' => '\\',
                        b'u' => {
                            let hex = &self.source[pos + 1..pos + 5];
                            let code = u32::from_str_radix(hex, 16).map_err(|_| {
                                ParseError::new(
                                    ErrorKind::InvalidEscape('u'),
                                    Span::new(pos, pos + 5),
                                    hex,
                                )
                            })?;
                            pos += 4;
                            char::from_u32(code).unwrap_or('\u{FFFD}')
                        }
                        c => {
                            return Err(ParseError::new(
                                ErrorKind::InvalidEscape(c as char),
                                Span::new(pos, pos + 1),
                                &self.source[pos..pos + 1],
                            ));
                        }
                    };
                    result.push(escaped);
                    pos += 1;
                }
                _ => {
                    let c = self.source[pos..].chars().next().unwrap();
                    result.push(c);
                    pos += c.len_utf8();
                }
            }
        }

        Ok(StringValue::Owned(result))
    }

    pub fn read_char(&mut self) -> Result<char, ParseError> {
        self.advance_byte();

        let start_pos = self.pos;
        let mut name = String::new();

        while self.pos < self.bytes.len() {
            let b = self.bytes[self.pos];
            if b.is_ascii_alphanumeric() || b == b'-' {
                name.push(b as char);
                self.pos += 1;
            } else {
                break;
            }
        }

        if name.is_empty() {
            if let Some(c) = self.peek_char() {
                self.advance();
                return Ok(c);
            }
            return Err(self.make_error(ErrorKind::InvalidCharLiteral("empty".to_string())));
        }

        match name.as_str() {
            "newline" => Ok('\n'),
            "space" => Ok(' '),
            "tab" => Ok('\t'),
            "return" => Ok('\r'),
            "backspace" => Ok('\x08'),
            "formfeed" => Ok('\x0C'),
            _ if name.starts_with('u') && name.len() == 5 => {
                let hex = &name[1..];
                if hex.chars().all(|c| c.is_ascii_hexdigit()) {
                    let code = u32::from_str_radix(hex, 16).unwrap();
                    char::from_u32(code)
                        .ok_or_else(|| self.make_error(ErrorKind::InvalidCharLiteral(name.clone())))
                } else {
                    Err(self.make_error(ErrorKind::InvalidCharLiteral(name)))
                }
            }
            _ if name.len() == 1 => Ok(name.chars().next().unwrap()),
            _ => {
                self.pos = start_pos + 1;
                Ok(name.chars().next().unwrap())
            }
        }
    }

    pub fn read_number(&mut self) -> &'a str {
        let start = self.pos;

        if matches!(self.peek_byte(), Some(b'-' | b'+')) {
            self.advance_byte();
        }

        while self.pos < self.bytes.len() {
            let b = self.bytes[self.pos];
            if b.is_ascii_alphanumeric()
                || b == b'.'
                || b == b'/'
                || b == b'+'
                || b == b'-'
            {
                self.pos += 1;
            } else {
                break;
            }
        }

        &self.source[start..self.pos]
    }

    #[inline(always)]
    fn is_symbol_byte(b: u8) -> bool {
        b.is_ascii_alphanumeric()
            || matches!(
                b,
                b'*' | b'+'
                    | b'!'
                    | b'-'
                    | b'_'
                    | b'\''
                    | b'?'
                    | b'<'
                    | b'>'
                    | b'='
                    | b'.'
                    | b'/'
                    | b':'
                    | b'#'
                    | b'&'
                    | b'%'
                    | b'$'
                    | b'|'
            )
    }

    fn is_symbol_char(c: char) -> bool {
        c.is_alphanumeric()
            || c == '*'
            || c == '+'
            || c == '!'
            || c == '-'
            || c == '_'
            || c == '\''
            || c == '?'
            || c == '<'
            || c == '>'
            || c == '='
            || c == '.'
            || c == '/'
            || c == ':'
            || c == '#'
            || c == '&'
            || c == '%'
            || c == '$'
            || c == '|'
    }

    fn is_symbol_start_char(c: char) -> bool {
        Self::is_symbol_char(c) && !c.is_ascii_digit()
    }

    pub fn read_symbol(&mut self) -> &'a str {
        let start = self.pos;

        while self.pos < self.bytes.len() {
            let b = self.bytes[self.pos];
            if b.is_ascii() {
                if Self::is_symbol_byte(b) {
                    self.pos += 1;
                } else {
                    break;
                }
            } else {
                let c = self.source[self.pos..].chars().next().unwrap();
                if Self::is_symbol_char(c) {
                    self.pos += c.len_utf8();
                } else {
                    break;
                }
            }
        }

        &self.source[start..self.pos]
    }

    pub fn read_keyword(&mut self) -> Result<&'a str, ParseError> {
        self.advance_byte();

        if self.peek_byte() == Some(b':') {
            self.advance_byte();
        }

        let name_start = self.pos;

        while self.pos < self.bytes.len() {
            let b = self.bytes[self.pos];
            if b.is_ascii() {
                if Self::is_symbol_byte(b) {
                    self.pos += 1;
                } else {
                    break;
                }
            } else {
                let c = self.source[self.pos..].chars().next().unwrap();
                if Self::is_symbol_char(c) {
                    self.pos += c.len_utf8();
                } else {
                    break;
                }
            }
        }

        if self.pos == name_start {
            return Err(self.make_error(ErrorKind::InvalidKeyword("empty keyword".to_string())));
        }

        Ok(&self.source[self.start..self.pos])
    }

    pub fn read_regex(&mut self) -> Result<StringValue<'a>, ParseError> {
        let content_start = self.pos;
        let mut has_escape = false;

        loop {
            let remaining = &self.bytes[self.pos..];
            match memchr2(b'"', b'\\', remaining) {
                None => {
                    self.pos = self.bytes.len();
                    return Err(self.make_error(ErrorKind::UnterminatedRegex));
                }
                Some(idx) => {
                    self.pos += idx;
                    match self.bytes[self.pos] {
                        b'"' => {
                            let content_end = self.pos;
                            self.pos += 1;
                            if has_escape {
                                return self.parse_regex_with_escapes(content_start, content_end);
                            } else {
                                return Ok(StringValue::Borrowed(&self.source[content_start..content_end]));
                            }
                        }
                        b'\\' => {
                            has_escape = true;
                            self.pos += 1;
                            if self.pos >= self.bytes.len() {
                                return Err(self.make_error(ErrorKind::UnterminatedRegex));
                            }
                            self.pos += 1;
                        }
                        _ => unreachable!(),
                    }
                }
            }
        }
    }

    fn parse_regex_with_escapes(&self, start: usize, end: usize) -> Result<StringValue<'a>, ParseError> {
        let mut result = bumpalo::collections::String::new_in(self.bump);
        let mut pos = start;

        while pos < end {
            match self.bytes[pos] {
                b'\\' => {
                    result.push('\\');
                    pos += 1;
                    if pos < end {
                        let c = self.source[pos..].chars().next().unwrap();
                        result.push(c);
                        pos += c.len_utf8();
                    }
                }
                _ => {
                    let c = self.source[pos..].chars().next().unwrap();
                    result.push(c);
                    pos += c.len_utf8();
                }
            }
        }

        Ok(StringValue::Owned(result))
    }

    fn read_hash_dispatch(&mut self) -> Result<Token<'a>, ParseError> {
        self.advance_byte();

        match self.peek_byte() {
            Some(b'{') => {
                self.advance_byte();
                Ok(Token::HashBrace)
            }
            Some(b'(') => {
                self.advance_byte();
                Ok(Token::HashParen)
            }
            Some(b'\'') => {
                self.advance_byte();
                Ok(Token::HashQuote)
            }
            Some(b'_') => {
                self.advance_byte();
                Ok(Token::HashUnderscore)
            }
            Some(b'#') => {
                self.advance_byte();
                Ok(Token::HashHash)
            }
            Some(b'?') => {
                self.advance_byte();
                if self.peek_byte() == Some(b'@') {
                    self.advance_byte();
                    Ok(Token::HashQuestionAt)
                } else {
                    Ok(Token::HashQuestion)
                }
            }
            Some(b'^') => {
                self.advance_byte();
                Ok(Token::Caret)
            }
            Some(b'"') => {
                self.advance_byte();
                let regex = self.read_regex()?;
                Ok(Token::Regex(regex))
            }
            Some(b) if !b.is_ascii() => {
                let c = self.peek_char().unwrap();
                if Self::is_symbol_start_char(c) {
                    let sym = self.read_symbol();
                    Ok(Token::TagSymbol(sym))
                } else {
                    Err(self.make_error(ErrorKind::UnexpectedChar(c)))
                }
            }
            Some(b) => {
                let c = b as char;
                if Self::is_symbol_start_char(c) {
                    let sym = self.read_symbol();
                    Ok(Token::TagSymbol(sym))
                } else {
                    Err(self.make_error(ErrorKind::UnexpectedChar(c)))
                }
            }
            None => Ok(Token::Hash),
        }
    }

    pub fn next_token(&mut self) -> Result<(Token<'a>, Span), ParseError> {
        self.skip_whitespace_and_comments();
        self.start = self.pos;

        let Some(b) = self.peek_byte() else {
            return Ok((Token::Eof, self.make_span()));
        };

        let token = match b {
            b'(' => {
                self.advance_byte();
                Token::LParen
            }
            b')' => {
                self.advance_byte();
                Token::RParen
            }
            b'[' => {
                self.advance_byte();
                Token::LBracket
            }
            b']' => {
                self.advance_byte();
                Token::RBracket
            }
            b'{' => {
                self.advance_byte();
                Token::LBrace
            }
            b'}' => {
                self.advance_byte();
                Token::RBrace
            }
            b'\'' => {
                self.advance_byte();
                Token::Quote
            }
            b'`' => {
                self.advance_byte();
                Token::Backtick
            }
            b'~' => {
                self.advance_byte();
                if self.peek_byte() == Some(b'@') {
                    self.advance_byte();
                    Token::TildeAt
                } else {
                    Token::Tilde
                }
            }
            b'@' => {
                self.advance_byte();
                Token::At
            }
            b'^' => {
                self.advance_byte();
                Token::Caret
            }
            b'#' => self.read_hash_dispatch()?,
            b'"' => {
                let s = self.read_string()?;
                Token::String(s)
            }
            b'\\' => {
                let c = self.read_char()?;
                Token::Char(c)
            }
            b':' => {
                let kw = self.read_keyword()?;
                Token::Keyword(kw)
            }
            b'-' | b'+' => {
                if let Some(next) = self.peek_byte_offset(1) {
                    if next.is_ascii_digit() {
                        let num = self.read_number();
                        Token::Number(num)
                    } else {
                        let sym = self.read_symbol();
                        Token::Symbol(sym)
                    }
                } else {
                    let sym = self.read_symbol();
                    Token::Symbol(sym)
                }
            }
            b'0'..=b'9' => {
                let num = self.read_number();
                Token::Number(num)
            }
            _ => {
                if !b.is_ascii() {
                    let c = self.peek_char().unwrap();
                    if Self::is_symbol_start_char(c) {
                        let sym = self.read_symbol();
                        Token::Symbol(sym)
                    } else {
                        self.advance();
                        return Err(self.make_error(ErrorKind::UnexpectedChar(c)));
                    }
                } else {
                    let c = b as char;
                    if Self::is_symbol_start_char(c) {
                        let sym = self.read_symbol();
                        Token::Symbol(sym)
                    } else {
                        self.advance_byte();
                        return Err(self.make_error(ErrorKind::UnexpectedChar(c)));
                    }
                }
            }
        };

        Ok((token, self.make_span()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_delimiters() {
        let bump = Bump::new();
        let mut lexer = Lexer::new("()[]{}",  &bump);
        assert!(matches!(lexer.next_token().unwrap().0, Token::LParen));
        assert!(matches!(lexer.next_token().unwrap().0, Token::RParen));
        assert!(matches!(lexer.next_token().unwrap().0, Token::LBracket));
        assert!(matches!(lexer.next_token().unwrap().0, Token::RBracket));
        assert!(matches!(lexer.next_token().unwrap().0, Token::LBrace));
        assert!(matches!(lexer.next_token().unwrap().0, Token::RBrace));
        assert!(matches!(lexer.next_token().unwrap().0, Token::Eof));
    }

    #[test]
    fn test_quotes() {
        let bump = Bump::new();
        let mut lexer = Lexer::new("'`~@~", &bump);
        assert!(matches!(lexer.next_token().unwrap().0, Token::Quote));
        assert!(matches!(lexer.next_token().unwrap().0, Token::Backtick));
        assert!(matches!(lexer.next_token().unwrap().0, Token::TildeAt));
        assert!(matches!(lexer.next_token().unwrap().0, Token::Tilde));
    }

    #[test]
    fn test_hash_dispatch() {
        let bump = Bump::new();
        let mut lexer = Lexer::new("#{#(#'#_#?#?@##", &bump);
        assert!(matches!(lexer.next_token().unwrap().0, Token::HashBrace));
        assert!(matches!(lexer.next_token().unwrap().0, Token::HashParen));
        assert!(matches!(lexer.next_token().unwrap().0, Token::HashQuote));
        assert!(matches!(
            lexer.next_token().unwrap().0,
            Token::HashUnderscore
        ));
        assert!(matches!(lexer.next_token().unwrap().0, Token::HashQuestion));
        assert!(matches!(
            lexer.next_token().unwrap().0,
            Token::HashQuestionAt
        ));
        assert!(matches!(lexer.next_token().unwrap().0, Token::HashHash));
    }

    #[test]
    fn test_string() {
        let bump = Bump::new();
        let mut lexer = Lexer::new(r#""hello\nworld""#, &bump);
        let (token, _) = lexer.next_token().unwrap();
        if let Token::String(sv) = token {
            assert_eq!(sv.as_str(), "hello\nworld");
        } else {
            panic!("Expected string token");
        }
    }

    #[test]
    fn test_string_unicode() {
        let bump = Bump::new();
        let mut lexer = Lexer::new(r#""\u0041""#, &bump);
        let (token, _) = lexer.next_token().unwrap();
        if let Token::String(sv) = token {
            assert_eq!(sv.as_str(), "A");
        } else {
            panic!("Expected string token");
        }
    }

    #[test]
    fn test_char() {
        let bump = Bump::new();
        let mut lexer = Lexer::new(r"\newline \space \tab \a", &bump);
        assert_eq!(lexer.next_token().unwrap().0, Token::Char('\n'));
        assert_eq!(lexer.next_token().unwrap().0, Token::Char(' '));
        assert_eq!(lexer.next_token().unwrap().0, Token::Char('\t'));
        assert_eq!(lexer.next_token().unwrap().0, Token::Char('a'));
    }

    #[test]
    fn test_char_unicode() {
        let bump = Bump::new();
        let mut lexer = Lexer::new(r"\u0041", &bump);
        assert_eq!(lexer.next_token().unwrap().0, Token::Char('A'));
    }

    #[test]
    fn test_number() {
        let bump = Bump::new();
        let mut lexer = Lexer::new("123 -456 3.14 2r1010 16rFF 1/2 1N 1M", &bump);
        assert_eq!(lexer.next_token().unwrap().0, Token::Number("123"));
        assert_eq!(lexer.next_token().unwrap().0, Token::Number("-456"));
        assert_eq!(lexer.next_token().unwrap().0, Token::Number("3.14"));
        assert_eq!(lexer.next_token().unwrap().0, Token::Number("2r1010"));
        assert_eq!(lexer.next_token().unwrap().0, Token::Number("16rFF"));
        assert_eq!(lexer.next_token().unwrap().0, Token::Number("1/2"));
        assert_eq!(lexer.next_token().unwrap().0, Token::Number("1N"));
        assert_eq!(lexer.next_token().unwrap().0, Token::Number("1M"));
    }

    #[test]
    fn test_symbol() {
        let bump = Bump::new();
        let mut lexer = Lexer::new("foo bar/baz +", &bump);
        assert_eq!(lexer.next_token().unwrap().0, Token::Symbol("foo"));
        assert_eq!(lexer.next_token().unwrap().0, Token::Symbol("bar/baz"));
        assert_eq!(lexer.next_token().unwrap().0, Token::Symbol("+"));
    }

    #[test]
    fn test_keyword() {
        let bump = Bump::new();
        let mut lexer = Lexer::new(":foo ::bar :ns/key", &bump);
        assert_eq!(lexer.next_token().unwrap().0, Token::Keyword(":foo"));
        assert_eq!(lexer.next_token().unwrap().0, Token::Keyword("::bar"));
        assert_eq!(lexer.next_token().unwrap().0, Token::Keyword(":ns/key"));
    }

    #[test]
    fn test_regex() {
        let bump = Bump::new();
        let mut lexer = Lexer::new(r#"#"hello\d+""#, &bump);
        let (token, _) = lexer.next_token().unwrap();
        if let Token::Regex(sv) = token {
            assert_eq!(sv.as_str(), r"hello\d+");
        } else {
            panic!("Expected regex token");
        }
    }

    #[test]
    fn test_tag_symbol() {
        let bump = Bump::new();
        let mut lexer = Lexer::new("#inst #uuid", &bump);
        assert_eq!(lexer.next_token().unwrap().0, Token::TagSymbol("inst"));
        assert_eq!(lexer.next_token().unwrap().0, Token::TagSymbol("uuid"));
    }

    #[test]
    fn test_whitespace_and_comments() {
        let bump = Bump::new();
        let mut lexer = Lexer::new("foo ; comment\n bar", &bump);
        assert_eq!(lexer.next_token().unwrap().0, Token::Symbol("foo"));
        assert_eq!(lexer.next_token().unwrap().0, Token::Symbol("bar"));
    }

    #[test]
    fn test_comma_as_whitespace() {
        let bump = Bump::new();
        let mut lexer = Lexer::new("foo,bar", &bump);
        assert_eq!(lexer.next_token().unwrap().0, Token::Symbol("foo"));
        assert_eq!(lexer.next_token().unwrap().0, Token::Symbol("bar"));
    }

    #[test]
    fn test_at_and_caret() {
        let bump = Bump::new();
        let mut lexer = Lexer::new("@foo ^meta", &bump);
        assert!(matches!(lexer.next_token().unwrap().0, Token::At));
        assert_eq!(lexer.next_token().unwrap().0, Token::Symbol("foo"));
        assert!(matches!(lexer.next_token().unwrap().0, Token::Caret));
        assert_eq!(lexer.next_token().unwrap().0, Token::Symbol("meta"));
    }

    #[test]
    fn test_span() {
        let bump = Bump::new();
        let mut lexer = Lexer::new("foo", &bump);
        let (_, span) = lexer.next_token().unwrap();
        assert_eq!(span.start, 0);
        assert_eq!(span.end, 3);
    }

    #[test]
    fn test_unterminated_string() {
        let bump = Bump::new();
        let mut lexer = Lexer::new(r#""hello"#, &bump);
        assert!(lexer.next_token().is_err());
    }

    #[test]
    fn test_unterminated_regex() {
        let bump = Bump::new();
        let mut lexer = Lexer::new(r#"#"hello"#, &bump);
        assert!(lexer.next_token().is_err());
    }

    #[test]
    fn test_string_escapes() {
        let bump = Bump::new();
        let mut lexer = Lexer::new(r#""\t\r\"\\""#, &bump);
        let (token, _) = lexer.next_token().unwrap();
        if let Token::String(sv) = token {
            assert_eq!(sv.as_str(), "\t\r\"\\");
        } else {
            panic!("Expected string token");
        }
    }

    #[test]
    fn test_invalid_string_escape() {
        let bump = Bump::new();
        let mut lexer = Lexer::new(r#""\q""#, &bump);
        assert!(lexer.next_token().is_err());
    }

    #[test]
    fn test_string_unterminated_after_backslash() {
        let bump = Bump::new();
        let mut lexer = Lexer::new(r#""hello\"#, &bump);
        assert!(lexer.next_token().is_err());
    }

    #[test]
    fn test_string_invalid_unicode_hex() {
        let bump = Bump::new();
        let mut lexer = Lexer::new(r#""\uZZZZ""#, &bump);
        assert!(lexer.next_token().is_err());
    }

    #[test]
    fn test_string_unicode_unterminated() {
        let bump = Bump::new();
        let mut lexer = Lexer::new(r#""\u00"#, &bump);
        assert!(lexer.next_token().is_err());
    }

    #[test]
    fn test_char_backspace() {
        let bump = Bump::new();
        let mut lexer = Lexer::new(r"\backspace", &bump);
        assert_eq!(lexer.next_token().unwrap().0, Token::Char('\x08'));
    }

    #[test]
    fn test_char_formfeed() {
        let bump = Bump::new();
        let mut lexer = Lexer::new(r"\formfeed", &bump);
        assert_eq!(lexer.next_token().unwrap().0, Token::Char('\x0C'));
    }

    #[test]
    fn test_char_return() {
        let bump = Bump::new();
        let mut lexer = Lexer::new(r"\return", &bump);
        assert_eq!(lexer.next_token().unwrap().0, Token::Char('\r'));
    }

    #[test]
    fn test_char_invalid_unicode_hex() {
        let bump = Bump::new();
        let mut lexer = Lexer::new(r"\uZZZZ", &bump);
        assert!(lexer.next_token().is_err());
    }

    #[test]
    fn test_char_multi_char_fallback() {
        let bump = Bump::new();
        let mut lexer = Lexer::new(r"\abc)", &bump);
        let (token, _) = lexer.next_token().unwrap();
        assert_eq!(token, Token::Char('a'));
    }

    #[test]
    fn test_char_special_char() {
        let bump = Bump::new();
        let mut lexer = Lexer::new(r"\!", &bump);
        assert_eq!(lexer.next_token().unwrap().0, Token::Char('!'));
    }

    #[test]
    fn test_char_empty_error() {
        let bump = Bump::new();
        let mut lexer = Lexer::new(r"\", &bump);
        assert!(lexer.next_token().is_err());
    }

    #[test]
    fn test_unexpected_char() {
        let bump = Bump::new();
        let mut lexer = Lexer::new("§", &bump);
        assert!(lexer.next_token().is_err());
    }

    #[test]
    fn test_hash_unexpected() {
        let bump = Bump::new();
        let mut lexer = Lexer::new("#)", &bump);
        assert!(lexer.next_token().is_err());
    }

    #[test]
    fn test_regex_with_escape() {
        let bump = Bump::new();
        let mut lexer = Lexer::new(r#"#"a\nb""#, &bump);
        let (token, _) = lexer.next_token().unwrap();
        if let Token::Regex(sv) = token {
            assert_eq!(sv.as_str(), r"a\nb");
        } else {
            panic!("Expected regex token");
        }
    }

    #[test]
    fn test_regex_unterminated_after_escape() {
        let bump = Bump::new();
        let mut lexer = Lexer::new(r#"#"test\"#, &bump);
        assert!(lexer.next_token().is_err());
    }

    #[test]
    fn test_plus_sign_symbol() {
        let bump = Bump::new();
        let mut lexer = Lexer::new("+ foo", &bump);
        assert_eq!(lexer.next_token().unwrap().0, Token::Symbol("+"));
    }

    #[test]
    fn test_hash_at_eof() {
        let bump = Bump::new();
        let mut lexer = Lexer::new("#", &bump);
        assert!(matches!(lexer.next_token().unwrap().0, Token::Hash));
    }

    #[test]
    fn test_empty_keyword() {
        let bump = Bump::new();
        let mut lexer = Lexer::new(":", &bump);
        assert!(lexer.next_token().is_err());
    }
}
