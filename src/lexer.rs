#![allow(
    clippy::missing_errors_doc,
    clippy::missing_panics_doc,
    clippy::must_use_candidate
)]

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
    String(String),
    Char(char),
    Symbol(&'a str),
    Keyword(&'a str),
    Regex(String),
    TagSymbol(&'a str),
    Eof,
}

pub struct Lexer<'a> {
    source: &'a str,
    pos: usize,
    start: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            pos: 0,
            start: 0,
        }
    }

    pub fn peek_char(&self) -> Option<char> {
        self.source[self.pos..].chars().next()
    }

    fn peek_char_offset(&self, offset: usize) -> Option<char> {
        self.source[self.pos..].chars().nth(offset)
    }

    pub fn advance(&mut self) -> Option<char> {
        let c = self.peek_char()?;
        self.pos += c.len_utf8();
        Some(c)
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
        loop {
            match self.peek_char() {
                Some(' ' | '\t' | '\n' | '\r' | ',') => {
                    self.advance();
                }
                Some(';') => {
                    while let Some(c) = self.peek_char() {
                        if c == '\n' {
                            break;
                        }
                        self.advance();
                    }
                }
                _ => break,
            }
        }
    }

    pub fn read_string(&mut self) -> Result<String, ParseError> {
        self.advance();
        let mut result = String::new();

        loop {
            match self.advance() {
                None => {
                    return Err(self.make_error(ErrorKind::UnterminatedString));
                }
                Some('"') => {
                    return Ok(result);
                }
                Some('\\') => {
                    let escaped = match self.advance() {
                        None => {
                            return Err(self.make_error(ErrorKind::UnterminatedString));
                        }
                        Some('n') => '\n',
                        Some('t') => '\t',
                        Some('r') => '\r',
                        Some('"') => '"',
                        Some('\\') => '\\',
                        Some('u') => {
                            let mut hex = String::new();
                            for _ in 0..4 {
                                match self.advance() {
                                    Some(c) if c.is_ascii_hexdigit() => hex.push(c),
                                    Some(c) => {
                                        return Err(self.make_error(ErrorKind::InvalidEscape(c)));
                                    }
                                    None => {
                                        return Err(self.make_error(ErrorKind::UnterminatedString));
                                    }
                                }
                            }
                            let code = u32::from_str_radix(&hex, 16).unwrap();
                            char::from_u32(code).unwrap_or('\u{FFFD}')
                        }
                        Some(c) => {
                            return Err(self.make_error(ErrorKind::InvalidEscape(c)));
                        }
                    };
                    result.push(escaped);
                }
                Some(c) => {
                    result.push(c);
                }
            }
        }
    }

    pub fn read_char(&mut self) -> Result<char, ParseError> {
        self.advance();

        let start_pos = self.pos;
        let mut name = String::new();

        while let Some(c) = self.peek_char() {
            if c.is_alphanumeric() || c == '-' {
                name.push(c);
                self.advance();
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
                self.pos = start_pos + name.chars().next().unwrap().len_utf8();
                Ok(name.chars().next().unwrap())
            }
        }
    }

    pub fn read_number(&mut self) -> Result<&'a str, ParseError> {
        let start = self.pos;

        if self.peek_char() == Some('-') || self.peek_char() == Some('+') {
            self.advance();
        }

        while let Some(c) = self.peek_char() {
            if c.is_ascii_alphanumeric()
                || c == '.'
                || c == '/'
                || c == '+'
                || c == '-'
                || c == 'x'
                || c == 'X'
                || c == 'r'
                || c == 'R'
                || c == 'N'
                || c == 'M'
            {
                self.advance();
            } else {
                break;
            }
        }

        Ok(&self.source[start..self.pos])
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

        while let Some(c) = self.peek_char() {
            if Self::is_symbol_char(c) {
                self.advance();
            } else {
                break;
            }
        }

        &self.source[start..self.pos]
    }

    pub fn read_keyword(&mut self) -> Result<&'a str, ParseError> {
        self.advance();

        if self.peek_char() == Some(':') {
            self.advance();
        }

        let start = self.pos;

        while let Some(c) = self.peek_char() {
            if Self::is_symbol_char(c) {
                self.advance();
            } else {
                break;
            }
        }

        if self.pos == start {
            return Err(self.make_error(ErrorKind::InvalidKeyword("empty keyword".to_string())));
        }

        Ok(&self.source[self.start..self.pos])
    }

    pub fn read_regex(&mut self) -> Result<String, ParseError> {
        let mut result = String::new();

        loop {
            match self.advance() {
                None => {
                    return Err(self.make_error(ErrorKind::UnterminatedRegex));
                }
                Some('"') => {
                    return Ok(result);
                }
                Some('\\') => {
                    result.push('\\');
                    match self.advance() {
                        None => {
                            return Err(self.make_error(ErrorKind::UnterminatedRegex));
                        }
                        Some(c) => {
                            result.push(c);
                        }
                    }
                }
                Some(c) => {
                    result.push(c);
                }
            }
        }
    }

    fn read_hash_dispatch(&mut self) -> Result<Token<'a>, ParseError> {
        self.advance();

        match self.peek_char() {
            Some('{') => {
                self.advance();
                Ok(Token::HashBrace)
            }
            Some('(') => {
                self.advance();
                Ok(Token::HashParen)
            }
            Some('\'') => {
                self.advance();
                Ok(Token::HashQuote)
            }
            Some('_') => {
                self.advance();
                Ok(Token::HashUnderscore)
            }
            Some('#') => {
                self.advance();
                Ok(Token::HashHash)
            }
            Some('?') => {
                self.advance();
                if self.peek_char() == Some('@') {
                    self.advance();
                    Ok(Token::HashQuestionAt)
                } else {
                    Ok(Token::HashQuestion)
                }
            }
            Some('^') => {
                self.advance();
                Ok(Token::Caret)
            }
            Some('"') => {
                self.advance();
                let regex = self.read_regex()?;
                Ok(Token::Regex(regex))
            }
            Some(c) if Self::is_symbol_start_char(c) => {
                let sym = self.read_symbol();
                Ok(Token::TagSymbol(sym))
            }
            Some(c) => Err(self.make_error(ErrorKind::UnexpectedChar(c))),
            None => Ok(Token::Hash),
        }
    }

    pub fn next_token(&mut self) -> Result<(Token<'a>, Span), ParseError> {
        self.skip_whitespace_and_comments();
        self.start = self.pos;

        let Some(c) = self.peek_char() else {
            return Ok((Token::Eof, self.make_span()));
        };

        let token = match c {
            '(' => {
                self.advance();
                Token::LParen
            }
            ')' => {
                self.advance();
                Token::RParen
            }
            '[' => {
                self.advance();
                Token::LBracket
            }
            ']' => {
                self.advance();
                Token::RBracket
            }
            '{' => {
                self.advance();
                Token::LBrace
            }
            '}' => {
                self.advance();
                Token::RBrace
            }
            '\'' => {
                self.advance();
                Token::Quote
            }
            '`' => {
                self.advance();
                Token::Backtick
            }
            '~' => {
                self.advance();
                if self.peek_char() == Some('@') {
                    self.advance();
                    Token::TildeAt
                } else {
                    Token::Tilde
                }
            }
            '@' => {
                self.advance();
                Token::At
            }
            '^' => {
                self.advance();
                Token::Caret
            }
            '#' => self.read_hash_dispatch()?,
            '"' => {
                let s = self.read_string()?;
                Token::String(s)
            }
            '\\' => {
                let c = self.read_char()?;
                Token::Char(c)
            }
            ':' => {
                let kw = self.read_keyword()?;
                Token::Keyword(kw)
            }
            '-' | '+' => {
                if let Some(next) = self.peek_char_offset(1) {
                    if next.is_ascii_digit() {
                        let num = self.read_number()?;
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
            '0'..='9' => {
                let num = self.read_number()?;
                Token::Number(num)
            }
            _ if Self::is_symbol_start_char(c) => {
                let sym = self.read_symbol();
                Token::Symbol(sym)
            }
            _ => {
                self.advance();
                return Err(self.make_error(ErrorKind::UnexpectedChar(c)));
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
        let mut lexer = Lexer::new("()[]{}");
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
        let mut lexer = Lexer::new("'`~@~");
        assert!(matches!(lexer.next_token().unwrap().0, Token::Quote));
        assert!(matches!(lexer.next_token().unwrap().0, Token::Backtick));
        assert!(matches!(lexer.next_token().unwrap().0, Token::TildeAt));
        assert!(matches!(lexer.next_token().unwrap().0, Token::Tilde));
    }

    #[test]
    fn test_hash_dispatch() {
        let mut lexer = Lexer::new("#{#(#'#_#?#?@##");
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
        let mut lexer = Lexer::new(r#""hello\nworld""#);
        let (token, _) = lexer.next_token().unwrap();
        assert_eq!(token, Token::String("hello\nworld".to_string()));
    }

    #[test]
    fn test_string_unicode() {
        let mut lexer = Lexer::new(r#""\u0041""#);
        let (token, _) = lexer.next_token().unwrap();
        assert_eq!(token, Token::String("A".to_string()));
    }

    #[test]
    fn test_char() {
        let mut lexer = Lexer::new(r"\newline \space \tab \a");
        assert_eq!(lexer.next_token().unwrap().0, Token::Char('\n'));
        assert_eq!(lexer.next_token().unwrap().0, Token::Char(' '));
        assert_eq!(lexer.next_token().unwrap().0, Token::Char('\t'));
        assert_eq!(lexer.next_token().unwrap().0, Token::Char('a'));
    }

    #[test]
    fn test_char_unicode() {
        let mut lexer = Lexer::new(r"\u0041");
        assert_eq!(lexer.next_token().unwrap().0, Token::Char('A'));
    }

    #[test]
    fn test_number() {
        let mut lexer = Lexer::new("123 -456 3.14 2r1010 16rFF 1/2 1N 1M");
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
        let mut lexer = Lexer::new("foo bar/baz +");
        assert_eq!(lexer.next_token().unwrap().0, Token::Symbol("foo"));
        assert_eq!(lexer.next_token().unwrap().0, Token::Symbol("bar/baz"));
        assert_eq!(lexer.next_token().unwrap().0, Token::Symbol("+"));
    }

    #[test]
    fn test_keyword() {
        let mut lexer = Lexer::new(":foo ::bar :ns/key");
        assert_eq!(lexer.next_token().unwrap().0, Token::Keyword(":foo"));
        assert_eq!(lexer.next_token().unwrap().0, Token::Keyword("::bar"));
        assert_eq!(lexer.next_token().unwrap().0, Token::Keyword(":ns/key"));
    }

    #[test]
    fn test_regex() {
        let mut lexer = Lexer::new(r#"#"hello\d+""#);
        let (token, _) = lexer.next_token().unwrap();
        assert_eq!(token, Token::Regex(r"hello\d+".to_string()));
    }

    #[test]
    fn test_tag_symbol() {
        let mut lexer = Lexer::new("#inst #uuid");
        assert_eq!(lexer.next_token().unwrap().0, Token::TagSymbol("inst"));
        assert_eq!(lexer.next_token().unwrap().0, Token::TagSymbol("uuid"));
    }

    #[test]
    fn test_whitespace_and_comments() {
        let mut lexer = Lexer::new("foo ; comment\n bar");
        assert_eq!(lexer.next_token().unwrap().0, Token::Symbol("foo"));
        assert_eq!(lexer.next_token().unwrap().0, Token::Symbol("bar"));
    }

    #[test]
    fn test_comma_as_whitespace() {
        let mut lexer = Lexer::new("foo,bar");
        assert_eq!(lexer.next_token().unwrap().0, Token::Symbol("foo"));
        assert_eq!(lexer.next_token().unwrap().0, Token::Symbol("bar"));
    }

    #[test]
    fn test_at_and_caret() {
        let mut lexer = Lexer::new("@foo ^meta");
        assert!(matches!(lexer.next_token().unwrap().0, Token::At));
        assert_eq!(lexer.next_token().unwrap().0, Token::Symbol("foo"));
        assert!(matches!(lexer.next_token().unwrap().0, Token::Caret));
        assert_eq!(lexer.next_token().unwrap().0, Token::Symbol("meta"));
    }

    #[test]
    fn test_span() {
        let mut lexer = Lexer::new("foo");
        let (_, span) = lexer.next_token().unwrap();
        assert_eq!(span.start, 0);
        assert_eq!(span.end, 3);
    }

    #[test]
    fn test_unterminated_string() {
        let mut lexer = Lexer::new(r#""hello"#);
        assert!(lexer.next_token().is_err());
    }

    #[test]
    fn test_unterminated_regex() {
        let mut lexer = Lexer::new(r#"#"hello"#);
        assert!(lexer.next_token().is_err());
    }

    #[test]
    fn test_string_escapes() {
        let mut lexer = Lexer::new(r#""\t\r\"\\""#);
        let (token, _) = lexer.next_token().unwrap();
        assert_eq!(token, Token::String("\t\r\"\\".to_string()));
    }

    #[test]
    fn test_invalid_string_escape() {
        let mut lexer = Lexer::new(r#""\q""#);
        assert!(lexer.next_token().is_err());
    }

    #[test]
    fn test_string_unterminated_after_backslash() {
        let mut lexer = Lexer::new(r#""hello\"#);
        assert!(lexer.next_token().is_err());
    }

    #[test]
    fn test_string_invalid_unicode_hex() {
        let mut lexer = Lexer::new(r#""\uZZZZ""#);
        assert!(lexer.next_token().is_err());
    }

    #[test]
    fn test_string_unicode_unterminated() {
        let mut lexer = Lexer::new(r#""\u00"#);
        assert!(lexer.next_token().is_err());
    }

    #[test]
    fn test_char_backspace() {
        let mut lexer = Lexer::new(r"\backspace");
        assert_eq!(lexer.next_token().unwrap().0, Token::Char('\x08'));
    }

    #[test]
    fn test_char_formfeed() {
        let mut lexer = Lexer::new(r"\formfeed");
        assert_eq!(lexer.next_token().unwrap().0, Token::Char('\x0C'));
    }

    #[test]
    fn test_char_return() {
        let mut lexer = Lexer::new(r"\return");
        assert_eq!(lexer.next_token().unwrap().0, Token::Char('\r'));
    }

    #[test]
    fn test_char_invalid_unicode_hex() {
        let mut lexer = Lexer::new(r"\uZZZZ");
        assert!(lexer.next_token().is_err());
    }

    #[test]
    fn test_char_multi_char_fallback() {
        let mut lexer = Lexer::new(r"\abc)");
        let (token, _) = lexer.next_token().unwrap();
        assert_eq!(token, Token::Char('a'));
    }

    #[test]
    fn test_char_special_char() {
        let mut lexer = Lexer::new(r"\!");
        assert_eq!(lexer.next_token().unwrap().0, Token::Char('!'));
    }

    #[test]
    fn test_char_empty_error() {
        let mut lexer = Lexer::new(r"\");
        assert!(lexer.next_token().is_err());
    }

    #[test]
    fn test_unexpected_char() {
        let mut lexer = Lexer::new("§");
        assert!(lexer.next_token().is_err());
    }

    #[test]
    fn test_hash_unexpected() {
        let mut lexer = Lexer::new("#)");
        assert!(lexer.next_token().is_err());
    }

    #[test]
    fn test_regex_with_escape() {
        let mut lexer = Lexer::new(r#"#"a\nb""#);
        let (token, _) = lexer.next_token().unwrap();
        assert_eq!(token, Token::Regex(r"a\nb".to_string()));
    }

    #[test]
    fn test_regex_unterminated_after_escape() {
        let mut lexer = Lexer::new(r#"#"test\"#);
        assert!(lexer.next_token().is_err());
    }

    #[test]
    fn test_plus_sign_symbol() {
        let mut lexer = Lexer::new("+ foo");
        assert_eq!(lexer.next_token().unwrap().0, Token::Symbol("+"));
    }

    #[test]
    fn test_hash_at_eof() {
        let mut lexer = Lexer::new("#");
        assert!(matches!(lexer.next_token().unwrap().0, Token::Hash));
    }

    #[test]
    fn test_empty_keyword() {
        let mut lexer = Lexer::new(":");
        assert!(lexer.next_token().is_err());
    }
}
