# Architecture

This document describes the internal architecture of clj-parser.

## Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                         clj-parser                              │
├─────────────────────────────────────────────────────────────────┤
│  Source Text (&str)                                             │
│       │                                                         │
│       ▼                                                         │
│  ┌─────────────┐                                                │
│  │   Lexer     │  Token stream with spans                       │
│  └─────────────┘                                                │
│       │                                                         │
│       ▼                                                         │
│  ┌─────────────┐                                                │
│  │   Parser    │  Arena-allocated AST                           │
│  └─────────────┘                                                │
│       │                                                         │
│       ▼                                                         │
│  Vec<Spanned<Form<'a>>>                                         │
│       │                                                         │
│       ├──────────────────┬──────────────────┐                   │
│       ▼                  ▼                  ▼                   │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐              │
│  │  Expander   │  │   Printer   │  │  Analysis   │              │
│  │  (macros)   │  │  (codegen)  │  │  (user)     │              │
│  └─────────────┘  └─────────────┘  └─────────────┘              │
└─────────────────────────────────────────────────────────────────┘
```

## Modules

### `lexer.rs` (818 lines)

The lexer converts source text into a stream of tokens with source spans.

**Key design decisions:**

1. **SIMD acceleration**: Uses `memchr` crate for fast scanning of string delimiters and special characters. This provides 2-4x speedup for string-heavy code.

2. **Zero-copy tokens**: String and symbol tokens store byte offsets into the original source, avoiding allocation.

3. **Single-pass**: The lexer operates in a single pass with no backtracking.

**Token types:**
- Delimiters: `(`, `)`, `[`, `]`, `{`, `}`
- Literals: strings, numbers, characters, regex
- Identifiers: symbols, keywords
- Dispatch: `#`, `'`, `` ` ``, `~`, `@`, `^`

### `parser.rs` (1625 lines)

Recursive descent parser that builds an AST from the token stream.

**Key design decisions:**

1. **Arena allocation**: All AST nodes are allocated in a `bumpalo` arena. This provides:
   - Cache-friendly memory layout
   - O(1) deallocation (drop the arena)
   - Zero individual allocations during parsing

2. **Zero-copy strings**: String values use `StringValue<'a>` which is either:
   - `Borrowed(&'a str)` - references source directly (no escapes)
   - `Owned(bumpalo::String<'a>)` - arena-allocated (has escapes)

3. **Reader conditionals**: Supports `:clj`, `:cljs`, `:cljr`, `:default` platforms with three behaviors:
   - `Allow` - evaluate and return matching branch
   - `Preserve` - keep as AST node
   - `Error` - reject reader conditionals

**Parsing strategy:**

```rust
fn parse_form(&mut self) -> Result<Form<'a>> {
    match self.peek()? {
        Token::LParen => self.parse_list(),
        Token::LBracket => self.parse_vector(),
        Token::LBrace => self.parse_map(),
        Token::Quote => self.parse_quote(),
        // ...
    }
}
```

### `expander.rs` (1183 lines)

Expands Clojure's syntax-quote (`` ` ``), unquote (`~`), and unquote-splice (`~@`) into explicit forms.

**Transformation rules:**

| Input | Output |
|-------|--------|
| `` `foo `` | `(quote foo)` |
| `` `(a b) `` | `(seq (concat (list (quote a)) (list (quote b))))` |
| `` `(a ~b) `` | `(seq (concat (list (quote a)) (list b)))` |
| `` `(a ~@b) `` | `(seq (concat (list (quote a)) b))` |
| `` `[a b] `` | `(apply vector (concat (list (quote a)) (list (quote b))))` |
| `` `{a b} `` | `(apply hash-map (concat (list (quote a)) (list (quote b))))` |
| `foo#` | `foo__N__auto__` (gensym) |

**Gensym handling:**

Symbols ending in `#` are auto-gensyms. Within a single syntax-quote, the same symbol always resolves to the same gensym:

```clojure
`(let [x# 1] x#)
;; => (seq (concat (list (quote let))
;;                 (list (apply vector (concat (list (quote x__0__auto__))
;;                                             (list (quote 1)))))
;;                 (list (quote x__0__auto__))))
```

### `printer.rs` (220 lines)

Converts AST back to Clojure source code.

**Key features:**
- Proper escaping for strings and characters
- Preserves reader macro syntax (`'`, `` ` ``, `~`, `~@`, `@`, `#'`)
- Handles all Form variants

```rust
pub fn print_form(form: &Form<'_>) -> String {
    match form {
        Form::Nil => "nil".to_string(),
        Form::List(items) => format!("({})", items.iter().map(print_form).join(" ")),
        // ...
    }
}

pub fn print_forms<'a>(forms: impl Iterator<Item = &'a Form<'a>>) -> String {
    forms.map(print_form).collect::<Vec<_>>().join("\n")
}
```

### `ast.rs` (106 lines)

Defines the core AST types.

**Form enum:**

```rust
pub enum Form<'a> {
    // Literals
    Nil,
    Bool(bool),
    Int(i64),
    BigInt(&'a str),
    Float(f64),
    BigDecimal(&'a str),
    Ratio { numer: i64, denom: i64 },
    Char(char),
    String(StringValue<'a>),
    Regex(StringValue<'a>),

    // Identifiers
    Symbol { ns: Option<&'a str>, name: &'a str },
    Keyword { ns: Option<&'a str>, name: &'a str, auto_resolve: bool },

    // Collections
    List(BumpVec<'a, Form<'a>>),
    Vector(BumpVec<'a, Form<'a>>),
    Map(BumpVec<'a, (Form<'a>, Form<'a>)>),
    Set(BumpVec<'a, Form<'a>>),

    // Reader macros
    Quote(&'a Form<'a>),
    SyntaxQuote(&'a Form<'a>),
    Unquote(&'a Form<'a>),
    UnquoteSplice(&'a Form<'a>),
    Deref(&'a Form<'a>),
    Var(&'a Form<'a>),

    // Other
    Meta { meta: &'a Form<'a>, form: &'a Form<'a> },
    AnonFn(BumpVec<'a, Form<'a>>),
    Tagged { tag: &'a str, form: &'a Form<'a> },
    ReaderCond { splicing: bool, branches: BumpVec<'a, (Form<'a>, Form<'a>)> },
    SymbolicVal(SymbolicVal),
}
```

### `span.rs` (106 lines)

Source location tracking for error messages.

```rust
pub struct Span {
    pub start: usize,  // byte offset
    pub end: usize,    // byte offset
}

pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}
```

### `error.rs` (152 lines)

Error types for parsing and expansion.

```rust
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub span: Span,
}

pub enum ParseErrorKind {
    UnexpectedToken(String),
    UnterminatedString,
    UnterminatedList,
    // ...
}

pub struct ExpandError {
    pub kind: ExpandErrorKind,
}

pub enum ExpandErrorKind {
    UnquoteOutsideSyntaxQuote,
    UnquoteSpliceNotInList,
}
```

## Memory Model

The parser uses arena allocation for all AST nodes:

```rust
let bump = Bump::new();          // Create arena
let forms = parse(src, &bump)?;  // Parse into arena
// Use forms...
drop(bump);                      // Free all memory at once
```

**Lifetime relationships:**

```
bump: &'a Bump
  │
  └── Form<'a>
        │
        ├── BumpVec<'a, Form<'a>>   (collections)
        ├── &'a Form<'a>            (wrapper forms)
        └── &'a str                 (strings, symbols)
```

## Testing Strategy

1. **Unit tests**: Test individual module behavior
   - Error paths in lexer/parser
   - Gensym generation in expander
   - Reader conditional handling

2. **Roundtrip tests**: `parse(input) -> print() == expected`
   - Verifies parser and printer are inverses
   - Covers all Form variants

3. **CLI tests**: Integration tests for `clj-expand` binary
   - stdin/stdout handling
   - File I/O
   - Error messages

4. **Real-world validation**: Test against actual Clojure codebases
   - clojure/clojure (141 files)
   - clojure/core.async (50 files)
