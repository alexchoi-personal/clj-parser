# clj-parser

A high-performance Clojure/ClojureScript parser written in Rust.

## Features

- **Zero-copy parsing** - String values reference the original source without allocation
- **Arena allocation** - All AST nodes allocated in a bump allocator for cache-friendly access
- **SIMD-accelerated lexing** - Uses `memchr` for fast string and delimiter scanning
- **Full Clojure syntax support** - Lists, vectors, maps, sets, reader macros, metadata, reader conditionals
- **Syntax-quote expansion** - Expand `` ` ~ ~@ `` into explicit `list/concat/quote` forms
- **CLI tool** - `clj-expand` binary for preprocessing Clojure files
- **Async parallel parsing** - Optional tokio-based concurrent file parsing

## Installation

```toml
[dependencies]
clojure-parser = "0.1.0"
bumpalo = "3"
```

## Usage

```rust
use clojure_parser::{parse, Bump};

fn main() {
    let source = r#"
        (defn hello [name]
          (println "Hello," name))
    "#;

    let bump = Bump::new();
    let forms = parse(source, &bump).unwrap();

    println!("Parsed {} forms", forms.len());
}
```

### With Platform-Specific Reader Conditionals

```rust
use clojure_parser::{parse_with_opts, ParseOpts, Platform, ReadCondBehavior, Bump};

fn main() {
    let source = "#?(:clj (java-fn) :cljs (js-fn))";

    let bump = Bump::new();
    let opts = ParseOpts {
        platform: Platform::Clj,
        read_cond: ReadCondBehavior::Allow,
    };

    let forms = parse_with_opts(source, opts, &bump).unwrap();
}
```

### Syntax-Quote Expansion

Expand Clojure's syntax-quote (`` ` ``), unquote (`~`), and unquote-splice (`~@`) into explicit forms:

```rust
use clojure_parser::{parse, Bump, Expander, print_forms};

fn main() {
    let source = "`(defn ~name [~@args] body)";

    let bump = Bump::new();
    let forms = parse(source, &bump).unwrap();

    let mut expander = Expander::new(&bump);
    let expanded: Vec<_> = forms.iter()
        .map(|f| expander.expand(&f.value).unwrap())
        .collect();

    println!("{}", print_forms(expanded.iter()));
    // Output: (seq (concat (list (quote defn)) (list name) (list (apply vector (concat args))) (list (quote body))))
}
```

**Transformation rules:**
- `` `foo `` → `(quote foo)`
- `` `(a ~b ~@c) `` → `(seq (concat (list (quote a)) (list b) c))`
- `` `[a ~b] `` → `(apply vector (concat (list (quote a)) (list b)))`
- `foo#` → `foo__N__auto__` (gensym)

### CLI Tool

The `clj-expand` binary expands syntax-quote forms in Clojure files:

```bash
# Install
cargo install --path .

# Usage
clj-expand input.clj                 # Read file, print to stdout
clj-expand input.clj -o output.clj   # Read file, write to file
cat input.clj | clj-expand           # Read stdin, print to stdout
```

### Async Parallel Parsing

```rust
use clojure_parser::{parse, Bump};
use futures::stream::{self, StreamExt};
use tokio::fs;

async fn parse_files(paths: Vec<PathBuf>) {
    let results: Vec<_> = stream::iter(paths)
        .map(|path| async move {
            let content = fs::read_to_string(&path).await?;
            let bump = Bump::new();
            parse(&content, &bump)
        })
        .buffer_unordered(num_cpus::get())
        .collect()
        .await;
}
```

## Supported Syntax

| Category | Forms |
|----------|-------|
| **Literals** | `nil`, `true`, `false`, integers, floats, ratios, BigInt (`N`), BigDecimal (`M`), chars, strings |
| **Numbers** | Decimal, hex (`0x`), octal (`0`), binary (`2r`), arbitrary radix (`36r`), scientific (`1e10`) |
| **Symbols** | `foo`, `ns/foo`, `.method`, `Class.` |
| **Keywords** | `:foo`, `:ns/foo`, `::auto`, `::alias/foo` |
| **Collections** | `()`, `[]`, `{}`, `#{}` |
| **Quote** | `'x`, `` `x ``, `~x`, `~@x` |
| **Dispatch** | `#"regex"`, `#'var`, `#()`, `#_`, `#{}`, `#tag` |
| **Metadata** | `^:key`, `^{:a 1}`, `^Type` |
| **Reader Conditionals** | `#?(:clj x :cljs y)`, `#?@(...)` |
| **Symbolic Values** | `##Inf`, `##-Inf`, `##NaN` |

## Performance

Benchmarked on parsing Clojure source files:

```
Throughput: ~125 MB/s
```

| Benchmark | Time |
|-----------|------|
| Simple forms | 1.2 µs |
| Complex forms (ns, defn, macros) | 4.8 µs |
| Nested structures | 1.5 µs |
| 1000 symbols | 45 µs |
| 1000 symbols (reused bump) | 28 µs |

## Architecture

```
Source Text (&str)
       │
       ▼
┌─────────────┐
│   Lexer     │  SIMD-accelerated tokenization
│  (memchr)   │  Produces Token + Span
└─────────────┘
       │
       ▼
┌─────────────┐
│   Parser    │  Recursive descent
│  (bumpalo)  │  Arena-allocated AST
└─────────────┘
       │
       ▼
Vec<Spanned<Form>>  Zero-copy AST with source spans
```

## AST Types

```rust
pub enum Form<'a> {
    Nil,
    Bool(bool),
    Int(i64),
    BigInt(&'a str),
    Float(f64),
    BigDecimal(&'a str),
    Ratio(i64, i64),
    Char(char),
    String(StringValue<'a>),
    Regex(&'a str),
    Symbol { ns: Option<&'a str>, name: &'a str },
    Keyword { ns: Option<&'a str>, name: &'a str, auto_resolve: bool },
    List(BumpVec<'a, Form<'a>>),
    Vector(BumpVec<'a, Form<'a>>),
    Map(BumpVec<'a, (Form<'a>, Form<'a>)>),
    Set(BumpVec<'a, Form<'a>>),
    Quote(&'a Form<'a>),
    SyntaxQuote(&'a Form<'a>),
    Unquote(&'a Form<'a>),
    UnquoteSplice(&'a Form<'a>),
    Deref(&'a Form<'a>),
    Var(&'a Form<'a>),
    Meta { meta: &'a Form<'a>, form: &'a Form<'a> },
    AnonFn(BumpVec<'a, Form<'a>>),
    Tagged { tag: &'a str, form: &'a Form<'a> },
    ReaderCond { splicing: bool, branches: BumpVec<'a, (&'a str, Form<'a>)> },
    SymbolicVal(SymbolicVal),
}
```

## License

Licensed under either of:

- Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
- MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

Copyright (c) 2025 Alex Choi
