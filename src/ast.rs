use bumpalo::collections::Vec as BumpVec;
use bumpalo::Bump;

#[derive(Debug, Clone, PartialEq)]
pub enum Form<'a> {
    Nil,
    Bool(bool),
    Int(i64),
    BigInt(&'a str),
    Float(f64),
    BigDecimal(&'a str),
    Ratio {
        numer: i64,
        denom: i64,
    },
    Char(char),
    String(StringValue<'a>),
    Regex(StringValue<'a>),
    Symbol {
        ns: Option<&'a str>,
        name: &'a str,
    },
    Keyword {
        ns: Option<&'a str>,
        name: &'a str,
        auto_resolve: bool,
    },
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
    Meta {
        meta: &'a Form<'a>,
        form: &'a Form<'a>,
    },
    AnonFn(BumpVec<'a, Form<'a>>),
    Tagged {
        tag: &'a str,
        form: &'a Form<'a>,
    },
    ReaderCond {
        splicing: bool,
        branches: BumpVec<'a, (Form<'a>, Form<'a>)>,
    },
    SymbolicVal(SymbolicVal),
}

#[derive(Debug, Clone, PartialEq)]
pub enum StringValue<'a> {
    Borrowed(&'a str),
    Owned(bumpalo::collections::String<'a>),
}

impl<'a> StringValue<'a> {
    pub fn as_str(&self) -> &str {
        match self {
            StringValue::Borrowed(s) => s,
            StringValue::Owned(s) => s.as_str(),
        }
    }

    pub fn new_borrowed(s: &'a str) -> Self {
        StringValue::Borrowed(s)
    }

    pub fn new_owned(bump: &'a Bump, s: &str) -> Self {
        let mut owned = bumpalo::collections::String::new_in(bump);
        owned.push_str(s);
        StringValue::Owned(owned)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SymbolicVal {
    Inf,
    NegInf,
    NaN,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Platform {
    Clj,
    Cljs,
    Cljr,
    Default,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ReadCondBehavior {
    Allow,
    Preserve,
    Error,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ParseOpts {
    pub platform: Platform,
    pub read_cond: ReadCondBehavior,
}

