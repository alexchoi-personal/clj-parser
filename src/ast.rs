#[derive(Debug, Clone, PartialEq)]
pub enum Form {
    Nil,
    Bool(bool),
    Int(i64),
    BigInt(String),
    Float(f64),
    BigDecimal(String),
    Ratio {
        numer: i64,
        denom: i64,
    },
    Char(char),
    String(String),
    Regex(String),
    Symbol {
        ns: Option<String>,
        name: String,
    },
    Keyword {
        ns: Option<String>,
        name: String,
        auto_resolve: bool,
    },
    List(Vec<Form>),
    Vector(Vec<Form>),
    Map(Vec<(Form, Form)>),
    Set(Vec<Form>),
    Quote(Box<Form>),
    SyntaxQuote(Box<Form>),
    Unquote(Box<Form>),
    UnquoteSplice(Box<Form>),
    Deref(Box<Form>),
    Var(Box<Form>),
    Meta {
        meta: Box<Form>,
        form: Box<Form>,
    },
    AnonFn(Vec<Form>),
    Tagged {
        tag: String,
        form: Box<Form>,
    },
    ReaderCond {
        splicing: bool,
        branches: Vec<(Form, Form)>,
    },
    SymbolicVal(SymbolicVal),
}

#[derive(Debug, Clone, PartialEq)]
pub enum SymbolicVal {
    Inf,
    NegInf,
    NaN,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Platform {
    Clj,
    Cljs,
    Cljr,
    Default,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ReadCondBehavior {
    Allow,
    Preserve,
    Error,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParseOpts {
    pub platform: Platform,
    pub read_cond: ReadCondBehavior,
}
