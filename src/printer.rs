use crate::ast::{Form, SymbolicVal};

pub fn print_form(form: &Form, buf: &mut String) {
    match form {
        Form::Nil => buf.push_str("nil"),
        Form::Bool(b) => buf.push_str(if *b { "true" } else { "false" }),
        Form::Int(n) => buf.push_str(&n.to_string()),
        Form::BigInt(s) => {
            buf.push_str(s);
            buf.push('N');
        }
        Form::Float(f) => {
            if f.is_nan() {
                buf.push_str("##NaN");
            } else if f.is_infinite() {
                if f.is_sign_positive() {
                    buf.push_str("##Inf");
                } else {
                    buf.push_str("##-Inf");
                }
            } else {
                let s = f.to_string();
                buf.push_str(&s);
                if !s.contains('.') && !s.contains('e') && !s.contains('E') {
                    buf.push_str(".0");
                }
            }
        }
        Form::BigDecimal(s) => {
            buf.push_str(s);
            buf.push('M');
        }
        Form::Ratio { numer, denom } => {
            buf.push_str(&numer.to_string());
            buf.push('/');
            buf.push_str(&denom.to_string());
        }
        Form::Char(c) => {
            buf.push('\\');
            match c {
                '\n' => buf.push_str("newline"),
                ' ' => buf.push_str("space"),
                '\t' => buf.push_str("tab"),
                '\r' => buf.push_str("return"),
                '\\' => buf.push('\\'),
                _ => buf.push(*c),
            }
        }
        Form::String(sv) => {
            buf.push('"');
            escape_string(sv.as_str(), buf);
            buf.push('"');
        }
        Form::Regex(sv) => {
            buf.push_str("#\"");
            buf.push_str(sv.as_str());
            buf.push('"');
        }
        Form::Symbol { ns, name } => {
            if let Some(ns) = ns {
                buf.push_str(ns);
                buf.push('/');
            }
            buf.push_str(name);
        }
        Form::Keyword {
            ns,
            name,
            auto_resolve,
        } => {
            if *auto_resolve {
                buf.push_str("::");
                if let Some(ns) = ns {
                    buf.push_str(ns);
                    buf.push('/');
                }
            } else {
                buf.push(':');
                if let Some(ns) = ns {
                    buf.push_str(ns);
                    buf.push('/');
                }
            }
            buf.push_str(name);
        }
        Form::List(items) => {
            buf.push('(');
            print_items(items.iter(), buf);
            buf.push(')');
        }
        Form::Vector(items) => {
            buf.push('[');
            print_items(items.iter(), buf);
            buf.push(']');
        }
        Form::Map(pairs) => {
            buf.push('{');
            let mut first = true;
            for (k, v) in pairs.iter() {
                if !first {
                    buf.push(' ');
                }
                first = false;
                print_form(k, buf);
                buf.push(' ');
                print_form(v, buf);
            }
            buf.push('}');
        }
        Form::Set(items) => {
            buf.push_str("#{");
            print_items(items.iter(), buf);
            buf.push('}');
        }
        Form::Quote(inner) => {
            buf.push('\'');
            print_form(inner, buf);
        }
        Form::SyntaxQuote(inner) => {
            buf.push('`');
            print_form(inner, buf);
        }
        Form::Unquote(inner) => {
            buf.push('~');
            print_form(inner, buf);
        }
        Form::UnquoteSplice(inner) => {
            buf.push_str("~@");
            print_form(inner, buf);
        }
        Form::Deref(inner) => {
            buf.push('@');
            print_form(inner, buf);
        }
        Form::Var(inner) => {
            buf.push_str("#'");
            print_form(inner, buf);
        }
        Form::Meta { meta, form } => {
            buf.push('^');
            print_form(meta, buf);
            buf.push(' ');
            print_form(form, buf);
        }
        Form::AnonFn(items) => {
            buf.push_str("#(");
            print_items(items.iter(), buf);
            buf.push(')');
        }
        Form::Tagged { tag, form } => {
            buf.push('#');
            buf.push_str(tag);
            buf.push(' ');
            print_form(form, buf);
        }
        Form::ReaderCond { splicing, branches } => {
            if *splicing {
                buf.push_str("#?@(");
            } else {
                buf.push_str("#?(");
            }
            let mut first = true;
            for (k, v) in branches.iter() {
                if !first {
                    buf.push(' ');
                }
                first = false;
                print_form(k, buf);
                buf.push(' ');
                print_form(v, buf);
            }
            buf.push(')');
        }
        Form::SymbolicVal(sv) => match sv {
            SymbolicVal::Inf => buf.push_str("##Inf"),
            SymbolicVal::NegInf => buf.push_str("##-Inf"),
            SymbolicVal::NaN => buf.push_str("##NaN"),
        },
    }
}

fn print_items<'a, I>(items: I, buf: &mut String)
where
    I: Iterator<Item = &'a Form<'a>>,
{
    let mut first = true;
    for item in items {
        if !first {
            buf.push(' ');
        }
        first = false;
        print_form(item, buf);
    }
}

fn escape_string(s: &str, buf: &mut String) {
    for c in s.chars() {
        match c {
            '\n' => buf.push_str("\\n"),
            '\t' => buf.push_str("\\t"),
            '\r' => buf.push_str("\\r"),
            '\\' => buf.push_str("\\\\"),
            '"' => buf.push_str("\\\""),
            _ => buf.push(c),
        }
    }
}

pub fn print_forms<'a>(forms: impl IntoIterator<Item = &'a Form<'a>>) -> String {
    let mut buf = String::new();
    let mut first = true;
    for form in forms {
        if !first {
            buf.push('\n');
        }
        first = false;
        print_form(form, &mut buf);
    }
    buf
}
