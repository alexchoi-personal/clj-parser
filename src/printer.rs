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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::StringValue;
    use bumpalo::collections::Vec as BumpVec;
    use bumpalo::Bump;

    fn print(form: &Form) -> String {
        let mut buf = String::new();
        print_form(form, &mut buf);
        buf
    }

    #[test]
    fn test_nil() {
        assert_eq!(print(&Form::Nil), "nil");
    }

    #[test]
    fn test_bool() {
        assert_eq!(print(&Form::Bool(true)), "true");
        assert_eq!(print(&Form::Bool(false)), "false");
    }

    #[test]
    fn test_int() {
        assert_eq!(print(&Form::Int(42)), "42");
        assert_eq!(print(&Form::Int(-123)), "-123");
        assert_eq!(print(&Form::Int(0)), "0");
    }

    #[test]
    fn test_bigint() {
        assert_eq!(print(&Form::BigInt("12345678901234567890")), "12345678901234567890N");
    }

    #[test]
    fn test_float() {
        assert_eq!(print(&Form::Float(3.14)), "3.14");
        assert_eq!(print(&Form::Float(1.0)), "1.0");
        assert_eq!(print(&Form::Float(-2.5)), "-2.5");
        assert_eq!(print(&Form::Float(f64::INFINITY)), "##Inf");
        assert_eq!(print(&Form::Float(f64::NEG_INFINITY)), "##-Inf");
        assert_eq!(print(&Form::Float(f64::NAN)), "##NaN");
    }

    #[test]
    fn test_bigdecimal() {
        assert_eq!(print(&Form::BigDecimal("3.14159265358979")), "3.14159265358979M");
    }

    #[test]
    fn test_ratio() {
        assert_eq!(print(&Form::Ratio { numer: 1, denom: 2 }), "1/2");
        assert_eq!(print(&Form::Ratio { numer: -3, denom: 4 }), "-3/4");
    }

    #[test]
    fn test_char() {
        assert_eq!(print(&Form::Char('a')), "\\a");
        assert_eq!(print(&Form::Char('\n')), "\\newline");
        assert_eq!(print(&Form::Char(' ')), "\\space");
        assert_eq!(print(&Form::Char('\t')), "\\tab");
        assert_eq!(print(&Form::Char('\r')), "\\return");
        assert_eq!(print(&Form::Char('\\')), "\\\\");
    }

    #[test]
    fn test_string() {
        assert_eq!(
            print(&Form::String(StringValue::Borrowed("hello"))),
            "\"hello\""
        );
        assert_eq!(
            print(&Form::String(StringValue::Borrowed("hello\nworld"))),
            "\"hello\\nworld\""
        );
        assert_eq!(
            print(&Form::String(StringValue::Borrowed("tab\there"))),
            "\"tab\\there\""
        );
        assert_eq!(
            print(&Form::String(StringValue::Borrowed("quote\"here"))),
            "\"quote\\\"here\""
        );
        assert_eq!(
            print(&Form::String(StringValue::Borrowed("back\\slash"))),
            "\"back\\\\slash\""
        );
    }

    #[test]
    fn test_regex() {
        assert_eq!(
            print(&Form::Regex(StringValue::Borrowed("\\d+"))),
            "#\"\\d+\""
        );
    }

    #[test]
    fn test_symbol() {
        assert_eq!(
            print(&Form::Symbol {
                ns: None,
                name: "foo"
            }),
            "foo"
        );
        assert_eq!(
            print(&Form::Symbol {
                ns: Some("my.ns"),
                name: "bar"
            }),
            "my.ns/bar"
        );
    }

    #[test]
    fn test_keyword() {
        assert_eq!(
            print(&Form::Keyword {
                ns: None,
                name: "foo",
                auto_resolve: false
            }),
            ":foo"
        );
        assert_eq!(
            print(&Form::Keyword {
                ns: Some("my.ns"),
                name: "bar",
                auto_resolve: false
            }),
            ":my.ns/bar"
        );
        assert_eq!(
            print(&Form::Keyword {
                ns: None,
                name: "baz",
                auto_resolve: true
            }),
            "::baz"
        );
        assert_eq!(
            print(&Form::Keyword {
                ns: Some("alias"),
                name: "qux",
                auto_resolve: true
            }),
            "::alias/qux"
        );
    }

    #[test]
    fn test_list() {
        let bump = Bump::new();
        let mut items = BumpVec::new_in(&bump);
        items.push(Form::Int(1));
        items.push(Form::Int(2));
        items.push(Form::Int(3));
        assert_eq!(print(&Form::List(items)), "(1 2 3)");
    }

    #[test]
    fn test_empty_list() {
        let bump = Bump::new();
        let items = BumpVec::new_in(&bump);
        assert_eq!(print(&Form::List(items)), "()");
    }

    #[test]
    fn test_vector() {
        let bump = Bump::new();
        let mut items = BumpVec::new_in(&bump);
        items.push(Form::Symbol { ns: None, name: "a" });
        items.push(Form::Symbol { ns: None, name: "b" });
        assert_eq!(print(&Form::Vector(items)), "[a b]");
    }

    #[test]
    fn test_map() {
        let bump = Bump::new();
        let mut pairs = BumpVec::new_in(&bump);
        pairs.push((
            Form::Keyword {
                ns: None,
                name: "a",
                auto_resolve: false,
            },
            Form::Int(1),
        ));
        pairs.push((
            Form::Keyword {
                ns: None,
                name: "b",
                auto_resolve: false,
            },
            Form::Int(2),
        ));
        assert_eq!(print(&Form::Map(pairs)), "{:a 1 :b 2}");
    }

    #[test]
    fn test_empty_map() {
        let bump = Bump::new();
        let pairs = BumpVec::new_in(&bump);
        assert_eq!(print(&Form::Map(pairs)), "{}");
    }

    #[test]
    fn test_set() {
        let bump = Bump::new();
        let mut items = BumpVec::new_in(&bump);
        items.push(Form::Int(1));
        items.push(Form::Int(2));
        items.push(Form::Int(3));
        assert_eq!(print(&Form::Set(items)), "#{1 2 3}");
    }

    #[test]
    fn test_quote() {
        let bump = Bump::new();
        let inner = bump.alloc(Form::Symbol { ns: None, name: "x" });
        assert_eq!(print(&Form::Quote(inner)), "'x");
    }

    #[test]
    fn test_syntax_quote() {
        let bump = Bump::new();
        let inner = bump.alloc(Form::Symbol { ns: None, name: "x" });
        assert_eq!(print(&Form::SyntaxQuote(inner)), "`x");
    }

    #[test]
    fn test_unquote() {
        let bump = Bump::new();
        let inner = bump.alloc(Form::Symbol { ns: None, name: "x" });
        assert_eq!(print(&Form::Unquote(inner)), "~x");
    }

    #[test]
    fn test_unquote_splice() {
        let bump = Bump::new();
        let inner = bump.alloc(Form::Symbol { ns: None, name: "xs" });
        assert_eq!(print(&Form::UnquoteSplice(inner)), "~@xs");
    }

    #[test]
    fn test_deref() {
        let bump = Bump::new();
        let inner = bump.alloc(Form::Symbol { ns: None, name: "atom" });
        assert_eq!(print(&Form::Deref(inner)), "@atom");
    }

    #[test]
    fn test_var() {
        let bump = Bump::new();
        let inner = bump.alloc(Form::Symbol {
            ns: Some("clojure.core"),
            name: "inc",
        });
        assert_eq!(print(&Form::Var(inner)), "#'clojure.core/inc");
    }

    #[test]
    fn test_meta() {
        let bump = Bump::new();
        let meta = bump.alloc(Form::Keyword {
            ns: None,
            name: "private",
            auto_resolve: false,
        });
        let form = bump.alloc(Form::Symbol { ns: None, name: "x" });
        assert_eq!(print(&Form::Meta { meta, form }), "^:private x");
    }

    #[test]
    fn test_anon_fn() {
        let bump = Bump::new();
        let mut items = BumpVec::new_in(&bump);
        items.push(Form::Symbol {
            ns: None,
            name: "inc",
        });
        items.push(Form::Symbol { ns: None, name: "%" });
        assert_eq!(print(&Form::AnonFn(items)), "#(inc %)");
    }

    #[test]
    fn test_tagged() {
        let bump = Bump::new();
        let form = bump.alloc(Form::String(StringValue::Borrowed("2023-01-01")));
        assert_eq!(
            print(&Form::Tagged {
                tag: "inst",
                form
            }),
            "#inst \"2023-01-01\""
        );
    }

    #[test]
    fn test_reader_cond() {
        let bump = Bump::new();
        let mut branches = BumpVec::new_in(&bump);
        branches.push((
            Form::Keyword {
                ns: None,
                name: "clj",
                auto_resolve: false,
            },
            Form::Int(1),
        ));
        branches.push((
            Form::Keyword {
                ns: None,
                name: "cljs",
                auto_resolve: false,
            },
            Form::Int(2),
        ));
        assert_eq!(
            print(&Form::ReaderCond {
                splicing: false,
                branches
            }),
            "#?(:clj 1 :cljs 2)"
        );
    }

    #[test]
    fn test_reader_cond_splicing() {
        let bump = Bump::new();
        let mut branches = BumpVec::new_in(&bump);
        branches.push((
            Form::Keyword {
                ns: None,
                name: "clj",
                auto_resolve: false,
            },
            Form::Int(1),
        ));
        assert_eq!(
            print(&Form::ReaderCond {
                splicing: true,
                branches
            }),
            "#?@(:clj 1)"
        );
    }

    #[test]
    fn test_symbolic_val() {
        assert_eq!(print(&Form::SymbolicVal(SymbolicVal::Inf)), "##Inf");
        assert_eq!(print(&Form::SymbolicVal(SymbolicVal::NegInf)), "##-Inf");
        assert_eq!(print(&Form::SymbolicVal(SymbolicVal::NaN)), "##NaN");
    }

    #[test]
    fn test_print_forms() {
        let forms = vec![Form::Int(1), Form::Int(2), Form::Int(3)];
        assert_eq!(print_forms(forms.iter()), "1\n2\n3");
    }

    #[test]
    fn test_print_forms_empty() {
        let forms: Vec<Form> = vec![];
        assert_eq!(print_forms(forms.iter()), "");
    }

    #[test]
    fn test_nested_structure() {
        let bump = Bump::new();
        let mut inner_list = BumpVec::new_in(&bump);
        inner_list.push(Form::Symbol {
            ns: None,
            name: "+",
        });
        inner_list.push(Form::Int(1));
        inner_list.push(Form::Int(2));

        let mut outer_vec = BumpVec::new_in(&bump);
        outer_vec.push(Form::Keyword {
            ns: None,
            name: "result",
            auto_resolve: false,
        });
        outer_vec.push(Form::List(inner_list));

        assert_eq!(print(&Form::Vector(outer_vec)), "[:result (+ 1 2)]");
    }

    #[test]
    fn test_string_owned() {
        let bump = Bump::new();
        let owned = StringValue::new_owned(&bump, "owned string");
        assert_eq!(print(&Form::String(owned)), "\"owned string\"");
    }

    #[test]
    fn test_complex_nested() {
        let bump = Bump::new();
        let mut map_pairs = BumpVec::new_in(&bump);
        map_pairs.push((
            Form::Keyword {
                ns: None,
                name: "name",
                auto_resolve: false,
            },
            Form::String(StringValue::Borrowed("Alice")),
        ));
        map_pairs.push((
            Form::Keyword {
                ns: None,
                name: "age",
                auto_resolve: false,
            },
            Form::Int(30),
        ));

        let mut set_items = BumpVec::new_in(&bump);
        set_items.push(Form::Keyword {
            ns: None,
            name: "admin",
            auto_resolve: false,
        });
        set_items.push(Form::Keyword {
            ns: None,
            name: "user",
            auto_resolve: false,
        });

        map_pairs.push((
            Form::Keyword {
                ns: None,
                name: "roles",
                auto_resolve: false,
            },
            Form::Set(set_items),
        ));

        assert_eq!(
            print(&Form::Map(map_pairs)),
            "{:name \"Alice\" :age 30 :roles #{:admin :user}}"
        );
    }
}
