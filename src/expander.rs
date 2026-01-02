use std::collections::HashMap;

use bumpalo::collections::Vec as BumpVec;
use bumpalo::Bump;

use crate::ast::Form;
use crate::error::{ExpandError, ExpandErrorKind};

pub struct Expander<'a> {
    bump: &'a Bump,
    gensym_counter: usize,
    gensym_map: HashMap<&'a str, &'a str>,
}

impl<'a> Expander<'a> {
    pub fn new(bump: &'a Bump) -> Self {
        Self {
            bump,
            gensym_counter: 0,
            gensym_map: HashMap::new(),
        }
    }

    pub fn expand(&mut self, form: &Form<'a>) -> Result<Form<'a>, ExpandError> {
        match form {
            Form::SyntaxQuote(inner) => self.expand_syntax_quote(inner),
            Form::Unquote(_) => Err(ExpandError::new(ExpandErrorKind::UnquoteOutsideSyntaxQuote)),
            Form::UnquoteSplice(_) => {
                Err(ExpandError::new(ExpandErrorKind::UnquoteOutsideSyntaxQuote))
            }
            Form::List(items) => {
                let mut expanded = BumpVec::new_in(self.bump);
                for item in items.iter() {
                    expanded.push(self.expand(item)?);
                }
                Ok(Form::List(expanded))
            }
            Form::Vector(items) => {
                let mut expanded = BumpVec::new_in(self.bump);
                for item in items.iter() {
                    expanded.push(self.expand(item)?);
                }
                Ok(Form::Vector(expanded))
            }
            Form::Map(pairs) => {
                let mut expanded = BumpVec::new_in(self.bump);
                for (k, v) in pairs.iter() {
                    expanded.push((self.expand(k)?, self.expand(v)?));
                }
                Ok(Form::Map(expanded))
            }
            Form::Set(items) => {
                let mut expanded = BumpVec::new_in(self.bump);
                for item in items.iter() {
                    expanded.push(self.expand(item)?);
                }
                Ok(Form::Set(expanded))
            }
            Form::Quote(inner) => {
                let expanded = self.expand(inner)?;
                let alloc = self.bump.alloc(expanded);
                Ok(Form::Quote(alloc))
            }
            Form::Deref(inner) => {
                let expanded = self.expand(inner)?;
                let alloc = self.bump.alloc(expanded);
                Ok(Form::Deref(alloc))
            }
            Form::Var(inner) => {
                let expanded = self.expand(inner)?;
                let alloc = self.bump.alloc(expanded);
                Ok(Form::Var(alloc))
            }
            Form::Meta { meta, form: inner } => {
                let expanded_meta = self.expand(meta)?;
                let expanded_form = self.expand(inner)?;
                Ok(Form::Meta {
                    meta: self.bump.alloc(expanded_meta),
                    form: self.bump.alloc(expanded_form),
                })
            }
            Form::AnonFn(items) => {
                let mut expanded = BumpVec::new_in(self.bump);
                for item in items.iter() {
                    expanded.push(self.expand(item)?);
                }
                Ok(Form::AnonFn(expanded))
            }
            Form::Tagged { tag, form: inner } => {
                let expanded = self.expand(inner)?;
                Ok(Form::Tagged {
                    tag,
                    form: self.bump.alloc(expanded),
                })
            }
            Form::ReaderCond { splicing, branches } => {
                let mut expanded = BumpVec::new_in(self.bump);
                for (k, v) in branches.iter() {
                    expanded.push((self.expand(k)?, self.expand(v)?));
                }
                Ok(Form::ReaderCond {
                    splicing: *splicing,
                    branches: expanded,
                })
            }
            _ => Ok(form.clone()),
        }
    }

    fn expand_syntax_quote(&mut self, form: &Form<'a>) -> Result<Form<'a>, ExpandError> {
        self.gensym_map.clear();
        self.expand_syntax_quote_inner(form)
    }

    fn expand_syntax_quote_inner(&mut self, form: &Form<'a>) -> Result<Form<'a>, ExpandError> {
        match form {
            Form::Unquote(inner) => Ok((*inner).clone()),
            Form::UnquoteSplice(_) => {
                Err(ExpandError::new(ExpandErrorKind::UnquoteSpliceNotInList))
            }
            Form::Symbol { ns, name } => {
                if ns.is_none() && name.ends_with('#') {
                    let gensym = self.get_or_create_gensym(name);
                    Ok(self.make_quote(Form::Symbol { ns: None, name: gensym }))
                } else {
                    Ok(self.make_quote(Form::Symbol { ns: *ns, name }))
                }
            }
            Form::List(items) => self.expand_list(items),
            Form::Vector(items) => self.expand_vector(items),
            Form::Map(pairs) => self.expand_map(pairs),
            Form::Set(items) => self.expand_set(items),
            Form::SyntaxQuote(inner) => {
                let expanded = self.expand_syntax_quote_inner(inner)?;
                Ok(self.make_quote(expanded))
            }
            _ => Ok(self.make_quote(form.clone())),
        }
    }

    fn expand_list(&mut self, items: &BumpVec<'a, Form<'a>>) -> Result<Form<'a>, ExpandError> {
        if items.is_empty() {
            return Ok(self.make_list_call(BumpVec::new_in(self.bump)));
        }

        let concat_args = self.build_concat_args(items)?;
        let concat_call = self.make_concat_call(concat_args);
        Ok(self.make_seq_call(concat_call))
    }

    fn expand_vector(&mut self, items: &BumpVec<'a, Form<'a>>) -> Result<Form<'a>, ExpandError> {
        if items.is_empty() {
            return Ok(Form::Vector(BumpVec::new_in(self.bump)));
        }

        let concat_args = self.build_concat_args(items)?;
        let concat_call = self.make_concat_call(concat_args);
        Ok(self.make_apply_call("vector", concat_call))
    }

    fn expand_map(
        &mut self,
        pairs: &BumpVec<'a, (Form<'a>, Form<'a>)>,
    ) -> Result<Form<'a>, ExpandError> {
        if pairs.is_empty() {
            return Ok(Form::Map(BumpVec::new_in(self.bump)));
        }

        let mut flat_items = BumpVec::new_in(self.bump);
        for (k, v) in pairs.iter() {
            flat_items.push(k.clone());
            flat_items.push(v.clone());
        }

        let concat_args = self.build_concat_args(&flat_items)?;
        let concat_call = self.make_concat_call(concat_args);
        Ok(self.make_apply_call("hash-map", concat_call))
    }

    fn expand_set(&mut self, items: &BumpVec<'a, Form<'a>>) -> Result<Form<'a>, ExpandError> {
        if items.is_empty() {
            return Ok(Form::Set(BumpVec::new_in(self.bump)));
        }

        let concat_args = self.build_concat_args(items)?;
        let concat_call = self.make_concat_call(concat_args);
        Ok(self.make_apply_call("hash-set", concat_call))
    }

    fn build_concat_args(
        &mut self,
        items: &BumpVec<'a, Form<'a>>,
    ) -> Result<BumpVec<'a, Form<'a>>, ExpandError> {
        let mut concat_args = BumpVec::new_in(self.bump);

        for item in items.iter() {
            match item {
                Form::UnquoteSplice(inner) => {
                    concat_args.push((*inner).clone());
                }
                Form::Unquote(inner) => {
                    concat_args.push(self.make_list_call_single((*inner).clone()));
                }
                _ => {
                    let expanded = self.expand_syntax_quote_inner(item)?;
                    concat_args.push(self.make_list_call_single(expanded));
                }
            }
        }

        Ok(concat_args)
    }

    fn get_or_create_gensym(&mut self, name: &'a str) -> &'a str {
        if let Some(&existing) = self.gensym_map.get(name) {
            return existing;
        }

        let base = &name[..name.len() - 1];
        let gensym_name = format!("{}__{}__auto__", base, self.gensym_counter);
        self.gensym_counter += 1;

        let allocated = self.bump.alloc_str(&gensym_name);
        self.gensym_map.insert(name, allocated);
        allocated
    }

    fn make_symbol(&self, name: &'static str) -> Form<'a> {
        Form::Symbol {
            ns: None,
            name: self.bump.alloc_str(name),
        }
    }

    fn make_quote(&self, form: Form<'a>) -> Form<'a> {
        let mut args = BumpVec::new_in(self.bump);
        args.push(self.make_symbol("quote"));
        args.push(form);
        Form::List(args)
    }

    fn make_list_call(&self, items: BumpVec<'a, Form<'a>>) -> Form<'a> {
        let mut args = BumpVec::new_in(self.bump);
        args.push(self.make_symbol("list"));
        for item in items {
            args.push(item);
        }
        Form::List(args)
    }

    fn make_list_call_single(&self, item: Form<'a>) -> Form<'a> {
        let mut args = BumpVec::new_in(self.bump);
        args.push(self.make_symbol("list"));
        args.push(item);
        Form::List(args)
    }

    fn make_concat_call(&self, items: BumpVec<'a, Form<'a>>) -> Form<'a> {
        let mut args = BumpVec::new_in(self.bump);
        args.push(self.make_symbol("concat"));
        for item in items {
            args.push(item);
        }
        Form::List(args)
    }

    fn make_seq_call(&self, form: Form<'a>) -> Form<'a> {
        let mut args = BumpVec::new_in(self.bump);
        args.push(self.make_symbol("seq"));
        args.push(form);
        Form::List(args)
    }

    fn make_apply_call(&self, fn_name: &'static str, concat_form: Form<'a>) -> Form<'a> {
        let mut args = BumpVec::new_in(self.bump);
        args.push(self.make_symbol("apply"));
        args.push(self.make_symbol(fn_name));
        args.push(concat_form);
        Form::List(args)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::StringValue;

    fn is_symbol_with_name(form: &Form<'_>, name: &str) -> bool {
        matches!(form, Form::Symbol { ns: None, name: n } if *n == name)
    }

    fn get_list_items<'a>(form: &'a Form<'a>) -> Option<&'a BumpVec<'a, Form<'a>>> {
        match form {
            Form::List(items) => Some(items),
            _ => None,
        }
    }

    fn make_bump() -> &'static Bump {
        Box::leak(Box::new(Bump::new()))
    }

    fn make_symbol<'a>(bump: &'a Bump, name: &str) -> Form<'a> {
        Form::Symbol {
            ns: None,
            name: bump.alloc_str(name),
        }
    }

    fn make_symbol_ns<'a>(bump: &'a Bump, ns: &str, name: &str) -> Form<'a> {
        Form::Symbol {
            ns: Some(bump.alloc_str(ns)),
            name: bump.alloc_str(name),
        }
    }

    fn make_keyword<'a>(bump: &'a Bump, name: &str) -> Form<'a> {
        Form::Keyword {
            ns: None,
            name: bump.alloc_str(name),
            auto_resolve: false,
        }
    }

    fn make_list<'a>(bump: &'a Bump, items: Vec<Form<'a>>) -> Form<'a> {
        let mut vec = BumpVec::new_in(bump);
        for item in items {
            vec.push(item);
        }
        Form::List(vec)
    }

    fn make_vector<'a>(bump: &'a Bump, items: Vec<Form<'a>>) -> Form<'a> {
        let mut vec = BumpVec::new_in(bump);
        for item in items {
            vec.push(item);
        }
        Form::Vector(vec)
    }

    fn make_map<'a>(bump: &'a Bump, pairs: Vec<(Form<'a>, Form<'a>)>) -> Form<'a> {
        let mut vec = BumpVec::new_in(bump);
        for (k, v) in pairs {
            vec.push((k, v));
        }
        Form::Map(vec)
    }

    fn make_set<'a>(bump: &'a Bump, items: Vec<Form<'a>>) -> Form<'a> {
        let mut vec = BumpVec::new_in(bump);
        for item in items {
            vec.push(item);
        }
        Form::Set(vec)
    }

    #[test]
    fn test_expand_simple_symbol() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let sym = make_symbol(bump, "foo");
        let syntax_quote = Form::SyntaxQuote(bump.alloc(sym));

        let result = expander.expand(&syntax_quote).unwrap();

        if let Form::List(items) = &result {
            assert_eq!(items.len(), 2);
            assert!(is_symbol_with_name(&items[0], "quote"));
            assert!(matches!(&items[1], Form::Symbol { ns: None, name } if *name == "foo"));
        } else {
            panic!("Expected list, got {:?}", result);
        }
    }

    #[test]
    fn test_expand_namespaced_symbol() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let sym = make_symbol_ns(bump, "clojure.core", "map");
        let syntax_quote = Form::SyntaxQuote(bump.alloc(sym));

        let result = expander.expand(&syntax_quote).unwrap();

        if let Form::List(items) = &result {
            assert_eq!(items.len(), 2);
            assert!(is_symbol_with_name(&items[0], "quote"));
            assert!(
                matches!(&items[1], Form::Symbol { ns: Some(n), name } if *n == "clojure.core" && *name == "map")
            );
        } else {
            panic!("Expected list");
        }
    }

    #[test]
    fn test_expand_keyword() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let kw = make_keyword(bump, "foo");
        let syntax_quote = Form::SyntaxQuote(bump.alloc(kw));

        let result = expander.expand(&syntax_quote).unwrap();

        if let Form::List(items) = &result {
            assert_eq!(items.len(), 2);
            assert!(is_symbol_with_name(&items[0], "quote"));
            assert!(
                matches!(&items[1], Form::Keyword { ns: None, name, auto_resolve: false } if *name == "foo")
            );
        } else {
            panic!("Expected list");
        }
    }

    #[test]
    fn test_expand_literal_int() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let int = Form::Int(42);
        let syntax_quote = Form::SyntaxQuote(bump.alloc(int));

        let result = expander.expand(&syntax_quote).unwrap();

        if let Form::List(items) = &result {
            assert_eq!(items.len(), 2);
            assert!(is_symbol_with_name(&items[0], "quote"));
            assert!(matches!(&items[1], Form::Int(42)));
        } else {
            panic!("Expected list");
        }
    }

    #[test]
    fn test_expand_empty_list() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let empty_list = make_list(bump, vec![]);
        let syntax_quote = Form::SyntaxQuote(bump.alloc(empty_list));

        let result = expander.expand(&syntax_quote).unwrap();

        if let Form::List(items) = &result {
            assert_eq!(items.len(), 1);
            assert!(is_symbol_with_name(&items[0], "list"));
        } else {
            panic!("Expected list");
        }
    }

    #[test]
    fn test_expand_list_with_symbols() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let a = make_symbol(bump, "a");
        let b = make_symbol(bump, "b");
        let list = make_list(bump, vec![a, b]);
        let syntax_quote = Form::SyntaxQuote(bump.alloc(list));

        let result = expander.expand(&syntax_quote).unwrap();

        if let Form::List(outer) = &result {
            assert_eq!(outer.len(), 2);
            assert!(is_symbol_with_name(&outer[0], "seq"));
            if let Form::List(concat_call) = &outer[1] {
                assert!(is_symbol_with_name(&concat_call[0], "concat"));
            } else {
                panic!("Expected concat call");
            }
        } else {
            panic!("Expected list");
        }
    }

    #[test]
    fn test_expand_list_with_unquote() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let a = make_symbol(bump, "a");
        let b = make_symbol(bump, "b");
        let unquoted_b = Form::Unquote(bump.alloc(b));
        let list = make_list(bump, vec![a, unquoted_b]);
        let syntax_quote = Form::SyntaxQuote(bump.alloc(list));

        let result = expander.expand(&syntax_quote).unwrap();

        if let Form::List(outer) = &result {
            assert!(is_symbol_with_name(&outer[0], "seq"));
            if let Form::List(concat_call) = &outer[1] {
                assert!(is_symbol_with_name(&concat_call[0], "concat"));
                assert_eq!(concat_call.len(), 3);
            } else {
                panic!("Expected concat call");
            }
        } else {
            panic!("Expected list");
        }
    }

    #[test]
    fn test_expand_list_with_unquote_splice() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let a = make_symbol(bump, "a");
        let c = make_symbol(bump, "c");
        let spliced_c = Form::UnquoteSplice(bump.alloc(c));
        let list = make_list(bump, vec![a, spliced_c]);
        let syntax_quote = Form::SyntaxQuote(bump.alloc(list));

        let result = expander.expand(&syntax_quote).unwrap();

        if let Form::List(outer) = &result {
            assert!(is_symbol_with_name(&outer[0], "seq"));
            if let Form::List(concat_call) = &outer[1] {
                assert!(is_symbol_with_name(&concat_call[0], "concat"));
                assert!(matches!(&concat_call[2], Form::Symbol { ns: None, name } if *name == "c"));
            } else {
                panic!("Expected concat call");
            }
        } else {
            panic!("Expected list");
        }
    }

    #[test]
    fn test_expand_empty_vector() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let empty_vec = make_vector(bump, vec![]);
        let syntax_quote = Form::SyntaxQuote(bump.alloc(empty_vec));

        let result = expander.expand(&syntax_quote).unwrap();
        assert!(matches!(&result, Form::Vector(v) if v.is_empty()));
    }

    #[test]
    fn test_expand_vector_with_unquote() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let a = make_symbol(bump, "a");
        let b = make_symbol(bump, "b");
        let unquoted_b = Form::Unquote(bump.alloc(b));
        let vec = make_vector(bump, vec![a, unquoted_b]);
        let syntax_quote = Form::SyntaxQuote(bump.alloc(vec));

        let result = expander.expand(&syntax_quote).unwrap();

        if let Form::List(outer) = &result {
            assert!(is_symbol_with_name(&outer[0], "apply"));
            assert!(is_symbol_with_name(&outer[1], "vector"));
            if let Form::List(concat_call) = &outer[2] {
                assert!(is_symbol_with_name(&concat_call[0], "concat"));
            } else {
                panic!("Expected concat call");
            }
        } else {
            panic!("Expected list");
        }
    }

    #[test]
    fn test_expand_empty_map() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let empty_map = make_map(bump, vec![]);
        let syntax_quote = Form::SyntaxQuote(bump.alloc(empty_map));

        let result = expander.expand(&syntax_quote).unwrap();
        assert!(matches!(&result, Form::Map(m) if m.is_empty()));
    }

    #[test]
    fn test_expand_map_with_unquote() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let k = make_keyword(bump, "a");
        let v = make_symbol(bump, "b");
        let unquoted_v = Form::Unquote(bump.alloc(v));
        let map = make_map(bump, vec![(k, unquoted_v)]);
        let syntax_quote = Form::SyntaxQuote(bump.alloc(map));

        let result = expander.expand(&syntax_quote).unwrap();

        if let Form::List(outer) = &result {
            assert!(is_symbol_with_name(&outer[0], "apply"));
            assert!(is_symbol_with_name(&outer[1], "hash-map"));
            if let Form::List(concat_call) = &outer[2] {
                assert!(is_symbol_with_name(&concat_call[0], "concat"));
            } else {
                panic!("Expected concat call");
            }
        } else {
            panic!("Expected list");
        }
    }

    #[test]
    fn test_expand_empty_set() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let empty_set = make_set(bump, vec![]);
        let syntax_quote = Form::SyntaxQuote(bump.alloc(empty_set));

        let result = expander.expand(&syntax_quote).unwrap();
        assert!(matches!(&result, Form::Set(s) if s.is_empty()));
    }

    #[test]
    fn test_expand_set_with_unquote() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let a = make_symbol(bump, "a");
        let b = make_symbol(bump, "b");
        let unquoted_b = Form::Unquote(bump.alloc(b));
        let set = make_set(bump, vec![a, unquoted_b]);
        let syntax_quote = Form::SyntaxQuote(bump.alloc(set));

        let result = expander.expand(&syntax_quote).unwrap();

        if let Form::List(outer) = &result {
            assert!(is_symbol_with_name(&outer[0], "apply"));
            assert!(is_symbol_with_name(&outer[1], "hash-set"));
            if let Form::List(concat_call) = &outer[2] {
                assert!(is_symbol_with_name(&concat_call[0], "concat"));
            } else {
                panic!("Expected concat call");
            }
        } else {
            panic!("Expected list");
        }
    }

    #[test]
    fn test_gensym_basic() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let gensym = make_symbol(bump, "foo#");
        let syntax_quote = Form::SyntaxQuote(bump.alloc(gensym));

        let result = expander.expand(&syntax_quote).unwrap();

        if let Form::List(items) = &result {
            assert!(is_symbol_with_name(&items[0], "quote"));
            if let Form::Symbol { ns: None, name } = &items[1] {
                assert!(name.starts_with("foo__"));
                assert!(name.ends_with("__auto__"));
            } else {
                panic!("Expected symbol");
            }
        } else {
            panic!("Expected list");
        }
    }

    #[test]
    fn test_gensym_same_within_syntax_quote() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let foo1 = make_symbol(bump, "foo#");
        let foo2 = make_symbol(bump, "foo#");
        let list = make_list(bump, vec![foo1, foo2]);
        let syntax_quote = Form::SyntaxQuote(bump.alloc(list));

        let result = expander.expand(&syntax_quote).unwrap();

        if let Form::List(outer) = &result {
            if let Form::List(concat_call) = &outer[1] {
                if let Form::List(list1) = &concat_call[1] {
                    if let Form::List(list2) = &concat_call[2] {
                        if let Form::List(quote1) = &list1[1] {
                            if let Form::List(quote2) = &list2[1] {
                                if let Form::Symbol { name: name1, .. } = &quote1[1] {
                                    if let Form::Symbol { name: name2, .. } = &quote2[1] {
                                        assert_eq!(*name1, *name2);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_gensym_different_across_syntax_quotes() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let foo1 = make_symbol(bump, "foo#");
        let syntax_quote1 = Form::SyntaxQuote(bump.alloc(foo1));
        let result1 = expander.expand(&syntax_quote1).unwrap();

        let foo2 = make_symbol(bump, "foo#");
        let syntax_quote2 = Form::SyntaxQuote(bump.alloc(foo2));
        let result2 = expander.expand(&syntax_quote2).unwrap();

        if let (Form::List(items1), Form::List(items2)) = (&result1, &result2) {
            if let (Form::Symbol { name: name1, .. }, Form::Symbol { name: name2, .. }) =
                (&items1[1], &items2[1])
            {
                assert_ne!(*name1, *name2);
            } else {
                panic!("Expected symbols");
            }
        } else {
            panic!("Expected lists");
        }
    }

    #[test]
    fn test_unquote_outside_syntax_quote_error() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let sym = make_symbol(bump, "foo");
        let unquote = Form::Unquote(bump.alloc(sym));

        let result = expander.expand(&unquote);
        assert!(result.is_err());
        assert!(matches!(
            result.unwrap_err().kind,
            ExpandErrorKind::UnquoteOutsideSyntaxQuote
        ));
    }

    #[test]
    fn test_unquote_splice_outside_syntax_quote_error() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let sym = make_symbol(bump, "foo");
        let splice = Form::UnquoteSplice(bump.alloc(sym));

        let result = expander.expand(&splice);
        assert!(result.is_err());
        assert!(matches!(
            result.unwrap_err().kind,
            ExpandErrorKind::UnquoteOutsideSyntaxQuote
        ));
    }

    #[test]
    fn test_unquote_splice_not_in_list_error() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let sym = make_symbol(bump, "foo");
        let splice = Form::UnquoteSplice(bump.alloc(sym));
        let syntax_quote = Form::SyntaxQuote(bump.alloc(splice));

        let result = expander.expand(&syntax_quote);
        assert!(result.is_err());
        assert!(matches!(
            result.unwrap_err().kind,
            ExpandErrorKind::UnquoteSpliceNotInList
        ));
    }

    #[test]
    fn test_nested_syntax_quote() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let sym = make_symbol(bump, "foo");
        let inner_sq = Form::SyntaxQuote(bump.alloc(sym));
        let outer_sq = Form::SyntaxQuote(bump.alloc(inner_sq));

        let result = expander.expand(&outer_sq).unwrap();
        assert!(matches!(&result, Form::List(_)));
    }

    #[test]
    fn test_expand_deref() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let sym = make_symbol(bump, "atom");
        let deref = Form::Deref(bump.alloc(sym));

        let result = expander.expand(&deref).unwrap();
        assert!(matches!(&result, Form::Deref(_)));
    }

    #[test]
    fn test_expand_var() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let sym = make_symbol(bump, "foo");
        let var = Form::Var(bump.alloc(sym));

        let result = expander.expand(&var).unwrap();
        assert!(matches!(&result, Form::Var(_)));
    }

    #[test]
    fn test_expand_meta() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let meta = make_keyword(bump, "private");
        let sym = make_symbol(bump, "foo");
        let meta_form = Form::Meta {
            meta: bump.alloc(meta),
            form: bump.alloc(sym),
        };

        let result = expander.expand(&meta_form).unwrap();
        assert!(matches!(&result, Form::Meta { .. }));
    }

    #[test]
    fn test_expand_tagged() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let s = Form::String(StringValue::Borrowed("2023-01-01"));
        let tagged = Form::Tagged {
            tag: bump.alloc_str("inst"),
            form: bump.alloc(s),
        };

        let result = expander.expand(&tagged).unwrap();
        assert!(matches!(&result, Form::Tagged { .. }));
    }

    #[test]
    fn test_expand_anon_fn() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let sym = make_symbol(bump, "+");
        let arg = make_symbol(bump, "%");
        let mut body = BumpVec::new_in(bump);
        body.push(sym);
        body.push(arg);
        body.push(Form::Int(1));
        let anon_fn = Form::AnonFn(body);

        let result = expander.expand(&anon_fn).unwrap();
        assert!(matches!(&result, Form::AnonFn(_)));
    }

    #[test]
    fn test_expand_reader_cond() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let clj = make_keyword(bump, "clj");
        let val1 = Form::Int(1);
        let cljs = make_keyword(bump, "cljs");
        let val2 = Form::Int(2);
        let mut branches = BumpVec::new_in(bump);
        branches.push((clj, val1));
        branches.push((cljs, val2));

        let reader_cond = Form::ReaderCond {
            splicing: false,
            branches,
        };

        let result = expander.expand(&reader_cond).unwrap();
        assert!(matches!(&result, Form::ReaderCond { splicing: false, .. }));
    }

    #[test]
    fn test_expand_literals() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        assert!(matches!(expander.expand(&Form::Nil).unwrap(), Form::Nil));
        assert!(matches!(
            expander.expand(&Form::Bool(true)).unwrap(),
            Form::Bool(true)
        ));
        assert!(matches!(
            expander.expand(&Form::Float(3.14)).unwrap(),
            Form::Float(f) if (f - 3.14).abs() < f64::EPSILON
        ));
        assert!(matches!(
            expander.expand(&Form::Char('a')).unwrap(),
            Form::Char('a')
        ));
    }

    #[test]
    fn test_expand_error_display() {
        use crate::span::Span;

        let error = ExpandError::new(ExpandErrorKind::UnquoteSpliceNotInList);
        let display = format!("{}", error);
        assert!(display.contains("unquote-splice"));

        let error_with_span =
            ExpandError::with_span(ExpandErrorKind::UnquoteOutsideSyntaxQuote, Span::new(5, 10));
        let display_span = format!("{}", error_with_span);
        assert!(display_span.contains("5..10"));
    }

    #[test]
    fn test_expand_error_kind_display() {
        let kind1 = ExpandErrorKind::UnquoteSpliceNotInList;
        assert!(format!("{}", kind1).contains("unquote-splice"));

        let kind2 = ExpandErrorKind::UnquoteOutsideSyntaxQuote;
        assert!(format!("{}", kind2).contains("unquote"));
    }

    #[test]
    fn test_nested_collections() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let inner_list = make_list(bump, vec![Form::Int(1), Form::Int(2)]);
        let outer_list = make_list(bump, vec![make_symbol(bump, "a"), inner_list]);
        let syntax_quote = Form::SyntaxQuote(bump.alloc(outer_list));

        let result = expander.expand(&syntax_quote).unwrap();
        assert!(matches!(&result, Form::List(_)));
    }

    #[test]
    fn test_complex_expansion() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let a = make_symbol(bump, "a");
        let b = make_symbol(bump, "b");
        let c = make_symbol(bump, "c");
        let unquoted_b = Form::Unquote(bump.alloc(b));
        let spliced_c = Form::UnquoteSplice(bump.alloc(c));
        let list = make_list(bump, vec![a, unquoted_b, spliced_c]);
        let syntax_quote = Form::SyntaxQuote(bump.alloc(list));

        let result = expander.expand(&syntax_quote).unwrap();

        if let Form::List(outer) = &result {
            assert!(is_symbol_with_name(&outer[0], "seq"));
            if let Form::List(concat_call) = &outer[1] {
                assert!(is_symbol_with_name(&concat_call[0], "concat"));
                assert_eq!(concat_call.len(), 4);
            } else {
                panic!("Expected concat call");
            }
        } else {
            panic!("Expected list");
        }
    }

    #[test]
    fn test_get_list_items_helper() {
        let bump = make_bump();
        let list = make_list(bump, vec![Form::Int(1)]);
        assert!(get_list_items(&list).is_some());
        assert!(get_list_items(&Form::Int(1)).is_none());
    }

    #[test]
    fn test_expand_quote_form() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let sym = make_symbol(bump, "foo");
        let quote = Form::Quote(bump.alloc(sym));

        let result = expander.expand(&quote).unwrap();
        assert!(matches!(&result, Form::Quote(_)));
    }

    #[test]
    fn test_expand_nested_list_in_vector() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let inner = make_list(bump, vec![Form::Int(1)]);
        let vec = make_vector(bump, vec![inner]);
        let syntax_quote = Form::SyntaxQuote(bump.alloc(vec));

        let result = expander.expand(&syntax_quote).unwrap();
        assert!(matches!(&result, Form::List(_)));
    }

    #[test]
    fn test_expand_mixed_collection() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let k = make_keyword(bump, "key");
        let v = make_vector(bump, vec![Form::Int(1), Form::Int(2)]);
        let map = make_map(bump, vec![(k, v)]);
        let syntax_quote = Form::SyntaxQuote(bump.alloc(map));

        let result = expander.expand(&syntax_quote).unwrap();
        assert!(matches!(&result, Form::List(_)));
    }

    #[test]
    fn test_expand_big_int() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let big_int = Form::BigInt(bump.alloc_str("12345678901234567890"));
        let syntax_quote = Form::SyntaxQuote(bump.alloc(big_int));

        let result = expander.expand(&syntax_quote).unwrap();
        if let Form::List(items) = &result {
            assert!(is_symbol_with_name(&items[0], "quote"));
            assert!(matches!(&items[1], Form::BigInt(_)));
        } else {
            panic!("Expected list");
        }
    }

    #[test]
    fn test_expand_big_decimal() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let big_decimal = Form::BigDecimal(bump.alloc_str("3.14159265358979323846M"));
        let syntax_quote = Form::SyntaxQuote(bump.alloc(big_decimal));

        let result = expander.expand(&syntax_quote).unwrap();
        if let Form::List(items) = &result {
            assert!(is_symbol_with_name(&items[0], "quote"));
            assert!(matches!(&items[1], Form::BigDecimal(_)));
        } else {
            panic!("Expected list");
        }
    }

    #[test]
    fn test_expand_ratio() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let ratio = Form::Ratio { numer: 1, denom: 3 };
        let syntax_quote = Form::SyntaxQuote(bump.alloc(ratio));

        let result = expander.expand(&syntax_quote).unwrap();
        if let Form::List(items) = &result {
            assert!(is_symbol_with_name(&items[0], "quote"));
            assert!(matches!(&items[1], Form::Ratio { numer: 1, denom: 3 }));
        } else {
            panic!("Expected list");
        }
    }

    #[test]
    fn test_expand_regex() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let regex = Form::Regex(StringValue::Borrowed("\\d+"));
        let syntax_quote = Form::SyntaxQuote(bump.alloc(regex));

        let result = expander.expand(&syntax_quote).unwrap();
        if let Form::List(items) = &result {
            assert!(is_symbol_with_name(&items[0], "quote"));
            assert!(matches!(&items[1], Form::Regex(_)));
        } else {
            panic!("Expected list");
        }
    }

    #[test]
    fn test_expand_symbolic_val() {
        use crate::ast::SymbolicVal;

        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let inf = Form::SymbolicVal(SymbolicVal::Inf);
        let syntax_quote = Form::SyntaxQuote(bump.alloc(inf));

        let result = expander.expand(&syntax_quote).unwrap();
        if let Form::List(items) = &result {
            assert!(is_symbol_with_name(&items[0], "quote"));
            assert!(matches!(&items[1], Form::SymbolicVal(SymbolicVal::Inf)));
        } else {
            panic!("Expected list");
        }
    }

    #[test]
    fn test_expand_auto_resolve_keyword() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let kw = Form::Keyword {
            ns: None,
            name: bump.alloc_str("foo"),
            auto_resolve: true,
        };
        let syntax_quote = Form::SyntaxQuote(bump.alloc(kw));

        let result = expander.expand(&syntax_quote).unwrap();
        if let Form::List(items) = &result {
            assert!(is_symbol_with_name(&items[0], "quote"));
            assert!(
                matches!(&items[1], Form::Keyword { ns: None, name, auto_resolve: true } if *name == "foo")
            );
        } else {
            panic!("Expected list");
        }
    }

    #[test]
    fn test_multiple_gensyms() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let foo = make_symbol(bump, "foo#");
        let bar = make_symbol(bump, "bar#");
        let list = make_list(bump, vec![foo, bar]);
        let syntax_quote = Form::SyntaxQuote(bump.alloc(list));

        let result = expander.expand(&syntax_quote).unwrap();

        if let Form::List(outer) = &result {
            if let Form::List(concat_call) = &outer[1] {
                if let Form::List(list1) = &concat_call[1] {
                    if let Form::List(list2) = &concat_call[2] {
                        if let Form::List(quote1) = &list1[1] {
                            if let Form::List(quote2) = &list2[1] {
                                if let Form::Symbol { name: name1, .. } = &quote1[1] {
                                    if let Form::Symbol { name: name2, .. } = &quote2[1] {
                                        assert!(name1.starts_with("foo__"));
                                        assert!(name2.starts_with("bar__"));
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
