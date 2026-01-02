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
                // Nested syntax-quote: first expand inner, then apply syntax-quote to the result
                // ``foo → `(quote foo) → (seq (concat (list (quote quote)) (list (quote foo))))
                let inner_expanded = self.expand_syntax_quote_inner(inner)?;
                self.expand_syntax_quote_inner(&inner_expanded)
            }
            Form::Quote(inner) => {
                // 'foo inside `... becomes (seq (concat (list (quote quote)) (list (quote foo))))
                let mut items = BumpVec::new_in(self.bump);
                items.push(Form::Symbol {
                    ns: None,
                    name: "quote",
                });
                items.push((*inner).clone());
                self.expand_list(&items)
            }
            Form::Deref(inner) => {
                // @foo inside `... becomes (seq (concat (list (quote deref)) (list (quote foo))))
                let mut items = BumpVec::new_in(self.bump);
                items.push(Form::Symbol {
                    ns: None,
                    name: "deref",
                });
                items.push((*inner).clone());
                self.expand_list(&items)
            }
            Form::Var(inner) => {
                // #'foo inside `... becomes (seq (concat (list (quote var)) (list (quote foo))))
                let mut items = BumpVec::new_in(self.bump);
                items.push(Form::Symbol { ns: None, name: "var" });
                items.push((*inner).clone());
                self.expand_list(&items)
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

    fn is_symbol_with_name(form: &Form<'_>, name: &str) -> bool {
        matches!(form, Form::Symbol { ns: None, name: n } if *n == name)
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
    fn test_vector_with_unquote_splice() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let sym = make_symbol(bump, "items");
        let splice = Form::UnquoteSplice(bump.alloc(sym));
        let vec = make_vector(bump, vec![Form::Int(1), splice]);
        let syntax_quote = Form::SyntaxQuote(bump.alloc(vec));

        let result = expander.expand(&syntax_quote).unwrap();
        if let Form::List(outer) = &result {
            assert!(is_symbol_with_name(&outer[0], "apply"));
            assert!(is_symbol_with_name(&outer[1], "vector"));
            if let Form::List(concat) = &outer[2] {
                assert!(is_symbol_with_name(&concat[0], "concat"));
                assert!(matches!(&concat[2], Form::Symbol { name, .. } if *name == "items"));
            }
        } else {
            panic!("Expected list");
        }
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
    fn test_set_with_unquote_splice() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let sym = make_symbol(bump, "items");
        let splice = Form::UnquoteSplice(bump.alloc(sym));
        let set = make_set(bump, vec![Form::Int(1), splice]);
        let syntax_quote = Form::SyntaxQuote(bump.alloc(set));

        let result = expander.expand(&syntax_quote).unwrap();
        if let Form::List(outer) = &result {
            assert!(is_symbol_with_name(&outer[0], "apply"));
            assert!(is_symbol_with_name(&outer[1], "hash-set"));
            if let Form::List(concat) = &outer[2] {
                assert!(is_symbol_with_name(&concat[0], "concat"));
                assert!(matches!(&concat[2], Form::Symbol { name, .. } if *name == "items"));
            }
        } else {
            panic!("Expected list");
        }
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
    fn test_map_with_unquote_splice_key() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let key_sym = make_symbol(bump, "k");
        let unquoted_key = Form::Unquote(bump.alloc(key_sym));
        let val = Form::Int(1);
        let map = make_map(bump, vec![(unquoted_key, val)]);
        let syntax_quote = Form::SyntaxQuote(bump.alloc(map));

        let result = expander.expand(&syntax_quote).unwrap();
        if let Form::List(outer) = &result {
            assert!(is_symbol_with_name(&outer[0], "apply"));
            assert!(is_symbol_with_name(&outer[1], "hash-map"));
        } else {
            panic!("Expected list");
        }
    }

    #[test]
    fn test_nested_unquote_in_list() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let sym = make_symbol(bump, "val");
        let unquote = Form::Unquote(bump.alloc(sym));
        let list = make_list(bump, vec![unquote]);
        let syntax_quote = Form::SyntaxQuote(bump.alloc(list));

        let result = expander.expand(&syntax_quote).unwrap();
        if let Form::List(outer) = &result {
            assert!(is_symbol_with_name(&outer[0], "seq"));
            if let Form::List(concat) = &outer[1] {
                assert!(is_symbol_with_name(&concat[0], "concat"));
                if let Form::List(list_call) = &concat[1] {
                    assert!(is_symbol_with_name(&list_call[0], "list"));
                    assert!(matches!(&list_call[1], Form::Symbol { name, .. } if *name == "val"));
                }
            }
        } else {
            panic!("Expected list");
        }
    }

    #[test]
    fn test_direct_unquote_in_syntax_quote() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let sym = make_symbol(bump, "direct");
        let unquote = Form::Unquote(bump.alloc(sym));
        let syntax_quote = Form::SyntaxQuote(bump.alloc(unquote));

        let result = expander.expand(&syntax_quote).unwrap();
        assert!(matches!(&result, Form::Symbol { name, .. } if *name == "direct"));
    }

    #[test]
    fn test_unquote_with_complex_form() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let inner_list = make_list(bump, vec![Form::Int(1), Form::Int(2)]);
        let unquote = Form::Unquote(bump.alloc(inner_list));
        let syntax_quote = Form::SyntaxQuote(bump.alloc(unquote));

        let result = expander.expand(&syntax_quote).unwrap();
        if let Form::List(items) = result {
            assert_eq!(items.len(), 2);
            assert!(matches!(items[0], Form::Int(1)));
            assert!(matches!(items[1], Form::Int(2)));
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

    #[test]
    fn test_gensym_counter_increments() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let foo1 = make_symbol(bump, "a#");
        let sq1 = Form::SyntaxQuote(bump.alloc(foo1));
        let _ = expander.expand(&sq1).unwrap();

        let foo2 = make_symbol(bump, "b#");
        let sq2 = Form::SyntaxQuote(bump.alloc(foo2));
        let result2 = expander.expand(&sq2).unwrap();

        if let Form::List(items) = &result2 {
            if let Form::Symbol { name, .. } = &items[1] {
                assert!(name.contains("__1__"));
            }
        }
    }

    #[test]
    fn test_gensym_with_namespace_not_expanded() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let sym = make_symbol_ns(bump, "my.ns", "foo#");
        let syntax_quote = Form::SyntaxQuote(bump.alloc(sym));

        let result = expander.expand(&syntax_quote).unwrap();
        if let Form::List(items) = &result {
            assert!(is_symbol_with_name(&items[0], "quote"));
            if let Form::Symbol { ns, name } = &items[1] {
                assert_eq!(*ns, Some("my.ns"));
                assert_eq!(*name, "foo#");
            } else {
                panic!("Expected symbol");
            }
        } else {
            panic!("Expected list");
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
    fn test_expand_error_source_trait() {
        use std::error::Error;

        let error = ExpandError::new(ExpandErrorKind::UnquoteSpliceNotInList);
        assert!(error.source().is_none());
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
    fn test_deeply_nested_syntax_quote() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let sym = make_symbol(bump, "x");
        let sq1 = Form::SyntaxQuote(bump.alloc(sym));
        let sq2 = Form::SyntaxQuote(bump.alloc(sq1));
        let sq3 = Form::SyntaxQuote(bump.alloc(sq2));

        let result = expander.expand(&sq3);
        assert!(result.is_ok());
    }

    #[test]
    fn test_quote_inside_syntax_quote() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let sym = make_symbol(bump, "foo");
        let quoted = Form::Quote(bump.alloc(sym));
        let syntax_quote = Form::SyntaxQuote(bump.alloc(quoted));

        let result = expander.expand(&syntax_quote).unwrap();
        if let Form::List(outer) = &result {
            assert!(is_symbol_with_name(&outer[0], "seq"));
            if let Form::List(concat) = &outer[1] {
                assert!(is_symbol_with_name(&concat[0], "concat"));
            }
        } else {
            panic!("Expected list from syntax-quote expansion");
        }
    }

    #[test]
    fn test_deref_inside_syntax_quote() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let sym = make_symbol(bump, "my-atom");
        let deref = Form::Deref(bump.alloc(sym));
        let syntax_quote = Form::SyntaxQuote(bump.alloc(deref));

        let result = expander.expand(&syntax_quote).unwrap();
        if let Form::List(outer) = &result {
            assert!(is_symbol_with_name(&outer[0], "seq"));
            if let Form::List(concat) = &outer[1] {
                assert!(is_symbol_with_name(&concat[0], "concat"));
                if let Form::List(list_call) = &concat[1] {
                    if let Form::List(quote_call) = &list_call[1] {
                        assert!(is_symbol_with_name(&quote_call[0], "quote"));
                        assert!(is_symbol_with_name(&quote_call[1], "deref"));
                    }
                }
            }
        } else {
            panic!("Expected list from deref in syntax-quote");
        }
    }

    #[test]
    fn test_var_inside_syntax_quote() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let sym = make_symbol(bump, "my-var");
        let var = Form::Var(bump.alloc(sym));
        let syntax_quote = Form::SyntaxQuote(bump.alloc(var));

        let result = expander.expand(&syntax_quote).unwrap();
        if let Form::List(outer) = &result {
            assert!(is_symbol_with_name(&outer[0], "seq"));
            if let Form::List(concat) = &outer[1] {
                assert!(is_symbol_with_name(&concat[0], "concat"));
                if let Form::List(list_call) = &concat[1] {
                    if let Form::List(quote_call) = &list_call[1] {
                        assert!(is_symbol_with_name(&quote_call[0], "quote"));
                        assert!(is_symbol_with_name(&quote_call[1], "var"));
                    }
                }
            }
        } else {
            panic!("Expected list from var in syntax-quote");
        }
    }

    #[test]
    fn test_expand_deref_with_syntax_quote_inside() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let inner_sym = make_symbol(bump, "atom");
        let inner_sq = Form::SyntaxQuote(bump.alloc(inner_sym));
        let deref = Form::Deref(bump.alloc(inner_sq));

        let result = expander.expand(&deref).unwrap();
        if let Form::Deref(inner) = result {
            if let Form::List(items) = inner {
                assert!(is_symbol_with_name(&items[0], "quote"));
            } else {
                panic!("Expected list inside deref");
            }
        } else {
            panic!("Expected Deref form");
        }
    }

    #[test]
    fn test_expand_var_with_syntax_quote_inside() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let inner_sym = make_symbol(bump, "my-fn");
        let inner_sq = Form::SyntaxQuote(bump.alloc(inner_sym));
        let var = Form::Var(bump.alloc(inner_sq));

        let result = expander.expand(&var).unwrap();
        if let Form::Var(inner) = result {
            if let Form::List(items) = inner {
                assert!(is_symbol_with_name(&items[0], "quote"));
            } else {
                panic!("Expected list inside var");
            }
        } else {
            panic!("Expected Var form");
        }
    }

    #[test]
    fn test_expand_quote_with_syntax_quote_inside() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let inner_sym = make_symbol(bump, "quoted");
        let inner_sq = Form::SyntaxQuote(bump.alloc(inner_sym));
        let quote = Form::Quote(bump.alloc(inner_sq));

        let result = expander.expand(&quote).unwrap();
        if let Form::Quote(inner) = result {
            if let Form::List(items) = inner {
                assert!(is_symbol_with_name(&items[0], "quote"));
            } else {
                panic!("Expected list inside quote");
            }
        } else {
            panic!("Expected Quote form");
        }
    }

    #[test]
    fn test_meta_with_syntax_quote_inside() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let inner_sym = make_symbol(bump, "val");
        let syntax_quote_inner = Form::SyntaxQuote(bump.alloc(inner_sym));
        let sym = make_symbol(bump, "foo");
        let meta_form = Form::Meta {
            meta: bump.alloc(syntax_quote_inner),
            form: bump.alloc(sym),
        };

        let result = expander.expand(&meta_form).unwrap();
        if let Form::Meta { meta, form } = result {
            if let Form::List(items) = meta {
                assert!(is_symbol_with_name(&items[0], "quote"));
            } else {
                panic!("Expected meta to be expanded syntax-quote");
            }
            assert!(matches!(form, Form::Symbol { .. }));
        } else {
            panic!("Expected Meta form");
        }
    }

    #[test]
    fn test_anon_fn_with_syntax_quote_inside() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let sym = make_symbol(bump, "x");
        let syntax_quote_inner = Form::SyntaxQuote(bump.alloc(sym));
        let mut body = BumpVec::new_in(bump);
        body.push(make_symbol(bump, "+"));
        body.push(syntax_quote_inner);
        let anon_fn = Form::AnonFn(body);

        let result = expander.expand(&anon_fn).unwrap();
        if let Form::AnonFn(items) = result {
            assert_eq!(items.len(), 2);
            if let Form::List(quote_list) = &items[1] {
                assert!(is_symbol_with_name(&quote_list[0], "quote"));
            } else {
                panic!("Expected expanded syntax-quote in anon fn");
            }
        } else {
            panic!("Expected AnonFn form");
        }
    }

    #[test]
    fn test_tagged_with_syntax_quote_inside() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let sym = make_symbol(bump, "value");
        let syntax_quote_inner = Form::SyntaxQuote(bump.alloc(sym));
        let tagged = Form::Tagged {
            tag: bump.alloc_str("my-tag"),
            form: bump.alloc(syntax_quote_inner),
        };

        let result = expander.expand(&tagged).unwrap();
        if let Form::Tagged { tag, form } = result {
            assert_eq!(tag, "my-tag");
            if let Form::List(items) = form {
                assert!(is_symbol_with_name(&items[0], "quote"));
            } else {
                panic!("Expected expanded syntax-quote in tagged form");
            }
        } else {
            panic!("Expected Tagged form");
        }
    }

    #[test]
    fn test_reader_cond_with_syntax_quote_inside() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let clj = make_keyword(bump, "clj");
        let sym = make_symbol(bump, "clj-val");
        let syntax_quote_val = Form::SyntaxQuote(bump.alloc(sym));
        let mut branches = BumpVec::new_in(bump);
        branches.push((clj, syntax_quote_val));

        let reader_cond = Form::ReaderCond {
            splicing: false,
            branches,
        };

        let result = expander.expand(&reader_cond).unwrap();
        if let Form::ReaderCond { splicing, branches } = result {
            assert!(!splicing);
            assert_eq!(branches.len(), 1);
            if let Form::List(items) = &branches[0].1 {
                assert!(is_symbol_with_name(&items[0], "quote"));
            } else {
                panic!("Expected expanded syntax-quote in reader cond");
            }
        } else {
            panic!("Expected ReaderCond form");
        }
    }

    #[test]
    fn test_reader_cond_splicing() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let clj = make_keyword(bump, "clj");
        let val = Form::Int(42);
        let mut branches = BumpVec::new_in(bump);
        branches.push((clj, val));

        let reader_cond = Form::ReaderCond {
            splicing: true,
            branches,
        };

        let result = expander.expand(&reader_cond).unwrap();
        if let Form::ReaderCond { splicing, .. } = result {
            assert!(splicing);
        } else {
            panic!("Expected ReaderCond form");
        }
    }

    #[test]
    fn test_reader_cond_key_with_syntax_quote() {
        let bump = make_bump();
        let mut expander = Expander::new(bump);

        let key_sym = make_symbol(bump, "platform");
        let key_sq = Form::SyntaxQuote(bump.alloc(key_sym));
        let val = Form::Int(1);
        let mut branches = BumpVec::new_in(bump);
        branches.push((key_sq, val));

        let reader_cond = Form::ReaderCond {
            splicing: false,
            branches,
        };

        let result = expander.expand(&reader_cond).unwrap();
        if let Form::ReaderCond { branches, .. } = result {
            if let Form::List(items) = &branches[0].0 {
                assert!(is_symbol_with_name(&items[0], "quote"));
            } else {
                panic!("Expected syntax-quote to be expanded in key");
            }
        } else {
            panic!("Expected ReaderCond form");
        }
    }
}
