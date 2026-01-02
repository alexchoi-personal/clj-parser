use clojure_parser::{parse_with_opts, print_forms, Bump, Expander, ParseOpts, Platform, ReadCondBehavior};

fn opts() -> ParseOpts {
    ParseOpts {
        platform: Platform::Default,
        read_cond: ReadCondBehavior::Preserve,
    }
}

fn roundtrip(input: &str) -> String {
    let bump = Bump::new();
    let forms = parse_with_opts(input, opts(), &bump).expect("parse failed");
    print_forms(forms.iter().map(|s| &s.value))
}

fn roundtrip_ast_eq(input: &str) {
    let bump1 = Bump::new();
    let forms1 = parse_with_opts(input, opts(), &bump1).expect("parse failed");
    let printed = print_forms(forms1.iter().map(|s| &s.value));

    let bump2 = Bump::new();
    let forms2 = parse_with_opts(&printed, opts(), &bump2).expect("re-parse failed");
    let printed2 = print_forms(forms2.iter().map(|s| &s.value));

    assert_eq!(printed, printed2, "AST roundtrip mismatch for input: {}", input);
}

fn expand_roundtrip(input: &str) -> String {
    let bump = Bump::new();
    let forms = parse_with_opts(input, opts(), &bump).expect("parse failed");
    let mut expander = Expander::new(&bump);
    let mut expanded = Vec::new();
    for form in forms.iter() {
        expanded.push(expander.expand(&form.value).expect("expand failed"));
    }
    print_forms(expanded.iter())
}

#[test]
fn test_nil() {
    assert_eq!(roundtrip("nil"), "nil");
}

#[test]
fn test_bool_true() {
    assert_eq!(roundtrip("true"), "true");
}

#[test]
fn test_bool_false() {
    assert_eq!(roundtrip("false"), "false");
}

#[test]
fn test_integers() {
    assert_eq!(roundtrip("42"), "42");
    assert_eq!(roundtrip("-123"), "-123");
    assert_eq!(roundtrip("0"), "0");
}

#[test]
fn test_floats() {
    assert_eq!(roundtrip("3.14"), "3.14");
    assert_eq!(roundtrip("-2.5"), "-2.5");
    roundtrip_ast_eq("1.0");
}

#[test]
fn test_bigint() {
    assert_eq!(roundtrip("123N"), "123N");
    assert_eq!(roundtrip("12345678901234567890N"), "12345678901234567890N");
}

#[test]
fn test_bigdecimal() {
    assert_eq!(roundtrip("1.5M"), "1.5M");
    assert_eq!(roundtrip("3.14159265358979M"), "3.14159265358979M");
}

#[test]
fn test_ratio() {
    assert_eq!(roundtrip("1/2"), "1/2");
    assert_eq!(roundtrip("-3/4"), "-3/4");
    assert_eq!(roundtrip("22/7"), "22/7");
}

#[test]
fn test_char_simple() {
    assert_eq!(roundtrip("\\a"), "\\a");
    assert_eq!(roundtrip("\\z"), "\\z");
    assert_eq!(roundtrip("\\A"), "\\A");
}

#[test]
fn test_char_special() {
    assert_eq!(roundtrip("\\newline"), "\\newline");
    assert_eq!(roundtrip("\\space"), "\\space");
    assert_eq!(roundtrip("\\tab"), "\\tab");
    assert_eq!(roundtrip("\\return"), "\\return");
    assert_eq!(roundtrip("\\\\"), "\\\\");
}

#[test]
fn test_string_simple() {
    assert_eq!(roundtrip("\"hello\""), "\"hello\"");
    assert_eq!(roundtrip("\"\""), "\"\"");
}

#[test]
fn test_string_escapes() {
    assert_eq!(roundtrip("\"hello\\nworld\""), "\"hello\\nworld\"");
    assert_eq!(roundtrip("\"tab\\there\""), "\"tab\\there\"");
    assert_eq!(roundtrip("\"quote\\\"here\""), "\"quote\\\"here\"");
    assert_eq!(roundtrip("\"back\\\\slash\""), "\"back\\\\slash\"");
}

#[test]
fn test_regex() {
    assert_eq!(roundtrip("#\"\\d+\""), "#\"\\d+\"");
    assert_eq!(roundtrip("#\"[a-z]+\""), "#\"[a-z]+\"");
    assert_eq!(roundtrip("#\"\""), "#\"\"");
}

#[test]
fn test_symbol() {
    assert_eq!(roundtrip("foo"), "foo");
    assert_eq!(roundtrip("my.ns/bar"), "my.ns/bar");
    assert_eq!(roundtrip("+"), "+");
    assert_eq!(roundtrip("->"), "->");
}

#[test]
fn test_keyword() {
    assert_eq!(roundtrip(":foo"), ":foo");
    assert_eq!(roundtrip(":my.ns/bar"), ":my.ns/bar");
    assert_eq!(roundtrip("::baz"), "::baz");
    assert_eq!(roundtrip("::alias/qux"), "::alias/qux");
}

#[test]
fn test_list_empty() {
    assert_eq!(roundtrip("()"), "()");
}

#[test]
fn test_list() {
    assert_eq!(roundtrip("(1 2 3)"), "(1 2 3)");
    assert_eq!(roundtrip("(+ 1 2)"), "(+ 1 2)");
    assert_eq!(roundtrip("(defn foo [x] x)"), "(defn foo [x] x)");
}

#[test]
fn test_vector_empty() {
    assert_eq!(roundtrip("[]"), "[]");
}

#[test]
fn test_vector() {
    assert_eq!(roundtrip("[1 2 3]"), "[1 2 3]");
    assert_eq!(roundtrip("[a b c]"), "[a b c]");
}

#[test]
fn test_map_empty() {
    assert_eq!(roundtrip("{}"), "{}");
}

#[test]
fn test_map() {
    assert_eq!(roundtrip("{:a 1 :b 2}"), "{:a 1 :b 2}");
    assert_eq!(roundtrip("{\"key\" \"value\"}"), "{\"key\" \"value\"}");
}

#[test]
fn test_set_empty() {
    assert_eq!(roundtrip("#{}"), "#{}");
}

#[test]
fn test_set() {
    assert_eq!(roundtrip("#{1 2 3}"), "#{1 2 3}");
    assert_eq!(roundtrip("#{:a :b :c}"), "#{:a :b :c}");
}

#[test]
fn test_quote() {
    assert_eq!(roundtrip("'x"), "'x");
    assert_eq!(roundtrip("'(1 2 3)"), "'(1 2 3)");
}

#[test]
fn test_syntax_quote() {
    assert_eq!(roundtrip("`x"), "`x");
    assert_eq!(roundtrip("`(a b c)"), "`(a b c)");
}

#[test]
fn test_unquote() {
    assert_eq!(roundtrip("~x"), "~x");
    assert_eq!(roundtrip("~(+ 1 2)"), "~(+ 1 2)");
}

#[test]
fn test_unquote_splice() {
    assert_eq!(roundtrip("~@xs"), "~@xs");
    assert_eq!(roundtrip("~@(list 1 2)"), "~@(list 1 2)");
}

#[test]
fn test_deref() {
    assert_eq!(roundtrip("@atom"), "@atom");
    assert_eq!(roundtrip("@(atom nil)"), "@(atom nil)");
}

#[test]
fn test_var() {
    assert_eq!(roundtrip("#'inc"), "#'inc");
    assert_eq!(roundtrip("#'clojure.core/inc"), "#'clojure.core/inc");
}

#[test]
fn test_meta_keyword() {
    assert_eq!(roundtrip("^:private x"), "^:private x");
    assert_eq!(roundtrip("^:dynamic *var*"), "^:dynamic *var*");
}

#[test]
fn test_meta_map() {
    assert_eq!(roundtrip("^{:a 1} form"), "^{:a 1} form");
    assert_eq!(roundtrip("^{:tag String} x"), "^{:tag String} x");
}

#[test]
fn test_anon_fn() {
    assert_eq!(roundtrip("#(+ % %2)"), "#(+ % %2)");
    assert_eq!(roundtrip("#(inc %)"), "#(inc %)");
    assert_eq!(roundtrip("#(* %1 %2 %3)"), "#(* %1 %2 %3)");
}

#[test]
fn test_tagged_literal() {
    roundtrip_ast_eq("#inst \"2024-01-01\"");
    roundtrip_ast_eq("#uuid \"550e8400-e29b-41d4-a716-446655440000\"");
    roundtrip_ast_eq("#my/tag {:data 123}");
}

#[test]
fn test_reader_conditional() {
    assert_eq!(roundtrip("#?(:clj x :cljs y)"), "#?(:clj x :cljs y)");
    assert_eq!(roundtrip("#?(:clj 1 :cljs 2 :default 0)"), "#?(:clj 1 :cljs 2 :default 0)");
}

#[test]
fn test_reader_conditional_splicing() {
    assert_eq!(roundtrip("#?@(:clj [1 2])"), "#?@(:clj [1 2])");
}

#[test]
fn test_symbolic_inf() {
    assert_eq!(roundtrip("##Inf"), "##Inf");
}

#[test]
fn test_symbolic_neg_inf() {
    assert_eq!(roundtrip("##-Inf"), "##-Inf");
}

#[test]
fn test_symbolic_nan() {
    assert_eq!(roundtrip("##NaN"), "##NaN");
}

#[test]
fn test_nested_collections() {
    assert_eq!(roundtrip("[[1 2] [3 4]]"), "[[1 2] [3 4]]");
    assert_eq!(roundtrip("{:a {:b {:c 1}}}"), "{:a {:b {:c 1}}}");
    assert_eq!(roundtrip("#{#{1} #{2}}"), "#{#{1} #{2}}");
    assert_eq!(roundtrip("(fn [x] (+ x 1))"), "(fn [x] (+ x 1))");
}

#[test]
fn test_deeply_nested() {
    roundtrip_ast_eq("[:result (+ 1 2)]");
    roundtrip_ast_eq("{:name \"Alice\" :age 30 :roles #{:admin :user}}");
    roundtrip_ast_eq("(let [x 1 y 2] (+ x y))");
}

#[test]
fn test_multiple_forms() {
    assert_eq!(roundtrip("1 2 3"), "1\n2\n3");
    assert_eq!(roundtrip("(ns foo) (defn bar [] 42)"), "(ns foo)\n(defn bar [] 42)");
}

#[test]
fn test_empty_input() {
    assert_eq!(roundtrip(""), "");
    assert_eq!(roundtrip("   "), "");
    assert_eq!(roundtrip("\n\n"), "");
}

#[test]
fn test_whitespace_preserved_in_strings() {
    assert_eq!(roundtrip("\"  spaces  \""), "\"  spaces  \"");
    assert_eq!(roundtrip("\"line1\\nline2\""), "\"line1\\nline2\"");
}

#[test]
fn test_complex_real_world() {
    roundtrip_ast_eq("(defn greet [name] (str \"Hello, \" name \"!\"))");
    roundtrip_ast_eq("(defmacro when [test & body] `(if ~test (do ~@body)))");
    roundtrip_ast_eq("{:db/id 123 :user/name \"Alice\" :user/email \"alice@example.com\"}");
}

#[test]
fn test_all_special_chars() {
    roundtrip_ast_eq("'x `y ~z @a #'b");
    roundtrip_ast_eq("(quote (syntax-quote (unquote (deref x))))");
}

#[test]
fn test_mixed_types_in_collection() {
    roundtrip_ast_eq("[nil true false 42 3.14 1/2 \\a \"str\" :kw sym]");
    roundtrip_ast_eq("{:nil nil :bool true :int 42 :float 3.14 :ratio 1/2}");
}

#[test]
fn test_string_all_escape_sequences() {
    assert_eq!(roundtrip("\"\\n\""), "\"\\n\"");
    assert_eq!(roundtrip("\"\\r\""), "\"\\r\"");
    assert_eq!(roundtrip("\"\\t\""), "\"\\t\"");
    assert_eq!(roundtrip("\"\\\\\""), "\"\\\\\"");
    assert_eq!(roundtrip("\"\\\"\""), "\"\\\"\"");
    roundtrip_ast_eq("\"line1\\nline2\\rline3\\ttab\"");
    roundtrip_ast_eq("\"escaped\\\\backslash\"");
    roundtrip_ast_eq("\"quoted\\\"string\"");
    roundtrip_ast_eq("\"all\\n\\r\\t\\\\\\\"escapes\"");
}

#[test]
fn test_string_unicode_escapes() {
    roundtrip_ast_eq("\"\\u0041\"");
    roundtrip_ast_eq("\"\\u03B1\\u03B2\\u03B3\"");
    roundtrip_ast_eq("\"hello\\u0020world\"");
    roundtrip_ast_eq("\"mix\\u0041and\\nescapes\"");
}

#[test]
fn test_string_empty() {
    assert_eq!(roundtrip("\"\""), "\"\"");
    roundtrip_ast_eq("\"\"");
}

#[test]
fn test_hex_numbers() {
    assert_eq!(roundtrip("0xFF"), "255");
    assert_eq!(roundtrip("0x0"), "0");
    assert_eq!(roundtrip("-0xFF"), "-255");
    assert_eq!(roundtrip("0xABCDEF"), "11259375");
    roundtrip_ast_eq("0x10");
}

#[test]
fn test_octal_numbers() {
    assert_eq!(roundtrip("017"), "15");
    assert_eq!(roundtrip("00"), "0");
    assert_eq!(roundtrip("010"), "8");
    roundtrip_ast_eq("077");
}

#[test]
fn test_radix_numbers() {
    assert_eq!(roundtrip("2r1010"), "10");
    assert_eq!(roundtrip("36rZZ"), "1295");
    assert_eq!(roundtrip("8r77"), "63");
    assert_eq!(roundtrip("16rFF"), "255");
    roundtrip_ast_eq("2r101010");
}

#[test]
fn test_scientific_notation() {
    roundtrip_ast_eq("1e10");
    roundtrip_ast_eq("1.5E-3");
    roundtrip_ast_eq("2.5e+10");
    roundtrip_ast_eq("1E10");
    roundtrip_ast_eq("3.14159e0");
}

#[test]
fn test_negative_floats() {
    roundtrip_ast_eq("-1.5");
    roundtrip_ast_eq("-3.14159");
    roundtrip_ast_eq("-0.001");
    roundtrip_ast_eq("-1e10");
    roundtrip_ast_eq("-1.5E-3");
}

#[test]
fn test_char_backspace() {
    roundtrip_ast_eq("\\backspace");
}

#[test]
fn test_char_formfeed() {
    roundtrip_ast_eq("\\formfeed");
}

#[test]
fn test_char_unicode() {
    roundtrip_ast_eq("\\u0041");
    roundtrip_ast_eq("\\u03B1");
}

#[test]
fn test_symbol_namespaced() {
    assert_eq!(roundtrip("foo/bar"), "foo/bar");
    assert_eq!(roundtrip("my.namespace/my-fn"), "my.namespace/my-fn");
    roundtrip_ast_eq("clojure.core/map");
}

#[test]
fn test_symbol_with_dots() {
    assert_eq!(roundtrip(".method"), ".method");
    assert_eq!(roundtrip("Class."), "Class.");
    assert_eq!(roundtrip(".toString"), ".toString");
    roundtrip_ast_eq("java.lang.String");
}

#[test]
fn test_keyword_namespaced() {
    assert_eq!(roundtrip(":foo/bar"), ":foo/bar");
    assert_eq!(roundtrip(":my.ns/key"), ":my.ns/key");
    roundtrip_ast_eq(":clojure.core/keyword");
}

#[test]
fn test_keyword_auto_resolve() {
    assert_eq!(roundtrip("::foo"), "::foo");
    assert_eq!(roundtrip("::alias/bar"), "::alias/bar");
    roundtrip_ast_eq("::my-keyword");
}

#[test]
fn test_nested_empty_collections() {
    roundtrip_ast_eq("[[]]");
    roundtrip_ast_eq("[{}]");
    roundtrip_ast_eq("[#{}]");
    roundtrip_ast_eq("{:a [] :b {} :c #{}}");
    roundtrip_ast_eq("#{[] {} #{}}");
    roundtrip_ast_eq("[[[[]]]]");
    roundtrip_ast_eq("{{} {}}");
}

#[test]
fn test_map_various_key_types() {
    roundtrip_ast_eq("{:keyword 1}");
    roundtrip_ast_eq("{\"string\" 2}");
    roundtrip_ast_eq("{symbol 3}");
    roundtrip_ast_eq("{42 4}");
    roundtrip_ast_eq("{3.14 5}");
    roundtrip_ast_eq("{[1 2] 6}");
    roundtrip_ast_eq("{{:a 1} 7}");
    roundtrip_ast_eq("{#{1} 8}");
    roundtrip_ast_eq("{nil 9}");
    roundtrip_ast_eq("{true 10 false 11}");
}

#[test]
fn test_expand_gensym() {
    let result = expand_roundtrip("`(let [x# 1] x#)");
    assert!(result.contains("x__"));
    assert!(result.contains("__auto__"));
    let count = result.matches("x__").count();
    assert!(count >= 2, "gensym should appear at least twice");
}

#[test]
fn test_expand_nested_unquote() {
    let result = expand_roundtrip("`(a ~(b ~c))");
    assert!(result.contains("concat"));
    assert!(result.contains("list"));
}

#[test]
fn test_expand_unquote_splice_in_list() {
    let result = expand_roundtrip("`(a ~@xs b)");
    assert!(result.contains("concat"));
    assert!(result.contains("xs"));
}

#[test]
fn test_expand_mixed_unquote_and_splice() {
    let result = expand_roundtrip("`(~a ~@b ~c)");
    assert!(result.contains("concat"));
    assert!(result.contains("seq"));
}

#[test]
fn test_expand_simple_syntax_quote() {
    let result = expand_roundtrip("`foo");
    assert!(result.contains("quote"));
    assert!(result.contains("foo"));
}

#[test]
fn test_expand_syntax_quote_with_list() {
    let result = expand_roundtrip("`(a b c)");
    assert!(result.contains("seq"));
    assert!(result.contains("concat"));
}

#[test]
fn test_expand_syntax_quote_with_vector() {
    let result = expand_roundtrip("`[a b c]");
    assert!(result.contains("apply"));
    assert!(result.contains("vector"));
}

#[test]
fn test_expand_syntax_quote_with_map() {
    let result = expand_roundtrip("`{:a 1 :b 2}");
    assert!(result.contains("apply"));
    assert!(result.contains("hash-map"));
}

#[test]
fn test_expand_syntax_quote_with_set() {
    let result = expand_roundtrip("`#{a b c}");
    assert!(result.contains("apply"));
    assert!(result.contains("hash-set"));
}

#[test]
fn test_expand_unquote_direct() {
    let result = expand_roundtrip("`~x");
    assert_eq!(result.trim(), "x");
}

#[test]
fn test_expand_multiple_gensyms() {
    let result = expand_roundtrip("`(let [x# 1 y# 2] (+ x# y#))");
    assert!(result.contains("x__"));
    assert!(result.contains("y__"));
}

#[test]
fn test_expand_preserves_non_gensym_symbols() {
    let result = expand_roundtrip("`(defn foo [x] x)");
    assert!(result.contains("defn"));
    assert!(result.contains("foo"));
}

#[test]
fn test_positive_hex() {
    assert_eq!(roundtrip("+0xFF"), "255");
}

#[test]
fn test_positive_radix() {
    assert_eq!(roundtrip("+2r1010"), "10");
}

#[test]
fn test_negative_radix() {
    assert_eq!(roundtrip("-2r1010"), "-10");
}

#[test]
fn test_regex_with_escapes() {
    roundtrip_ast_eq("#\"\\d+\"");
    roundtrip_ast_eq("#\"\\s*\"");
    roundtrip_ast_eq("#\"\\w+\"");
    roundtrip_ast_eq("#\"a\\nb\"");
}

#[test]
fn test_regex_empty() {
    assert_eq!(roundtrip("#\"\""), "#\"\"");
}

#[test]
fn test_anon_fn_with_rest_arg() {
    roundtrip_ast_eq("#(apply + %&)");
}

#[test]
fn test_meta_symbol() {
    roundtrip_ast_eq("^String x");
    roundtrip_ast_eq("^{:doc \"docs\"} foo");
}

#[test]
fn test_ratio_negative() {
    assert_eq!(roundtrip("-3/4"), "-3/4");
    roundtrip_ast_eq("-1/2");
}

#[test]
fn test_bigint_large() {
    roundtrip_ast_eq("999999999999999999999999N");
}

#[test]
fn test_bigdecimal_large() {
    roundtrip_ast_eq("3.14159265358979323846M");
}

#[test]
fn test_char_non_ascii() {
    roundtrip_ast_eq("\\u4E2D");
}
