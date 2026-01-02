use bumpalo::Bump;
use clojure_parser::{parse, Expander, print_forms};
use criterion::{Criterion, black_box, criterion_group, criterion_main};

const SIMPLE_FORMS: &str = r#"
(def x 42)
(defn foo [a b] (+ a b))
{:key "value" :num 123}
[1 2 3 4 5]
#{:a :b :c}
"#;

const COMPLEX_FORMS: &str = r#"
(ns my.namespace
  (:require [clojure.string :as str]
            [clojure.set :refer [union intersection]])
  (:import [java.util Date UUID]))

(defn process-data
  "Process the given data with various transformations."
  [data & {:keys [filter-fn map-fn reduce-fn]
           :or {filter-fn identity
                map-fn identity
                reduce-fn +}}]
  (->> data
       (filter filter-fn)
       (map map-fn)
       (reduce reduce-fn)))

(def config
  {:db {:host "localhost"
        :port 5432
        :name "mydb"}
   :cache {:ttl 3600
           :max-size 1000}
   :features #{:auth :logging :metrics}})

(defmacro with-timing [name & body]
  `(let [start# (System/nanoTime)
         result# (do ~@body)
         elapsed# (- (System/nanoTime) start#)]
     (println ~name "took" (/ elapsed# 1e6) "ms")
     result#))

#?(:clj (defn platform-fn [] "Clojure")
   :cljs (defn platform-fn [] "ClojureScript"))
"#;

const NESTED_STRUCTURE: &str = r#"
{:level1 {:level2 {:level3 {:level4 {:level5 {:data [1 2 3 4 5]}}}}}}
[[[[[1 2] [3 4]] [[5 6] [7 8]]] [[[9 10] [11 12]] [[13 14] [15 16]]]]]
(((((println "deeply nested")))))
"#;

const SYNTAX_QUOTE_SIMPLE: &str = r#"
`foo
`(a b c)
`[1 2 3]
`{:a 1 :b 2}
"#;

const SYNTAX_QUOTE_COMPLEX: &str = r#"
`(defn ~name [~@args]
  (let [x# (first ~@args)
        y# (second ~@args)]
    {:result (+ x# y#)
     :inputs [~@args]}))
"#;

const SYNTAX_QUOTE_MACRO: &str = r#"
`(let [start# (System/nanoTime)
       result# (do ~@body)
       elapsed# (- (System/nanoTime) start#)]
   (println ~name "took" (/ elapsed# 1e6) "ms")
   result#)
"#;

fn bench_simple_forms(c: &mut Criterion) {
    c.bench_function("parse simple forms", |b| {
        b.iter(|| {
            let bump = Bump::new();
            let result = parse(black_box(SIMPLE_FORMS), &bump);
            black_box(result.is_ok())
        })
    });
}

fn bench_complex_forms(c: &mut Criterion) {
    c.bench_function("parse complex forms", |b| {
        b.iter(|| {
            let bump = Bump::new();
            let result = parse(black_box(COMPLEX_FORMS), &bump);
            black_box(result.is_ok())
        })
    });
}

fn bench_nested_structure(c: &mut Criterion) {
    c.bench_function("parse nested structure", |b| {
        b.iter(|| {
            let bump = Bump::new();
            let result = parse(black_box(NESTED_STRUCTURE), &bump);
            black_box(result.is_ok())
        })
    });
}

fn bench_many_tokens(c: &mut Criterion) {
    let many_symbols: String = (0..1000).map(|i| format!("sym{} ", i)).collect();
    c.bench_function("parse 1000 symbols", |b| {
        b.iter(|| {
            let bump = Bump::new();
            let result = parse(black_box(&many_symbols), &bump);
            black_box(result.is_ok())
        })
    });
}

fn bench_with_reused_bump(c: &mut Criterion) {
    let many_symbols: String = (0..1000).map(|i| format!("sym{} ", i)).collect();
    c.bench_function("parse 1000 symbols (reused bump)", |b| {
        let mut bump = Bump::new();
        b.iter(|| {
            bump.reset();
            let result = parse(black_box(&many_symbols), &bump);
            black_box(result.is_ok())
        })
    });
}

fn bench_expand_simple(c: &mut Criterion) {
    c.bench_function("expand simple syntax-quote", |b| {
        b.iter(|| {
            let bump = Bump::new();
            let forms = parse(black_box(SYNTAX_QUOTE_SIMPLE), &bump).unwrap();
            let mut expander = Expander::new(&bump);
            for f in forms.iter() {
                black_box(expander.expand(&f.value).unwrap());
            }
        })
    });
}

fn bench_expand_complex(c: &mut Criterion) {
    c.bench_function("expand complex syntax-quote", |b| {
        b.iter(|| {
            let bump = Bump::new();
            let forms = parse(black_box(SYNTAX_QUOTE_COMPLEX), &bump).unwrap();
            let mut expander = Expander::new(&bump);
            for f in forms.iter() {
                black_box(expander.expand(&f.value).unwrap());
            }
        })
    });
}

fn bench_expand_macro(c: &mut Criterion) {
    c.bench_function("expand macro-style syntax-quote", |b| {
        b.iter(|| {
            let bump = Bump::new();
            let forms = parse(black_box(SYNTAX_QUOTE_MACRO), &bump).unwrap();
            let mut expander = Expander::new(&bump);
            for f in forms.iter() {
                black_box(expander.expand(&f.value).unwrap());
            }
        })
    });
}

fn bench_print_simple(c: &mut Criterion) {
    let bump = Bump::new();
    let forms = parse(SIMPLE_FORMS, &bump).unwrap();
    let form_refs: Vec<_> = forms.iter().map(|f| &f.value).collect();

    c.bench_function("print simple forms", |b| {
        b.iter(|| {
            black_box(print_forms(form_refs.iter().copied()))
        })
    });
}

fn bench_print_complex(c: &mut Criterion) {
    let bump = Bump::new();
    let forms = parse(COMPLEX_FORMS, &bump).unwrap();
    let form_refs: Vec<_> = forms.iter().map(|f| &f.value).collect();

    c.bench_function("print complex forms", |b| {
        b.iter(|| {
            black_box(print_forms(form_refs.iter().copied()))
        })
    });
}

fn bench_expand_and_print(c: &mut Criterion) {
    c.bench_function("expand + print macro", |b| {
        b.iter(|| {
            let bump = Bump::new();
            let forms = parse(black_box(SYNTAX_QUOTE_MACRO), &bump).unwrap();
            let mut expander = Expander::new(&bump);
            let expanded: Vec<_> = forms.iter()
                .map(|f| expander.expand(&f.value).unwrap())
                .collect();
            black_box(print_forms(expanded.iter()))
        })
    });
}

criterion_group!(
    benches,
    bench_simple_forms,
    bench_complex_forms,
    bench_nested_structure,
    bench_many_tokens,
    bench_with_reused_bump,
    bench_expand_simple,
    bench_expand_complex,
    bench_expand_macro,
    bench_print_simple,
    bench_print_complex,
    bench_expand_and_print
);
criterion_main!(benches);
