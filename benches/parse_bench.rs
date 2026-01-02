use bumpalo::Bump;
use clojure_parser::parse;
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

criterion_group!(
    benches,
    bench_simple_forms,
    bench_complex_forms,
    bench_nested_structure,
    bench_many_tokens,
    bench_with_reused_bump
);
criterion_main!(benches);
