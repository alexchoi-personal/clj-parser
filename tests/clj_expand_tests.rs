#![allow(deprecated)]

use assert_cmd::Command;
use predicates::prelude::*;
use std::fs;
use tempfile::TempDir;

fn cmd() -> Command {
    Command::cargo_bin("clj-expand").unwrap()
}

#[test]
fn stdin_to_stdout_simple_form() {
    cmd()
        .write_stdin("(+ 1 2)")
        .assert()
        .success()
        .stdout("(+ 1 2)\n");
}

#[test]
fn stdin_to_stdout_syntax_quote() {
    cmd()
        .write_stdin("`(a ~b ~@c)")
        .assert()
        .success()
        .stdout(predicate::str::contains("seq"))
        .stdout(predicate::str::contains("concat"));
}

#[test]
fn stdin_to_stdout_multiple_forms() {
    cmd()
        .write_stdin("(def x 1) (def y 2)")
        .assert()
        .success()
        .stdout("(def x 1)\n(def y 2)\n");
}

#[test]
fn stdin_to_stdout_empty_input() {
    cmd().write_stdin("").assert().success().stdout("\n");
}

#[test]
fn read_from_file() {
    let dir = TempDir::new().unwrap();
    let input_path = dir.path().join("input.clj");
    fs::write(&input_path, "(defn foo [x] (+ x 1))").unwrap();

    cmd()
        .arg(&input_path)
        .assert()
        .success()
        .stdout("(defn foo [x] (+ x 1))\n");
}

#[test]
fn read_from_file_syntax_quote() {
    let dir = TempDir::new().unwrap();
    let input_path = dir.path().join("input.clj");
    fs::write(&input_path, "`(list ~x)").unwrap();

    cmd()
        .arg(&input_path)
        .assert()
        .success()
        .stdout(predicate::str::contains("seq"))
        .stdout(predicate::str::contains("concat"));
}

#[test]
fn write_to_output_file() {
    let dir = TempDir::new().unwrap();
    let output_path = dir.path().join("output.clj");

    cmd()
        .args(["-o", output_path.to_str().unwrap()])
        .write_stdin("(+ 1 2 3)")
        .assert()
        .success()
        .stdout("");

    let content = fs::read_to_string(&output_path).unwrap();
    assert_eq!(content, "(+ 1 2 3)\n");
}

#[test]
fn write_to_output_file_long_flag() {
    let dir = TempDir::new().unwrap();
    let output_path = dir.path().join("output.clj");

    cmd()
        .args(["--output", output_path.to_str().unwrap()])
        .write_stdin("(def bar 42)")
        .assert()
        .success();

    let content = fs::read_to_string(&output_path).unwrap();
    assert_eq!(content, "(def bar 42)\n");
}

#[test]
fn read_file_write_to_output_file() {
    let dir = TempDir::new().unwrap();
    let input_path = dir.path().join("input.clj");
    let output_path = dir.path().join("output.clj");
    fs::write(&input_path, "(ns my.namespace)").unwrap();

    cmd()
        .arg(&input_path)
        .args(["-o", output_path.to_str().unwrap()])
        .assert()
        .success();

    let content = fs::read_to_string(&output_path).unwrap();
    assert_eq!(content, "(ns my.namespace)\n");
}

#[test]
fn error_invalid_input_unbalanced_parens() {
    cmd()
        .write_stdin("(+ 1 2")
        .assert()
        .failure()
        .code(1)
        .stderr(predicate::str::contains("Parse error"));
}

#[test]
fn error_invalid_input_unbalanced_bracket() {
    cmd()
        .write_stdin("[1 2 3")
        .assert()
        .failure()
        .code(1)
        .stderr(predicate::str::contains("Parse error"));
}

#[test]
fn error_invalid_input_extra_closing() {
    cmd()
        .write_stdin("(+ 1 2))")
        .assert()
        .failure()
        .code(1)
        .stderr(predicate::str::contains("Parse error"));
}

#[test]
fn error_missing_input_file() {
    cmd()
        .arg("/nonexistent/path/to/file.clj")
        .assert()
        .failure()
        .code(1)
        .stderr(predicate::str::contains("Error reading"));
}

#[test]
fn error_missing_input_file_shows_path() {
    cmd()
        .arg("/some/missing/file.clj")
        .assert()
        .failure()
        .code(1)
        .stderr(predicate::str::contains("/some/missing/file.clj"));
}

#[test]
fn error_output_to_invalid_directory() {
    cmd()
        .args(["-o", "/nonexistent/directory/output.clj"])
        .write_stdin("(+ 1 2)")
        .assert()
        .failure()
        .code(1)
        .stderr(predicate::str::contains("Error writing"));
}

#[test]
fn version_flag() {
    cmd()
        .arg("--version")
        .assert()
        .success()
        .stdout(predicate::str::contains("clj-expand"));
}

#[test]
fn help_flag() {
    cmd()
        .arg("--help")
        .assert()
        .success()
        .stdout(predicate::str::contains("Parse and expand Clojure syntax-quote forms"))
        .stdout(predicate::str::contains("-o"))
        .stdout(predicate::str::contains("--output"));
}

#[test]
fn complex_nested_forms() {
    cmd()
        .write_stdin("(defmacro when [test & body] `(if ~test (do ~@body)))")
        .assert()
        .success()
        .stdout(predicate::str::contains("defmacro"))
        .stdout(predicate::str::contains("when"));
}

#[test]
fn preserves_data_structures() {
    cmd()
        .write_stdin("{:key [1 2 3] :other #{:a :b}}")
        .assert()
        .success()
        .stdout(predicate::str::contains("{"))
        .stdout(predicate::str::contains(":key"))
        .stdout(predicate::str::contains("[1 2 3]"));
}

#[test]
fn handles_strings_with_escapes() {
    cmd()
        .write_stdin(r#"(println "hello\nworld")"#)
        .assert()
        .success()
        .stdout(predicate::str::contains("println"))
        .stdout(predicate::str::contains("hello"));
}

#[test]
fn handles_regex_literals() {
    cmd()
        .write_stdin(r#"#"\d+""#)
        .assert()
        .success();
}

#[test]
fn handles_metadata() {
    cmd()
        .write_stdin("^:private (defn foo [] nil)")
        .assert()
        .success()
        .stdout(predicate::str::contains("defn"));
}

#[test]
fn handles_reader_conditionals() {
    cmd()
        .write_stdin("#?(:clj 1 :cljs 2)")
        .assert()
        .success();
}

#[test]
fn handles_namespaced_keywords() {
    cmd()
        .write_stdin("::keyword :ns/keyword")
        .assert()
        .success()
        .stdout(predicate::str::contains("::keyword"))
        .stdout(predicate::str::contains(":ns/keyword"));
}

#[test]
fn handles_quote_forms() {
    cmd()
        .write_stdin("'(1 2 3)")
        .assert()
        .success()
        .stdout("'(1 2 3)\n");
}

#[test]
fn handles_deref() {
    cmd()
        .write_stdin("@atom")
        .assert()
        .success()
        .stdout("@atom\n");
}

#[test]
fn handles_var_quote() {
    cmd()
        .write_stdin("#'some-var")
        .assert()
        .success()
        .stdout("#'some-var\n");
}

#[test]
fn handles_anonymous_function() {
    cmd()
        .write_stdin("#(+ % 1)")
        .assert()
        .success()
        .stdout("#(+ % 1)\n");
}

#[test]
fn whitespace_and_comments_preserved_semantically() {
    cmd()
        .write_stdin("; comment\n(+ 1 2)")
        .assert()
        .success()
        .stdout("(+ 1 2)\n");
}
