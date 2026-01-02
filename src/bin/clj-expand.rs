use clap::Parser;
use clojure_parser::{parse, print_forms, Bump, Expander};
use std::fs;
use std::io::{self, Read, Write};
use std::path::PathBuf;
use std::process;

#[derive(Parser)]
#[command(name = "clj-expand")]
#[command(about = "Parse and expand Clojure syntax-quote forms")]
#[command(version)]
struct Args {
    #[arg(help = "Input file (reads from stdin if not provided)")]
    input: Option<PathBuf>,

    #[arg(short, long, help = "Output file (writes to stdout if not provided)")]
    output: Option<PathBuf>,
}

fn main() {
    let args = Args::parse();

    let source = match &args.input {
        Some(path) => match fs::read_to_string(path) {
            Ok(content) => content,
            Err(e) => {
                eprintln!("Error reading {}: {e}", path.display());
                process::exit(1);
            }
        },
        None => {
            let mut buf = String::new();
            if let Err(e) = io::stdin().read_to_string(&mut buf) {
                eprintln!("Error reading stdin: {e}");
                process::exit(1);
            }
            buf
        }
    };

    let bump = Bump::new();
    let forms = match parse(&source, &bump) {
        Ok(forms) => forms,
        Err(e) => {
            eprintln!("Parse error: {e}");
            process::exit(1);
        }
    };

    // Expand syntax-quote forms
    let mut expander = Expander::new(&bump);
    let mut expanded = Vec::new();
    for spanned in forms.iter() {
        match expander.expand(&spanned.value) {
            Ok(form) => expanded.push(form),
            Err(e) => {
                eprintln!("Expand error: {e}");
                process::exit(1);
            }
        }
    }

    let output = print_forms(expanded.iter()) + "\n";

    match &args.output {
        Some(path) => {
            if let Err(e) = fs::write(path, &output) {
                eprintln!("Error writing {}: {e}", path.display());
                process::exit(1);
            }
        }
        None => {
            if let Err(e) = io::stdout().write_all(output.as_bytes()) {
                eprintln!("Error writing to stdout: {e}");
                process::exit(1);
            }
        }
    }
}
