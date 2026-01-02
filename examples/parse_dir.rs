use bumpalo::Bump;
use clojure_parser::parse;
use std::env;
use std::fs;
use std::path::Path;
use std::time::Instant;

fn find_clj_files(dir: &Path, files: &mut Vec<String>) {
    if let Ok(entries) = fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                find_clj_files(&path, files);
            } else if let Some(ext) = path.extension() {
                if ext == "clj" || ext == "cljs" || ext == "cljc" {
                    if let Some(s) = path.to_str() {
                        files.push(s.to_string());
                    }
                }
            }
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let dir = if args.len() > 1 { &args[1] } else { "." };

    let mut files = Vec::new();
    find_clj_files(Path::new(dir), &mut files);
    files.sort();

    println!("Found {} Clojure files in {}", files.len(), dir);
    println!();

    let mut success = 0;
    let mut failed = 0;
    let mut total_forms = 0;
    let mut total_bytes = 0;
    let mut errors: Vec<(String, String)> = Vec::new();

    let start = Instant::now();

    for file in &files {
        match fs::read_to_string(file) {
            Ok(content) => {
                total_bytes += content.len();
                let bump = Bump::new();
                match parse(&content, &bump) {
                    Ok(forms) => {
                        success += 1;
                        total_forms += forms.len();
                    }
                    Err(e) => {
                        failed += 1;
                        errors.push((file.clone(), format!("{}", e)));
                    }
                }
            }
            Err(e) => {
                failed += 1;
                errors.push((file.clone(), format!("Read error: {}", e)));
            }
        }
    }

    let elapsed = start.elapsed();

    println!("Results:");
    println!("  Parsed: {} files ({} forms)", success, total_forms);
    println!("  Failed: {} files", failed);
    println!("  Total:  {} bytes", total_bytes);
    println!("  Time:   {:?}", elapsed);
    println!(
        "  Speed:  {:.2} MB/s",
        (total_bytes as f64 / 1_000_000.0) / elapsed.as_secs_f64()
    );

    if !errors.is_empty() {
        println!();
        println!("Errors:");
        for (file, error) in &errors {
            println!("  {}", file);
            println!("    {}", error);
            println!();
        }
    }
}
