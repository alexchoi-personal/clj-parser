use bumpalo::Bump;
use clojure_parser::parse;
use futures::stream::{self, StreamExt};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use std::time::Instant;
use tokio::fs;

async fn find_clj_files(dir: &Path) -> Vec<PathBuf> {
    let mut files = Vec::new();
    let mut stack = vec![dir.to_path_buf()];

    while let Some(current_dir) = stack.pop() {
        let mut entries = match fs::read_dir(&current_dir).await {
            Ok(e) => e,
            Err(_) => continue,
        };

        while let Ok(Some(entry)) = entries.next_entry().await {
            let path = entry.path();
            if path.is_dir() {
                stack.push(path);
            } else if let Some(ext) = path.extension() {
                if ext == "clj" || ext == "cljs" || ext == "cljc" {
                    files.push(path);
                }
            }
        }
    }

    files.sort();
    files
}

struct ParseResult {
    file: PathBuf,
    forms: usize,
    bytes: usize,
    error: Option<String>,
}

async fn parse_file(path: PathBuf) -> ParseResult {
    match fs::read_to_string(&path).await {
        Ok(content) => {
            let bytes = content.len();
            let bump = Bump::new();
            match parse(&content, &bump) {
                Ok(forms) => ParseResult {
                    file: path,
                    forms: forms.len(),
                    bytes,
                    error: None,
                },
                Err(e) => ParseResult {
                    file: path,
                    forms: 0,
                    bytes,
                    error: Some(format!("{}", e)),
                },
            }
        }
        Err(e) => ParseResult {
            file: path,
            forms: 0,
            bytes: 0,
            error: Some(format!("Read error: {}", e)),
        },
    }
}

#[tokio::main]
async fn main() {
    let args: Vec<String> = std::env::args().collect();
    let dir = if args.len() > 1 { &args[1] } else { "." };
    let concurrency: usize = args
        .get(2)
        .and_then(|s| s.parse().ok())
        .unwrap_or_else(num_cpus);

    println!("Scanning for Clojure files in {}...", dir);
    let files = find_clj_files(Path::new(dir)).await;
    println!("Found {} Clojure files", files.len());
    println!("Using {} concurrent tasks", concurrency);
    println!();

    let start = Instant::now();
    let file_count = files.len();

    let progress = Arc::new(AtomicUsize::new(0));
    let progress_clone = progress.clone();

    let results: Vec<ParseResult> = stream::iter(files)
        .map(|path| {
            let progress = progress_clone.clone();
            async move {
                let result = parse_file(path).await;
                let done = progress.fetch_add(1, Ordering::Relaxed) + 1;
                if done % 20 == 0 || done == file_count {
                    eprint!("\rParsing... {}/{}", done, file_count);
                }
                result
            }
        })
        .buffer_unordered(concurrency)
        .collect()
        .await;

    eprintln!();

    let elapsed = start.elapsed();

    let mut success = 0;
    let mut failed = 0;
    let mut total_forms = 0;
    let mut total_bytes = 0;
    let mut errors: Vec<(PathBuf, String)> = Vec::new();

    for result in results {
        total_bytes += result.bytes;
        if let Some(err) = result.error {
            failed += 1;
            errors.push((result.file, err));
        } else {
            success += 1;
            total_forms += result.forms;
        }
    }

    println!();
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
            println!("  {}", file.display());
            println!("    {}", error);
            println!();
        }
    }
}

fn num_cpus() -> usize {
    std::thread::available_parallelism()
        .map(|n| n.get())
        .unwrap_or(4)
}
