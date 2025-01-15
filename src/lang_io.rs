use proc_macro2::{Span};
use std::fmt;
use std::path::Path;
use syn::Error;
use std::ops::Range;

/// Wrapper for `syn::Error` with additional source file path and source text for enhanced error reporting.
pub struct SynErrorWrapper<'a> {
    error: Error,
    source_path: &'a Path,
    source_text: &'a [u8],
}

impl<'a> SynErrorWrapper<'a> {
    /// Create a new `SynErrorWrapper` from a single error, source file path, and source text.
    pub fn new(error: Error, source_path: &'a Path, source_text: &'a [u8]) -> Self {
        Self {
            error,
            source_path,
            source_text,
        }
    }

    /// Fetch a snippet of the source text corresponding to the span's byte range.
    fn extract_source_snippet(&self, range: Range<usize>) -> &'a str {
        if range.start < self.source_text.len() && range.end <= self.source_text.len() {
            std::str::from_utf8(&self.source_text[range])
                .unwrap_or("<invalid UTF-8>")
        } else {
            "<out of range>"
        }
    }
}

impl fmt::Display for SynErrorWrapper<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Iterate over all errors, including child errors.
        for error in self.error.clone().into_iter() {
            let span: Span = error.span();
            let start = span.start(); // LineColumn struct
            let line = start.line;
            let column = start.column;

            // Extract the byte range of the span
            let byte_range = span.byte_range();
            let source_snippet = self.extract_source_snippet(byte_range);

            writeln!(
                f,
                "Error: {error}\n{}:{}:{}:\n    {}\n    {}^",
                self.source_path.display(),
                line,
                column,
                source_snippet,
                " ".repeat(column)
            )?;
        }
        Ok(())
    }
}



impl fmt::Debug for SynErrorWrapper<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f) // Use the Display implementation
    }
}

impl std::error::Error for SynErrorWrapper<'_> {}





#[test]
fn test_parse_files_with_errors() {
    // Simulated file paths (for reporting purposes)
    let fake_file1_path = std::path::PathBuf::from("fake_file1.rs");
    let fake_file2_path = std::path::PathBuf::from("fake_file2.rs");

    // Simulated file contents with syntax errors
    let fake_file1_content = r#"
        fn incomplete_function() {
            let x = )
    "#; // Syntax error: Unclosed parenthesis

    let fake_file2_content = r#"
        struct TestStruct {
            field1: u32,
            field2: u32 u32
        }
    "#; // Syntax error: `aync` is not a valid type or keyword

    let mut errors = Vec::new();

    // Parse fake file 1 and intentionally trigger syntax errors
    if let Err(err) = syn::parse_str::<syn::DeriveInput>(fake_file1_content) {
        errors.push(SynErrorWrapper::new(
            err,
            &fake_file1_path,
            fake_file1_content.as_bytes(),
        ));
    }

    // Parse fake file 2 and intentionally trigger syntax errors
    if let Err(err) = syn::parse_str::<syn::DeriveInput>(fake_file2_content) {
        errors.push(SynErrorWrapper::new(
            err,
            &fake_file2_path,
            fake_file2_content.as_bytes(),
        ));
    }

    // Report errors without panicking
    if !errors.is_empty() {
        eprintln!("Errors encountered:");
        for error in &errors {
            eprintln!("{}", error);
        }
    }

    // Fail the test gracefully if there are no errors (indicating the test did not behave as expected)
    assert!(
        !errors.is_empty(),
        "Test encountered no syntax errors. Expected syntax errors to occur."
    );
}
