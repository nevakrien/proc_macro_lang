use core_engine::proc_macro2;
use core_engine::syn;

use proc_macro2::Span;
use std::fmt;
use std::path::Path;
use syn::Error;

pub struct LineMap<'a> {
    pub lines: Vec<&'a str>,
}

impl<'a> LineMap<'a> {
    pub fn new(text: &'a str) -> Self {
        LineMap {
            lines: text.lines().collect(),
        }
    }

    pub fn get_line(&self, i: usize) -> Option<&'a str> {
        self.lines.get(i).copied()
    }
}

/// Wrapper for `syn::Error` with additional source info for enhanced error reporting.
/// this type is only used when we parse tokens outside of a proc_macro.
pub struct SynErrorWrapper<'a, 'b> {
    pub error: Error,
    pub source_path: &'a Path,
    pub lines: &'b LineMap<'a>,
}

impl<'a, 'b> SynErrorWrapper<'a, 'b> {
    /// Create a new `SynErrorWrapper` from a single error, source file path, and source text.
    pub fn new(error: Error, source_path: &'a Path, lines: &'b LineMap<'a>) -> Self {
        Self {
            error,
            source_path,
            lines,
        }
    }
}
impl fmt::Display for SynErrorWrapper<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Iterate over all errors, including child errors.
        for error in self.error.clone().into_iter() {
            let span: Span = error.span();
            let start = span.start(); // LineColumn struct
            let line = start.line;
            let column = start.column;

            // Retrieve the entire line containing the error
            let source_line = self
                .lines
                .get_line(line.saturating_sub(1)) // Adjust for 0-based index
                .unwrap_or("<source line unavailable>");

            writeln!(
                f,
                "Error: {error}\n{}:{}:{}:\n{}\n{}^",
                self.source_path.display(),
                line,
                column,
                source_line,
                " ".repeat(column)
            )?;
        }
        Ok(())
    }
}

impl fmt::Debug for SynErrorWrapper<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f) // Use the Display implementation
    }
}

impl std::error::Error for SynErrorWrapper<'_, '_> {}

#[test]
fn test_parse_files_with_errors() {
    // Simulated file paths (for reporting purposes)
    let fake_file1_path = std::path::Path::new("fake_file1.rs");
    let fake_file2_path = std::path::Path::new("fake_file2.rs");

    // Simulated file contents with syntax errors
    let fake_file1_content = r#"
        fn incomplete_function() {
            let x = )
            ();
            a
            )
    "#; // Syntax error: Unclosed parenthesis

    let fake_file2_content = r#"
        struct TestStruct {
            field1: u32,
            field2: u32 u32
            field2: u1 u32
        }
    "#; // Syntax error: Repeated type

    // Create LineMaps for the source files
    let lines1 = LineMap::new(fake_file1_content);
    let lines2 = LineMap::new(fake_file2_content);

    let mut errors = Vec::new();

    // Parse fake file 1 and intentionally trigger syntax errors
    if let Err(err) = syn::parse_str::<syn::DeriveInput>(fake_file1_content) {
        errors.push(SynErrorWrapper::new(err, fake_file1_path, &lines1));
    }

    // Parse fake file 2 and intentionally trigger syntax errors
    if let Err(err) = syn::parse_str::<syn::DeriveInput>(fake_file2_content) {
        errors.push(SynErrorWrapper::new(err, fake_file2_path, &lines2));
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
