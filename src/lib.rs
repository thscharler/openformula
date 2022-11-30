//!
//! Builds an open formula parser.
//!
//! ```
//! use iparse::{Parser, Span, Tracer};
//! use iparse::tracer::CTracer;
//! use openformula::parser::Expr;
//!     
//! let trace = CTracer::new();
//! let ast = Expr::parse(&trace, Span::new("1+1")).unwrap();
//! ```

#![warn(absolute_paths_not_starting_with_crate)]
// NO #![warn(box_pointers)]
#![warn(elided_lifetimes_in_paths)]
#![warn(explicit_outlives_requirements)]
#![warn(keyword_idents)]
#![warn(macro_use_extern_crate)]
#![warn(meta_variable_misuse)]
#![warn(missing_abi)]
// NOT_ACCURATE #![warn(missing_copy_implementations)]
// #![warn(missing_debug_implementations)]
#![warn(non_ascii_idents)]
#![warn(noop_method_call)]
// NO #![warn(or_patterns_back_compat)]
#![warn(pointer_structural_match)]
#![warn(semicolon_in_expressions_from_macros)]
// NOT_ACCURATE #![warn(single_use_lifetimes)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unreachable_pub)]
#![allow(unsafe_code)]
#![warn(unsafe_op_in_unsafe_fn)]
#![warn(unstable_features)]
// NO #![warn(unused_crate_dependencies)]
// NO #![warn(unused_extern_crates)]
#![warn(unused_import_braces)]
#![warn(unused_lifetimes)]
#![warn(unused_qualifications)]
// NO #![warn(unused_results)]
#![warn(variant_size_differences)]
#![deny(dead_code)]
#![deny(clippy::needless_lifetimes)]
#![deny(missing_docs)]

extern crate core;

pub mod ast;
pub mod conv;
pub mod error;
pub mod format;
#[allow(missing_docs)]
pub mod parser;
pub mod tokens;
