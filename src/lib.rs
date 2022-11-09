//!
//! Builds an open formula parser.
//!

#![warn(absolute_paths_not_starting_with_crate)]
// NO #![warn(box_pointers)]
#![warn(elided_lifetimes_in_paths)]
#![warn(explicit_outlives_requirements)]
#![warn(keyword_idents)]
#![warn(macro_use_extern_crate)]
#![warn(meta_variable_misuse)]
#![warn(missing_abi)]
// NOT_ACCURATE #![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(non_ascii_idents)]
#![warn(noop_method_call)]
// NO #![warn(or_patterns_back_compat)]
#![warn(pointer_structural_match)]
#![warn(semicolon_in_expressions_from_macros)]
// NOT_ACCURATE #![warn(single_use_lifetimes)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![allow(unreachable_pub)]
#![allow(unsafe_code)]
#![warn(unsafe_op_in_unsafe_fn)]
#![warn(unstable_features)]
// NO #![warn(unused_crate_dependencies)]
// NO #![warn(unused_extern_crates)]
#![warn(unused_import_braces)]
#![warn(unused_lifetimes)]
#![allow(unused_qualifications)]
// NO #![warn(unused_results)]
#![warn(variant_size_differences)]
#![allow(dead_code)]
#![allow(clippy::needless_lifetimes)]

mod ast;
mod dbg_ast;
mod error;
mod parse;
mod refs;

pub use ast::*;
pub use error::*;
pub use parse::*;
pub use refs::*;

/// Converts into a format that can be used in a formula.
pub trait ToFormula {
    /// Converts into a format that can be used in a formula.
    fn to_formula(&self) -> Result<String, std::fmt::Error>;
}
