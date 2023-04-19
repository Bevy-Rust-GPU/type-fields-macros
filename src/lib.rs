extern crate proc_macro;

mod paths;
mod functions;

use proc_macro::TokenStream;
use syn::parse_macro_input;

/// Create a struct and corresponding `Function` implementation
/// for each function in the  provided trait.
#[proc_macro_attribute]
pub fn functions(_: TokenStream, input: TokenStream) -> TokenStream {
    functions::impl_functions(parse_macro_input!(input))
}

/// Implements a same-named mutable getter for each public named field in a given struct.
/// Enables the `Struct::field` syntax used to compose a path from function pointers.
#[proc_macro_derive(Field)]
pub fn paths(input: TokenStream) -> TokenStream {
    paths::impl_paths(parse_macro_input!(input))
}

