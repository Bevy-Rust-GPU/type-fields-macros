extern crate proc_macro;

mod semigroup;
mod applicative;
mod monad;
mod closure;
mod copointed;
mod functions;
mod functor;
mod monoid;
mod paths;
mod pointed;

use proc_macro::TokenStream;
use syn::parse_macro_input;

/// Create a struct and corresponding `Function` implementation
/// for each function in the  provided trait.
/// Created structs are named by Pascal-casing their function name,
/// and appending F.
/// ex. my_func becomes MyFuncF.
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

/// Derive `Closure<T>` for a type that implements `Function<T>`.
#[proc_macro_derive(Closure)]
pub fn closure(input: TokenStream) -> TokenStream {
    closure::impl_closure(parse_macro_input!(input))
}

/// Derive `Pointed` for a newtype.
#[proc_macro_derive(Pointed)]
pub fn pointed(input: TokenStream) -> TokenStream {
    pointed::impl_pointed(parse_macro_input!(input))
}

/// Derive `Copointed` for a newtype.
#[proc_macro_derive(Copointed)]
pub fn copointed(input: TokenStream) -> TokenStream {
    copointed::impl_copointed(parse_macro_input!(input))
}

/// Derive `Functor` for a newtype.
#[proc_macro_derive(Functor)]
pub fn functor(input: TokenStream) -> TokenStream {
    functor::impl_functor(parse_macro_input!(input))
}

/// Derive `Applicative` for a newtype.
#[proc_macro_derive(Applicative)]
pub fn applicative(input: TokenStream) -> TokenStream {
    applicative::impl_applicative(parse_macro_input!(input))
}

/// Derive `Monad` for a newtype.
#[proc_macro_derive(Monad)]
pub fn monad(input: TokenStream) -> TokenStream {
    monad::impl_monad(parse_macro_input!(input))
}

/// Derive `Monoid` for a newtype.
#[proc_macro_derive(Monoid)]
pub fn monoid(input: TokenStream) -> TokenStream {
    monoid::impl_monoid(parse_macro_input!(input))
}

/// Derive `Semigroup` for a newtype.
#[proc_macro_derive(Semigroup)]
pub fn semigroup(input: TokenStream) -> TokenStream {
    semigroup::impl_semigroup(parse_macro_input!(input))
}

