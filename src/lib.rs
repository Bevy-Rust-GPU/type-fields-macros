extern crate proc_macro;

mod semigroup;
mod applicative;
mod monad;
mod closure;
mod copointed;
mod functions;
mod fmap;
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

/// Derive `Fmap` for a newtype.
#[proc_macro_derive(Fmap)]
pub fn fmap(input: TokenStream) -> TokenStream {
    fmap::impl_fmap(parse_macro_input!(input))
}

/// Derive `Apply` for a newtype.
#[proc_macro_derive(Apply)]
pub fn apply(input: TokenStream) -> TokenStream {
    applicative::impl_apply(parse_macro_input!(input))
}

/// Derive `Chain` for a newtype.
#[proc_macro_derive(Chain)]
pub fn chain(input: TokenStream) -> TokenStream {
    monad::impl_chain(parse_macro_input!(input))
}

/// Derive `Mempty` for a newtype.
#[proc_macro_derive(Mempty)]
pub fn mempty(input: TokenStream) -> TokenStream {
    monoid::impl_mempty(parse_macro_input!(input))
}

/// Derive `Mappend` for a newtype.
#[proc_macro_derive(Mappend)]
pub fn mappend(input: TokenStream) -> TokenStream {
    semigroup::impl_mappend(parse_macro_input!(input))
}

