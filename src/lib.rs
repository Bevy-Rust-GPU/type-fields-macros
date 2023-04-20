extern crate proc_macro;

mod closure;
mod copointed;
mod functions;
mod paths;
mod pointed;

use proc_macro::TokenStream;
use syn::parse_macro_input;

macro_rules! newtype_derive {
    ($derive:ident :: $fn:ident (#$ident:ident, #$ty:ident) => { $($tt:tt)* }) => {
        #[proc_macro_derive($derive)]
        pub fn $fn(input: TokenStream) -> TokenStream {
            let input: syn::DeriveInput = parse_macro_input!(input);
            let $ident = input.ident;

            let $ty = input
                .generics
                .type_params()
                .map(|type_param| type_param.ident.clone())
                .next()
                .unwrap();

            let out = quote::quote!(
                $($tt)*
            );

            //panic!("{out:}");

            out.into()
        }
    }
}

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

// Derive `Fmap` for a newtype.
newtype_derive! {
    Fmap::fmap(#ident, #ty) => {
        impl<#ty, _Function> type_fields::t_funk::Fmap<_Function> for #ident<#ty>
        where
            _Function: type_fields::t_funk::Closure<#ty>,
        {
            type Fmap = #ident<_Function::Output>;

            fn fmap(self, f: _Function) -> Self::Fmap {
                type_fields::t_funk::Pointed::point(f.call(type_fields::t_funk::Copointed::copoint(self)))
            }
        }
    }
}

// Derive `Apply` for a newtype.
newtype_derive! {
    Apply::apply(#ident, #ty) => {
        impl<#ty, _Value> type_fields::t_funk::Apply<#ident<_Value>> for #ident<#ty>
        where
            #ty: type_fields::t_funk::Closure<_Value>,
        {
            type Apply = #ident<#ty::Output>;

            fn apply(self, a: #ident<_Value>) -> Self::Apply
            where
                #ty: type_fields::t_funk::Closure<_Value>,
            {
                type_fields::t_funk::Pointed::point(
                    type_fields::t_funk::Copointed::copoint(self)
                        .call(type_fields::t_funk::Copointed::copoint(a)),
                )
            }
        }
    }
}

// Derive `Chain` for a newtype.
newtype_derive! {
    Chain::chain(#ident, #ty) => {
        impl<#ty, _Function> type_fields::t_funk::Chain<_Function> for #ident<#ty>
        where
            _Function: type_fields::t_funk::Closure<#ty>,
        {
            type Chain = _Function::Output;

            fn chain(self, f: _Function) -> Self::Chain {
                f.call(type_fields::t_funk::Copointed::copoint(self))
            }
        }
    }
}

// Derive `Mempty` for a newtype.
newtype_derive! {
    Mempty::mempty(#ident, #ty) => {
        impl<#ty> type_fields::t_funk::Mempty for #ident<#ty>
        where
            #ty: type_fields::t_funk::Mempty,
        {
            type Mempty = #ident<#ty::Mempty>;

            fn mempty() -> Self::Mempty {
                type_fields::t_funk::Pointed::point(#ty::mempty())
            }
        }
    }
}

// Derive `Mappend` for a newtype.
newtype_derive! {
    Mappend::mappend(#ident, #ty) => {
        impl<#ty, _Type> type_fields::t_funk::Mappend<#ident<_Type>> for #ident<#ty>
        where
            #ty: type_fields::t_funk::Mappend<_Type>,
        {
            type Mappend = #ident<#ty::Mappend>;

            fn mappend(self, t: #ident<_Type>) -> Self::Mappend {
                type_fields::t_funk::Pointed::point(
                    self.0.mappend(type_fields::t_funk::Copointed::copoint(t)),
                )
            }
        }
    }
}
