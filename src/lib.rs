extern crate proc_macro;

mod closure;
mod copointed;
mod functions;
mod lenses;
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
                .collect::<Vec<_>>();

            let $ty = quote::quote!(#(#$ty),*);

            let out = quote::quote!(
                $($tt)*
            );

            //panic!("{out:}");

            out.into()
        }
    }
}

/// Implements a same-named mutable getter for each public named field in a given struct.
/// Enables the `Struct::field` syntax used to compose a path from function pointers.
#[proc_macro_derive(Field)]
pub fn paths(input: TokenStream) -> TokenStream {
    paths::impl_paths(parse_macro_input!(input))
}

/// For each field in the annotated struct:
/// - Implement getter / setter Function types
/// - Define a const representing the lens over said getter / setter
#[proc_macro_derive(Lenses)]
pub fn lenses(input: TokenStream) -> TokenStream {
    lenses::impl_lenses(parse_macro_input!(input))
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

// Derive `Replace` for a newtype.
newtype_derive! {
    Replace::replace(#ident, #ty) => {
        impl<#ty, U> type_fields::t_funk::Replace<U> for #ident<#ty>
        where
            #ident<#ty>: type_fields::t_funk::Fmap<type_fields::t_funk::CurriedA<type_fields::t_funk::function::Const, U>>,
        {
            type Replace = <#ident<#ty> as type_fields::t_funk::Fmap<type_fields::t_funk::CurriedA<type_fields::t_funk::function::Const, U>>>::Fmap;

            fn replace(self, t: U) -> Self::Replace {
                type_fields::t_funk::Fmap::fmap(self, type_fields::t_funk::Curry::curry_a(type_fields::t_funk::function::Const, t))
            }
        }
    }
}

// Derive `Apply` for a newtype.
newtype_derive! {
    Pure::pure(#ident, #ty) => {
        impl<#ty> type_fields::t_funk::Pure for #ident<#ty>
        {
            type Pure<U> = #ident<U>;

            fn pure<U>(t: U) -> Self::Pure<U> {
                #ident(t)
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

// Derive `Then` for a newtype.
newtype_derive! {
    Then::then(#ident, #ty) => {
        impl<#ty, _Function> type_fields::t_funk::Then<_Function> for #ident<#ty>
        where
            #ident<#ty>: type_fields::t_funk::Replace<type_fields::t_funk::function::Id>,
            <#ident<#ty> as type_fields::t_funk::Replace<type_fields::t_funk::function::Id>>::Replace: type_fields::t_funk::Apply<_Function>,
        {
            type Then = <<#ident<#ty> as type_fields::t_funk::Replace<type_fields::t_funk::function::Id>>::Replace as type_fields::t_funk::Apply<_Function>>::Apply;

            fn then(self, f: _Function) -> Self::Then {
               type_fields::t_funk::Apply::apply(type_fields::t_funk::Replace::replace(self, type_fields::t_funk::function::Id), f)
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
                    type_fields::t_funk::Copointed::copoint(self)
                        .mappend(type_fields::t_funk::Copointed::copoint(t)),
                )
            }
        }

        impl<#ty> type_fields::t_funk::Mappend<()> for #ident<#ty>
        {
            type Mappend = #ident<#ty>;

            fn mappend(self, _: ()) -> Self::Mappend {
                self
            }
        }
    }
}

// Derive `Mconcat` for a newtype.
newtype_derive! {
    Mconcat::mconcat(#ident, #ty) => {
        impl<T> type_fields::t_funk::Mconcat for #ident<#ty>
        where
            T: type_fields::t_funk::Mempty + type_fields::t_funk::Foldr<type_fields::t_funk::MappendF, <#ident<#ty> as type_fields::t_funk::Mempty>::Mempty>,
        {
            type Mconcat = <#ident<#ty> as type_fields::t_funk::Foldr<type_fields::t_funk::MappendF, <#ident<#ty> as type_fields::t_funk::Mempty>::Mempty>>::Foldr;

            fn mconcat(self) -> Self::Mconcat {
                type_fields::t_funk::Foldr::foldr(self, type_fields::t_funk::MappendF::default(), <#ident<#ty> as type_fields::t_funk::Mempty>::mempty())
            }
        }
    }
}

// Derive `FoldMap` for a newtype.
newtype_derive! {
    FoldMap::fold_map(#ident, #ty) => {
        impl<#ty, _Function> type_fields::t_funk::FoldMap<_Function> for #ident<#ty>
        where
            #ident<#ty>: type_fields::t_funk::Fmap<_Function>,
            <#ident<#ty> as type_fields::t_funk::Fmap<_Function>>::Fmap: type_fields::t_funk::Mconcat,
        {
            type FoldMap = <<#ident<#ty> as type_fields::t_funk::Fmap<_Function>>::Fmap as type_fields::t_funk::Mconcat>::Mconcat;

            fn fold_map(self, f: _Function) -> Self::FoldMap {
                type_fields::t_funk::Mconcat::mconcat(type_fields::t_funk::Fmap::fmap(self, f))
            }
        }
    }
}

// Derive `Foldr` for a newtype.
newtype_derive! {
    Foldr::foldr(#ident, #ty) => {
        impl<#ty, _Function, _Acc> type_fields::t_funk::Foldr<_Function, _Acc> for #ident<#ty>
        where
            #ty: type_fields::t_funk::Foldr<_Function, _Acc>,
        {
            type Foldr = #ident<<#ty as type_fields::t_funk::Foldr<_Function, _Acc>>::Foldr>;

            fn foldr(self, f: _Function, z: _Acc) -> Self::Foldr {
                type_fields::t_funk::Pointed::point(
                    type_fields::t_funk::Foldr::foldr(
                        type_fields::t_funk::Copointed::copoint(self),
                        f,
                        z
                    )
                )
            }
        }
    }
}

// Derive `Foldl` for a newtype.
newtype_derive! {
    Foldl::foldl(#ident, #ty) => {
        impl<#ty, _Function, _Acc> type_fields::t_funk::Foldl<_Function, _Acc> for #ident<#ty>
        where
            #ty: type_fields::t_funk::Foldl<_Function, _Acc>,
        {
            type Foldl = #ident<<#ty as type_fields::t_funk::Foldl<_Function, _Acc>>::Foldl>;

            fn foldl(self, f: _Function, z: _Acc) -> Self::Foldl {
                type_fields::t_funk::Pointed::point(
                    type_fields::t_funk::Foldl::foldl(
                        type_fields::t_funk::Copointed::copoint(self),
                        f,
                        z
                    )
                )
            }
        }
    }
}

// Derive `Fold` for a newtype.
newtype_derive! {
    Fold::fold(#ident, #ty) => {
        impl<#ty> type_fields::t_funk::Fold for #ident<#ty>
        where
            #ident<#ty>: type_fields::t_funk::FoldMap<type_fields::t_funk::function::Id>,
        {
            type Fold = <#ident<#ty> as type_fields::t_funk::FoldMap<type_fields::t_funk::function::Id>>::FoldMap;

            fn fold(self) -> Self::Fold {
                type_fields::t_funk::FoldMap::<type_fields::t_funk::function::Id>::fold_map(self, type_fields::t_funk::function::Id)
            }
        }
    }
}

// Derive `Id` for a `Function`.
newtype_derive! {
    Id::id(#ident, #ty) => {
        impl<#ty> type_fields::t_funk::category::Id for #ident<#ty> {
            type Id = Self;

            fn id(self) -> Self::Id {
                self
            }
        }
    }
}

// Derive `Compose` for a `Function`.
newtype_derive! {
    Compose::compose(#ident, #ty) => {
        impl<_Function, #ty> type_fields::t_funk::Compose<_Function> for #ident<#ty> {
            fn compose(self, f: _Function) -> type_fields::t_funk::Composed<Self, _Function> {
                type_fields::t_funk::Pointed::point((self, f))
            }
        }
    }
}

// Derive `First` for a `Function`.
newtype_derive! {
    First::first(#ident, #ty) => {
        impl<#ty> type_fields::t_funk::arrow::First for #ident<#ty>
        where
            #ident<#ty>: type_fields::t_funk::Split<type_fields::t_funk::function::Id>,
        {
            type First = <#ident<#ty> as type_fields::t_funk::Split<type_fields::t_funk::function::Id>>::Split;

            fn first(self) -> Self::First {
                type_fields::t_funk::Split::split(self, type_fields::t_funk::function::Id)
            }
        }
    }
}

// Derive `Second` for a `Function`.
newtype_derive! {
    Second::second(#ident, #ty) => {
        impl<#ty> type_fields::t_funk::arrow::Second for #ident<#ty>
        where
            type_fields::t_funk::function::Id: type_fields::t_funk::Split<#ident<#ty>>,
        {
            type Second = <type_fields::t_funk::function::Id as type_fields::t_funk::Split<#ident<#ty>>>::Split;

            fn second(self) -> Self::Second {
                type_fields::t_funk::Split::split(type_fields::t_funk::function::Id, self)
            }
        }
    }
}
