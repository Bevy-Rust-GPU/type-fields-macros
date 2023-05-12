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

#[proc_macro_derive(Functor)]
pub fn functor(input: TokenStream) -> TokenStream {
    let fmap = fmap(input.clone());
    let replace = replace(input);
    fmap.into_iter().chain(replace.into_iter()).collect()
}

// Derive `Fmap` for a newtype.
newtype_derive! {
    Fmap::fmap(#ident, #ty) => {
        impl<_Function, #ty> type_fields::t_funk::Fmap<_Function> for #ident<#ty>
        where
            _Function: type_fields::t_funk::Closure<#ty>,
        {
            type Fmap = #ident<_Function::Output>;

            #[allow(non_snake_case)]
            fn fmap(self, f: _Function) -> Self::Fmap {
                let #ident(#ty) = self;
                #ident(f.call(#ty))
            }
        }
    }
}

// Derive `Replace` for a newtype.
newtype_derive! {
    Replace::replace(#ident, #ty) => {
        impl<#ty, U> type_fields::t_funk::Replace<U> for #ident<#ty>
        where
            #ident<#ty>: type_fields::t_funk::Fmap<type_fields::t_funk::Curry2A<type_fields::t_funk::function::Const, U>>,
        {
            type Replace = <#ident<#ty> as type_fields::t_funk::Fmap<type_fields::t_funk::Curry2A<type_fields::t_funk::function::Const, U>>>::Fmap;

            fn replace(self, t: U) -> Self::Replace {
                type_fields::t_funk::Fmap::fmap(self, type_fields::t_funk::Curry2::prefix2(type_fields::t_funk::function::Const, t))
            }
        }
    }
}

#[proc_macro_derive(Applicative)]
pub fn applicative(input: TokenStream) -> TokenStream {
    let pure = pure(input.clone());
    let apply = apply(input);
    pure.into_iter().chain(apply.into_iter()).collect()
}

// Derive `Pure` for a newtype.
newtype_derive! {
    Pure::pure(#ident, #ty) => {
        impl<#ty> type_fields::t_funk::applicative::Pure for #ident<#ty>
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

            #[allow(non_snake_case)]
            fn apply(self, a: #ident<_Value>) -> Self::Apply
            where
                #ty: type_fields::t_funk::Closure<_Value>,
            {
                let #ident(#ty) = self;
                let #ident(_Value) = a;
                #ident(
                    #ty.call(_Value),
                )
            }
        }
    }
}

#[proc_macro_derive(Monad)]
pub fn monad(input: TokenStream) -> TokenStream {
    let chain = chain(input.clone());
    let then = then(input);
    chain.into_iter().chain(then.into_iter()).collect()
}

// Derive `Chain` for a newtype.
newtype_derive! {
    Chain::chain(#ident, #ty) => {
        impl<#ty, _Function> type_fields::t_funk::monad::Chain<_Function> for #ident<#ty>
        where
            _Function: type_fields::t_funk::Closure<#ty>,
        {
            type Chain = _Function::Output;

            #[allow(non_snake_case)]
            fn chain(self, f: _Function) -> Self::Chain {
                let #ident(#ty) = self;
                f.call(#ty)
            }
        }
    }
}

// Derive `Then` for a newtype.
/*
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
*/
newtype_derive! {
    Then::then(#ident, #ty) => {
        impl<#ty, _Function> type_fields::t_funk::Then<_Function> for #ident<#ty> where #ident<#ty>: type_fields::t_funk::monad::Chain<type_fields::t_funk::Curry2A<type_fields::t_funk::function::Const, _Function>>
        {
            type Then = <#ident<#ty> as type_fields::t_funk::monad::Chain<type_fields::t_funk::Curry2A<type_fields::t_funk::function::Const, _Function>>>::Chain;

            fn then(self, f: _Function) -> Self::Then {
               type_fields::t_funk::monad::Chain::<_>::chain(self, type_fields::t_funk::Curry2::prefix2(type_fields::t_funk::function::Const, f))
            }
        }
    }
}

#[proc_macro_derive(Semigroup)]
pub fn semigroup(input: TokenStream) -> TokenStream {
    let mappend = mappend(input.clone());
    mappend.into()
}

// Derive `Mappend` for a newtype.
newtype_derive! {
    Mappend::mappend(#ident, #ty) => {
        impl<#ty, _Type> type_fields::t_funk::Mappend<#ident<_Type>> for #ident<#ty>
        where
            #ty: type_fields::t_funk::Mappend<_Type>,
        {
            type Mappend = #ident<#ty::Mappend>;

            #[allow(non_snake_case)]
            fn mappend(self, t: #ident<_Type>) -> Self::Mappend {
                let #ident(#ty) = self;
                let #ident(_Type) = t;
                #ident(
                    #ty.mappend(_Type),
                )
            }
        }

        impl<#ty> type_fields::t_funk::Mappend<type_fields::t_funk::list::hlist::Nil> for #ident<#ty>
        {
            type Mappend = #ident<#ty>;

            fn mappend(self, _: type_fields::t_funk::list::hlist::Nil) -> Self::Mappend {
                self
            }
        }
    }
}

#[proc_macro_derive(Monoid)]
pub fn monoid(input: TokenStream) -> TokenStream {
    let mempty = mempty(input.clone());
    let mconcat = mconcat(input.clone());
    mempty.into_iter().chain(mconcat.into_iter()).collect()
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
                #ident(#ty::mempty())
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

#[proc_macro_derive(Foldable)]
pub fn foldable(input: TokenStream) -> TokenStream {
    let fold_map = fold_map(input.clone());
    let foldl = foldl(input.clone());
    let foldr = foldr(input);
    fold_map
        .into_iter()
        .chain(foldl.into_iter())
        .chain(foldr.into_iter())
        .collect()
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

            #[allow(non_snake_case)]
            fn foldr(self, f: _Function, z: _Acc) -> Self::Foldr {
                let #ident(#ty) = self;
                #ident(
                    type_fields::t_funk::Foldr::foldr(
                        #ty,
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

            #[allow(non_snake_case)]
            fn foldl(self, f: _Function, z: _Acc) -> Self::Foldl {
                let #ident(#ty) = self;
                #ident(
                    type_fields::t_funk::Foldl::foldl(
                        #ty,
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

#[proc_macro_derive(Category)]
pub fn category(input: TokenStream) -> TokenStream {
    let id = id(input.clone());
    let compose = compose(input.clone());
    id.into_iter().chain(compose.into_iter()).collect()
}

// Derive `Id` for a `Function`.
newtype_derive! {
    Id::id(#ident, #ty) => {
        impl<#ty> type_fields::t_funk::category::Id for #ident<#ty> where #ident<#ty>: Default {
            type Id = type_fields::t_funk::function::Id;

            fn id() -> Self::Id {
                type_fields::t_funk::function::Id
            }
        }
    }
}

// Derive `Compose` for a `Function`.
newtype_derive! {
    Compose::compose(#ident, #ty) => {
        impl<_Function, #ty> type_fields::t_funk::category::Compose<_Function> for #ident<#ty> {
            type Compose = type_fields::t_funk::Composed<Self, _Function>;
            fn compose(self, f: _Function) -> Self::Compose {
                type_fields::t_funk::Composed(self, f)
            }
        }
    }
}

#[proc_macro_derive(Arrow)]
pub fn arrow(input: TokenStream) -> TokenStream {
    let arr = arr(input.clone());
    let split = split(input.clone());
    let fanout = fanout(input.clone());
    let first = arrow_first(input.clone());
    let second = arrow_second(input.clone());
    arr.into_iter()
        .chain(split.into_iter())
        .chain(fanout.into_iter())
        .chain(first.into_iter())
        .chain(second.into_iter())
        .collect()
}

// Derive `Arr` for a `Function`.
newtype_derive! {
    Arr::arr(#ident, #ty) => {
        impl<_Function, #ty> type_fields::t_funk::arrow::Arr<_Function> for #ident<#ty>
        {
            type Arr = _Function;

            fn arr(f: _Function) -> Self::Arr {
                f
            }
        }
    }
}

// Derive `arrow::First` for a `Function`.
newtype_derive! {
    ArrowFirst::arrow_first(#ident, #ty) => {
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

// Derive `arrow::Second` for a `Function`.
newtype_derive! {
    ArrowSecond::arrow_second(#ident, #ty) => {
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

// Derive `Split` for a `Function`.
newtype_derive! {
    Split::split(#ident, #ty) => {
        impl<_Arrow, #ty> type_fields::t_funk::Split<_Arrow> for #ident<#ty> {
            type Split = type_fields::t_funk::Splitted<Self, _Arrow>;

            fn split(self, a: _Arrow) -> Self::Split {
                type_fields::t_funk::Splitted(self, a)
            }
        }
    }
}

// Derive `Fanout` for a `Split` implementor.
newtype_derive! {
    Fanout::fanout(#ident, #ty) => {
        impl<_Arrow, #ty> type_fields::t_funk::Fanout<_Arrow> for #ident<#ty>
        where
            #ident<#ty>: type_fields::t_funk::Split<_Arrow>,
            #ident<#ty>: type_fields::t_funk::Arr<type_fields::t_funk::MakePair>,
            <#ident<#ty> as type_fields::t_funk::Split<_Arrow>>::Split: type_fields::t_funk::category::Compose<<#ident<#ty> as type_fields::t_funk::Arr<type_fields::t_funk::MakePair>>::Arr>,
            <#ident<#ty> as type_fields::t_funk::Arr<type_fields::t_funk::MakePair>>::Arr: type_fields::t_funk::category::Compose<<#ident<#ty> as type_fields::t_funk::Split<_Arrow>>::Split>,
        {
            type Fanout = <<#ident<#ty> as type_fields::t_funk::Arr<type_fields::t_funk::MakePair>>::Arr as type_fields::t_funk::category::ComposeL<
                <#ident<#ty> as type_fields::t_funk::Split<_Arrow>>::Split,
            >>::ComposeL;

            fn fanout(self, f: _Arrow) -> Self::Fanout {
                type_fields::t_funk::ComposeL::compose_l(
                    <#ident<#ty> as type_fields::t_funk::Arr<type_fields::t_funk::MakePair>>::arr(type_fields::t_funk::MakePair),
                    type_fields::t_funk::Split::split(self, f)
                )
            }
        }
    }
}
