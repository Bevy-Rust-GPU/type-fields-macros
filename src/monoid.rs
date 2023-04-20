use proc_macro::TokenStream;
use quote::quote;
use syn::DeriveInput;

pub fn impl_mempty(input: DeriveInput) -> TokenStream {
    let ident = input.ident;

    let tys = input
        .generics
        .type_params()
        .map(|type_param| type_param.ident.clone())
        .collect::<Vec<_>>();

    let out = quote!(
        impl<#(#tys),*> type_fields::t_funk::Mempty for #ident<#(#tys),*>
        where
            #(#tys),*: type_fields::t_funk::Mempty,
        {
            type Mempty = #ident<#(#tys),*::Mempty>;

            fn mempty() -> Self::Mempty {
                type_fields::t_funk::Pointed::point(#(#tys),*::mempty())
            }
        }
    );

    //panic!("{out:}");

    out.into()
}
