use proc_macro::TokenStream;
use quote::quote;
use syn::DeriveInput;

pub fn impl_applicative(input: DeriveInput) -> TokenStream {
    let ident = input.ident;

    let tys = input
        .generics
        .type_params()
        .map(|type_param| type_param.ident.clone())
        .collect::<Vec<_>>();

    let out = quote!(
        impl<#(#tys),*, _Value> type_fields::t_funk::Apply<#ident<_Value>> for #ident<#(#tys),*>
        where
            #(#tys),*: type_fields::t_funk::Closure<_Value>,
        {
            type Apply = #ident<#(#tys),*::Output>;

            fn apply(self, a: #ident<_Value>) -> Self::Apply
            where
                #(#tys),*: type_fields::t_funk::Closure<_Value>,
            {
                type_fields::t_funk::Pointed::point(
                    type_fields::t_funk::Copointed::copoint(self)
                        .call(type_fields::t_funk::Copointed::copoint(a)),
                )
            }
        }
    );

    //panic!("{out:}");

    out.into()
}
