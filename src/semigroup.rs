use proc_macro::TokenStream;
use quote::quote;
use syn::DeriveInput;

pub fn impl_mappend(input: DeriveInput) -> TokenStream {
    let ident = input.ident;

    let tys = input
        .generics
        .type_params()
        .map(|type_param| type_param.ident.clone())
        .collect::<Vec<_>>();

    let out = quote!(
        impl<_Type #(, #tys)*> type_fields::t_funk::Mappend<#ident<_Type>> for #ident<#(#tys),*>
        where
            #(#tys),*: type_fields::t_funk::Mappend<_Type>,
        {
            type Mappend = #ident<#(#tys),*::Mappend>;

            fn mappend(self, t: #ident<_Type>) -> Self::Mappend {
                type_fields::t_funk::Pointed::point(
                    self.0.mappend(type_fields::t_funk::Copointed::copoint(t)),
                )
            }
        }
    );

    //panic!("{out:}");

    out.into()
}
