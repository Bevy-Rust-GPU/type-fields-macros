use proc_macro::TokenStream;
use quote::quote;
use syn::DeriveInput;

pub fn impl_copointed(input: DeriveInput) -> TokenStream {
    let ident = input.ident;

    let tys = input
        .generics
        .type_params()
        .map(|type_param| type_param.ident.clone())
        .collect::<Vec<_>>();

    let copointed = match tys.len() {
        0 => panic!("Can't derive Copointed for a type with no inner."),
        1 => quote!(#(#tys),*),
        _ => quote!((#(#tys),*)),
    };

    let out = quote!(
        impl<#(#tys),*> type_fields::t_funk::Copointed for #ident<#(#tys),*> {
            type Copointed = #copointed;

            #[allow(non_snake_case)]
            fn copoint(self) -> Self::Copointed {
                let #ident(#(#tys),*) = self;
                #copointed
            }
        }
    );

    //panic!("{out:}");

    out.into()
}
