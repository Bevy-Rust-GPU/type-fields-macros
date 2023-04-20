use proc_macro::TokenStream;
use quote::quote;
use syn::DeriveInput;

pub fn impl_chain(input: DeriveInput) -> TokenStream {
    let ident = input.ident;

    let tys = input
        .generics
        .type_params()
        .map(|type_param| type_param.ident.clone())
        .collect::<Vec<_>>();

    let out = quote!(
        impl<#(#tys),*, _Function> type_fields::t_funk::Chain<_Function> for #ident<#(#tys),*>
        where
            _Function: type_fields::t_funk::Closure<#(#tys),*>,
        {
            type Chain = _Function::Output;

            fn chain(self, f: _Function) -> Self::Chain {
                f.call(type_fields::t_funk::Copointed::copoint(self))
            }
        }
    );

    //panic!("{out:}");

    out.into()
}
