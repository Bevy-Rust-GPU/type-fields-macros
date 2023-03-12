extern crate proc_macro;

use convert_case::{Case, Casing};
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{parse_macro_input, Ident, ItemStruct, Visibility};

#[proc_macro_derive(Field)]
pub fn paths(input: TokenStream) -> TokenStream {
    impl_paths(parse_macro_input!(input))
}

fn impl_paths(input: ItemStruct) -> TokenStream {
    let mut tokens = TokenStream2::default();

    let ident = input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let mut paths_upper = vec![];
    let mut paths_camel = vec![];

    for field in input.fields.iter() {
        let Visibility::Public(_) = field.vis else { continue };

        let path = field.ident.as_ref().unwrap();

        let path_upper = Ident::new(&path.to_string().to_case(Case::UpperSnake), path.span());
        paths_upper.push(path_upper);

        let mut path_camel = ident.to_string()
            + "_"
            + &field
                .ident
                .as_ref()
                .unwrap()
                .to_string()
                .to_case(Case::Pascal);

        path_camel.get_mut(0..1).unwrap().make_ascii_uppercase();
        let path_camel = Ident::new(&path_camel, field.ident.as_ref().unwrap().span());

        paths_camel.push(path_camel.clone());

        let ty = &field.ty;

        tokens = quote! {
            #tokens

            #[derive(Debug, Default, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
            #[allow(non_camel_case_types)]
            pub struct #path_camel;

            impl #impl_generics type_fields::path::Path<#ident #ty_generics> for #path_camel #where_clause {
                type Type = #ty;

                fn field(t: &mut #ident #ty_generics) -> &mut Self::Type {
                    &mut t.#path
                }
            }

            impl type_fields::field::FieldPath for #path_camel {
                type Type = #path_camel;
            }
        };
    }

    tokens = quote! {
        #tokens

        impl #impl_generics #ident #ty_generics #where_clause {
            #(
                #[allow(non_upper_case_globals)]
                pub const #paths_upper: #paths_camel = #paths_camel;
            )*
        }
    };

    tokens.into()
}
