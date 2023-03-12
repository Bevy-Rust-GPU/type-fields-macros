extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{parse_macro_input, ItemStruct, Visibility};

/// Implements a same-named mutable getter for each public named field in a given struct.
/// Enables the `Struct::field` syntax used to compose a path from function pointers.
#[proc_macro_derive(Field)]
pub fn paths(input: TokenStream) -> TokenStream {
    impl_paths(parse_macro_input!(input))
}

fn impl_paths(input: ItemStruct) -> TokenStream {
    let mut tokens = TokenStream2::default();

    let ident = input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    for field in input.fields.iter() {
        let Visibility::Public(_) = field.vis else { continue };

        let path = field.ident.as_ref().unwrap();
        let ty = &field.ty;

        tokens = quote! {
            #tokens

            impl #impl_generics #ident #ty_generics #where_clause {
                pub fn #path(&mut self) -> &mut #ty {
                    &mut self.#path
                }
            }
        };
    }

    tokens.into()
}
