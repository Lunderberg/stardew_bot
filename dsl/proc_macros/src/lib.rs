use proc_macro::TokenStream;
use quote::quote;
use syn::{DeriveInput, parse_macro_input};

#[proc_macro_derive(RustNativeObject)]
pub fn derive_rust_native_object(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let class_name = &input.ident;

    let expanded = quote! {
        impl ::dsl::RustNativeObject for #class_name {}
    };

    TokenStream::from(expanded)
}
