use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(RustNativeObject)]
pub fn derive_rust_native_object(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let class_name = &input.ident;

    let expanded = quote! {
        impl ::dotnet_debugger::bytecode::RustNativeObject
            for #class_name {}
    };

    TokenStream::from(expanded)
}
