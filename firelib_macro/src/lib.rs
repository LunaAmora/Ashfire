use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(FlowControl)]
pub fn derive_short_circuit(item: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(item as DeriveInput);
    let struct_name = &ast.ident;

    let expanded = quote! {
        impl<T> firelib::FlowControl for #struct_name<T> {}
        impl<T> FromResidual<ControlFlow<#struct_name<T>, Infallible>> for #struct_name<T> {
            fn from_residual(residual: ControlFlow<#struct_name<T>, Infallible>) -> Self {
                <Self as firelib::FlowControl>::from_residual(residual)
            }
        }
    };

    TokenStream::from(expanded)
}
