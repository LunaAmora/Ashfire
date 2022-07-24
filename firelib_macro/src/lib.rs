use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(FlowControl)]
pub fn derive_flow(item: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(item as DeriveInput);
    let struct_name = &ast.ident;
    let params = &ast.generics.params;

    let expanded = if params.is_empty() {
        quote! {
            impl firelib::FlowControl for #struct_name {}
            impl FromResidual<ControlFlow<#struct_name, Infallible>> for #struct_name {
                fn from_residual(residual: ControlFlow<#struct_name, Infallible>) -> Self {
                    <Self as firelib::FlowControl>::from_residual(residual)
                }
            }
        }
    } else {
        quote! {
            impl<#params> firelib::FlowControl for #struct_name<#params> {}
            impl<#params> FromResidual<ControlFlow<#struct_name<#params>, Infallible>> for #struct_name<#params> {
                fn from_residual(residual: ControlFlow<#struct_name<#params>, Infallible>) -> Self {
                    <Self as firelib::FlowControl>::from_residual(residual)
                }
            }
        }
    };

    TokenStream::from(expanded)
}
