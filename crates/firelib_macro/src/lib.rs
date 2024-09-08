use proc_macro::TokenStream;
use quote::{__private::TokenStream as QuoteStream, quote};
use syn::{parse_macro_input, punctuated::Punctuated, DeriveInput, Meta, Token};

/// Derive macro generating an impl of the trait `FlowControl`.
#[proc_macro_derive(FlowControl)]
pub fn derive_flow(item: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(item as DeriveInput);
    let (imp, struct_name) = extract_generics(&ast);

    TokenStream::from(quote! {
        #imp firelib::FlowControl for #struct_name {}

        #imp std::ops::FromResidual<std::ops::ControlFlow<#struct_name, std::convert::Infallible>> for #struct_name {
            fn from_residual(residual: std::ops::ControlFlow<#struct_name, std::convert::Infallible>) -> Self {
                <Self as firelib::FlowControl>::__from_residual(residual)
            }
        }

        #imp std::ops::FromResidual<core::result::Result<std::convert::Infallible, firelib::Error>> for #struct_name {
            fn from_residual(residual: core::result::Result<std::convert::Infallible, firelib::Error>) -> Self {
                <Self as firelib::FlowControl>::__from_error(residual)
            }
        }
    })
}

type AttributeArgs = Punctuated<Meta, Token![,]>;

/// Attribute macro generating a simple impl of the trait `Alternative`
/// based on the given pairs of field names and patterns.
///
/// # Examples
///
/// ```compile_fail
/// #[alternative(value, None)]
/// struct Alter<T> {
///     pub value: Option<T>,
/// }
/// ```
#[proc_macro_attribute]
pub fn alternative(attr: TokenStream, item: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(item as DeriveInput);
    let args = parse_macro_input!(attr with AttributeArgs::parse_terminated);

    let (imp, struct_name) = extract_generics(&ast);

    assert!(args.len() <= 2, "Only two arguments are supported!");
    let name = args.first().expect("Missing match attribute name");
    let pattern = args.last().expect("Missing match attribute pattern");

    TokenStream::from(quote! {
        #ast

        #imp std::ops::Try for #struct_name {
            type Output = <Self as firelib::choice::Alternative>::ChoiceOutput;
            type Residual = <Self as firelib::choice::Alternative>::ChoiceResidual;

            fn from_output(output: Self::Output) -> Self {
                Self::from(output)
            }

            fn branch(self) -> std::ops::ControlFlow<Self::Residual, Self::Output> {
                match self {
                    Self {#name: #pattern, ..} => std::ops::ControlFlow::Continue(Self::Output::default()),
                    Self {#name, ..} => std::ops::ControlFlow::Break(#name),
                }
            }
        }

        #imp std::ops::FromResidual for #struct_name {
            fn from_residual(residual: <Self as std::ops::Try>::Residual) -> Self {
                Self::from(residual)
            }
        }
    })
}

fn extract_generics(ast: &DeriveInput) -> (QuoteStream, QuoteStream) {
    let struct_name = &ast.ident;
    let params = &ast.generics.params;

    if params.is_empty() {
        (quote!(impl), quote!(#struct_name))
    } else {
        (quote!(impl<#params>), quote!(#struct_name<#params>))
    }
}
