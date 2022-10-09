use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, AttributeArgs, DeriveInput, NestedMeta};

/// Derive macro generating an impl of the trait `FlowControl`.
#[proc_macro_derive(FlowControl)]
pub fn derive_flow(item: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(item as DeriveInput);
    let (imp, struct_name) = extract_generics(&ast);

    TokenStream::from(quote! {
        #imp firelib::FlowControl for #struct_name {}

        #imp FromResidual<ControlFlow<#struct_name, Infallible>> for #struct_name {
            fn from_residual(residual: ControlFlow<#struct_name, Infallible>) -> Self {
                <Self as firelib::FlowControl>::__from_residual(residual)
            }
        }

        #imp FromResidual<Result<Infallible, anyhow::Error>> for #struct_name {
            fn from_residual(residual: Result<Infallible, anyhow::Error>) -> Self {
                <Self as firelib::FlowControl>::__from_error(residual)
            }
        }
    })
}

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
    let args = parse_macro_input!(attr as AttributeArgs);

    let (imp, struct_name) = extract_generics(&ast);
    let matcher = generate_matcher(args);

    TokenStream::from(quote! {
        #ast

        #imp firelib::Alternative for #struct_name {}

        #imp Try for #struct_name {
            type Output = Self;
            type Residual = Self;

            fn from_output(output: Self::Output) -> Self {
                output
            }

            fn branch(self) -> ControlFlow<Self::Residual, Self::Output> {
                match self {
                    Self {#matcher, ..} => ControlFlow::Continue(self),
                    _ => ControlFlow::Break(self),
                }
            }
        }

        #imp FromResidual for #struct_name {
            fn from_residual(residual: <Self as Try>::Residual) -> Self {
                residual
            }
        }
    })
}

type QuoteStream = quote::__private::TokenStream;

fn generate_matcher(args: Vec<NestedMeta>) -> QuoteStream {
    let mut matcher = QuoteStream::new();
    let mut i = 0;

    loop {
        let (name, pattern) = (
            args.get(i).expect("Missing match attribute name"),
            args.get(i + 1).expect("Missing match attribute pattern"),
        );

        matcher = if i == 0 {
            quote!(#name: #pattern)
        } else {
            quote!(#matcher, #name: #pattern)
        };

        i += 2;
        if args.len() - i == 0 {
            break;
        }
    }

    matcher
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
