use quote::quote;
use syn::visit_mut::{self, VisitMut};
use syn::{parse_quote, DeriveInput, Expr, Lit, LitInt};

struct StripAndReplaceGenerics;

impl VisitMut for StripAndReplaceGenerics {
    // fn visit_type_mut(&mut self, node: &mut Type) {
    //     if let Expr::Lit(expr) = &node {
    //         if let Lit::Int(int) = &expr.lit {
    //             if int.suffix() == "u256" {
    //                 let digits = int.base10_digits();
    //                 let unsuffixed: LitInt = syn::parse_str(digits).unwrap();
    //                 *node = parse_quote!(bigint::u256!(#unsuffixed));
    //                 return;
    //             }
    //         }
    //     }

    //     // Delegate to the default impl to visit nested expressions.
    //     visit_mut::visit_expr_mut(self, node);
    // }
}

pub(crate) fn replace_generics(code: DeriveInput) {
    let mut result = code.clone();
    StripAndReplaceGenerics.visit_derive_input_mut(&mut result);
}
