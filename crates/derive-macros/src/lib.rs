extern crate proc_macro;
use quote::quote;

mod data;
#[proc_macro_derive(Data, attributes(display))]
pub fn derive_data(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    data::derive(input)
}

mod expr_display;
#[proc_macro_derive(ExprDisplay, attributes(display))]
pub fn derive_expr_display(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    expr_display::derive(input)
}

mod expr_substitute_bindings;
#[proc_macro_derive(ExprSubstituteBindings, attributes(substitute))]
pub fn derive_expr_substitute_bindings(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    expr_substitute_bindings::derive(input)
}
