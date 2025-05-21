use super::*;

pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input: syn::DeriveInput = syn::parse_macro_input!(input);
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let ident = input.ident;
    let data_ty = &input.generics.type_params().next().unwrap().ident;
    match input.data {
        syn::Data::Enum(data) => {
            let variants: Vec<_> = data
                .variants
                .iter()
                .map(|variant| {
                    let ident = &variant.ident;
                    quote! {
                        Self::#ident { data, .. } => data,
                    }
                })
                .collect();
            quote! {
                impl #impl_generics #ident #ty_generics #where_clause {
                    pub fn data(&self) -> & #data_ty {
                        match self {
                            #(#variants)*
                        }
                    }
                    pub fn data_mut(&mut self) -> &mut #data_ty {
                        match self {
                            #(#variants)*
                        }
                    }
                }
            }
        }
        _ => panic!("yo"),
    }
    .into()
}
