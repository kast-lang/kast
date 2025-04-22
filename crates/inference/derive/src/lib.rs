extern crate proc_macro;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::spanned::Spanned;

#[proc_macro_derive(Same)]
pub fn derive_try_hash(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input: syn::DeriveInput = syn::parse_macro_input!(input);
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let ident = input.ident;
    let same_trait = quote! { kast_inference::Same };

    struct Field {
        /// `0` is a valid name originally
        name: syn::Member,
        /// because `0` is not a valid name anymore
        new_name: syn::Ident,
    }
    fn fields(fields: &syn::Fields) -> impl Iterator<Item = Field> + '_ {
        fields.iter().enumerate().map(|(index, field)| Field {
            name: match &field.ident {
                Some(ident) => syn::Member::Named(ident.clone()),
                None => syn::Member::Unnamed(syn::Index {
                    index: index as u32,
                    span: field.span(),
                }),
            },
            new_name: syn::Ident::new(&format!("__self_{index}"), field.span()),
        })
    }

    match input.data {
        syn::Data::Struct(data) => {
            let hash_fields = fields(&data.fields).map(|field| {
                let field_name = &field.name;
                quote! { #same_trait::hash(&self.#field_name, hasher); }
            });
            let check_same_fields = fields(&data.fields).map(|field| {
                let field_name = &field.name;
                quote! { if !#same_trait::is_same(&self.#field_name, &other.#field_name) { return false; } }
            });
            quote! {
                impl #impl_generics #same_trait for #ident #ty_generics #where_clause {
                    fn is_same(&self, other: &Self) -> bool {
                        #(#check_same_fields)*;
                        true
                    }
                    fn hash(&self, hasher: &mut impl std::hash::Hasher) {
                        #(#hash_fields)*;
                    }
                }
            }
        }
        syn::Data::Enum(data) => {
            let check_same_variants = data.variants.iter().map(|variant| {
                let field_names = fields(&variant.fields).map(|field| field.name.clone()).collect::<Vec<_>>();
                let field_self_names = variant.fields.iter().enumerate().map(|(index, _field)|
                    format_ident!("__self_{}", index)).collect::<Vec<_>>();
                let field_other_names = variant.fields.iter().enumerate().map(|(index, _field)|
                    format_ident!("__other_{}", index)).collect::<Vec<_>>();
                let check_same_fields = field_self_names.iter().zip(field_other_names.iter()).map(|(field_self_name, field_other_name)| {
                    quote! { if !#same_trait::is_same(#field_self_name, #field_other_name) { return false; } }
                });
                let variant_name = &variant.ident;
                quote! {
                    (Self::#variant_name { #(#field_names: #field_self_names),* },
                     Self::#variant_name { #(#field_names: #field_other_names),* }) => {
                        #(#check_same_fields)*
                    }
                }
            });
            let hash_variants = data.variants.iter().map(|variant| {
                let field_names = fields(&variant.fields).map(|field| field.name.clone()).collect::<Vec<_>>();
                let field_self_names = variant.fields.iter().enumerate().map(|(index, _field)|
                    format_ident!("__self_{}", index)).collect::<Vec<_>>();
                let hash_fields = field_self_names.iter().map(|field_name| {
                    quote! { #same_trait::hash(#field_name, hasher); }
                });
                let variant_name = &variant.ident;
                quote! {
                    Self::#variant_name { #(#field_names: #field_self_names),* } => {
                        #(#hash_fields)*
                    }
                }
            });
            quote! {
                impl #impl_generics #same_trait for #ident #ty_generics #where_clause {
                    fn is_same(&self, other: &Self) -> bool {
                        match (self, other) {
                            #(#check_same_variants,)*
                            (_, _) => return false,
                        }
                        true
                    }
                    fn hash(&self, hasher: &mut impl std::hash::Hasher) {
                        match self {
                            #(#hash_variants,)*
                        }
                    }
                }
            }
        }
        syn::Data::Union(_) => panic!("union no support"),
    }
    .into()
}
