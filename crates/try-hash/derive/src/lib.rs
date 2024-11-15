extern crate proc_macro;
use quote::quote;
use syn::spanned::Spanned;

#[proc_macro_derive(TryHash)]
pub fn derive_try_hash(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input: syn::DeriveInput = syn::parse_macro_input!(input);
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let ident = input.ident;
    let try_hash = quote! { try_hash::TryHash };

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
            let field_names = fields(&data.fields).map(|field| field.name);
            quote! {
                impl #impl_generics #try_hash for #ident #ty_generics #where_clause {
                    type Error = Box<dyn std::error::Error>;
                    fn try_hash(&self, hasher: &mut impl std::hash::Hasher) -> Result<(), Self::Error> {
                        #(#try_hash::try_hash(&self.#field_names, hasher)?;)*
                        Ok(())
                    }
                }
            }
        }
        syn::Data::Enum(data) => {
            let variants = data.variants.iter().map(|variant| {
                let ident = &variant.ident;
                let field_names = fields(&variant.fields).map(|field| field.name);
                let field_new_names: Vec<_> = fields(&variant.fields).map(|field| field.new_name).collect();
                quote! {
                    Self::#ident { #(#field_names : #field_new_names),* } => {
                        #(#try_hash::try_hash(#field_new_names, hasher)?;)*
                    }
                }
            });
            quote! {
                impl #impl_generics try_hash::TryHash for #ident #ty_generics #where_clause {
                    type Error = Box<dyn std::error::Error>;
                    fn try_hash(&self, hasher: &mut impl std::hash::Hasher) -> Result<(), Self::Error> {
                        std::hash::Hash::hash(&std::mem::discriminant(self), hasher);
                        match self {
                            #(#variants)*
                        }
                        Ok(())
                    }
                }
            }
        }
        syn::Data::Union(_) => panic!("union no support"),
    }
    .into()
}
