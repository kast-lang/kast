extern crate proc_macro;
use proc_macro2::TokenStream;
use quote::quote;
use syn::spanned::Spanned;

#[proc_macro_derive(TryHash, attributes(try_hash))]
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
        do_try: bool,
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
            do_try: field
                .attrs
                .iter()
                .any(|attr| attr.path().is_ident("try_hash")),
        })
    }
    fn hash_fields(fields: impl IntoIterator<Item = Field>) -> TokenStream {
        fn hash_field(field: Field) -> TokenStream {
            let ident = &field.new_name;
            if field.do_try {
                quote!( try_hash::TryHash::try_hash(#ident, hasher)?; )
            } else {
                quote!( std::hash::Hash::hash(#ident, hasher); )
            }
        }
        let hash_fields = fields.into_iter().map(hash_field);
        quote! {
            #( #hash_fields )*
        }
    }
    match input.data {
        syn::Data::Struct(data) => {
            let field_names = fields(&data.fields).map(|field| field.name);
            let field_new_names = fields(&data.fields).map(|field| field.new_name);
            let hash_fields = hash_fields(fields(&data.fields));
            quote! {
                impl #impl_generics #try_hash for #ident #ty_generics #where_clause {
                    type Error = Box<dyn std::error::Error + Send + Sync>;
                    fn try_hash(&self, hasher: &mut impl std::hash::Hasher) -> Result<(), Self::Error> {
                        let Self { #(#field_names: #field_new_names),* } = self;
                        #hash_fields;
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
                let hash_fields = hash_fields(fields(&variant.fields));
                quote! {
                    Self::#ident { #(#field_names : #field_new_names),* } => {
                        #hash_fields
                    }
                }
            });
            quote! {
                impl #impl_generics try_hash::TryHash for #ident #ty_generics #where_clause {
                    type Error = Box<dyn std::error::Error + Send + Sync>;
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
