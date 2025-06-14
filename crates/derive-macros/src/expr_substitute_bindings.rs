use super::*;

pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input: syn::DeriveInput = syn::parse_macro_input!(input);
    let default_data_type = input
        .generics
        .params
        .iter()
        .find_map(|param| {
            if let syn::GenericParam::Type(param) = param {
                Some(param.default.as_ref().unwrap())
            } else {
                None
            }
        })
        .map(|ty| quote! {<#ty>});
    let ident = input.ident;
    let substituted_fields = |fields: &syn::Fields| {
        fields
            .iter()
            .map(|field| {
                let ident = field.ident.as_ref().unwrap();
                let skip = field
                    .attrs
                    .iter()
                    .filter(|attr| attr.path().is_ident("substitute"))
                    .any(|attr| {
                        let name = attr.parse_args::<syn::Ident>().unwrap();
                        name == "skip"
                    });
                if skip {
                    return quote!( #ident: #ident );
                }
                quote! {
                    #ident: #ident.substitute_bindings(kast, cache)
                }
            })
            .collect::<Vec<_>>()
    };

    match input.data {
        syn::Data::Struct(data) => {
            let substituted_fields = substituted_fields(&data.fields);
            let field_names = data
                .fields
                .iter()
                .map(|field| field.ident.as_ref().unwrap());
            quote! {
                impl SubstituteBindings for #ident #default_data_type {
                    type Target = Self;
                    fn substitute_bindings(self, kast: &Kast, cache: &mut RecurseCache) -> Self {
                        let Self { #(#field_names),* } = self;
                        Self {
                            #(#substituted_fields),*
                        }
                    }
                }
            }
        }
        syn::Data::Enum(data) => {
            let variants = data.variants.iter().map(|variant| {
                let ident = &variant.ident;

                let with = variant
                    .attrs
                    .iter()
                    .filter(|attr| attr.path().is_ident("substitute"))
                    .find_map(|attr| {
                        let name_value = attr.parse_args::<syn::MetaNameValue>().unwrap();
                        if name_value.path.is_ident("with") {
                            Some(name_value.value)
                        } else {
                            None
                        }
                    });
                if let Some(expr) = with {
                    return quote! { Self::#ident { .. } => self.#expr(kast, cache), };
                }

                let field_names = variant
                    .fields
                    .iter()
                    .map(|field| field.ident.as_ref().unwrap());
                let substituted_fields = substituted_fields(&variant.fields);
                quote! {
                    Self::#ident { #(#field_names,)* } => Self::#ident {
                        #(#substituted_fields),*
                    },
                }
            });
            quote! {
                impl SubstituteBindings for #ident #default_data_type {
                    type Target = Self;
                    fn substitute_bindings(self, kast: &Kast, cache: &mut RecurseCache) -> Self {
                        // println!("before sub = {self}");
                        let result = match self {
                            #(#variants)*
                        };
                        // println!("after sub = {result}");
                        result
                    }
                }
            }
        }
        _ => panic!("yo"),
    }
    .into()
}
