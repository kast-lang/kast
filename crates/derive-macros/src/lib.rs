extern crate proc_macro;
use quote::quote;


#[proc_macro_derive(Data, attributes(display))]
pub fn derive_data(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input: syn::DeriveInput = syn::parse_macro_input!(input);
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let ident = input.ident;
    let data_ty = &input.generics.type_params().next().unwrap().ident;
    match input.data {
        syn::Data::Enum(data) => {
            let variants: Vec<_> = data.variants.iter().map(|variant| {
                let ident = &variant.ident;
                quote! {
                    Self::#ident { data, .. } => data,
                }
            }).collect();
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

#[proc_macro_derive(ExprDisplay, attributes(display))]
pub fn derive_expr_display(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input: syn::DeriveInput = syn::parse_macro_input!(input);
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let ident = input.ident;
    let f = syn::Ident::new("f71y26t3t5r4152637687234", proc_macro2::Span::mixed_site());

    let field_writes = |fields: &syn::Fields| {
        fields
            .iter()
            .map(|field| {
                let ident = field.ident.as_ref().unwrap();
                let skip = ident == "data"
                    || field
                        .attrs
                        .iter()
                        .filter(|attr| attr.path().is_ident("display"))
                        .any(|attr| {
                            let name = attr.parse_args::<syn::Ident>().unwrap();
                            name == "skip"
                        });
                if skip {
                    return quote!( let _ = #ident; );
                }
                if let syn::Type::Path(ty) = &field.ty {
                    if ty.path.segments[0].ident == "Option" {
                        return quote! {
                            match #ident {
                                Some(value) => writeln!(#f, "{} = {value}", stringify!(#ident)),
                                None => writeln!(#f, "{} = None", stringify!(#ident)),
                            }?;
                        };
                    }
                    if ty.path.segments[0].ident == "Vec" {
                        return quote! {
                            writeln!(#f, "{} = [", stringify!(#ident))?;
                            for item in #ident {
                                writeln!(#f, "    {item}")?;
                            }
                            writeln!(#f, "]")?;
                        };
                    }
                }
                quote! { writeln!(#f, "{} = {}", stringify!(#ident), #ident)?; }
            })
            .collect::<Vec<_>>()
    };

    match input.data {
        syn::Data::Struct(data) => {
            let field_names = data
                .fields
                .iter()
                .map(|field| field.ident.as_ref().unwrap());

            let field_writes = field_writes(&data.fields);
            quote! {
                impl #impl_generics std::fmt::Display for #ident #ty_generics #where_clause {
                    fn fmt(&self, #f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                        use std::fmt::Write;
                        writeln!(#f, "{{")?;
                        {
                            let mut #f = pad_adapter::PadAdapter::new(#f);
                            let Self { #(#field_names,)* } = self;
                            #(#field_writes)*
                        }
                        write!(#f, "}}")
                    }
                }
            }
        }
        syn::Data::Enum(data) => {
            let variant_names = data.variants.iter().map(|variant| {
                let ident = &variant.ident;
                quote! {
                    Self::#ident { .. } => stringify!(#ident),
                }
            });
            let variant_fields = data.variants.iter().map(|variant| {
                let ident = &variant.ident;
                let field_names = variant
                    .fields
                    .iter()
                    .map(|field| field.ident.as_ref().unwrap());

                let field_writes = field_writes(&variant.fields);
                quote! {
                    Self::#ident { #(#field_names,)* } => {
                        #(#field_writes)*
                    },
                }
            });
            quote! {
                impl #impl_generics std::fmt::Display for #ident #ty_generics #where_clause {
                    fn fmt(&self, #f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                        use std::fmt::Write;
                        let name = match self {
                            #(#variant_names)*
                        };
                        writeln!(#f, "{name} {{")?;
                        {
                            let mut #f = pad_adapter::PadAdapter::new(#f);
                            match self {
                                #(#variant_fields)*
                            }
                        }
                        write!(#f, "}}")
                    }
                }
            }
        }
        _ => panic!("yo"),
    }
    .into()
}
