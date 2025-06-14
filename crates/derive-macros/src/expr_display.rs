use super::*;

pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input: syn::DeriveInput = syn::parse_macro_input!(input);
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let ident = input.ident;
    let f = syn::Ident::new("f71y26t3t5r4152637687234", proc_macro2::Span::mixed_site());

    let show_data = input
        .attrs
        .iter()
        .filter(|attr| attr.path().is_ident("display"))
        .any(|attr| attr.parse_args::<syn::Ident>().unwrap() == "show_data");
    let show_data = show_data.then(|| quote! { write!(#f, " :: {}", self.data())? });

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
                            write!(#f, "{} = ", stringify!(#ident))?;
                            match #ident {
                                Some(value) => {
                                    kast_util::FormatterExt::write(#f, value)?;
                                    writeln!(#f)?;
                                },
                                None => writeln!(#f, "None")?,
                            };
                        };
                    }
                    if ty.path.segments[0].ident == "Vec" {
                        return quote! {
                            writeln!(#f, "{} = [", stringify!(#ident))?;
                            for item in #ident {
                                write!(#f, "    ")?;
                                kast_util::FormatterExt::write(#f, item)?;
                                writeln!(#f)?;
                            }
                            writeln!(#f, "]")?;
                        };
                    }
                }
                quote! {
                    write!(#f, "{} = ", stringify!(#ident))?;
                    kast_util::FormatterExt::write(#f, #ident)?;
                    writeln!(#f)?;
                }
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
                            let mut #f = pad_adapter::PadAdapter::with_padding(#f, "    ");
                            let #f = &mut #f;
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
                            let mut #f = pad_adapter::PadAdapter::with_padding(#f, "    ");
                            let #f = &mut #f;
                            match self {
                                #(#variant_fields)*
                            }
                        }
                        write!(#f, "}}")?;
                        #show_data;
                        Ok(())
                    }
                }
            }
        }
        _ => panic!("yo"),
    }
    .into()
}
