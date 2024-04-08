use quote::quote;
use syn::{parse_macro_input, DeriveInput}; // Import the DeriveInput struct from the syn crate


fn inner_type(ty: &syn::Type) -> Option<&syn::Type> {

    let segment = match ty {
        syn::Type::Path(ref p) => {

            if p.path.segments.first().is_some_and(|segment| segment.ident == "Option") {
                p.path.segments.first().unwrap()
            } else {
                return None;
            }
        },
        _ => return None,
    };

    // Returns the inner type of the Option
    if let syn::PathArguments::AngleBracketed(ref inner_ty) = segment.arguments {
        if let syn::GenericArgument::Type(ref t) = inner_ty.args.first().unwrap() {
            return Some(t);
        }
    }
    None 
}

#[proc_macro_derive(Builder)]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {

    let ast = parse_macro_input!(input as DeriveInput);
    let name = &ast.ident;
    let bname = format!("{}Builder", name);
    let bident = proc_macro2::Ident::new(&bname, name.span());

    let fields = match ast.data {
        syn::Data::Struct(ref data) => match data.fields {
            syn::Fields::Named(ref fields) => fields.named.iter().collect::<Vec<_>>(),
            _ => unimplemented!(),
        },
        _ => unimplemented!(),
    };

    let optionized = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        if inner_type(ty).is_some() {
            quote! {
                #name: #ty
            }
        } else {
            quote! {
            #name: std::option::Option<#ty>        }
        }
    });

    let method = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        if let Some(inner_ty) = inner_type(ty) {
            quote! {
                pub fn #name(&mut self, #name: #inner_ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            }
        } else {
            quote! {
                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            }
        }
    });

    let build_fields = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        if inner_type(ty).is_some() {
            quote! {
                #name: self.#name.clone()
            }
        } else {
            quote! {
                #name: self.#name.clone().ok_or(concat!(stringify!(#name), " is not set"))?
            }
        }
    });

    let build_empty = fields.iter().map(|f| {
        let name = &f.ident;
        quote! { #name: None }
    });

    let expanded = quote! {
        pub struct #bident {
            #(#optionized,)*
        }

        impl #bident {
            #(#method)*
            pub fn build(&self) -> Result<#name, Box<dyn std::error::Error>> {
                Ok(#name {
                    #(#build_fields,)*
                })
            }
        }

        impl #name {
            fn builder() -> #bident {
                #bident {
                    #(#build_empty,)*
                }
            }
        }
    };

    expanded.into()
    // eprintln!("{:#?}", ast); 
    // TokenStream::new()
    //unimplemented!()
}
