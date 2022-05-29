use proc_macro2::{Ident, Span, TokenStream};
use quote::{quote, ToTokens};
use std::collections::HashSet;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::{parse, parse2, FnArg, Item, Lit, Meta, NestedMeta, Token};
use walkdir::WalkDir;

#[proc_macro_attribute]
pub fn test_files(
    raw_args: proc_macro::TokenStream,
    raw_item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    match test_files2(raw_args.into(), raw_item.into()) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

fn test_files2(raw_args: TokenStream, raw_item: TokenStream) -> Result<TokenStream, syn::Error> {
    let args_meta: Punctuated<NestedMeta, Token![,]> =
        parse::Parser::parse2(Punctuated::parse_terminated, raw_args)?;
    let args = Args::parse(&args_meta)?;

    let item: Item = parse2(raw_item)?;
    let item = if let Item::Fn(item) = item {
        item
    } else {
        return Err(syn::Error::new(item.span(), "expected function"));
    };

    let mut arg_specs = Vec::new();
    for arg in &item.sig.inputs {
        let attrs = match arg {
            FnArg::Receiver(arg) => &arg.attrs,
            FnArg::Typed(arg) => &arg.attrs,
        };
        let mut suffix = None;
        for attr in attrs {
            let meta = if let Ok(meta) = attr.parse_meta() {
                meta
            } else {
                continue;
            };
            if meta.path().is_ident("suffix") {
                if suffix.is_some() {
                    return Err(syn::Error::new(
                        attr.span(),
                        "Duplicate #[suffix] attribute",
                    ));
                }
                let meta = if let Meta::NameValue(meta) = &meta {
                    meta
                } else {
                    return Err(syn::Error::new(meta.span(), "Expected #[suffix = ...]"));
                };
                let lit = if let Lit::Str(lit) = &meta.lit {
                    lit
                } else {
                    return Err(syn::Error::new(
                        meta.lit.span(),
                        "Expected a string literal",
                    ));
                };
                suffix = Some(lit.value());
            }
        }
        let suffix = suffix
            .ok_or_else(|| syn::Error::new(arg.span(), "Missing argument: #[suffix = ...]"))?;
        arg_specs.push(ArgSpec { suffix });
    }

    let mut file_names = HashSet::new();
    let mut stems = HashSet::new();
    for entry in WalkDir::new(&args.dir).sort_by_file_name() {
        let entry = entry.map_err(|e| {
            syn::Error::new(
                args_meta.span(),
                format_args!("error during walkdir: {}", e),
            )
        })?;
        let file_name = entry.path().strip_prefix(&args.dir).map_err(|_| {
            syn::Error::new(
                args_meta.span(),
                format_args!("{} is not in {}", entry.path().display(), args.dir),
            )
        })?;
        let file_name = file_name.to_str().ok_or_else(|| {
            syn::Error::new(
                args_meta.span(),
                format_args!("invalid file name: {}", entry.path().display()),
            )
        })?;
        file_names.insert(file_name.to_owned());
        for arg_spec in &arg_specs {
            if file_name.ends_with(&arg_spec.suffix) {
                stems.insert(file_name[..file_name.len() - arg_spec.suffix.len()].to_owned());
            }
        }
    }
    let sorted_stems = {
        let mut sorted_stems = stems.into_iter().collect::<Vec<_>>();
        sorted_stems.sort();
        sorted_stems
    };

    let function_name = &item.sig.ident;

    let test_functions = sorted_stems
        .iter()
        .map(|stem| {
            let test_function_name = format!("test_{}", stem.replace("/", "_"));
            let test_function_ident = Ident::new(&test_function_name, Span::call_site());
            let arguments = arg_specs.iter().map(|spec| {
                let filename = format!("{}/{}{}", args.dir, stem, spec.suffix);
                quote! {
                    #filename.into()
                }
            });
            quote! {
                #[test]
                fn #test_function_ident() {
                    super::#function_name(
                        #(
                            #arguments,
                        )*
                    );
                }
            }
        })
        .collect::<Vec<_>>();

    let base_function = {
        let mut base_function = item.clone();
        // Remove #[test] from the function attributes
        base_function.attrs.retain(|attr| {
            if let Ok(meta) = attr.parse_meta() {
                !meta.path().is_ident("test")
            } else {
                true
            }
        });
        for arg in &mut base_function.sig.inputs {
            let attrs = match arg {
                FnArg::Receiver(arg) => &mut arg.attrs,
                FnArg::Typed(arg) => &mut arg.attrs,
            };
            // Remove #[suffix = "..."] from the parameter attributes
            attrs.retain(|attr| {
                if let Ok(meta) = attr.parse_meta() {
                    !meta.path().is_ident("suffix")
                } else {
                    true
                }
            });
        }
        base_function
    };

    let expanded = quote! {
        #[cfg(test)]
        #base_function

        #[cfg(test)]
        mod #function_name {
            #(
                #test_functions
            )*
        }
    };
    Ok(expanded)
}

#[derive(Debug, Clone)]
struct Args {
    dir: String,
}

impl Args {
    fn parse(meta: &Punctuated<NestedMeta, Token![,]>) -> Result<Self, syn::Error> {
        let mut dir = None;
        for arg in meta {
            if let NestedMeta::Meta(arg) = arg {
                if arg.path().is_ident("dir") {
                    if dir.is_some() {
                        return Err(syn::Error::new(arg.path().span(), "duplicate argument"));
                    }
                    if let Meta::NameValue(arg) = arg {
                        if let Lit::Str(lit) = &arg.lit {
                            dir = Some(lit.value());
                            continue;
                        } else {
                            return Err(syn::Error::new(arg.lit.span(), "invalid argument value"));
                        }
                    } else {
                        return Err(syn::Error::new(arg.span(), "invalid argument value"));
                    }
                } else {
                    return Err(syn::Error::new(
                        arg.path().span(),
                        format_args!("unknown argument: {}", arg.path().to_token_stream()),
                    ));
                }
            } else {
                return Err(syn::Error::new(arg.span(), "invalid argument"));
            };
        }
        let dir = dir.ok_or_else(|| syn::Error::new(meta.span(), "missing argument: dir"))?;
        Ok(Args { dir })
    }
}

#[derive(Debug, Clone)]
struct ArgSpec {
    suffix: String,
}
