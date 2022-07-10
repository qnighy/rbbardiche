use proc_macro2::{Ident, Span, TokenStream};
use quote::{quote, ToTokens};
use std::collections::HashSet;
use std::path::PathBuf;
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
        let typeinfo = match arg {
            FnArg::Receiver(_) => ArgType::Other,
            FnArg::Typed(arg) => match &*arg.ty {
                syn::Type::Path(path) => {
                    if let Some(segment) = path.path.segments.last() {
                        if segment.ident.to_string() == "PendingFile" {
                            ArgType::PendingFile
                        } else {
                            ArgType::Other
                        }
                    } else {
                        ArgType::Other
                    }
                }
                _ => ArgType::Other,
            },
        };
        arg_specs.push(ArgSpec { suffix, typeinfo });
    }

    let mut matched_file_names = HashSet::new();
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
                matched_file_names.insert(file_name.to_owned());
            }
        }
    }
    let sorted_stems = {
        let mut sorted_stems = stems.into_iter().collect::<Vec<_>>();
        sorted_stems.sort();
        sorted_stems
    };
    let sorted_matched_file_names = {
        let mut sorted_matched_file_names = matched_file_names.into_iter().collect::<Vec<_>>();
        sorted_matched_file_names.sort();
        sorted_matched_file_names
    };

    let function_name = &item.sig.ident;

    let mut test_functions = Vec::new();
    for stem in &sorted_stems {
        let test_function_name = format!("test_{}", stem.replace("/", "_"));
        let test_function_ident = Ident::new(&test_function_name, Span::call_site());
        let arguments = arg_specs.iter().map(|spec| {
            let filename = format!("{}/{}{}", args.dir, stem, spec.suffix);
            quote! {
                #filename.into()
            }
        });
        let mut should_panic = None;
        for arg_spec in &arg_specs {
            if arg_spec.typeinfo != ArgType::PendingFile {
                continue;
            }
            let path = format!("{}{}", stem, arg_spec.suffix);
            let path = PathBuf::from(&args.dir).join(path);
            match std::fs::read(&path) {
                Ok(contents) => {
                    should_panic = Some(String::from_utf8_lossy(&contents).into_owned());
                }
                Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
                Err(e) => {
                    return Err(syn::Error::new(
                        item.span(),
                        format_args!("error reading {}: {}", path.display(), e),
                    ));
                }
            }
        }
        let should_panic_attr = if let Some(message) = should_panic {
            quote! {
                #[should_panic = #message]
            }
        } else {
            quote! {}
        };
        test_functions.push(quote! {
            #[test]
            #should_panic_attr
            fn #test_function_ident() {
                super::#function_name(
                    #(
                        #arguments,
                    )*
                );
            }
        });
    }

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
    let check_testcases_function = {
        let rs = &args.rs;
        let dir = &args.dir;
        let arg_specs_code = arg_specs
            .iter()
            .map(|arg_spec| {
                let suffix = &arg_spec.suffix;
                quote! {
                    testfiles::__rt::ArgSpec {
                        suffix: String::from(#suffix),
                    },
                }
            })
            .collect::<Vec<_>>();
        quote! {
            #[test]
            fn __check_testcases() {
                testfiles::__rt::check(
                    &testfiles::__rt::WalkConfig {
                        rs: String::from(#rs),
                        dir: String::from(#dir),
                        arg_specs: vec![
                            #(#arg_specs_code)*
                        ],
                    },
                    vec![
                        #(
                            String::from(#sorted_matched_file_names),
                        )*
                    ]
                );
            }
        }
    };

    let expanded = quote! {
        #[cfg(test)]
        #base_function

        #[cfg(test)]
        mod #function_name {
            #check_testcases_function
            #(
                #test_functions
            )*
        }
    };
    Ok(expanded)
}

#[derive(Debug, Clone)]
struct Args {
    rs: String,
    dir: String,
}

impl Args {
    fn parse(meta: &Punctuated<NestedMeta, Token![,]>) -> Result<Self, syn::Error> {
        let mut rs = None;
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
                } else if arg.path().is_ident("rs") {
                    if rs.is_some() {
                        return Err(syn::Error::new(arg.path().span(), "duplicate argument"));
                    }
                    if let Meta::NameValue(arg) = arg {
                        if let Lit::Str(lit) = &arg.lit {
                            rs = Some(lit.value());
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
        let rs = rs.ok_or_else(|| syn::Error::new(meta.span(), "missing argument: rs"))?;
        let dir = dir.ok_or_else(|| syn::Error::new(meta.span(), "missing argument: dir"))?;
        Ok(Args { rs, dir })
    }
}

#[derive(Debug, Clone)]
struct ArgSpec {
    suffix: String,
    typeinfo: ArgType,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ArgType {
    PendingFile,
    Other,
}
