use proc_macro::{Ident, TokenStream};
use proc_macro2::TokenTree;
use std::env::args;
use quote::{quote, format_ident, ToTokens};
use syn::{parse_macro_input, Block, FnArg, ItemFn, Pat, ReturnType, Type, Receiver, NestedMeta, Meta, Path};
use syn::token::{Comma, Token};
use syn::parse_quote::ParseQuote;
use syn::parse::Parser;

#[proc_macro]
pub fn capp(input : TokenStream) -> TokenStream {
    let parsed = parse_macro_input!(input as proc_macro2::TokenStream); 

    let mut it = parsed.into_iter();
    let fname = it.next();

    if let Some (fname) = fname {
        let mut result = proc_macro2::TokenStream::from(fname);

        for t in it {
            let tst = proc_macro2::TokenStream::from(t);
            result = quote! {
                ( #result )( #tst )
            };
        }

        result.into()
    } else {
        panic!("Supply a function name");
    }
}


enum CurryBox {
    Arc,
    Rc,
    Box,
}

struct CurryConfig {
    name : String,                  // name of the new curried function
    curry_box : Option<CurryBox>,   // what box do we use for the dyn Fn? (keep original, if none)
    bundle : bool,                  // do we "bundle" the Self argument for object methods?
}

impl ToString for CurryBox {
    fn to_string(&self) -> String {
        match self {
            CurryBox::Arc => String::from("Arc"),
            CurryBox::Rc => String::from("Rc"),
            CurryBox::Box => String::from("Box"),
        }
    }
}

impl CurryBox {
    fn to_token_stream(&self) -> proc_macro2::TokenStream {
        match self {
            CurryBox::Arc => quote! {Arc},
            CurryBox::Rc => quote!{Rc},
            CurryBox::Box => quote!{Box},
        }
    }
}

impl Default for CurryConfig {
    fn default() -> Self {
        Self {
            name : String::from(""),
            curry_box : Some(CurryBox::Rc),
            bundle : true,
        }
    }
}

#[proc_macro_attribute]
pub fn curry(attr: TokenStream, item: TokenStream) -> TokenStream {

    let parsed_attr = syn::punctuated::Punctuated::<syn::NestedMeta, syn::Token![,]>::parse_terminated
        .parse(attr)
        .unwrap();

    let parsed = parse_macro_input!(item as ItemFn);

    let options : Vec<CurryConfig> = parse_curry_configs(parsed_attr);

    generate_currys(parsed, options).into()
}

fn get_lit_str(l : &syn::Lit) -> Option<String> {
    match l {
        syn::Lit::Str(l) => {
            Some(l.value())
        }
        _ => None,
    }
}

fn parse_curry_config_args(curry_box : Option<CurryBox>, args : Punctuated<NestedMeta, Comma>) -> CurryConfig {
    let mut conf = CurryConfig::default();
    conf.curry_box = curry_box;

    for arg in args.iter() {
        match arg {
            NestedMeta::Meta(m) => {
                match m {
                    Meta::Path(p) => {
                        let p_str = quote! {#p}.to_string();
                        if p_str == "no_bundle" {
                            conf.bundle = false;
                        } else {
                            panic!("Invalid curry config argument");
                        }
                    }
                    _ => {
                        panic!("Invalid curry config argument");
                    }
                }
            },
            NestedMeta::Lit(l) => {
                let l = get_lit_str(l).unwrap();
                conf.name = l;
            },
        }
    }

    conf
}

fn parse_curry_config(curry_box : Path, curry_args : Option<Punctuated<NestedMeta, Comma>>) -> CurryConfig {
    let curry_box_s = quote!{#curry_box}.to_string();
    let cb = if curry_box_s == "Arc" {
        Some(CurryBox::Arc)
    } else if curry_box_s == "Rc" {
        Some(CurryBox::Rc)
    } else if curry_box_s == "Box" {
        Some(CurryBox::Box)
    } else if curry_box_s == "Orig" {
        None
    } else {
        panic!("Invalid curry box")
    };

    match curry_args {
        None => {
            let mut config = CurryConfig::default();
            config.curry_box = cb;
            config
        },
        Some(args) => {
            parse_curry_config_args(cb, args)
        },
    }
}

fn parse_curry_configs(attr : Punctuated<NestedMeta, Comma>) -> Vec<CurryConfig> {
    let mut configs = vec![];
    for e in attr.iter() {
        match e {
            NestedMeta::Meta(m) =>  {
                match m {
                    Meta::List(l) => {
                        let next = parse_curry_config(l.path.clone(), Some(l.nested.clone()));
                        configs.push(next);
                    },
                    Meta::Path(p) => {
                        let next = parse_curry_config(p.clone(), None);
                        configs.push(next);
                    },
                    _ => {
                        panic!("Invalid curry config specification");
                    },
                }
            },
            NestedMeta::Lit(l) => {
                panic!("Invalid curry config specification");
            },
        }
    }

    // update empty config with default
    if configs.len() == 0 {
        configs.push(CurryConfig::default())
    }

    configs
}

/*
 * Format for macro arguments:
 * curry(Box("n1", bundle), Arc("n2", bundle, swap), Arc("n3"))
 */

use syn::punctuated::Punctuated;
use syn::spanned::Spanned;

fn extract_arg_pat(a : FnArg) -> Box<Pat> {
    match a {
        FnArg::Typed(p) => p.pat,
        _ => panic!("Invalid occurrence of `self`"), // TODO
    }
}

fn extract_arg_idents(fn_args :Punctuated<FnArg, syn::token::Comma>) -> (Option<Receiver>, Vec<Box<Pat>>) {
    let first_arg = fn_args[0].clone();

    let (recv, fn_args) = match first_arg {
        FnArg::Typed(p) => (None, fn_args.into_iter().skip(0)),
        FnArg::Receiver(r) => (Some(r), fn_args.into_iter().skip(1)),
    };
    (recv, fn_args.map(extract_arg_pat).collect::<Vec<_>>())
}

fn extract_arg_pat_idents(fn_args : Punctuated<FnArg, syn::token::Comma>) -> Vec<Box<Pat>> {
    return fn_args.into_iter().map(extract_arg_pat).collect::<Vec<_>>();
}

fn generate_body (fn_args : &[Box<Pat>], body : Box<Block>, curry_box : &CurryBox) -> proc_macro2::TokenStream {
    let mut acc = quote! {#body};
    let c_box = curry_box.to_token_stream();
    for arg in fn_args.iter().rev() {
        acc = quote! {
            #c_box ::new(move |#arg| {#acc})
        }
    };
    return quote! { return #acc }
} 

fn extract_type(a : FnArg) -> Box<Type> {
    match a {
        FnArg::Typed(p) => p.ty,
        _ => panic!("Not supported on types with `self`."),
    }
}

fn extract_arg_types(fn_args: Punctuated<FnArg, syn::token::Comma>) -> Vec<Box<Type>> {
    let mut result = vec![];

    for a in fn_args.into_iter() {
        match a {
            FnArg::Typed(_) => result.push(extract_type(a)),
            _ => {},
        }
    }
    return result;
}

fn extract_return_type(a : ReturnType) -> Box<Type> {
    match a {
        ReturnType::Type(_, p) => p,
        _ => panic!("Not supported on functions without return types!"), //TODO change this?
    }
}

fn curry_fn_name (i : syn::Ident, prefix : String) -> syn::Ident {
    let i_str = i.to_string();
    let i_str = format!("{}{}", prefix, i_str);
    syn::Ident::new(&i_str, i.span())
}

fn generate_return_type(
    fn_arg_types: &[Box<Type>],
    fn_return_type: Box<Type>,
    curry_box : &CurryBox,
) -> proc_macro2::TokenStream {
    
    let mut acc = quote! { #fn_return_type };
    let c_box = curry_box.to_token_stream();

    for t in fn_arg_types.into_iter().rev() {
        acc = quote! {
            #c_box<dyn Fn(#t) -> #acc>
        }
    };

    return acc
}

// when swapping, we need to rename and reconstruct the input function
fn rename_function(parsed : ItemFn, new_name : String) -> proc_macro2::TokenStream {
    let fn_body = parsed.block.clone();
    let sig = parsed.sig.clone();
    let vis = parsed.vis.clone();

    let fn_args = sig.inputs;
    let fn_return_type = extract_return_type(sig.output);

    let new_name_ident = syn::Ident::new(&new_name, parsed.span());

    let result = quote! {
        #vis fn #new_name_ident (#fn_args) -> #fn_return_type {
            #fn_body
        }
    };

    result
}

// When generating bundled curried functions, we need to clone self
// TODO: this is probably brittle - can there be inner impls inside of an impl function definition??
fn fix_body_self(body : proc_macro2::TokenStream) -> proc_macro2::TokenStream {
    let tok : proc_macro2::TokenStream = quote! {#body};
    let newtok = tok.into_iter().map(
        |t| {
            match t {
                TokenTree::Group(g) => 
                    TokenTree::Group(proc_macro2::Group::new(g.delimiter(), fix_body_self(g.stream()))),
                TokenTree::Punct(p) => TokenTree::Punct(p),
                TokenTree::Literal(l) => TokenTree::Literal(l),
                TokenTree::Ident(i) => {
                    if i.to_string() == "self" {
                        TokenTree::Ident(proc_macro2::Ident::new("c_self", i.span()))
                    } else {
                        TokenTree::Ident(i)
                    }
                },
            }
        }
    );
    return proc_macro2::TokenStream::from_iter(newtok);
}


fn generate_curry(parsed: ItemFn, options : CurryConfig) -> proc_macro2::TokenStream {
    let fn_body = parsed.block.clone();
    let sig = parsed.sig.clone();
    let vis = parsed.vis.clone();

    let new_fn_name = curry_fn_name(sig.ident.clone(), options.name);

    let fn_args = sig.inputs;
    let fn_return_type = extract_return_type(sig.output);

    let (recv, arg_idents) = extract_arg_idents(fn_args.clone());
    let first_ident = arg_idents.first().unwrap();
    let arg_types = extract_arg_types(fn_args.clone());
    let first_type = &arg_types.first().unwrap();

    let (initial_args 
        ,remaining_args
        ,remaining_types) = match recv.clone() {
        None => {
            (quote! { (#first_ident : #first_type) } 
            ,&arg_idents[1..]
            ,&arg_types[1..])
        }
        Some(recv) => {
            if options.bundle {
                (quote! { (#recv, #first_ident : #first_type) }, 
                 &arg_idents[1..],
                 &arg_types[1..])
            } else {
                (quote! { (#recv) }
                ,&arg_idents[0..]
                ,&arg_types[0..])
            }
        }
    };

    match options.curry_box {
        None => {
            let orig_result = rename_function(parsed.clone(), new_fn_name.to_string());
            return orig_result
        }
        Some(curry_box) => {
            let mut curried_body = generate_body(remaining_args, fn_body.clone(), &curry_box);
            match &recv {
                Some(_) => {
                    curried_body = fix_body_self(curried_body);
                    curried_body = quote! {
                        let mut c_self = self.clone();
                        #curried_body
                    };
                }
                None => {}
            }

            let curried_return_type = generate_return_type(remaining_types, fn_return_type, &curry_box);
        
            let curry_result = quote! {
                #vis fn #new_fn_name #initial_args -> #curried_return_type {
                    #curried_body 
                }
            };
            return curry_result
        }
    }
}

fn generate_currys(parsed: ItemFn, options : Vec<CurryConfig>) -> proc_macro2::TokenStream {
    let mut result = quote!{};

    for cfg in options {
        let next = generate_curry(parsed.clone(), cfg);
        result = quote! {
            #result
            #next
        }
    }

    result
}
