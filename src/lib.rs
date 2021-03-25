use once_cell::sync::Lazy;
use proc_macro::{TokenStream, TokenTree};
use proc_macro2::{Ident, Span};
use quote::{quote, ToTokens};
use std::cell::{Cell, RefCell};
use std::collections::{HashMap, HashSet};
use std::sync::Mutex;
use std::sync::{Arc, RwLock};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::{
    bracketed, parse_macro_input, AttributeArgs, Data, DeriveInput, Fields, GenericParam, ItemImpl,
    Meta, NestedMeta, Token, TraitBoundModifier, Type, TypeParamBound, WherePredicate,
};
use thiserror::Error;

mod replace;

#[derive(Error, Debug)]
enum Error {
    #[error("global data unavailable")]
    GlobalUnavailable,
    #[error("can't find mixin with name: {0}")]
    NoMixin(String),
    #[error("invalid expansion of the mixin")]
    InvalidExpansion,
    #[error("syn error: {0}")]
    SynError(#[from] syn::Error),
    #[error("lex error: {0}")]
    LexError(#[from] proc_macro::LexError),
}

impl Error {
    fn to_compile_error(self) -> TokenStream {
        let txt = self.to_string();
        if let Error::SynError(e) = self {
            return TokenStream::from(e.to_compile_error());
        }
        let err = syn::Error::new(Span::call_site(), txt).to_compile_error();
        TokenStream::from(err)
    }
}

struct MixinInsertArgs {
    mixins: Vec<Type>,
}

impl Parse for MixinInsertArgs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let types: Punctuated<Type, Token![,]> = input.parse_terminated(Type::parse)?;
        Ok(Self {
            mixins: types.into_iter().collect(),
        })
    }
}

#[proc_macro_attribute]
pub fn insert2(args: TokenStream, input: TokenStream) -> TokenStream {
    let args = parse_macro_input!(args as MixinInsertArgs);
    insert2_impl(args, input).unwrap_or_else(Error::to_compile_error)
}

fn insert2_impl(args: MixinInsertArgs, input: TokenStream) -> Result<TokenStream, Error> {
    let mut stream = "()".parse().expect("Foo");

    // Substitute any generic variables
    // let declaration_tree = replace::replace_generics(declaration_tree);

    Ok(stream)
}

#[proc_macro_attribute]
pub fn insert(args: TokenStream, input: TokenStream) -> TokenStream {
    let args = parse_macro_input!(args as AttributeArgs);
    insert_impl(args, input).unwrap_or_else(Error::to_compile_error)
}

fn insert_impl(args: AttributeArgs, input: TokenStream) -> Result<TokenStream, Error> {
    let mut the_struct: DeriveInput = syn::parse(input)?;
    let the_struct_name = the_struct.ident.to_string();

    // Get names of mixins to append
    let mut mixin_names = HashSet::new();
    for nested_meta in args {
        if let NestedMeta::Meta(meta) = nested_meta {
            if let Meta::Path(path) = meta {
                for path_segment in path.segments.iter() {
                    mixin_names.insert(path_segment.ident.to_string());
                }
            }
        }
    }

    let data = GLOBAL_DATA.lock().map_err(|_| Error::GlobalUnavailable)?;
    let mut mixed_fields = Vec::new();
    let mut mixed_impls = Vec::new();
    for mixin_name in mixin_names {
        let mixin = data
            .get(&mixin_name)
            .ok_or_else(|| Error::NoMixin(mixin_name.clone()))?;
        let input: TokenStream = mixin.declaration.parse()?;
        let the_mixin: DeriveInput = syn::parse(input)?;
        if let Data::Struct(st) = the_mixin.data {
            if let Fields::Named(named) = st.fields {
                mixed_fields.push(named.named);
            }
        }
        for extension in &mixin.extensions {
            let source = extension.replace(&mixin_name, &the_struct_name);
            let stream: TokenStream = source.parse()?;
            mixed_impls.push(stream);
        }
    }

    if let Data::Struct(ref mut st) = the_struct.data {
        if let Fields::Named(ref mut named) = st.fields {
            for fields in mixed_fields {
                named.named.extend(fields.into_pairs());
            }
        }
    }

    let mut stream = TokenStream::from(the_struct.into_token_stream());
    for impls in mixed_impls {
        stream.extend(impls);
    }
    Ok(stream)
}

struct Mixin {
    declaration: String,
    extensions: Vec<String>,
}

impl Mixin {
    fn from(input: &TokenStream) -> Self {
        Self {
            declaration: input.to_string(),
            extensions: Vec::new(),
        }
    }
}

struct Mixin2 {
    declaration: String,
    ident: String,
    extensions: Vec<String>,
    type_generics: Vec<String>,
}

struct MixinTraitBound {
    ident: String,
    must_implement: Vec<String>,
    default: Option<String>,
}

struct MixinTraitBounds(HashMap<String, Vec<MixinTraitBound>>);

impl MixinTraitBounds {
    fn new(inner: HashMap<String, Vec<MixinTraitBound>>) -> Self {
        Self(inner)
    }

    fn add(&mut self, key: String, value: MixinTraitBound) {
        let mut v = Vec::new();
        // TODO append
        v.push(value);
        self.0.insert(key, v);
    }
}

fn stringify_trait_bound(tb: &syn::TraitBound) -> String {
    let mut result: String = "".to_owned();

    match tb.modifier {
        TraitBoundModifier::Maybe(token) => {
            result.push_str(quote!( #token ).to_string().as_ref());
        }
        TraitBoundModifier::None => {}
    }

    let path = &tb.path;
    result.push_str(quote!( #path ).to_string().as_ref());

    result
}

impl Parse for Mixin2 {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let declaration = input.clone().to_string();
        let declaration_tree: DeriveInput = input.parse()?;
        let ident = declaration_tree.ident.to_string();

        let mut type_generics = Vec::new();
        let mut trait_bounds = MixinTraitBounds::new(HashMap::new());

        for generic in declaration_tree.generics.params.iter() {
            match generic {
                GenericParam::Type(param) => {
                    let mut must_implement = Vec::new();

                    for attr in param.attrs.iter() {
                        return Err(syn::Error::new_spanned(
                            attr,
                            "Attributes on generic types in mixins are not supported",
                        ));
                    }

                    for bound in param.bounds.iter() {
                        match bound {
                            TypeParamBound::Trait(trait_bound) => {
                                if let Some(lifetimes) = &trait_bound.lifetimes {
                                    return Err(syn::Error::new_spanned(
                                        lifetimes,
                                        "Lifetime bounds in mixins are not supported",
                                    ));
                                }

                                must_implement.push(stringify_trait_bound(trait_bound));
                            }
                            TypeParamBound::Lifetime(lt) => {
                                return Err(syn::Error::new_spanned(
                                    lt,
                                    "Lifetime bounds in mixins are not supported",
                                ));
                            }
                        }
                    }

                    let ident = (&param.ident).to_string();
                    type_generics.push(ident.clone());
                    trait_bounds.add(
                        ident.clone(),
                        MixinTraitBound {
                            ident,
                            must_implement,
                            default: param.default.as_ref().map(|t| quote!( #t ).to_string()),
                        },
                    );
                }
                GenericParam::Lifetime(LifetimeDef) => {
                    return Err(syn::Error::new_spanned(
                        generic,
                        "Lifetime generics in mixins are not supported",
                    ));
                }
                GenericParam::Const(ConstParam) => {
                    return Err(syn::Error::new_spanned(
                        generic,
                        "Const generics in mixins are not supported",
                    ));
                }
            }
        }

        if let Some(where_clause) = declaration_tree.generics.where_clause {
            for predicate in where_clause.predicates.iter() {
                match predicate {
                    WherePredicate::Type(predicate_type) => {
                        if let Some(lt) = &predicate_type.lifetimes {
                            return Err(syn::Error::new_spanned(
                                lt,
                                "Where clauses in mixins with lifetimes are not supported",
                            ));
                        }

                        let ident = &predicate_type.bounded_ty;
                        let mut must_implement = Vec::new();

                        for bound in predicate_type.bounds.iter() {
                            match bound {
                                TypeParamBound::Trait(trait_bound) => {
                                    if let Some(lifetimes) = &trait_bound.lifetimes {
                                        return Err(syn::Error::new_spanned(
                                            lifetimes,
                                            "Lifetime bounds in mixins are not supported",
                                        ));
                                    }
                                    must_implement.push(stringify_trait_bound(trait_bound));
                                }
                                TypeParamBound::Lifetime(_) => {
                                    return Err(syn::Error::new_spanned(
                                        bound, "Lifetime bounds on predicate types in mixins are not supported"
                                    ));
                                }
                            }
                        }

                        let ident = quote!( #ident ).to_string();
                        trait_bounds.add(
                            ident.clone(),
                            MixinTraitBound {
                                ident,
                                must_implement,
                                default: None,
                            },
                        );
                    }
                    WherePredicate::Lifetime(_) => {
                        return Err(syn::Error::new_spanned(
                            predicate,
                            "Where clauses in mixins with lifetimes are not supported",
                        ));
                    }
                    WherePredicate::Eq(_) => {
                        return Err(syn::Error::new_spanned(
                            predicate,
                            "Where clauses in mixins with '=' are not supported",
                        ));
                    }
                }
            }
        }

        Ok(Self {
            declaration,
            extensions: Vec::new(),
            type_generics,
            ident,
        })
    }
}

static GLOBAL_DATA: Lazy<Mutex<HashMap<String, Mixin>>> = Lazy::new(|| Mutex::new(HashMap::new()));
static GLOBAL_DATA_2: Lazy<Mutex<HashMap<String, Mixin2>>> =
    Lazy::new(|| Mutex::new(HashMap::new()));

#[proc_macro_attribute]
pub fn declare(_attribute: TokenStream, input: TokenStream) -> TokenStream {
    declare_impl(input).unwrap_or_else(Error::to_compile_error)
}

fn declare_impl(input: TokenStream) -> Result<TokenStream, Error> {
    // Keep it just to let the compiler check it
    let mut output: TokenStream = "#[allow(dead_code)]".parse()?;
    output.extend(input.clone().into_iter());

    // Consume the struct
    let mixin = Mixin::from(&input);
    let input: DeriveInput = syn::parse(input).unwrap();
    let name = input.ident.to_string();
    let generics = input.generics;
    let mut data = GLOBAL_DATA.lock().map_err(|_| Error::GlobalUnavailable)?;
    data.insert(name, mixin);
    // And give the empty output back
    Ok(output)
}

#[proc_macro_attribute]
pub fn declare2(_attribute: TokenStream, input: TokenStream) -> TokenStream {
    declare2_impl(input).unwrap_or_else(Error::to_compile_error)
}

fn declare2_impl(input: TokenStream) -> Result<TokenStream, Error> {
    // Keep it just to let the compiler check it
    let mut output: TokenStream = "#[allow(dead_code)]".parse()?;
    output.extend(input.clone().into_iter());

    // Consume the struct
    let code = input.clone().to_string();
    let mixin: Mixin2 = syn::parse(input.clone())?;
    let name = mixin.ident.to_string();
    let mut data = GLOBAL_DATA_2.lock().map_err(|_| Error::GlobalUnavailable)?;
    data.insert(name, mixin);

    // TODO
    // let mut output2: TokenStream = "const _: fn() = || {
    //     fn assert_impl_all<T: ?Sized $(+ $trait)+>() {}
    //     assert_impl_all::<$type>();
    // };".parse()?;
    // output.extend(output2);

    // And give the empty output back
    Ok(output)
}

#[proc_macro_attribute]
pub fn expand(_attribute: TokenStream, input: TokenStream) -> TokenStream {
    expand_impl(input).unwrap_or_else(Error::to_compile_error)
}

fn expand_impl(input: TokenStream) -> Result<TokenStream, Error> {
    // Keep it just to let the compiler check it
    let mut output: TokenStream = "#[allow(dead_code)]".parse()?;
    output.extend(input.clone().into_iter());

    let code = input.to_string();
    let ident = input.into_iter().skip(1).next();
    let name;
    match ident {
        Some(TokenTree::Ident(ident)) => {
            name = ident.to_string();
        }
        _ => {
            return Err(Error::InvalidExpansion);
        }
    }
    let mut data = GLOBAL_DATA.lock().map_err(|_| Error::GlobalUnavailable)?;
    let mixin = data.get_mut(&name).ok_or_else(|| Error::NoMixin(name))?;
    mixin.extensions.push(code);
    // Drops the original impl
    Ok(output)
}

#[proc_macro_attribute]
pub fn expand2(_attribute: TokenStream, input: TokenStream) -> TokenStream {
    expand2_impl(input).unwrap_or_else(Error::to_compile_error)
}

fn expand2_impl(input: TokenStream) -> Result<TokenStream, Error> {
    // Keep it just to let the compiler check it
    let mut output: TokenStream = "#[allow(dead_code)]".parse()?;
    output.extend(input.clone().into_iter());

    let mut declaration_tree: DeriveInput = syn::parse(input.clone())?;
    let generics = &declaration_tree.generics;
    if generics.params.len() > 0 || generics.where_clause.is_some() {
        Err(syn::Error::new_spanned(
            declaration_tree.generics,
            "#[mixin::expand] blocks do not take generics"
        ))?
    }

    let code = input.to_string();
    let name = declaration_tree.ident.to_string();
    let mut data = GLOBAL_DATA.lock().map_err(|_| Error::GlobalUnavailable)?;
    let mixin = data.get_mut(&name).ok_or_else(|| Error::NoMixin(name))?;
    mixin.extensions.push(code);
    // Drops the original impl
    Ok(output)
}
