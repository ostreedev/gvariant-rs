//! This is an implementation detail of the [gvariant](../gvariant/index.html) crate

extern crate proc_macro;
use proc_macro::TokenStream;
use std::{collections::HashSet, error::Error};

mod generate_impl;
mod type_parser;
use generate_impl::{escape, size_of};
use syn::{Ident, LitStr, Token, parse::Parse, parse_macro_input, punctuated::Punctuated};

use type_parser::{one, GVariantType};

#[proc_macro]
pub fn gv_type(input: TokenStream) -> TokenStream {
    let typestr = parse_macro_input!(input as LitStr).value();
    let spec = type_parser::one(typestr.as_bytes()).unwrap();
    marker_type(&spec, "gv_decls::").parse().unwrap()
}

struct TypeDecl {
    name: Ident,
    typestr: LitStr,
}
impl Parse for TypeDecl {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        input.parse::<syn::token::Type>()?;
        let name = input.parse()?;
        input.parse::<Token![=]>()?;
        let typestr = input.parse()?;
        Ok(Self {name, typestr})
    }
}
struct TypeStmtList {
    stmts: Punctuated<TypeDecl, Token![;]>,
}
impl Parse for TypeStmtList {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Self {stmts: Punctuated::<TypeDecl, Token![;]>::parse_terminated(input)?})
    }
}

#[proc_macro]
pub fn define_gv(input: TokenStream) -> TokenStream {
    let typestrs = parse_macro_input!(input as TypeStmtList);
    let mut seen = HashSet::new();
    let mut out = TokenStream::new();
    for stmt in typestrs.stmts {
        let ts : TokenStream = generate_impl::generate_types(&one(stmt.typestr.value().as_ref()).unwrap(), &mut seen)
            .unwrap()
            .parse()
            .unwrap();
        out.extend(Some(ts));
    }
    out
}

#[proc_macro]
pub fn reference_gv(input: TokenStream) -> TokenStream {
    let type_stmts = parse_macro_input!(input as TypeStmtList);
    let mut out = TokenStream::new();
    for stmt in type_stmts.stmts {
        let gvt = type_parser::one(stmt.typestr.value().as_bytes()).unwrap();
        let ts : TokenStream = format!("type {} = {};", stmt.name.to_string(), marker_type(&gvt, "gv_decls::")).parse().unwrap();
        out.extend(Some(ts));
    }
    out
}

pub(crate) fn marker_type(t: &GVariantType, decls_ns: &str) -> String {
    match t {
        GVariantType::B => "gvariant::Bool".to_string(),
        GVariantType::Y => "u8".to_string(),
        GVariantType::N => "i16".to_string(),
        GVariantType::Q => "u16".to_string(),
        GVariantType::I => "i32".to_string(),
        GVariantType::U => "u32".to_string(),
        GVariantType::X => "i64".to_string(),
        GVariantType::T => "u64".to_string(),
        GVariantType::D => "f64".to_string(),
        GVariantType::S => "gvariant::Str".to_string(),
        GVariantType::O => "gvariant::Str".to_string(),
        GVariantType::G => "gvariant::Str".to_string(),
        GVariantType::V => "gvariant::Variant".to_string(),
        GVariantType::A(t) => match size_of(t) {
            None => format!("gvariant::NonFixedWidthArray::<{}>", marker_type(t, decls_ns)),
            Some(_) => format!("[{}]", marker_type(t, decls_ns)),
        },
        GVariantType::M(t) => match size_of(t) {
            None => format!("gvariant::MaybeNonFixedSize::<{}>", marker_type(t, decls_ns)),
            Some(_) => format!("gvariant::MaybeFixedSize::<{}>", marker_type(t, decls_ns)),
        },
        GVariantType::Tuple(_) | GVariantType::DictItem(_) => {
            format!("{decls_ns}Structure{name}", decls_ns = decls_ns, name = escape(t.to_string()))
        }
    }
}
