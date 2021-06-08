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
    type_for_typestr(typestr.as_ref()).unwrap().parse().unwrap()
}

#[proc_macro]
pub fn gv_marker(input: TokenStream) -> TokenStream {
    let typestr = parse_macro_input!(input as LitStr).value();
    marker_for_typestr(typestr.as_ref()).unwrap().parse().unwrap()
}

fn marker_for_typestr(gv_typestr: &[u8]) -> Result<String, Box<dyn Error>> {
    let spec = type_parser::one(gv_typestr)?;
    Ok(format!("Marker{}", escape(spec.to_string())))
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
        let ts : TokenStream = format!("type {} = gv_decls::Marker{};", stmt.name.to_string(), escape(stmt.typestr.value())).parse().unwrap();
        out.extend(Some(ts));
    }
    out
}

fn type_for_typestr(gv_typestr: &[u8]) -> Result<String, Box<dyn Error>> {
    let spec = type_parser::one(gv_typestr)?;
    Ok(marker_type(&spec))
}

pub(crate) fn marker_type(t: &GVariantType) -> String {
    match t {
        GVariantType::B => "Bool".to_string(),
        GVariantType::Y => "u8".to_string(),
        GVariantType::N => "i16".to_string(),
        GVariantType::Q => "u16".to_string(),
        GVariantType::I => "i32".to_string(),
        GVariantType::U => "u32".to_string(),
        GVariantType::X => "i64".to_string(),
        GVariantType::T => "u64".to_string(),
        GVariantType::D => "f64".to_string(),
        GVariantType::S => "Str".to_string(),
        GVariantType::O => "Str".to_string(),
        GVariantType::G => "Str".to_string(),
        GVariantType::V => "Variant".to_string(),
        GVariantType::A(t) => match size_of(t) {
            None => format!("NonFixedWidthArray::<{}>", marker_type(t)),
            Some(_) => format!("[{}]", marker_type(t)),
        },
        GVariantType::M(t) => match size_of(t) {
            None => format!("MaybeNonFixedSize::<{}>", marker_type(t)),
            Some(_) => format!("MaybeFixedSize::<{}>", marker_type(t)),
        },
        GVariantType::Tuple(_) | GVariantType::DictItem(_) => {
            format!("Structure{name}", name = escape(t.to_string()))
        }
    }
}
