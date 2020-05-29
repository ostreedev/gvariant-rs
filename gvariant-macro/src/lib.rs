//! This is an implementation detail of the [gvariant](../gvariant/index.html) crate

extern crate proc_macro;
use proc_macro::TokenStream;
use std::error::Error;

mod generate_impl;
mod type_parser;
use generate_impl::{escape, size_of};
use syn::{parse_macro_input, LitStr};

use type_parser::{one, GVariantType};

#[proc_macro]
pub fn gv_type(input: TokenStream) -> TokenStream {
    let typestr = parse_macro_input!(input as LitStr).value();
    type_for_typestr(typestr.as_ref()).unwrap().parse().unwrap()
}

#[proc_macro]
pub fn define_gv(input: TokenStream) -> TokenStream {
    let typestr = parse_macro_input!(input as LitStr).value();
    generate_impl::generate_types(&one(typestr.as_ref()).unwrap())
        .unwrap()
        .parse()
        .unwrap()
}

fn type_for_typestr(gv_typestr: &[u8]) -> Result<String, Box<dyn Error>> {
    let spec = type_parser::one(gv_typestr)?;
    Ok(marker_type(&spec))
}

pub(crate) fn marker_type(t: &GVariantType) -> String {
    match t {
        GVariantType::B => "::gvariant::Bool".to_string(),
        GVariantType::Y => "u8".to_string(),
        GVariantType::N => "i16".to_string(),
        GVariantType::Q => "u16".to_string(),
        GVariantType::I => "i32".to_string(),
        GVariantType::U => "u32".to_string(),
        GVariantType::X => "i64".to_string(),
        GVariantType::T => "u64".to_string(),
        GVariantType::D => "f64".to_string(),
        GVariantType::S => "::gvariant::Str".to_string(),
        GVariantType::O => "::gvariant::Str".to_string(),
        GVariantType::G => "::gvariant::Str".to_string(),
        GVariantType::V => "::gvariant::Variant".to_string(),
        GVariantType::A(t) => match size_of(t) {
            None => format!("::gvariant::NonFixedWidthArray::<{}>", marker_type(t)),
            Some(_) => format!("[{}]", marker_type(t)),
        },
        GVariantType::M(t) => match size_of(t) {
            None => format!("::gvariant::MaybeNonFixedSize::<{}>", marker_type(t)),
            Some(_) => format!("::gvariant::MaybeFixedSize::<{}>", marker_type(t)),
        },
        GVariantType::Tuple(_) | GVariantType::DictItem(_) => {
            format!("Structure{name}", name = escape(t.to_string()))
        }
    }
}
