extern crate proc_macro;
use proc_macro::TokenStream;
use std::error::Error;

mod generate_impl;
mod type_parser;
use generate_impl::{escape, size_of};
use syn::{parse_macro_input, LitStr};

use type_parser::{one, GVariantType};

#[proc_macro]
pub fn gv(input: TokenStream) -> TokenStream {
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
    let mut code: Vec<u8> = vec![];
    marker_type(&spec, &mut code)?;
    Ok(String::from_utf8(code)?)
}

pub(crate) fn marker_type(t: &GVariantType, f: &mut impl std::io::Write) -> std::io::Result<()> {
    match t {
        GVariantType::B => write!(f, "::gvariant::Bool"),
        GVariantType::Y => write!(f, "u8"),
        GVariantType::N => write!(f, "i16"),
        GVariantType::Q => write!(f, "u16"),
        GVariantType::I => write!(f, "i32"),
        GVariantType::U => write!(f, "u32"),
        GVariantType::X => write!(f, "i64"),
        GVariantType::T => write!(f, "u64"),
        GVariantType::D => write!(f, "f64"),
        GVariantType::S => write!(f, "::gvariant::Str"),
        GVariantType::O => write!(f, "::gvariant::Str"),
        GVariantType::G => write!(f, "::gvariant::Str"),
        GVariantType::V => write!(f, "::gvariant::Variant"),
        GVariantType::A(t) => match size_of(t) {
            None => {
                write!(f, "::gvariant::NonFixedWidthArray::<")?;
                marker_type(t, f)?;
                write!(f, ">")
            }
            Some(_) => {
                write!(f, "[")?;
                marker_type(t, f)?;
                write!(f, "]")
            }
        },
        GVariantType::M(t) => {
            match size_of(t) {
                None => write!(f, "::gvariant::MaybeNonFixedSize::<")?,
                Some(_) => write!(f, "::gvariant::MaybeFixedSize::<")?,
            };
            marker_type(t, f)?;
            write!(f, ">")
        }
        GVariantType::Tuple(_) | GVariantType::DictItem(_) => write!(
            f,
            "_gvariant_macro_{name}::Structure{name}",
            name = escape(t.to_string())
        ),
    }
}
