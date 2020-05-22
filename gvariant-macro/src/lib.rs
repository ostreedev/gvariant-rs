extern crate proc_macro;
use proc_macro::TokenStream;
use std::error::Error;

mod generate_impl;
mod type_parser;
use generate_impl::escape;

use type_parser::GVariantType;

#[proc_macro]
pub fn gv(input: TokenStream) -> TokenStream {
    type_for_typestr(&tokenstream_to_typestr(input))
        .unwrap()
        .parse()
        .unwrap()
}

#[proc_macro]
pub fn define_gv(input: TokenStream) -> TokenStream {
    let typestr = tokenstream_to_typestr(input);
    generate_impl::generate_types(&typestr)
        .unwrap()
        .parse()
        .unwrap()
}

fn tokenstream_to_typestr(input: TokenStream) -> std::vec::Vec<u8> {
    let arg = input.into_iter().next().expect("Missing argument");
    let gv;
    if let proc_macro::TokenTree::Literal(lit) = arg {
        gv = lit.to_string().as_bytes().to_vec();
    } else {
        panic!("Argument must be string literal");
    }
    match gv.as_slice() {
        [b'"', .., b'"'] => (),
        _ => panic!("Argument must be a string literal"),
    };
    gv.as_slice()[1..gv.len() - 1].to_owned()
}

fn type_for_typestr(gv_typestr: &[u8]) -> Result<String, Box<dyn Error>> {
    let spec = type_parser::one(gv_typestr)?;
    let mut code: Vec<u8> = vec![];
    marker_type(&spec, &mut code)?;
    Ok(String::from_utf8(code)?)
}

fn marker_type(t: &GVariantType, f: &mut impl std::io::Write) -> std::io::Result<()> {
    match t {
        GVariantType::B => write!(f, "::gvariant::marker::B"),
        GVariantType::Y => write!(f, "::gvariant::marker::Y"),
        GVariantType::N => write!(f, "::gvariant::marker::N"),
        GVariantType::Q => write!(f, "::gvariant::marker::Q"),
        GVariantType::I => write!(f, "::gvariant::marker::I"),
        GVariantType::U => write!(f, "::gvariant::marker::U"),
        GVariantType::X => write!(f, "::gvariant::marker::X"),
        GVariantType::T => write!(f, "::gvariant::marker::T"),
        GVariantType::D => write!(f, "::gvariant::marker::D"),
        GVariantType::S => write!(f, "::gvariant::marker::S"),
        GVariantType::O => write!(f, "::gvariant::marker::O"),
        GVariantType::G => write!(f, "::gvariant::marker::G"),
        GVariantType::V => write!(f, "::gvariant::marker::V"),
        GVariantType::A(t) => {
            write!(f, "::gvariant::marker::A::<")?;
            marker_type(t, f)?;
            write!(f, ">")
        }
        GVariantType::M(t) => {
            write!(f, "::gvariant::marker::M::<")?;
            marker_type(t, f)?;
            write!(f, ">")
        }
        GVariantType::Tuple(_) | GVariantType::DictItem(_) => write!(
            f,
            "_gvariant_macro_{name}::Marker{name}",
            name = escape(t.to_string())
        ),
    }
}
