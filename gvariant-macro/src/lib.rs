extern crate proc_macro;
use proc_macro::TokenStream;
use std::error::Error;

mod type_parser;

use type_parser::GVariantType;

#[proc_macro]
pub fn gv(input: TokenStream) -> TokenStream {
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
    let gv = &gv.as_slice()[1..gv.len() - 1];
    type_for_typestr(gv).unwrap().parse().unwrap()
}

fn type_for_typestr(gv_typestr: &[u8]) -> Result<String, Box<dyn Error>> {
    let spec = type_parser::one(gv_typestr)?;
    let mut code: Vec<u8> = vec![];
    marker_type(&spec, &mut code)?;
    Ok(String::from_utf8(code)?)
}

fn marker_type(t : &GVariantType, f: &mut impl std::io::Write) -> std::io::Result<()> {
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
        },
        GVariantType::M(t) => {
            write!(f, "::gvariant::marker::M::<")?;
            marker_type(t, f)?;
            write!(f, ">")
        },
        GVariantType::Tuple(_) | GVariantType::DictItem(_) => {
            write!(f, "Structure{}", escape(t.to_string()))
        }
    }
}

fn escape(x: String) -> String {
    let mut out = x.into_bytes();
    for c in &mut out.iter_mut() {
        *c = match *c {
            b'(' => b'c',
            b'{' => b'f',
            b')' => b'7',
            b'}' => b'3',
            _ => *c,
        }
    }
    String::from_utf8(out).unwrap()
}