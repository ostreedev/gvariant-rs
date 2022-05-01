use std::error::Error;
use std::fmt;

#[derive(Debug)]
struct ParseError {
    message: &'static str,
}
impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Parse Error: {}", self.message)
    }
}
impl Error for ParseError {}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum GVariantType {
    B,
    Y,
    N,
    Q,
    I,
    U,
    X,
    T,
    D,
    S,
    O,
    G,
    V,
    A(Box<GVariantType>),
    M(Box<GVariantType>),
    Tuple(Vec<GVariantType>),
    DictItem(Box<[GVariantType; 2]>),
}

impl fmt::Display for GVariantType {
    fn fmt(&self, out: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            GVariantType::B => write!(out, "b")?,
            GVariantType::Y => write!(out, "y")?,
            GVariantType::N => write!(out, "n")?,
            GVariantType::Q => write!(out, "q")?,
            GVariantType::I => write!(out, "i")?,
            GVariantType::U => write!(out, "u")?,
            GVariantType::X => write!(out, "x")?,
            GVariantType::T => write!(out, "t")?,
            GVariantType::D => write!(out, "d")?,
            GVariantType::S => write!(out, "s")?,
            GVariantType::O => write!(out, "o")?,
            GVariantType::G => write!(out, "g")?,
            GVariantType::V => write!(out, "v")?,
            GVariantType::A(x) => write!(out, "a{}", x)?,
            GVariantType::M(x) => write!(out, "m{}", x)?,
            GVariantType::Tuple(items) => {
                write!(out, "(")?;
                for x in items {
                    write!(out, "{}", &x)?;
                }
                write!(out, ")")?;
            }
            GVariantType::DictItem(x) => {
                write!(out, "{{{}{}}}", &x[0], &x[1])?;
            }
        };
        Ok(())
    }
}

pub(crate) fn one(spec: &[u8]) -> Result<GVariantType, Box<dyn Error>> {
    let mut it = spec.iter().copied();
    let out = parse_one_type_spec(
        it.next().ok_or(ParseError {
            message: "Type string is empty",
        })?,
        &mut it,
    )?;
    if it.next().is_some() {
        return Err(ParseError {
            message: "More than one type in type string",
        }
        .into());
    }
    Ok(out)
}

fn parse_one_type_spec(
    c: u8,
    it: &mut impl Iterator<Item = u8>,
) -> Result<GVariantType, Box<dyn Error>> {
    Ok(match c {
        b'b' => GVariantType::B,
        b'y' => GVariantType::Y,
        b'n' => GVariantType::N,
        b'q' => GVariantType::Q,
        b'i' => GVariantType::I,
        b'u' => GVariantType::U,
        b'x' => GVariantType::X,
        b't' => GVariantType::T,
        b'd' => GVariantType::D,

        b's' => GVariantType::S,
        b'o' => GVariantType::O,
        b'g' => GVariantType::G,
        b'v' => GVariantType::V,

        b'a' => GVariantType::A(Box::new(parse_one_type_spec(
            it.next().ok_or(ParseError {
                message: "Array item type required",
            })?,
            it,
        )?)),
        b'm' => GVariantType::M(Box::new(parse_one_type_spec(
            it.next().ok_or(ParseError {
                message: "Maybe item type required",
            })?,
            it,
        )?)),

        b'(' => {
            let mut subtype = vec![];
            loop {
                let c = it.next().ok_or(ParseError {
                    message: "EOF when inside tuple definition",
                })?;
                if c == b')' {
                    break;
                }
                subtype.push(parse_one_type_spec(c, it)?);
            }
            GVariantType::Tuple(subtype)
        }
        b'{' => {
            let a = parse_one_type_spec(
                it.next().ok_or(ParseError {
                    message: "EOF: Dict entry requires two types",
                })?,
                it,
            )?;
            let b = parse_one_type_spec(
                it.next().ok_or(ParseError {
                    message: "EOF: Dict entry requires two types",
                })?,
                it,
            )?;
            if it.next().ok_or(ParseError {
                message: "EOF when expecting }",
            })? != b'}'
            {
                return Err(Box::new(ParseError {
                    message: "Expected }, got something else",
                }));
            }
            GVariantType::DictItem(Box::new([a, b]))
        }
        _ => {
            return Err(Box::new(ParseError {
                message: "Unexpected charactor",
            }))
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn parsing_complex_type() {
        let c = one(b"(bynqiuxtdsogvaamb(abi)m(yy)a{si})").unwrap();

        use super::GVariantType::*;
        assert_eq!(
            c,
            GVariantType::Tuple(vec![
                B,
                Y,
                N,
                Q,
                I,
                U,
                X,
                T,
                D,
                S,
                O,
                G,
                V,
                A(Box::new(A(Box::new(M(Box::new(B)))))),
                Tuple(vec![A(Box::new(B)), I]),
                M(Box::new(Tuple(vec![Y, Y]))),
                A(Box::new(DictItem(Box::new([S, I]))))
            ])
        );
        assert_eq!(one(b"i").unwrap(), I);
    }
}
