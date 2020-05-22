use std::error::Error;
use std::io::Write;

use crate::{
    marker_type,
    type_parser::{one, GVariantType},
};

pub(crate) fn generate_types(gv_typestr: &[u8]) -> Result<String, Box<dyn Error>> {
    let spec = one(gv_typestr)?;
    Ok(match &spec {
        GVariantType::Tuple(children) => generate_tuple(&spec, children)?,
        GVariantType::DictItem(_) => todo!(),
        // Everything else is a builtin
        _ => "".to_owned(),
    })
}

fn generate_tuple(
    spec: &GVariantType,
    children: &Vec<GVariantType>,
) -> Result<String, Box<dyn Error>> {
    let mut code: Vec<u8> = vec![];
    let alignment = align_of(&spec);
    let size = size_of(&spec);
    let sizedtrait = if size.is_some() {
        "FixedSize"
    } else {
        "NonFixedSize"
    };
    let n_frames: usize = children
        .into_iter()
        .filter(|x| size_of(x).is_some())
        .count();
    // After all of the items have been added, a framing offset is appended, in
    // reverse order, for each non-fixed-sized item that is not the last item in
    // the structure.
    let n_frame_offsets = if let Some(last_item) = children.last() {
        if size_of(last_item).is_none() {
            n_frames - 1
        } else {
            n_frames
        }
    } else {
        n_frames
    };
    write!(
        code,
        "
mod _gvariant_macro_{spec} {{
    #[macro_use]
    use ref_cast::RefCast;
    use ::gvariant::aligned_bytes::{{AlignedSlice, AsAligned}};
    use ::gvariant::marker::GVariantMarker;
    use ::gvariant::offset::{{align_offset, AlignedOffset}};
    use std::convert::TryInto;

    #[derive(Debug, RefCast)]
    #[repr(transparent)]
    pub(crate) struct Marker{spec} {{
        data: ::gvariant::aligned_bytes::AlignedSlice<::gvariant::aligned_bytes::A{alignment}>,
    }}
    impl GVariantMarker for Marker{spec} {{
        type Alignment = ::gvariant::aligned_bytes::A{alignment};
        const SIZE: Option<usize> = {size:?};
        fn _mark(data: &::gvariant::aligned_bytes::AlignedSlice<Self::Alignment>) -> &Self {{
            Self::ref_cast(data.as_ref())
        }}
    }}
    impl ::gvariant::marker::{sizedtrait} for Marker{spec} {{}}
    impl Marker{spec} {{
        pub fn split(&self) -> (\n",
        spec = escape(spec.to_string()),
        alignment = alignment,
        size = size,
        sizedtrait = sizedtrait
    )?;

    // Write out the return type:
    for child in children {
        write!(code, "                &")?;
        marker_type(child, &mut code)?;
        write!(code, ",\n")?;
    }
    write!(code, "            ) {{\n")?;

    if n_frame_offsets > 0 {
        write!(
            code,
            "
            let osz = ::gvariant::offset_size(self.data.len());
            let frame_offset_offset = self.data.len() - osz as usize;
"
        )?;
    }
    for ((n, child), (i, a, b, c)) in children.iter().enumerate().zip(generate_table(children)) {
        let fo_plus = if i == -1 {
            "".to_string()
        } else {
            format!(
                "::gvariant::nth_last_frame_offset(&self.data, osz, {}) + ",
                i
            )
        };
        write!(code, "\n            // {ty}\n            let offset_{n} : AlignedOffset<::gvariant::aligned_bytes::A{calign}> = align_offset::<::gvariant::aligned_bytes::A{b}>({fo_plus}{a}) | AlignedOffset::<::gvariant::aligned_bytes::A{calign}>::try_new({c}).unwrap();\n",
            ty=child.to_string(), n=n, a=a, b=b, c=c, fo_plus=fo_plus, calign=align_of(child))?;
        let end = if let Some(size) = size_of(child) {
            format!("offset_{n}.to_usize() + {size}", n = n, size = size)
        } else if n == children.len() - 1 {
            if n_frame_offsets == 0 {
                "self.data.len()".to_string()
            } else {
                format!(
                    "self.data.len() - osz * {n_frame_offsets}",
                    n_frame_offsets = n_frame_offsets
                )
            }
        } else {
            format!(
                "::gvariant::nth_last_frame_offset(&self.data, osz, {i})",
                i = i + 1
            )
        };
        write!(
            code,
            "            let end_{n} : usize = {end};\n",
            n = n,
            end = end
        )?;
    }
    write!(code, "            (\n")?;
    for (n, child) in children.iter().enumerate() {
        write!(code, "                ")?;
        marker_type(child, &mut code)?;
        write!(
            code,
            "::_mark(&self.data.as_aligned()[..end_{n}][offset_{n}..]),\n",
            n = n
        )?;
    }
    write!(
        code,
        "            )
        }}
    }}
"
    )?;
    if size.is_some() {
        write_packed_struct(spec, children.as_ref(), &mut code)?;
        write!(
            code,
            "
            impl ::gvariant::RustType for Marker{spec} {{
                type RefType = Structure{spec};
                fn default_ref() -> &'static Self::RefType {{
                    todo!()
                }}
            }}",
            spec = escape(spec.to_string())
        )?;
    }

    write!(
        code,
        "
}}
"
    )?;
    Ok(String::from_utf8(code)?)
}

pub(crate) fn escape(x: String) -> String {
    let mut out = x.into_bytes();
    for c in &mut out.iter_mut() {
        *c = match *c {
            b'(' => b'C',
            b'{' => b'F',
            b')' => b'7',
            b'}' => b'3',
            _ => *c,
        }
    }
    String::from_utf8(out).unwrap()
}

fn align_of(t: &GVariantType) -> usize {
    match t {
        GVariantType::B | GVariantType::Y => 1,
        // Each integer type has alignment equal to its fixed size.
        GVariantType::N | GVariantType::Q => 2,
        GVariantType::I | GVariantType::U => 4,
        GVariantType::X | GVariantType::T => 8,
        GVariantType::D => 8,
        // Including object paths and signature strings, strings are not
        // fixed-sized and have an alignment of 1.
        GVariantType::S | GVariantType::O | GVariantType::G => 1,
        // The alignment of a maybe type is always equal to the alignment of
        // its element type.
        GVariantType::M(t) => align_of(t),
        // The alignment of an array type is always equal to the alignment of
        // its element type.
        GVariantType::A(t) => align_of(t),
        // The alignment of a container type is equal to the largest alignment
        // of any potential child of that container.
        GVariantType::Tuple(subtypes) => subtypes.iter().map(align_of).max().unwrap_or(1),
        GVariantType::DictItem(subtypes) => align_of(&subtypes[0]).max(align_of(&subtypes[1])),
        // This means that the variant type has an alignment of 8 (since it
        // could potentially contain a value of any other type and the maximum
        // alignment is 8).
        GVariantType::V => 8,
    }
}

fn size_of(t: &GVariantType) -> Option<usize> {
    match t {
        GVariantType::B => Some(1),
        GVariantType::Y => Some(1),
        GVariantType::N => Some(2),
        GVariantType::Q => Some(2),
        GVariantType::I => Some(4),
        GVariantType::U => Some(4),
        GVariantType::X => Some(8),
        GVariantType::T => Some(8),
        GVariantType::D => Some(8),
        GVariantType::S
        | GVariantType::O
        | GVariantType::G
        | GVariantType::V
        | GVariantType::A(_)
        | GVariantType::M(_) => None,
        GVariantType::Tuple(subtypes) => {
            let mut pos: usize = 0;
            if subtypes.is_empty() {
                // the fixed size must be non-zero. This case would only occur
                // for structures of the unit type or structures containing
                // only such structures (recursively). This problem issolved by
                // arbitrary declaring that the serialised encoding of an
                // instance of the unit typeis a single zero byte (size 1).
                return Some(1);
            }
            for t in subtypes {
                pos = align(pos, align_of(t));
                match size_of(t) {
                    Some(s) => pos += s,
                    None => return None,
                }
            }
            // the fixed sized must be a multiple of the alignment of the
            // structure. This is accomplished by adding zero-filled padding
            // bytes to the end of any fixed-width structure until this
            // property becomes true.
            pos = align(pos, align_of(t));
            Some(pos)
        }
        GVariantType::DictItem(x) => match (size_of(&x[0]), size_of(&x[1])) {
            (Some(a), Some(b)) => {
                let mut pos = a;
                pos = align(pos, align_of(&x[1]));
                pos += b;
                pos = align(pos, align_of(&t));
                Some(pos)
            }
            _ => None,
        },
    }
}

// This is a streight port of the Python code from the GVariant paper section
// 3.2.2 Computing the Table
//
// The offset of an item n is then:
//
//     let (i, a, b, c) = table[n];
//     let off = (frame_offset[i] + a + b - 1) & !(b - 1) | c
//
// which is:
//
//     align<B>(frame_offset[i] + a) | c
//
// Postconditions:
//
// * b and c are aligned to the child alignment
fn generate_table(children: &[GVariantType]) -> Vec<(isize, usize, u8, usize)> {
    let (mut i, mut a, mut b, mut c) = (-1, 0, 1, 0);
    let mut table = vec![];
    for child in children {
        let al = align_of(child);
        if al <= b {
            // merge rule #1
            c = align(c, al)
        } else {
            // merge rule #2
            a = a + align(c, b);
            b = al;
            c = 0;
        }
        table.push((i, a, b as u8, c));
        if let Some(size) = size_of(child) {
            // merge rule #3
            c += size;
        } else {
            // item is not fixed-sized
            i += 1;
            a = 0;
            b = 1;
            c = 0;
        }
    }
    return table;
}

struct RustType<'a>(&'a GVariantType);
impl<'a> Display for RustType<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            GVariantType::B => write!(f, "::gvariant::GVariantBool"),
            GVariantType::Y => write!(f, "u8"),
            GVariantType::N => write!(f, "i16"),
            GVariantType::Q => write!(f, "u16"),
            GVariantType::I => write!(f, "i32"),
            GVariantType::U => write!(f, "u32"),
            GVariantType::X => write!(f, "i64"),
            GVariantType::T => write!(f, "u64"),
            GVariantType::D => write!(f, "f64"),
            GVariantType::S | GVariantType::O | GVariantType::G => write!(f, "::gvariant::S"),
            GVariantType::V => write!(f, "::gvariant::marker::V"),
            GVariantType::A(_) => todo!(),
            GVariantType::M(_) => todo!(),
            GVariantType::Tuple(_) | GVariantType::DictItem(_) => {
                if let Some(_) = size_of(&self.0) {
                    write!(f, "Structure{}", escape(self.0.to_string()))
                } else {
                    todo!()
                }
            }
        }
    }
}

fn write_packed_struct(
    gv: &GVariantType,
    children: &[GVariantType],
    out: &mut impl std::io::Write,
) -> Result<(), Box<dyn Error>> {
    writeln!(out, "#[derive(Default,Debug)]")?;
    writeln!(out, "#[repr(align({}))]", align_of(gv))?;
    let escaped = escape(gv.to_string());

    // This is only called for fixed-size structures
    writeln!(out, "pub(crate) struct Structure{} {{", escaped)?;

    let mut last_end = 0;
    let mut padding_count = 0;
    for (n, (child, &(_, a, b, c))) in children
        .iter()
        .zip(generate_table(&children).iter())
        .enumerate()
    {
        let start = align(a, b as usize) | c;
        let end = start + size_of(child).unwrap();
        let padding = start - last_end;
        if padding > 0 {
            writeln!(out, "    _padding_{} : [u8;{}],", padding_count, padding)?;
            padding_count += 1;
        }
        writeln!(out, "    // {} bytes {}..{}", child, start, end)?;
        writeln!(out, "    pub field_{} : {},", n, RustType(&child))?;
        last_end = end;
    }
    let padding = size_of(gv).unwrap() - last_end;
    if padding > 0 {
        writeln!(out, "    _padding_{} : [u8;{}],", padding_count, padding)?;
    }
    writeln!(
        out,
        "}}
        unsafe impl AllBitPatternsValid for Structure{escaped} {{}}
        unsafe impl AlignOf for Structure{escaped} {{
            type AlignOf = ::gvariant::aligned_bytes::A{align};
        }}
        ",
        escaped = escaped,
        align = align_of(gv)
    )?;

    Ok(())
}

fn align(off: usize, alignment: usize) -> usize {
    (off + alignment - 1) & !(alignment - 1)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_alignment() {
        assert_eq!(align_of(&one(b"s").unwrap()), 1);
        assert_eq!(align_of(&one(b"i").unwrap()), 4);
        assert_eq!(align_of(&one(b"(uy)").unwrap()), 4);
        assert_eq!(align_of(&one(b"(ti)").unwrap()), 8);
    }

    #[test]
    fn test_size() {
        assert_eq!(size_of(&one(b"s").unwrap()), None);
        assert_eq!(size_of(&one(b"i").unwrap()), Some(4));
        assert_eq!(size_of(&one(b"a(uu)").unwrap()), None);
        assert_eq!(size_of(&one(b"(uu)").unwrap()), Some(8));
        assert_eq!(size_of(&one(b"(uy)").unwrap()), Some(8));
        assert_eq!(size_of(&one(b"(ti)").unwrap()), Some(16));
    }

    #[test]
    fn test_align() {
        assert_eq!(align(0, 1), 0);
        assert_eq!(align(1, 1), 1);
        assert_eq!(align(0, 4), 0);
        assert_eq!(align(3, 4), 4);
        assert_eq!(align(4, 4), 4);
        assert_eq!(align(6, 4), 8);
        assert_eq!(align(17, 4), 20);
    }
}
