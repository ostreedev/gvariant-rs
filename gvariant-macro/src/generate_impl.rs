use std::error::Error;
use std::io::Write;

use crate::{marker_type, type_parser::GVariantType};

pub(crate) fn generate_types(spec: &GVariantType) -> Result<String, Box<dyn Error>> {
    Ok(match spec {
        GVariantType::Tuple(children) => {
            let mut out = "".to_string();
            for child in children {
                out += generate_types(child)?.as_ref();
            }
            out += generate_tuple(&spec, children)?.as_ref();
            out
        }
        GVariantType::DictItem(_) => todo!(),
        GVariantType::A(x) => generate_types(x)?,
        GVariantType::M(x) => generate_types(x)?,

        // Everything else is a builtin
        _ => "".to_owned(),
    })
}

fn generate_tuple(
    spec: &GVariantType,
    children: &[GVariantType],
) -> Result<String, Box<dyn Error>> {
    let size = size_of(&spec);
    let mut out = vec![];
    if size.is_some() {
        write_packed_struct(spec, children, &mut out)
    } else {
        write_non_fixed_size_structure(spec, children, &mut out)
    }?;
    Ok(String::from_utf8(out).unwrap())
}

fn write_non_fixed_size_structure(
    spec: &GVariantType,
    children: &[GVariantType],
    code: &mut impl Write,
) -> Result<(), Box<dyn Error>> {
    let alignment = align_of(&spec);
    let n_frames: usize = children.iter().filter(|x| size_of(x).is_none()).count();
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
    #[derive(Debug, RefCast)]
    #[repr(transparent)]
    pub(crate) struct Structure{spec} {{
        data: AlignedSlice<::gvariant::aligned_bytes::A{alignment}>,
    }}
    impl ::gvariant::Cast for Structure{spec} {{
        fn default_ref() -> &'static Self {{
            &Self::ref_cast(::gvariant::aligned_bytes::empty_aligned())
        }}
        fn try_from_aligned_slice(slice:&AlignedSlice<Self::AlignOf>) -> Result<&Self, ::gvariant::casting::WrongSize> {{
            Ok(Self::ref_cast(slice))
        }}
        fn try_from_aligned_slice_mut(slice:&mut AlignedSlice<Self::AlignOf>) -> Result<&mut Self, ::gvariant::casting::WrongSize> {{
            Ok(Self::ref_cast_mut(slice))
        }}
    }}
    unsafe impl ::gvariant::casting::AllBitPatternsValid for Structure{spec} {{}}
    unsafe impl ::gvariant::casting::AlignOf for Structure{spec} {{
        type AlignOf = ::gvariant::aligned_bytes::A{alignment};
    }}
    impl Structure{spec} {{
        pub fn to_tuple(&self) -> (\n",
        spec = escape(spec.to_string()),
        alignment = alignment,
    )?;

    // Write out the return type:
    for child in children {
        write!(code, "                &")?;
        marker_type(child, code)?;
        writeln!(code, ",")?;
    }
    writeln!(code, "            ) {{")?;

    if n_frame_offsets > 0 {
        writeln!(
            code,
            "
            let osz = ::gvariant::offset_size(self.data.len());"
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
        writeln!(code, "\n            // {ty}\n            let offset_{n} : AlignedOffset<::gvariant::aligned_bytes::A{calign}> = align_offset::<::gvariant::aligned_bytes::A{b}>({fo_plus}{a}) | AlignedOffset::<::gvariant::aligned_bytes::A{calign}>::try_new({c}).unwrap();",
            ty=child.to_string(), n=n, a=a, b=b, c=c, fo_plus=fo_plus, calign=align_of(child))?;
        let end = if let Some(size) = size_of(child) {
            format!("offset_{n}.to_usize() + {size}", n = n, size = size)
        } else if n == children.len() - 1 {
            if n_frame_offsets == 0 {
                "self.data.len()".to_string()
            } else {
                format!(
                    "self.data.len() - osz as usize * {n_frame_offsets}",
                    n_frame_offsets = n_frame_offsets
                )
            }
        } else {
            format!(
                "::gvariant::nth_last_frame_offset(&self.data, osz, {i})",
                i = i + 1
            )
        };
        writeln!(
            code,
            "            let end_{n} : usize = {end};",
            n = n,
            end = end
        )?;
    }
    writeln!(code, "            (")?;
    for (n, child) in children.iter().enumerate() {
        write!(code, "                ")?;
        marker_type(child, code)?;
        writeln!(
            code,
            "::from_aligned_slice(&self.data.as_aligned()[..end_{n}][offset_{n}..]),",
            n = n
        )?;
    }
    writeln!(
        code,
        "            )
        }}
    }}"
    )?;

    Ok(())
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

pub(crate) fn size_of(t: &GVariantType) -> Option<usize> {
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
                // instance of the unit type is a single zero byte (size 1).
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
            a += align(c, b);
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
    table
}

fn write_packed_struct(
    gv: &GVariantType,
    children: &[GVariantType],
    out: &mut impl std::io::Write,
) -> Result<(), Box<dyn Error>> {
    writeln!(out, "#[derive(Default,Debug,Copy,Clone)]")?;
    writeln!(out, "#[repr(C)]")?;
    let escaped = escape(gv.to_string());

    // This is only called for fixed-size structures
    writeln!(out, "pub(crate) struct Structure{} {{", escaped)?;

    let mut field_arglist = vec![];
    let mut get_fields = vec![];
    let mut tuple_fields = vec![];
    let mut set_fields = "".to_string();
    let mut eq = vec![];
    let mut defaults = vec![];
    let mut types = vec![];

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
            set_fields.push_str(&format!(
                "_padding_{} : [0u8;{}],\n",
                padding_count, padding
            ));
            padding_count += 1;
        }
        let mut rust_type = vec![];
        marker_type(&child, &mut rust_type).unwrap();
        let rust_type: String = String::from_utf8(rust_type).unwrap();
        writeln!(out, "    // {} bytes {}..{}", child, start, end)?;
        writeln!(out, "    pub field_{} : {},", n, rust_type)?;
        field_arglist.push(format!("field_{} : {}", n, rust_type));
        get_fields.push(format!("self.field_{}", n));
        tuple_fields.push(format!("value.{}", n));
        types.push(rust_type);
        set_fields.push_str(format!("field_{} : field_{},\n", n, n).as_str());
        defaults.push("0".to_string());
        eq.push(format!("self.field_{n} == other.field_{n}", n = n));
        last_end = end;
    }
    let padding = size_of(gv).unwrap() - last_end;
    if padding > 0 {
        writeln!(out, "    _padding_{} : [u8;{}],", padding_count, padding)?;
        set_fields.push_str(&format!(
            "_padding_{} : [0u8;{}],\n",
            padding_count, padding
        ));
    }
    writeln!(
        out,
        "}}
        unsafe impl ::gvariant::casting::AllBitPatternsValid for Structure{escaped} {{}}
        unsafe impl ::gvariant::casting::AlignOf for Structure{escaped} {{
            type AlignOf = ::gvariant::aligned_bytes::A{align};
        }}
        impl Structure{escaped} {{
            pub const fn new({field_arglist}) -> Structure{escaped} {{
                Structure{escaped} {{ {set_fields} }}
            }}
            pub fn to_tuple(self) -> ({tuple}) {{
                ({get_fields})
            }}
            pub fn from_tuple(value : ({tuple})) -> Self {{
                Self::new({tuple_fields})
            }}
        }}
        impl ::gvariant::Cast for Structure{escaped} {{
            fn default_ref() -> &'static Self {{
                static s : Structure{escaped} = Structure{escaped}::new({defaults});
                &s
            }}
            fn try_from_aligned_slice(slice:&::gvariant::aligned_bytes::AlignedSlice<Self::AlignOf>) -> Result<&Self, ::gvariant::casting::WrongSize> {{
                ::gvariant::casting::try_cast_slice_to::<Self>(slice)
            }}
            fn try_from_aligned_slice_mut(slice:&mut ::gvariant::aligned_bytes::AlignedSlice<Self::AlignOf>) -> Result<&mut Self, ::gvariant::casting::WrongSize> {{
                ::gvariant::casting::try_cast_slice_to_mut::<Self>(slice)
            }}
        }}
        impl PartialEq for Structure{escaped} {{
            fn eq(&self, other: &Self) -> bool {{
                {eq}
            }}
        }}
        impl From<({tuple})> for Structure{escaped} {{
            fn from(value : ({tuple})) -> Self {{
                Self::from_tuple(value)
            }}
        }}
        impl From<Structure{escaped}> for ({tuple}) {{
            fn from(value : Structure{escaped}) -> Self {{
                value.to_tuple()
            }}
        }}
        ",
        escaped = escaped,
        align = align_of(gv),
        get_fields = get_fields.join(", "),
        tuple_fields = tuple_fields.join(", "),
        field_arglist = field_arglist.join(", "),
        set_fields = set_fields,
        defaults = defaults.join(", "),
        eq = eq.join(" && "),
        tuple = types.join(", ")
    )?;

    Ok(())
}

fn align(off: usize, alignment: usize) -> usize {
    (off + alignment - 1) & !(alignment - 1)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::type_parser::one;

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
