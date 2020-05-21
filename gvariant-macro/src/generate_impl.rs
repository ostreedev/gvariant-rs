use std::error::Error;
use std::io::Write;

use crate::type_parser::{one, GVariantType};

pub(crate) fn generate_types(gv_typestr: &[u8]) -> Result<String, Box<dyn Error>> {
    let spec = one(gv_typestr)?;
    let mut code: Vec<u8> = vec![];
    let name = "Structure".to_owned() + escape(spec.to_string()).as_ref();
    let alignment = align_of(&spec);
    let size = size_of(&spec);
    let sizedtrait = if size.is_some() {
        "FixedSize"
    } else {
        "NonFixedSize"
    };
    write!(code, "
mod _gvariant_macro {{
    #[macro_use]
    use ref_cast::RefCast;
    use ::gvariant::aligned_bytes::AsAligned;
    use ::gvariant::marker::GVariantMarker;

    #[derive(Debug, RefCast)]
    #[repr(transparent)]
    pub(crate) struct {name} {{
        data: ::gvariant::aligned_bytes::AlignedSlice<::gvariant::aligned_bytes::A{alignment}>,
    }}
    impl GVariantMarker for {name} {{
        type Alignment = ::gvariant::aligned_bytes::A{alignment};
        const SIZE: Option<usize> = {size:?};
        fn _mark(data: &::gvariant::aligned_bytes::AlignedSlice<Self::Alignment>) -> &Self {{
            Self::ref_cast(data.as_ref())
        }}
    }}
    impl ::gvariant::marker::{sizedtrait} for {name} {{}}
    impl {name} {{
        const N_FRAME_OFFSETS: usize = 1;
        pub fn split(&self) -> (&[u8], &i32) {{
            let osz = ::gvariant::offset_size(self.data.len());

            let frame_0 = ..::gvariant::nth_last_frame_offset(&self.data, osz, 0);
            let frame_1_start = ::gvariant::offset::align_offset::<<::gvariant::marker::I as ::gvariant::marker::GVariantMarker>::Alignment>(frame_0.end);
            let frame_1_end = self.data.len() - Self::N_FRAME_OFFSETS * osz as usize;

            (
                ::gvariant::marker::S::_mark(&self.data[frame_0].as_aligned()).to_rs(),
                ::gvariant::marker::I::_mark(
                    &self.data[..frame_1_end][frame_1_start..][..::gvariant::marker::I::SIZE.unwrap()],
                )
                .to_rs_ref(),
            )
        }}
    }}
}}
", name=name, alignment=alignment, size=size, sizedtrait=sizedtrait)?;
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
}
