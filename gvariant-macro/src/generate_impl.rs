use std::io::Write;
use std::{collections::HashSet, error::Error};

use crate::{marker_type, type_parser::GVariantType};

pub(crate) fn generate_types(
    spec: &GVariantType,
    defined_types: &mut HashSet<GVariantType>,
) -> Result<String, Box<dyn Error>> {
    if defined_types.contains(spec) {
        // If we've already defined this type in this scope.  Don't want to
        // define it again otherwise we get "must be defined only once in the
        // type namespace of this module" compiler errors:
        return Ok("".to_owned());
    }
    defined_types.insert(spec.clone());
    Ok(match spec {
        GVariantType::Tuple(children) => {
            let mut out = "".to_string();
            for child in children {
                out += generate_types(child, defined_types)?.as_ref();
            }
            out += generate_tuple(spec, children)?.as_ref();
            out
        }
        GVariantType::DictItem(children) => {
            let mut out = "".to_string();
            out += generate_types(&children[0], defined_types)?.as_ref();
            out += generate_types(&children[1], defined_types)?.as_ref();
            out += generate_tuple(spec, children.as_ref())?.as_ref();
            out
        }
        GVariantType::A(x) => generate_types(x, defined_types)?,
        GVariantType::M(x) => generate_types(x, defined_types)?,

        // Everything else is a builtin
        _ => "".to_owned(),
    })
}

fn generate_tuple(
    spec: &GVariantType,
    children: &[GVariantType],
) -> Result<String, Box<dyn Error>> {
    let size = size_of(spec);
    let mut out = vec![];
    if size.is_some() {
        write_packed_struct(spec, children, &mut out)
    } else {
        write_non_fixed_size_structure(spec, children, &mut out)
    }?;
    write!(
        out,
        "impl std::fmt::Debug for Structure{} {{
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {{
            std::fmt::Debug::fmt(&self.to_tuple(), f)
        }}
    }}",
        escape(spec.to_string())
    )?;
    Ok(String::from_utf8(out).unwrap())
}

fn write_non_fixed_size_structure(
    spec: &GVariantType,
    children: &[GVariantType],
    code: &mut impl Write,
) -> Result<(), Box<dyn Error>> {
    let alignment = align_of(spec);
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
    let escaped = escape(spec.to_string());
    let types: Vec<String> = children.iter().map(marker_type).collect();
    let mut tuple = vec![b'('];
    for child in children {
        write!(tuple, "                &'a {},", marker_type(child))?;
    }
    writeln!(tuple, "            )")?;
    let tuple = String::from_utf8(tuple)?;

    write!(
        code,
        "
    #[repr(transparent)]
    pub(crate) struct Structure{spec} {{
        data: AlignedSlice<aligned_bytes::A{alignment}>,
    }}
    impl Structure{spec} {{
        fn from_aligned_slice_mut(slice: &mut AlignedSlice<<Self as AlignOf>::AlignOf>) -> &mut Self {{
            // This is safe because Structure{spec} is repr(transparent) around
            // this same type:
            unsafe {{&mut *(slice as *mut AlignedSlice<aligned_bytes::A{alignment}> as *mut Structure{spec})}}
        }}
    }}
    impl ToOwned for Structure{spec} {{
        type Owned = Owned<Self>;
        fn to_owned(&self) -> Self::Owned {{
            Owned::from_bytes(&*self.data)
        }}
    }}
    impl ::gvariant::Cast for Structure{spec} {{
        fn default_ref() -> &'static Self {{
            let d = empty_aligned();
            // This is safe because Structure{spec} is repr(transparent) around
            // this same type:
            unsafe {{&*(d as *const AlignedSlice<aligned_bytes::A{alignment}> as *const Structure{spec})}}
        }}
        fn try_from_aligned_slice(slice:&AlignedSlice<Self::AlignOf>) -> Result<&Self, ::gvariant::casting::WrongSize> {{
            // This is safe because Structure{spec} is repr(transparent) around
            // this same type:
            Ok(unsafe {{&*(slice as *const AlignedSlice<aligned_bytes::A{alignment}> as *const Structure{spec})}})
        }}
        fn try_from_aligned_slice_mut(slice:&mut AlignedSlice<Self::AlignOf>) -> Result<&mut Self, ::gvariant::casting::WrongSize> {{
            // This is safe because Structure{spec} is repr(transparent) around
            // this same type:
            Ok(Self::from_aligned_slice_mut(slice))
        }}
    }}
    unsafe impl ::gvariant::casting::AllBitPatternsValid for Structure{spec} {{}}
    unsafe impl ::gvariant::casting::AlignOf for Structure{spec} {{
        type AlignOf = aligned_bytes::A{alignment};
    }}
    impl<'a> Structure<'a> for Structure{spec} {{
        type RefTuple = {tuple};
        fn to_tuple(&'a self) -> {tuple} {{ (",
        spec = escaped,
        alignment = alignment,
        tuple = tuple,
    )?;

    let mut serialize_types = vec![];
    let mut serialize_types2 = vec![];
    let mut serialize_cmds = vec![];

    // After a non-fixed size element we may need to include padding - and the
    // amount of padding can't be determined until runtime.  For example with:
    // (si) there will be up to 3B of padding after the s such that the i is 4B
    // aligned.  Similarly for (sit), there may be either 4 or 0 bytes of
    // padding between the i and the t such that the t is 8B aligned.
    //
    // However for (styi) we know that we're already 8B aligned when we reach
    // the y, and after writing 1B for the y we're 8B aligned but with an offset
    // of 1B, so we know at compile time that we need to write 7B of padding.
    //
    // base and offset keeps track of this for us.  base is our base alignment,
    // either 1, 2, 4 or 8, and offset is the offset to that alignment.
    let mut base = 8;
    let mut offset = 0;
    let mut nth_frame_offset = 0;

    for ((n, child), (i, a, b, c)) in children.iter().enumerate().zip(generate_table(children)) {
        let last_child = n == children.len() - 1;
        writeln!(
            code,
            "
            // {ty}
            get_child_elem::<{marker_type}, aligned_bytes::A{b}>(
                self.data.as_aligned(),
                {i},
                {a},
                {c},
                {child_size:?},
                {last_child},
                {n_frame_offsets}),",
            ty = child,
            marker_type = marker_type(child),
            i = i,
            a = a,
            b = b,
            c = c,
            child_size = size_of(child),
            last_child = last_child,
            n_frame_offsets = n_frame_offsets
        )?;
        if align_of(child) <= base {
            // Statically known number of padding bytes
            let old_offset = offset;
            offset = align(offset, align_of(child));
            let n_padding = offset - old_offset;
            if n_padding > 0 {
                serialize_cmds.push(format!("f.write_all(b\"{}\")?;", "\\0".repeat(n_padding)));
                serialize_cmds.push(format!("off += {};", n_padding));
            }
        } else {
            // Need to dynamically insert padding
            offset = 0;
            base = align_of(child);
            serialize_cmds.push(format!("off += write_padding::<A{}, _>(off, f)?;", base));
        }
        match size_of(child) {
            Some(x) => {
                offset += x;
                serialize_cmds.push(format!("self.{}.serialize(f)?;", n));
                serialize_cmds.push(format!("off += {};", x));
            }
            None => {
                base = 1;
                offset = 0;
                serialize_cmds.push(format!("off += self.{}.serialize(f)?;", n));
                if !last_child {
                    serialize_cmds.push(format!(
                        "framing_offsets[{}] = off;",
                        n_frame_offsets - nth_frame_offset - 1
                    ));
                    nth_frame_offset += 1;
                }
            }
        }
        serialize_types.push(format!(
            "T{}: ::gvariant::SerializeTo<{}> + Copy",
            n,
            marker_type(child)
        ));
        serialize_types2.push(format!("T{},", n));
    }
    writeln!(
        code,
        "            )
        }}
    }}
    impl<'a> From<&'a Structure{escaped}> for ({tuple}) {{
        fn from(value : &'a Structure{escaped}) -> Self {{
            value.to_tuple()
        }}
    }}
    impl PartialEq for Structure{escaped} {{
        fn eq(&self, other: &Self) -> bool {{
            self.to_tuple() == other.to_tuple()
        }}
    }}
    impl<{serialize_types}> ::gvariant::SerializeTo<Structure{escaped}> for &({serialize_types2}) {{
        fn serialize(self, f: &mut impl std::io::Write) -> std::io::Result<usize> {{
            let mut off: usize = 0;
            let mut framing_offsets : [usize; {n_frame_offsets}] = [0; {n_frame_offsets}];
            {serialize_cmds}
            off = write_offsets(off, &framing_offsets, f)?;
            Ok(off)
        }}
    }}
    ",
        escaped = escaped,
        tuple = types
            .iter()
            .map(|x| format!("&'a {}, ", x))
            .collect::<Vec<String>>()
            .join(""),
        n_frame_offsets = n_frame_offsets,
        serialize_types = serialize_types.join(", "),
        serialize_types2 = serialize_types2.join(" "),
        serialize_cmds = serialize_cmds.join("\n"),
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
                pos = align(pos, align_of(t));
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
    writeln!(out, "#[derive(Default,Copy,Clone)]")?;
    writeln!(out, "#[repr(C)]")?;
    let escaped = escape(gv.to_string());

    // This is only called for fixed-size structures
    writeln!(out, "pub(crate) struct Structure{} {{", escaped)?;

    let mut field_arglist = vec![];
    let mut get_fields = vec![];
    let mut set_fields = "".to_string();
    let mut eq = vec!["true".to_owned()];
    let mut defaults = vec![];
    let mut types = vec![];
    let mut tuple = vec![];
    let mut serialize_types = vec![];
    let mut serialize_types2 = vec![];
    let mut serialize_cmds = vec![];

    let mut last_end = 0;
    let mut padding_count = 0;
    for (n, (child, &(_, a, b, c))) in children
        .iter()
        .zip(generate_table(children).iter())
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
            serialize_cmds.push(format!("f.write_all(b\"{}\")?;", "\\0".repeat(padding)));
            padding_count += 1;
        }
        let rust_type: String = marker_type(child);
        writeln!(out, "    // {} bytes {}..{}", child, start, end)?;
        writeln!(out, "    pub field_{} : {},", n, rust_type)?;
        field_arglist.push(format!("field_{} : {}", n, rust_type));
        get_fields.push(format!("&self.field_{},", n));
        tuple.push(format!("&'a {},", rust_type));
        serialize_types.push(format!(
            "T{}: ::gvariant::SerializeTo<{}> + Copy",
            n, rust_type
        ));
        serialize_types2.push(format!("T{},", n));
        serialize_cmds.push(format!("self.{}.serialize(f)?;", n));
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
        serialize_cmds.push(format!("f.write_all(b\"{}\")?;", "\\0".repeat(padding)));
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
        }}
        impl<'a> ::gvariant::Structure<'a> for Structure{escaped} {{
            type RefTuple = ({tuple});
            fn to_tuple(&'a self) -> ({tuple}) {{
                ({get_fields})
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
        impl<'a> From<&'a Structure{escaped}> for ({tuple}) {{
            fn from(value : &'a Structure{escaped}) -> Self {{
                value.to_tuple()
            }}
        }}
        impl<{serialize_types}> ::gvariant::SerializeTo<Structure{escaped}> for &({serialize_types2}) {{
            fn serialize(self, f: &mut impl std::io::Write) -> std::io::Result<usize> {{
                {serialize_cmds}
                Ok(core::mem::size_of::<Structure{escaped}>())
            }}
        }}
        ",
        escaped = escaped,
        align = align_of(gv),
        get_fields = get_fields.join(" "),
        field_arglist = field_arglist.join(", "),
        serialize_types = serialize_types.join(", "),
        serialize_types2 = serialize_types2.join(" "),
        serialize_cmds = serialize_cmds.join("\n"),
        set_fields = set_fields,
        defaults = defaults.join(", "),
        eq = eq.join(" && "),
        tuple = tuple.join(" ")
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
