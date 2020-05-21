use crate::type_parser::one;
use std::error::Error;
use std::io::Write;

pub(crate) fn generate_types(gv_typestr: &[u8]) -> Result<String, Box<dyn Error>> {
    let spec = one(gv_typestr)?;
    let mut code: Vec<u8> = vec![];
    let name = "Structure".to_owned() + escape(spec.to_string()).as_ref();
    write!(code, "
mod _gvariant_macro {{
    #[macro_use]
    use ref_cast::RefCast;
    use ::gvariant::aligned_bytes::AsAligned;
    use ::gvariant::marker::GVariantMarker;

    #[derive(Debug, RefCast)]
    #[repr(transparent)]
    pub(crate) struct {name} {{
        data: ::gvariant::aligned_bytes::AlignedSlice<::gvariant::aligned_bytes::A4>,
    }}
    impl GVariantMarker for {name} {{
        type Alignment = ::gvariant::aligned_bytes::A4;
        const SIZE: Option<usize> = None;
        fn _mark(data: &::gvariant::aligned_bytes::AlignedSlice<Self::Alignment>) -> &Self {{
            Self::ref_cast(data.as_ref())
        }}
    }}
    impl ::gvariant::marker::NonFixedSize for {name} {{}}
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
", name=name)?;
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
