use std::error::Error;
use std::io::Write;

pub(crate) fn generate_types(_gv_typestr: &[u8]) -> Result<String, Box<dyn Error>> {
    let mut code: Vec<u8> = vec![];
    write!(code, "
mod _gvariant_macro {{
    #[macro_use]
    use ref_cast::RefCast;
    use ::gvariant::aligned_bytes::AsAligned;
    use ::gvariant::marker::GVariantMarker;

    #[derive(Debug, RefCast)]
    #[repr(transparent)]
    pub(crate) struct StructureCsi7 {{
        data: ::gvariant::aligned_bytes::AlignedSlice<::gvariant::aligned_bytes::A4>,
    }}
    impl GVariantMarker for StructureCsi7 {{
        type Alignment = ::gvariant::aligned_bytes::A4;
        const SIZE: Option<usize> = None;
        fn _mark(data: &::gvariant::aligned_bytes::AlignedSlice<Self::Alignment>) -> &Self {{
            Self::ref_cast(data.as_ref())
        }}
    }}
    impl ::gvariant::marker::NonFixedSize for StructureCsi7 {{}}
    impl StructureCsi7 {{
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
")?;
    Ok(String::from_utf8(code)?)
}
