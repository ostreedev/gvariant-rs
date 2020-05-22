use std::{
    convert::TryInto,
    error::Error,
    ffi::CStr,
    fmt::{Debug, Display},
};

use ref_cast::RefCast;

pub mod aligned_bytes;
use offset::align_offset;

mod casting;
pub mod offset;

use aligned_bytes::AlignedTo;
use marker::GVariantMarker;
use marker::NonFixedSize;

pub mod marker {
    use super::aligned_bytes::{AlignedSlice, Alignment, AsAligned};
    use ref_cast::RefCast;

    pub trait GVariantMarker {
        type Alignment: Alignment;
        const ALIGNMENT: usize = std::mem::align_of::<Self::Alignment>();
        const SIZE: Option<usize>;
        fn mark<'a, Data: AsAligned<Self::Alignment> + ?Sized>(data: &'a Data) -> &'a Self {
            Self::_mark(data.as_aligned())
        }
        fn _mark(data: &AlignedSlice<Self::Alignment>) -> &Self;
    }
    pub trait FixedSize {}
    pub trait NonFixedSize {}

    macro_rules! fixed_size_marker {
        ($name:ident, $alignment:ident, $size:literal) => {
            #[derive(RefCast, Debug)]
            #[repr(transparent)]
            pub struct $name {
                pub data: super::aligned_bytes::AlignedSlice<super::aligned_bytes::$alignment>,
            }
            impl GVariantMarker for $name {
                type Alignment = super::aligned_bytes::$alignment;
                const SIZE: Option<usize> = Some($size);
                fn _mark(data: &AlignedSlice<Self::Alignment>) -> &Self {
                    Self::ref_cast(data.as_ref())
                }
            }
            impl FixedSize for $name {}
        };
    }
    macro_rules! non_fixed_size_marker {
        ($name:ident, $alignment:ident) => {
            #[derive(RefCast, Debug)]
            #[repr(transparent)]
            pub struct $name {
                pub data: AlignedSlice<super::aligned_bytes::$alignment>,
            }
            impl GVariantMarker for $name {
                type Alignment = super::aligned_bytes::$alignment;
                const SIZE: Option<usize> = None;
                fn _mark(data: &AlignedSlice<Self::Alignment>) -> &Self {
                    Self::ref_cast(data.as_ref())
                }
            }

            impl NonFixedSize for $name {}
        };
    }

    fixed_size_marker!(B, A1, 1); // bool
    fixed_size_marker!(Y, A1, 1); // u8
    fixed_size_marker!(N, A2, 2); // i16
    fixed_size_marker!(Q, A2, 2); // u16
    fixed_size_marker!(I, A4, 4); // i32
    fixed_size_marker!(U, A4, 4); // u32
    fixed_size_marker!(X, A8, 4); // i64
    fixed_size_marker!(T, A8, 4); // u64
    fixed_size_marker!(D, A8, 4); // f64

    non_fixed_size_marker!(S, A1); // str
    non_fixed_size_marker!(O, A1); // str
    non_fixed_size_marker!(G, A1); // str
    non_fixed_size_marker!(V, A8); // Variant

    #[derive(Debug, RefCast)]
    #[repr(transparent)]
    pub struct M<T: GVariantMarker + ?Sized> {
        // Option<T>
        item: std::marker::PhantomData<T>,
        pub data: super::aligned_bytes::AlignedSlice<T::Alignment>,
    }
    impl<Item: GVariantMarker + ?Sized> GVariantMarker for M<Item> {
        type Alignment = Item::Alignment;
        const SIZE: Option<usize> = None;
        fn _mark(data: &AlignedSlice<Self::Alignment>) -> &Self {
            Self::ref_cast(data)
        }
    }
    impl<T: GVariantMarker + ?Sized> NonFixedSize for M<T> {}

    #[derive(Debug, RefCast)]
    #[repr(transparent)]
    pub struct A<T: GVariantMarker + ?Sized> {
        // [T]
        item: std::marker::PhantomData<T>,
        pub data: super::aligned_bytes::AlignedSlice<T::Alignment>,
    }
    impl<Item: GVariantMarker + ?Sized> GVariantMarker for A<Item> {
        type Alignment = Item::Alignment;
        const SIZE: Option<usize> = None;
        fn _mark(data: &AlignedSlice<Self::Alignment>) -> &Self {
            Self::ref_cast(data)
        }
    }
    impl<T: GVariantMarker + ?Sized> NonFixedSize for A<T> {}
}

pub trait RustType: marker::GVariantMarker {
    type RefType: ?Sized;
    fn default_ref() -> &'static Self::RefType;
}

use casting::{try_cast_slice_to, try_cast_slice_to_mut};

macro_rules! impl_rusttype_for_marker {
    ($marker:ident, $RefType:ty, $default:expr) => {
        impl RustType for marker::$marker {
            type RefType = $RefType;
            fn default_ref() -> &'static Self::RefType {
                $default
            }
        }
    };
}
macro_rules! impl_fixed_size_rusttype_for_marker {
    ($marker:ident, $RustType:ty, $RefType:ty, $default:expr, $alignment:ident, $size:literal) => {
        impl_rusttype_for_marker!($marker, $RefType, &$default);
        impl RustType for marker::A<marker::$marker> {
            type RefType = [$RefType];
            fn default_ref() -> &'static Self::RefType {
                &[]
            }
        }

        impl_fixed_size_to_rs_ref!($marker, $RefType, $default, $alignment, $size);
        impl_to_rs_for_to_rs_ref!($marker, $RefType, $alignment, $size);
    };
}
macro_rules! impl_fixed_size_to_rs_ref {
    ($marker:ident, $RefType:ty, $default:expr, $alignment:ident, $size:literal) => {
        impl marker::$marker {
            pub fn to_rs_ref(&self) -> &$RefType {
                match try_cast_slice_to(&self.data) {
                    Err(_) => &$default,
                    Ok(x) => x,
                }
            }
            pub fn to_rs_mut(&mut self) -> Result<&mut $RefType, NonNormal> {
                match try_cast_slice_to_mut(&mut self.data) {
                    Err(_) => Err(NonNormal::WrongSize),
                    Ok(x) => Ok(x),
                }
            }
        }
    };
}
macro_rules! impl_to_rs_for_to_rs_ref {
    ($marker:ident, $RustType:ty, $alignment:ident, $size:literal) => {
        impl marker::$marker {
            pub fn to_rs(&self) -> $RustType {
                *self.to_rs_ref()
            }
        }
    };
}

impl_fixed_size_to_rs_ref!(B, u8, 0, A1, 1);
impl marker::B {
    pub fn to_rs(&self) -> bool {
        *self.to_rs_ref() != 0
    }
}
impl RustType for marker::B {
    type RefType = GVariantBool;
    fn default_ref() -> &'static Self::RefType {
        GVariantBool::ref_cast(&0)
    }
}
impl_fixed_size_rusttype_for_marker!(Y, u8, u8, 0, A1, 1); // u8
impl_fixed_size_rusttype_for_marker!(N, i16, i16, 0, A2, 2); // i16
impl_fixed_size_rusttype_for_marker!(Q, u16, u16, 0, A2, 2); // u16
impl_fixed_size_rusttype_for_marker!(I, i32, i32, 0, A4, 4); // i32
impl_fixed_size_rusttype_for_marker!(U, u32, u32, 0, A4, 4); // u32
impl_fixed_size_rusttype_for_marker!(X, i64, i64, 0, A8, 8); // i64
impl_fixed_size_rusttype_for_marker!(T, u64, u64, 0, A8, 8); // u64
impl_fixed_size_rusttype_for_marker!(D, f64, f64, 0., A8, 8); // f64

impl_rusttype_for_marker!(S, [u8], b""); // str
impl_rusttype_for_marker!(O, [u8], b""); // str
impl_rusttype_for_marker!(G, [u8], b""); // str

pub struct Variant {}
impl_rusttype_for_marker!(V, Variant, &Variant {}); // Variant

#[derive(Debug)]
pub struct WrongSize {}
impl Error for WrongSize {}
impl Display for WrongSize {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "WrongSize")
    }
}

macro_rules! string {
    ($marker:ident) => {
        impl marker::$marker {
            pub fn to_rs(&self) -> &[u8] {
                self.to_bytes()
            }
            pub fn to_bytes(&self) -> &[u8] {
                let d: &[u8] = self.data.as_ref();
                match d.last() {
                    Some(b'\0') => &d[..d.len() - 1],
                    _ => b"",
                }
            }
            pub fn to_cstr(&self) -> &CStr {
                let mut d: &[u8] = self.data.as_ref();
                match d.last() {
                    Some(b'\0') => {}
                    _ => {
                        d = b"\0";
                    }
                }
                CStr::from_bytes_with_nul(&d[..=d.into_iter().position(|x| *x == b'\0').unwrap()])
                    .unwrap()
            }
            pub fn try_as_mut(&mut self) -> Result<&mut [u8], NonNormal> {
                let d: &mut [u8] = self.data.as_mut();
                match d.last() {
                    Some(b'\0') => {
                        let length = d.len() - 1;
                        Ok(&mut d[..length])
                    }
                    Some(_) => Err(NonNormal::NotNullTerminated),
                    None => Err(NonNormal::WrongSize),
                }
            }
        }
        impl PartialEq for marker::$marker {
            fn eq(&self, other: &Self) -> bool {
                self.to_bytes() == other.to_bytes()
            }
        }
    };
}

string!(S);
string!(O);
string!(G);

#[derive(Debug)]
pub enum NonNormal {
    NotNullTerminated,
    WrongSize,
}
impl Display for NonNormal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "GVariant data not in normal form: {:?}", self)
    }
}
impl Error for NonNormal {}

// #### 2.5.3.1 Fixed Width Arrays
//
// In this case, the serialised form of each array element is packed
// sequentially, with no extra padding or framing, to obtain the array. Since
// all fixed-sized values have a size that is a multiple of their alignment
// requirement, and since all elements in the array will have the same alignment
// requirements, all elements are automatically aligned.
//
// The length of the array can be determined by taking the size of the array and
// dividing by the fixed element size. This will always work since all
// fixed-size values have a non-zero size.

use casting::AllBitPatternsValid;

impl<T: GVariantMarker + marker::FixedSize + RustType + ?Sized> marker::A<T>
where
    <marker::A<T> as GVariantMarker>::Alignment: AlignedTo<T::Alignment>,
    T::RefType: Sized + AllBitPatternsValid,
{
    pub fn to_slice(&self) -> &[T::RefType] {
        match casting::cast_slice(&self.data) {
            Err(_) => &[],
            Ok(x) => x,
        }
    }
}

// 2.3.6 Framing Offsets
//
// If a container contains non-fixed-size child elements, it is the
// responsibility of the container to be able to determine their sizes. This is
// done using framing offsets.
//
// A framing offset is an integer of some predetermined size. The size is always
// a power of 2. The size is determined from the overall size of the container
// byte sequence. It is chosen to be just large enough to reference each of the
// byte boundaries in the container.
//
// As examples, a container of size 0 would have framing offsets of size 0
// (since no bits are required to represent no choice). A container of sizes 1
// through 255 would have framing offsets of size 1 (since 256 choices can be
// represented with a single byte). A container of sizes 256 through 65535 would
// have framing offsets of size 2. A container of size 65536 would have framing
// offsets of size 4.
//
// There is no theoretical upper limit in how large a framing offset can be.
// This fact (along with the absence of other limitations in the serialisation
// format) allows for values of arbitrary size.
//
// When serialising, the proper framing offset size must be determined by “trial
// and error” — checking each size to determine if it will work. It is possible,
// since the size of the offsets is included in the size of the container, that
// having larger offsets might bump the size of the container up into the next
// category, which would then require larger offsets. Such containers, however,
// would not be considered to be in “normal form”. The smallest possible offset
// size must be used if the serialised data is to be in normal form.
//
// Framing offsets always appear at the end of containers and are unaligned.
// They are always stored in little-endian byte order.

#[derive(Debug, Copy, Clone)]
pub enum OffsetSize {
    U0 = 0,
    U1 = 1,
    U2 = 2,
    U4 = 4,
    U8 = 8,
}

pub fn offset_size(len: usize) -> OffsetSize {
    match len {
        0 => OffsetSize::U0,
        0x1..=0xFF => OffsetSize::U1,
        0x100..=0xFFFF => OffsetSize::U2,
        0x10000..=0xFFFFFFFF => OffsetSize::U4,
        0x100000000..=0xFFFFFFFFFFFFFFFF => OffsetSize::U8,
        _ => unreachable!(),
    }
}

fn read_uint(data: &[u8], size: OffsetSize, n: usize) -> usize {
    let s = n * size as usize;
    match size {
        OffsetSize::U0 => 0,
        OffsetSize::U1 => data[s] as usize,
        OffsetSize::U2 => u16::from_le_bytes(data[s..s + 2].try_into().unwrap()) as usize,
        OffsetSize::U4 => u32::from_le_bytes(data[s..s + 4].try_into().unwrap()) as usize,
        OffsetSize::U8 => u64::from_le_bytes(data[s..s + 8].try_into().unwrap()) as usize,
    }
}

fn read_last_frame_offset(data: &[u8]) -> (OffsetSize, usize) {
    let osz = offset_size(data.len());
    (osz, read_uint(&data[data.len() - osz as usize..], osz, 0))
}

// Non-fixed width arrays

impl<T: marker::GVariantMarker + ?Sized> marker::A<T> {
    pub fn len(&self) -> usize {
        if let Some(size) = T::SIZE {
            if self.data.len() % size == 0 {
                self.data.len() / size
            } else {
                0
            }
        } else if self.data.is_empty() {
            0
        } else {
            // Since determining the length of the array relies on our ability
            // to count the number of framing offsets and since the number of
            // framing offsets is determined from how much space they take up,
            // zero byte framing offsets are not permitted in arrays, even in
            // the case where all other serialised data has a size of zero. This
            // special exception avoids having to divide zero by zero and wonder
            // what the answer is.
            let (osz, lfo) = read_last_frame_offset(&self.data);
            match osz {
                OffsetSize::U0 => unreachable!(),
                x => (self.data.len() - lfo) / (x as usize),
            }
        }
    }
}
pub struct NonFixedSizeArrayIterator<'a, Item: GVariantMarker + NonFixedSize + ?Sized> {
    slice: &'a marker::A<Item>,
    next_start: usize,
    offset_idx: usize,
    offset_size: OffsetSize,
}
impl<'a, Item: GVariantMarker + NonFixedSize + 'static + ?Sized> Iterator
    for NonFixedSizeArrayIterator<'a, Item>
{
    type Item = &'a Item;
    fn next(&mut self) -> Option<Self::Item> {
        if self.offset_idx == self.slice.data.len() {
            None
        } else {
            let start = align_offset::<Item::Alignment>(self.next_start);
            let end = read_uint(
                &self.slice.data.as_ref()[self.offset_idx..],
                self.offset_size,
                0,
            );
            self.offset_idx += self.offset_size as usize;
            self.next_start = end;
            if end < start || end >= self.slice.data.len() {
                // If the framing offsets (or calculations based on them)
                // indicate that any part of the byte sequence of a child value
                // would fall outside of the byte sequence of the parent then
                // the child is given the default value for its type.
                Some(Item::_mark(aligned_bytes::empty_aligned()))
            } else {
                Some(Item::_mark(&self.slice.data[..end][start..]))
            }
        }
    }
}

impl<'a, Item: GVariantMarker + NonFixedSize + 'static + ?Sized> IntoIterator
    for &'a marker::A<Item>
{
    type Item = &'a Item;
    type IntoIter = NonFixedSizeArrayIterator<'a, Item>;
    fn into_iter(self) -> Self::IntoIter {
        let (osz, lfo) = read_last_frame_offset(&self.data);
        NonFixedSizeArrayIterator {
            slice: self,
            next_start: 0,
            offset_idx: lfo,
            offset_size: osz,
        }
    }
}
impl<Item: GVariantMarker + NonFixedSize + 'static + ?Sized> core::ops::Index<usize>
    for marker::A<Item>
{
    type Output = Item;
    fn index(&self, index: usize) -> &Self::Output {
        let (osz, lfo) = read_last_frame_offset(&self.data);
        let frame_offsets = &self.data.as_ref()[lfo..];
        let end = read_uint(frame_offsets, osz, index);
        let start = align_offset::<Item::Alignment>(match index {
            0 => 0,
            x => read_uint(frame_offsets, osz, x - 1),
        });
        if start < self.data.len() && end < self.data.len() && start <= end {
            Item::_mark(&self.data[..end][start..])
        } else {
            // Start or End Boundary of a Child Falls Outside the Container
            //
            // If the framing offsets (or calculations based on them) indicate
            // that any part of the byte sequence of a child value would fall
            // outside of the byte sequence of the parent then the child is given
            // the default value for its type.
            Item::_mark(aligned_bytes::empty_aligned())
        }
    }
}

// 2.5.2 Maybes
//
// Maybes are encoded differently depending on if their element type is
// fixed-sized not.
//
// The alignment of a maybe type is always equal to the alignment of its element
// type.

impl<T: GVariantMarker + ?Sized> marker::M<T> {
    pub fn to_option(&self) -> Option<&T> {
        if let Some(size) = T::SIZE {
            // 2.5.2.1 Maybe of a Fixed-Sized Element
            //
            // For the `Nothing` case, the serialised data is the empty byte
            // sequence.  For the `Just` case, the serialised data is exactly
            // equal to the serialised data of the child.  This is always
            // distinguishable from the `Nothing` case because all fixed-sized
            // values have a non-zero size.
            if self.data.len() == size {
                Some(T::_mark(&self.data))
            } else {
                // Wrong Size for Fixed Sized Maybe
                //
                // In the event that a maybe instance with a fixed element size
                // is not exactly equal to the size of that element, then the
                // value is taken to be `Nothing`.
                None
            }
        } else {
            if self.data.is_empty() {
                // #### 2.5.2.2 Maybe of a Non-Fixed-Sized Element
                //
                // For the `Nothing` case, the serialised data is, again, the empty
                // byte sequence.
                None
            } else {
                // For the Just case, the serialised form is the serialised data of
                // the child element, followed by a single zero byte. This extra
                // byte ensures that the `Just` case is distinguishable from the
                // `Nothing` case even in the event that the child value has a size
                // of zero.
                Some(T::_mark(&self.data[..self.data.len() - 1]))
            }
        }
    }
}

impl<'a, T: GVariantMarker> From<&'a marker::M<T>> for Option<&'a T>
where
    <marker::M<T> as GVariantMarker>::Alignment: AlignedTo<T::Alignment>,
{
    fn from(m: &'a marker::M<T>) -> Self {
        m.to_option()
    }
}

impl<T: GVariantMarker> PartialEq for marker::M<T>
where
    T: PartialEq,
    <marker::M<T> as GVariantMarker>::Alignment: AlignedTo<T::Alignment>,
{
    fn eq(&self, other: &Self) -> bool {
        self.to_option() == other.to_option()
    }
}

#[derive(Debug, RefCast)]
#[repr(transparent)]
pub struct GVariantBool(u8);
impl GVariantBool {
    pub fn to_bool(&self) -> bool {
        self.0 > 0
    }
    pub fn to_rs(&self) -> bool {
        self.0 != 0
    }
}
unsafe impl AllBitPatternsValid for GVariantBool {}

pub fn nth_last_frame_offset(data: &[u8], osz: OffsetSize, n: usize) -> usize {
    let off = data.len() - (n + 1) * osz as usize;
    read_uint(&data[off..], osz, 0)
}

#[cfg(test)]
mod tests {
    use super::*;
    use aligned_bytes::{copy_to_align, AlignedSlice, A8};

    #[test]
    fn test_numbers() {
        let data = copy_to_align(&[1, 2, 3, 4, 5, 6, 7, 8, 9]);
        let aligned_slice: &AlignedSlice<A8> = data.as_ref();

        // If the size doesn't match exactly it should default to 0:
        assert_eq!(marker::I::mark(&aligned_slice[..0]).to_rs(), 0);
        assert_eq!(marker::I::mark(&aligned_slice[..3]).to_rs(), 0);
        assert_eq!(marker::I::mark(&aligned_slice[..5]).to_rs(), 0);
        assert_eq!(marker::I::mark(&aligned_slice[..8]).to_rs(), 0);

        // Common case (Little endian):
        assert_eq!(marker::B::mark(&aligned_slice[..1]).to_rs(), true);
        assert_eq!(marker::Y::mark(&aligned_slice[..1]).to_rs(), 0x01);
        assert_eq!(marker::N::mark(&aligned_slice[..2]).to_rs(), 0x0201);
        assert_eq!(marker::Q::mark(&aligned_slice[..2]).to_rs(), 0x0201);
        assert_eq!(marker::I::mark(&aligned_slice[..4]).to_rs(), 0x04030201);
        assert_eq!(marker::U::mark(&aligned_slice[..4]).to_rs(), 0x04030201);
        assert_eq!(
            marker::X::mark(&aligned_slice[..8]).to_rs(),
            0x0807060504030201
        );
        assert_eq!(
            marker::T::mark(&aligned_slice[..8]).to_rs(),
            0x0807060504030201
        );
        assert_eq!(
            marker::D::mark(&aligned_slice[..8]).to_rs(),
            f64::from_bits(0x0807060504030201)
        );
    }
    #[test]
    fn test_non_fixed_width_maybe() {
        assert_eq!(marker::M::<marker::S>::mark(b"").to_option(), None);
        assert_eq!(
            marker::M::<marker::S>::mark(b"\0")
                .to_option()
                .unwrap()
                .to_bytes(),
            b""
        );
        assert_eq!(
            marker::M::<marker::S>::mark(b"hello world\0\0")
                .to_option()
                .unwrap()
                .to_bytes(),
            b"hello world"
        );
    }

    use IntoIterator;
    #[test]
    fn test_non_fixed_width_array() {
        let a_s = marker::A::<marker::S>::mark(b"");
        assert_eq!(a_s.len(), 0);
        assert!(a_s.into_iter().collect::<Vec<_>>().is_empty());

        let a_s = marker::A::<marker::S>::mark(b"hello\0world\0\x06\x0c");
        assert_eq!(a_s.len(), 2);
        assert_eq!(
            a_s.into_iter().map(|x| x.to_bytes()).collect::<Vec<_>>(),
            &[b"hello", b"world"]
        );
        assert_eq!(a_s[0].to_bytes(), b"hello");
        assert_eq!(a_s[1].to_bytes(), b"world");
    }

    #[test]
    fn test_spec_examples() {
        assert_eq!(marker::S::mark(b"hello world\0").to_bytes(), b"hello world");
        assert_eq!(
            marker::M::<marker::S>::mark(b"hello world\0\0")
                .to_option()
                .unwrap()
                .to_bytes(),
            b"hello world"
        );
        let aob = marker::A::<marker::B>::mark([1u8, 0, 0, 1, 1].as_ref());
        assert_eq!(aob.len(), 5);
        assert_eq!(
            aob.to_slice()
                .iter()
                .map(|x| x.to_bool())
                .collect::<Vec<_>>(),
            [true, false, false, true, true]
        );

        // String Array Example
        //
        // With type 'as':
        let v: Vec<_> = marker::A::<marker::S>::mark(b"i\0can\0has\0strings?\0\x02\x06\x0a\x13")
            .into_iter()
            .map(|x| x.to_bytes())
            .collect();
        assert_eq!(v, [b"i".as_ref(), b"can", b"has", b"strings?"]);

        // Array of Bytes Example
        //
        // With type 'ay':
        let aob = marker::A::<marker::Y>::mark(&[0x04u8, 0x05, 0x06, 0x07]);
        assert_eq!(aob.len(), 4);
        assert_eq!(aob.to_slice(), &[0x04u8, 0x05, 0x06, 0x07]);

        // Array of Integers Example
        //
        // With type 'ai':
        let data = copy_to_align(b"\x04\0\0\0\x02\x01\0\0");
        let aoi = marker::A::<marker::I>::_mark(data.as_ref());
        assert_eq!(aoi.len(), 2);
        assert_eq!(aoi.to_slice(), [4, 258]);

        // Dictionary Entry Example
        //
        // With type '{si}':
        //    'a sp 'k 'e  'y \0 -- --   02 02 00 00 06has a value of {'a key', 514}
    }

    #[test]
    fn test_gvariantstr() {
        assert_eq!(marker::S::mark(b"".as_ref()).to_bytes(), b"");
        assert_eq!(marker::S::mark(b"\0".as_ref()).to_bytes(), b"");
        assert_eq!(
            marker::S::mark(b"hello world\0".as_ref()).to_bytes(),
            b"hello world"
        );
    }
}