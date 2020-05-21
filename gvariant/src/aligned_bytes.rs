use crate::offset::AlignedOffset;
use std::ops::{
    Deref, DerefMut, Index, Range, RangeFrom, RangeFull, RangeInclusive, RangeTo, RangeToInclusive,
};
use std::{borrow::Cow, fmt::Debug};

// This is unsafe because it must only be implemented for zero-sized types
pub unsafe trait Alignment: Debug {
    const ALIGNMENT: usize;
}
// This is a promise that the type is aligned as described by T.  It is
// unsafe because safe code must be able to assume the given alignment
// for e.g. pointer casts
pub unsafe trait AlignedTo<T: Alignment>: Alignment {}

// 1-byte alignment e.g. no alignment
#[derive(Debug, Copy, Clone)]
pub struct A1;
unsafe impl Alignment for A1 {
    const ALIGNMENT: usize = 1;
}
unsafe impl AlignedTo<A1> for A1 {}

/// 2-byte alignment
#[repr(align(2))]
#[derive(Debug, Copy, Clone)]
pub struct A2;
unsafe impl Alignment for A2 {
    const ALIGNMENT: usize = 2;
}
unsafe impl AlignedTo<A1> for A2 {}
unsafe impl AlignedTo<A2> for A2 {}

/// 4-byte alignment
#[repr(align(4))]
#[derive(Debug, Copy, Clone)]
pub struct A4;
unsafe impl Alignment for A4 {
    const ALIGNMENT: usize = 4;
}
unsafe impl AlignedTo<A1> for A4 {}
unsafe impl AlignedTo<A2> for A4 {}
unsafe impl AlignedTo<A4> for A4 {}

/// 8-byte alignment
#[repr(align(8))]
#[derive(Debug, Copy, Clone)]
pub struct A8;
unsafe impl Alignment for A8 {
    const ALIGNMENT: usize = 8;
}
unsafe impl AlignedTo<A1> for A8 {}
unsafe impl AlignedTo<A2> for A8 {}
unsafe impl AlignedTo<A4> for A8 {}
unsafe impl AlignedTo<A8> for A8 {}

pub trait AsAligned<A: Alignment> {
    fn as_aligned(&self) -> &AlignedSlice<A>;
}
pub trait AsAlignedMut<A: Alignment>: AsAligned<A> {
    fn as_aligned_mut(&mut self) -> &mut AlignedSlice<A>;
}
pub trait TryAsAligned<A: Alignment> {
    fn try_as_aligned(&self) -> Result<&AlignedSlice<A>, Misaligned>;
}
pub trait TryAsAlignedMut<A: Alignment>: TryAsAligned<A> {
    fn try_as_aligned_mut(&mut self) -> Result<&mut AlignedSlice<A>, Misaligned>;
}

#[repr(C)]
#[derive(Debug)]
pub struct AlignedSlice<A: Alignment> {
    alignment: A,
    data: [u8],
}

impl<A: Alignment> ToOwned for AlignedSlice<A> {
    type Owned = Box<AlignedSlice<A>>;
    fn to_owned(&self) -> Self::Owned {
        let mut owned = alloc_aligned(self.len());
        owned.copy_from_slice(self);
        owned
    }
}

/// Aligns the given data to the alignment as given by the type parameter A.
///
/// This is convenient for when you can't ensure the alignment of your data in
/// advance.  By using `Cow` we only have to make a copy of the data if it is
/// not aligned.
pub fn copy_to_align<'a, A: Alignment>(data: &'a [u8]) -> Cow<'a, AlignedSlice<A>> {
    if is_aligned_to::<A>(data) {
        Cow::Borrowed(data.try_as_aligned().unwrap())
    } else {
        let mut copy = alloc_aligned::<A>(data.len());
        copy.copy_from_slice(data);
        Cow::Owned(copy)
    }
}

pub fn alloc_aligned<A: Alignment>(size: usize) -> Box<AlignedSlice<A>> {
    let layout = std::alloc::Layout::from_size_align(size, A::ALIGNMENT).unwrap();
    unsafe {
        // This is safe because:
        //
        // * We use the same size here as passed into layout, so the slice only
        //   points to genuinely allocated memory
        // * We use alloc_zeroed so the memory is completely initialised
        // * All zeros is a valid bit pattern for [u8], as is any bit pattern
        // * We can cast to AlignedSlice because the representation is the same
        //   as [u8] modulo alignment, and appropriate alignment has been
        //   specified in layout
        let bs = std::slice::from_raw_parts_mut(std::alloc::alloc_zeroed(layout), size);
        Box::from_raw(to_alignedslice_unchecked_mut(bs))
    }
}

impl<A: Alignment> Deref for AlignedSlice<A> {
    type Target = [u8];
    fn deref(&self) -> &Self::Target {
        &self.data
    }
}
impl<A: Alignment> DerefMut for AlignedSlice<A> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}
impl<T: AsRef<[u8]>> AsAligned<A1> for T {
    fn as_aligned(&self) -> &AlignedSlice<A1> {
        let s = self.as_ref();
        s.as_aligned()
    }
}
impl<T: AsRef<[u8]> + AsMut<[u8]>> AsAlignedMut<A1> for T {
    fn as_aligned_mut(&mut self) -> &mut AlignedSlice<A1> {
        let s = self.as_mut();
        s.as_aligned_mut()
    }
}
impl<A: Alignment, T: AsRef<[u8]>> TryAsAligned<A> for T {
    fn try_as_aligned(&self) -> Result<&AlignedSlice<A>, Misaligned> {
        self.as_ref().try_as_aligned()
    }
}
impl<A: Alignment, T: AsRef<[u8]> + AsMut<[u8]>> TryAsAlignedMut<A> for T {
    fn try_as_aligned_mut(&mut self) -> Result<&mut AlignedSlice<A>, Misaligned> {
        self.as_mut().try_as_aligned_mut()
    }
}
impl AsAligned<A1> for [u8] {
    fn as_aligned(&self) -> &AlignedSlice<A1> {
        // An alignment of A1 is the same as no alignment, so this is safe
        unsafe { to_alignedslice_unchecked(self) }
    }
}
impl AsAlignedMut<A1> for [u8] {
    fn as_aligned_mut(&mut self) -> &mut AlignedSlice<A1> {
        unsafe { to_alignedslice_unchecked_mut(self) }
    }
}
impl<A: Alignment> TryAsAligned<A> for [u8] {
    fn try_as_aligned(&self) -> Result<&AlignedSlice<A>, Misaligned> {
        self.as_aligned().try_as_aligned()
    }
}
impl<A: Alignment> TryAsAlignedMut<A> for [u8] {
    fn try_as_aligned_mut(&mut self) -> Result<&mut AlignedSlice<A>, Misaligned> {
        self.as_aligned_mut().try_as_aligned_mut()
    }
}

impl<FromA: Alignment, ToA: Alignment> AsAligned<ToA> for AlignedSlice<FromA>
where
    FromA: AlignedTo<ToA>,
{
    fn as_aligned(&self) -> &AlignedSlice<ToA> {
        // This is a narrowing, so will always succeed
        let initial_align = std::mem::align_of_val(self);
        let out = unsafe { &*(self as *const Self as *const AlignedSlice<ToA>) };
        assert!(initial_align >= std::mem::align_of_val(out));
        out
    }
}
impl<FromA: Alignment, ToA: Alignment> AsAlignedMut<ToA> for AlignedSlice<FromA>
where
    FromA: AlignedTo<ToA>,
{
    fn as_aligned_mut(&mut self) -> &mut AlignedSlice<ToA> {
        // This is a narrowing, so will always succeed
        let initial_align = std::mem::align_of_val(self);
        let out = unsafe { &mut *(self as *mut Self as *mut AlignedSlice<ToA>) };
        assert!(initial_align >= std::mem::align_of_val(out));
        out
    }
}
impl<FromA: Alignment, ToA: Alignment> TryAsAligned<ToA> for AlignedSlice<FromA> {
    fn try_as_aligned(&self) -> Result<&AlignedSlice<ToA>, Misaligned> {
        if std::mem::align_of::<FromA>() >= std::mem::align_of::<ToA>() {
            // We're narrowing the alignment here, which is always fine.  Using
            // as_aligned might be better in this instance
            Ok(unsafe { &*(self as *const Self as *const AlignedSlice<ToA>) })
        } else if is_aligned_to::<ToA>(self) {
            // We're widening the alignment, fall back to runtime check
            Ok(unsafe { &*(self as *const Self as *const AlignedSlice<ToA>) })
        } else {
            Err(Misaligned {})
        }
    }
}
impl<FromA: Alignment, ToA: Alignment> TryAsAlignedMut<ToA> for AlignedSlice<FromA> {
    fn try_as_aligned_mut(&mut self) -> Result<&mut AlignedSlice<ToA>, Misaligned> {
        if std::mem::align_of::<FromA>() >= std::mem::align_of::<ToA>() {
            // We're narrowing the alignment here, which is always fine.  Using
            // as_aligned might be better in this instance
            Ok(unsafe { &mut *(self as *mut Self as *mut AlignedSlice<ToA>) })
        } else if is_aligned_to::<ToA>(self) {
            // We're widening the alignment, fall back to runtime check
            Ok(unsafe { &mut *(self as *mut Self as *mut AlignedSlice<ToA>) })
        } else {
            Err(Misaligned {})
        }
    }
}

impl<FromA, ToA: Alignment> AsRef<AlignedSlice<ToA>> for AlignedSlice<FromA>
where
    FromA: AlignedTo<ToA>,
{
    fn as_ref(&self) -> &AlignedSlice<ToA> {
        self.as_aligned()
    }
}

impl<FromA, ToA: Alignment> AsMut<AlignedSlice<ToA>> for AlignedSlice<FromA>
where
    FromA: AlignedTo<ToA>,
{
    fn as_mut(&mut self) -> &mut AlignedSlice<ToA> {
        self.as_aligned_mut()
    }
}

impl<A: Alignment> Index<RangeTo<usize>> for AlignedSlice<A> {
    type Output = AlignedSlice<A>;
    fn index(&self, index: RangeTo<usize>) -> &Self::Output {
        // Truncating the slice on the right doesn't affect the alignment
        unsafe { to_alignedslice_unchecked(&self.data[index]) }
    }
}
impl<A: Alignment> Index<RangeToInclusive<usize>> for AlignedSlice<A> {
    type Output = AlignedSlice<A>;
    fn index(&self, index: RangeToInclusive<usize>) -> &Self::Output {
        // Truncating the slice on the left doesn't affect the alignment
        unsafe { to_alignedslice_unchecked(&self.data[index]) }
    }
}
impl<A: Alignment> Index<Range<usize>> for AlignedSlice<A> {
    type Output = AlignedSlice<A1>;
    fn index(&self, index: Range<usize>) -> &Self::Output {
        // Truncating the slice on the left can affect the alignment, so we
        // return an unaligned slice here
        &self.data[index].as_aligned()
    }
}
impl<A: Alignment> Index<RangeInclusive<usize>> for AlignedSlice<A> {
    type Output = AlignedSlice<A1>;
    fn index(&self, index: RangeInclusive<usize>) -> &Self::Output {
        // Truncating the slice on the left can affect the alignment, so we
        // return an unaligned slice here
        &self.data[index].as_aligned()
    }
}
impl<A: Alignment> Index<RangeFrom<usize>> for AlignedSlice<A> {
    type Output = AlignedSlice<A1>;
    fn index(&self, index: RangeFrom<usize>) -> &Self::Output {
        // Truncating the slice on the left can affect the alignment, so we
        // return an unaligned slice here
        &self.data[index].as_aligned()
    }
}
impl<A: Alignment> Index<RangeFull> for AlignedSlice<A> {
    type Output = AlignedSlice<A>;
    fn index(&self, _: RangeFull) -> &Self::Output {
        &self
    }
}

impl<A: Alignment> Index<Range<AlignedOffset<A>>> for AlignedSlice<A> {
    type Output = AlignedSlice<A>;
    fn index(&self, index: Range<AlignedOffset<A>>) -> &Self::Output {
        // index.start and index.end are guaranteed to be a multiple of
        // A::ALIGNMENT, so this is safe:
        unsafe {
            to_alignedslice_unchecked(&self.data[index.start.to_usize()..index.end.to_usize()])
        }
    }
}
impl<A: Alignment> Index<RangeInclusive<AlignedOffset<A>>> for AlignedSlice<A> {
    type Output = AlignedSlice<A>;
    fn index(&self, index: RangeInclusive<AlignedOffset<A>>) -> &Self::Output {
        // index.start and index.end are guaranteed to be a multiple of
        // A::ALIGNMENT, so this is safe:
        unsafe {
            to_alignedslice_unchecked(&self.data[index.start().to_usize()..=index.end().to_usize()])
        }
    }
}
impl<A: Alignment> Index<RangeFrom<AlignedOffset<A>>> for AlignedSlice<A> {
    type Output = AlignedSlice<A>;
    fn index(&self, index: RangeFrom<AlignedOffset<A>>) -> &Self::Output {
        // index.start is guaranteed to be a multiple of A::ALIGNMENT, so this
        // is safe:
        unsafe { to_alignedslice_unchecked(&self.data[index.start.to_usize()..]) }
    }
}

unsafe fn to_alignedslice_unchecked<'a, A: Alignment>(value: &'a [u8]) -> &'a AlignedSlice<A> {
    debug_assert!(is_aligned_to::<A>(value));
    #[allow(unused_unsafe)]
    unsafe {
        &*(value as *const [u8] as *const AlignedSlice<A>)
    }
}
unsafe fn to_alignedslice_unchecked_mut<'a, A: Alignment>(
    value: &'a mut [u8],
) -> &'a mut AlignedSlice<A> {
    debug_assert!(is_aligned_to::<A>(value));
    #[allow(unused_unsafe)]
    unsafe {
        &mut *(value as *mut [u8] as *mut AlignedSlice<A>)
    }
}

fn is_aligned_to<A: Alignment>(value: &[u8]) -> bool {
    is_aligned(value, A::ALIGNMENT)
}
pub fn is_aligned(value: &[u8], alignment: usize) -> bool {
    value as *const [u8] as *const u8 as usize % alignment == 0
}

#[derive(Debug)]
pub struct Misaligned {}
impl std::error::Error for Misaligned {}
impl std::fmt::Display for Misaligned {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Misaligned")
    }
}

impl<'a> std::convert::From<&'a [u8]> for &'a AlignedSlice<A1> {
    fn from(value: &'a [u8]) -> Self {
        // Everything is 1B aligned, so this is safe:
        unsafe { to_alignedslice_unchecked(value) }
    }
}
impl<'a> std::convert::From<&'a mut [u8]> for &'a mut AlignedSlice<A1> {
    fn from(value: &'a mut [u8]) -> Self {
        // Everything is 1B aligned, so this is safe:
        unsafe { to_alignedslice_unchecked_mut(value) }
    }
}
impl<A: Alignment> AsRef<[u8]> for AlignedSlice<A> {
    fn as_ref(&self) -> &[u8] {
        &self.data
    }
}
impl<A: Alignment> AsMut<[u8]> for AlignedSlice<A> {
    fn as_mut(&mut self) -> &mut [u8] {
        &mut self.data
    }
}

fn align_bytes<'a, A: Alignment>(value: &'a [u8]) -> &'a AlignedSlice<A> {
    let p = value as *const [u8] as *const u8 as usize;
    let offset = p.wrapping_neg() & (A::ALIGNMENT - 1);
    value[offset..].try_as_aligned().unwrap()
}
pub fn empty_aligned<A: Alignment>() -> &'static AlignedSlice<A> {
    &align_bytes(b"        ")[..1]
}
