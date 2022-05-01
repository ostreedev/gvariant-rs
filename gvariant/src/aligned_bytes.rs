//! Byte slices with statically guaranteed alignment
//!
//! The gvariant crates operates by reinterpreting byte buffers as a Rust type.
//! For these casts to be valid the alignment of the underlying data must be
//! sufficient for the target type.  We don't perform any of our own
//! allocations, relying on data passed from the user, as a result proper
//! alignment of byte buffers is the responsibility of the user.
//!
//! This library defines a type [`AlignedSlice<A>`][AlignedSlice] which
//! represents an aligned byte buffer aligned to the alignment given by `A`. `A`
//! may be:
//!
//! * [`A1`] - 1B aligned aka unaligned
//! * [`A2`] - 2B aligned
//! * [`A4`] - 4B aligned
//! * [`A8`] - 8B aligned
//!
//! As a type parameter the alignment is known statically at compile time.  This
//! allows eliding runtime checks.
//!
//! You can convert [`AlignedSlice`]s to lower alignments infallibly and for
//! free (using [`AsAligned::as_aligned`] and [`AsAlignedMut::as_aligned_mut`]).
//! Going in the opposite direction and coming from a plain `&[u8]` slice
//! requires runtime checks ([`TryAsAligned::try_as_aligned`] and
//! [`TryAsAlignedMut::try_as_aligned_mut`]) and may require copying the data
//! ([`copy_to_align`]).
//!
//! The [`AsAligned`] trait is provided to make accepting any data of the
//! appropriate alignment convenient.  For example: use a `&impl AsAligned<A2>`
//! parameter to accept any data with 2B or greater alignment.
//!
//! [`alloc_aligned`] is provided to make it easy and safe to create aligned
//! buffers. Example reading data from file into aligned buffer:
//!
//! ```rust
//! # use gvariant::aligned_bytes::{A8, alloc_aligned};
//! # use std::io::Read;
//! # fn foo() -> Result<(), Box<dyn std::error::Error>> {
//! # let mut file = std::fs::File::open("")?;
//! let mut buf = alloc_aligned::<A8>(4096);
//! let len = file.read(buf.as_mut())?;
//! let aligned_data = &buf[..len];
//! # Ok(()) }
//! ```
//!
//! I've not yet implemented it, but it may become necessary to create an
//! equivalent to `Vec<u8>` but for aligned memory.  We'll see how we get on.
//!
//! #### Efficiency of statically known alignment
//!
//! Every GVariant container type has alignment >= any of its children.  This
//! means that if a byte slice is aligned for the parent it will still be
//! aligned when we access the children, so we only need to check the alignment
//! once when constructing the buffer rather than on every access.
//!
//! For example: The structure `(nu)` (or `(i16, u32)` in rust terms) has
//! alignment 4: So we create an `AlignedSlice<A4>` containing the complete 8B
//! structure.  The alignment is checked at run-time when we create the slice.
//! The `i16` is extracted with `data[..2]`, which still has type
//! `AlignedSlice<A4>`. The conversion to `AlignedSlice<A2>` as required by the
//! `i16`'s doesn't require any runtime checks.
//!
//! #### Serialisation alignment vs. platform alignment requirements
//!
//! Note: there are two related, but subtly different concepts of alignment at
//! use in this library:
//!
//! 1. The GVariant serialisation format has a concept of alignment of data
//!    types which informs where padding should be placed and affects the size
//!    of fixed size structures.
//! 2. There are the alignment requirements of the types we're casting to that
//!    Rust and the underlying platform requires of us when we cast.  These are
//!    the values that `std::mem::align_of<>()` returns.
//!
//! These alignments are usually the same, but in some circumstances can differ.
//! For example: I believe that on 32-bit x86 `align_of<u64>() == 4` - while of
//! course the serialisation format doesn't depend on the platform in use.  This
//! library assumes (and asserts in code) that the former alignment is always >=
//! the latter.

pub use crate::offset::{align_offset, AlignedOffset};

#[cfg(feature = "alloc")]
use alloc::{
    borrow::{Cow, ToOwned},
    boxed::Box,
};
use core::ops::{
    Deref, DerefMut, Index, IndexMut, Range, RangeFrom, RangeFull, RangeInclusive, RangeTo,
    RangeToInclusive,
};
use core::{
    cmp::{max, min},
    fmt::Debug,
};
#[cfg(feature = "std")]
use std::io::IoSliceMut;

/// A trait for our alignment structs [`A1`], [`A2`], [`A4`] and [`A8`].
///
/// Ideally we'd just use const-generics here, but they're not available on the
/// rust stable channel yet.  This is the type-level equivalent of
/// `enum Alignment {A1 = 1, A2 = 2, A4 = 4, A8 = 8}`.
///
/// This is unsafe because it must only be implemented for zero-sized types.  Do
/// not implement this trait.  The implementations in this module should be the
/// only implementations.
pub unsafe trait Alignment: Debug {
    const ALIGNMENT: usize;
}
/// This is a promise that the type is aligned as described by A.
///
/// It is unsafe because safe code must be able to assume the given alignment
/// for e.g. pointer casts.
///
/// It can be used as a type constraint in where clauses like so:
///
///     # use gvariant::aligned_bytes::{A4, AlignedTo};
///     # fn x<A>()
///     where A : AlignedTo<A4>
///     # {}
///
/// which means:
///
/// ```ignore
/// where A::ALIGNMENT >= 4
/// ```
pub unsafe trait AlignedTo<A: Alignment>: Alignment {}

/// 1-byte alignment e.g. no alignment
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

/// Allows narrowing the alignment of a `&AlignedSlice`
///
/// This can be convenient to accept any [`AlignedSlice`] with sufficient
/// alignment as an argument to a function.  For example:
///
///     # use gvariant::aligned_bytes::{A2, AsAligned};

///     fn foo(data : &impl AsAligned<A2>) {
///         let data = data.as_aligned();
///     # }
///
/// or if a function requires a specific alignment you can do:
///
///     # use gvariant::aligned_bytes::{A2, A4, AlignedSlice, AsAligned, empty_aligned};
///     # fn bar(a: &AlignedSlice<A2>) {}
///     # let data = empty_aligned::<A4>();
///     bar(data.as_aligned());
///
/// Mostly we convert from `AlignedSlice<A8>` -> `AlignedSlice<A4>` ->
/// `AlignedSlice<A2>` -> `AlignedSlice<A1>`, but this can also be used to go
/// from `&[u8]` to `AlignedSlice<A1>`.
///
/// This is intended as a 0 overhead abstraction.  In release mode this should
/// compile down to nothing.
pub trait AsAligned<A: Alignment> {
    fn as_aligned(&self) -> &AlignedSlice<A>;
}
/// Allows narrowing the alignment of a `&mut AlignedSlice`
///
/// Just the same as [`AsAligned`], but `mut`.
pub trait AsAlignedMut<A: Alignment>: AsAligned<A> {
    fn as_aligned_mut(&mut self) -> &mut AlignedSlice<A>;
}
/// Allows widening the alignment by performing fallible runtime checks.
pub trait TryAsAligned<A: Alignment> {
    fn try_as_aligned(&self) -> Result<&AlignedSlice<A>, Misaligned>;
}
/// Allows widening the alignment by performing fallible runtime checks.
pub trait TryAsAlignedMut<A: Alignment>: TryAsAligned<A> {
    fn try_as_aligned_mut(&mut self) -> Result<&mut AlignedSlice<A>, Misaligned>;
}

/// A byte array, but with a compile-time guaranteed minimum alignment
///
/// The aligment requirement is specfied by the type parameter A.  It will be
/// [`A1`], [`A2`], [`A4`] or [`A8`].
#[repr(C)]
#[derive(Debug, Eq)]
pub struct AlignedSlice<A: Alignment> {
    alignment: A,
    data: [u8],
}

impl<A: Alignment> PartialEq for AlignedSlice<A> {
    fn eq(&self, other: &Self) -> bool {
        self.data == other.data
    }
}

#[cfg(feature = "alloc")]
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
#[cfg(feature = "alloc")]
pub fn copy_to_align<A: Alignment>(data: &[u8]) -> Cow<'_, AlignedSlice<A>> {
    if is_aligned_to::<A>(data) {
        Cow::Borrowed(data.try_as_aligned().unwrap())
    } else {
        let mut copy = alloc_aligned::<A>(data.len());
        copy.copy_from_slice(data);
        Cow::Owned(copy)
    }
}

/// Allocate a new boxed [`AlignedSlice`].
///
/// Data is initialised to all 0.
#[cfg(feature = "alloc")]
pub fn alloc_aligned<A: Alignment>(size: usize) -> Box<AlignedSlice<A>> {
    let layout = alloc::alloc::Layout::from_size_align(size, A::ALIGNMENT).unwrap();
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
        let bs = core::slice::from_raw_parts_mut(alloc::alloc::alloc_zeroed(layout), size);
        Box::from_raw(to_alignedslice_unchecked_mut(bs))
    }
}

/// Read the contents of the `Read` into an `AlignedSlice`
///
/// This can be considered equivalent to `Read::read_to_end()`, but for
/// `AlignedSlice` rather than `Vec`.
///
/// If we know how much data there is to read (a common case) then setting
/// `size_hint` will allow us to perform just a single allocation and no
/// copying.
#[cfg(feature = "std")]
pub fn read_to_slice<A: Alignment, R: std::io::Read>(
    mut r: R,
    size_hint: Option<usize>,
) -> std::io::Result<Box<AlignedSlice<A>>> {
    let mut bytes_read = 0;
    let mut eof_byte = [0u8];
    let mut buf = alloc_aligned(size_hint.unwrap_or(4000));

    loop {
        let mut ios = [
            IoSliceMut::new(&mut buf.as_mut()[bytes_read..]),
            IoSliceMut::new(&mut eof_byte),
        ];
        let this_read = r.read_vectored(&mut ios)?;
        bytes_read += this_read;
        if this_read == 0 {
            // EOF
            return Ok(realloc(buf, bytes_read));
        }
        if bytes_read > buf.len() {
            // Ran out of space
            buf = realloc(buf, max(bytes_read * 2, 4000));
            buf[bytes_read - 1] = eof_byte[0];
        }
    }
}

fn realloc<A: Alignment>(s: Box<AlignedSlice<A>>, new_size: usize) -> Box<AlignedSlice<A>> {
    if s.len() == new_size {
        s
    } else {
        // TODO: Optimise to use alloc::realloc
        let mut new_buf = alloc_aligned(new_size);
        let up_to = min(new_size, s.len());
        new_buf.as_mut()[..up_to].copy_from_slice(s[..up_to].as_ref());
        new_buf
    }
}

impl<A: Alignment> AlignedSlice<A> {
    pub fn split_at(&self, mid: usize) -> (&AlignedSlice<A>, &[u8]) {
        let (before, after) = self.data.split_at(mid);
        // This is safe because before.as_ptr() will be the same as
        // self.data.as_ptr(), so must still be aligned.
        (unsafe { to_alignedslice_unchecked(before) }, after)
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
        let initial_align = core::mem::align_of_val(self);
        let out = unsafe { &*(self as *const Self as *const AlignedSlice<ToA>) };
        assert!(initial_align >= core::mem::align_of_val(out));
        out
    }
}
impl<FromA: Alignment, ToA: Alignment> AsAlignedMut<ToA> for AlignedSlice<FromA>
where
    FromA: AlignedTo<ToA>,
{
    fn as_aligned_mut(&mut self) -> &mut AlignedSlice<ToA> {
        // This is a narrowing, so will always succeed
        let initial_align = core::mem::align_of_val(self);
        let out = unsafe { &mut *(self as *mut Self as *mut AlignedSlice<ToA>) };
        assert!(initial_align >= core::mem::align_of_val(out));
        out
    }
}
impl<FromA: Alignment, ToA: Alignment> TryAsAligned<ToA> for AlignedSlice<FromA> {
    fn try_as_aligned(&self) -> Result<&AlignedSlice<ToA>, Misaligned> {
        // If narrowing the alignment we know it's fine (at compile time).  If
        // widening the alignment we must fall back to runtime check
        if core::mem::align_of::<FromA>() >= core::mem::align_of::<ToA>()
            || is_aligned_to::<ToA>(self)
        {
            Ok(unsafe { &*(self as *const Self as *const AlignedSlice<ToA>) })
        } else {
            Err(Misaligned {})
        }
    }
}
impl<FromA: Alignment, ToA: Alignment> TryAsAlignedMut<ToA> for AlignedSlice<FromA> {
    fn try_as_aligned_mut(&mut self) -> Result<&mut AlignedSlice<ToA>, Misaligned> {
        // If narrowing the alignment we know it's fine (at compile time).  If
        // widening the alignment we must fall back to runtime check
        if core::mem::align_of::<FromA>() >= core::mem::align_of::<ToA>()
            || is_aligned_to::<ToA>(self)
        {
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

impl<A: Alignment> Index<usize> for AlignedSlice<A> {
    type Output = u8;
    fn index(&self, index: usize) -> &Self::Output {
        &self.data[index]
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
        self.data[index].as_aligned()
    }
}
impl<A: Alignment> Index<RangeInclusive<usize>> for AlignedSlice<A> {
    type Output = AlignedSlice<A1>;
    fn index(&self, index: RangeInclusive<usize>) -> &Self::Output {
        // Truncating the slice on the left can affect the alignment, so we
        // return an unaligned slice here
        self.data[index].as_aligned()
    }
}
impl<A: Alignment> Index<RangeFrom<usize>> for AlignedSlice<A> {
    type Output = AlignedSlice<A1>;
    fn index(&self, index: RangeFrom<usize>) -> &Self::Output {
        // Truncating the slice on the left can affect the alignment, so we
        // return an unaligned slice here
        self.data[index].as_aligned()
    }
}
impl<A: Alignment> Index<RangeFull> for AlignedSlice<A> {
    type Output = AlignedSlice<A>;
    fn index(&self, _: RangeFull) -> &Self::Output {
        self
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
impl<A: Alignment> IndexMut<usize> for AlignedSlice<A> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.data[index]
    }
}
impl<A: Alignment> IndexMut<RangeTo<usize>> for AlignedSlice<A> {
    fn index_mut(&mut self, index: RangeTo<usize>) -> &mut Self::Output {
        // Truncating the slice on the right doesn't affect the alignment
        unsafe { to_alignedslice_unchecked_mut(&mut self.data[index]) }
    }
}
impl<A: Alignment> IndexMut<RangeToInclusive<usize>> for AlignedSlice<A> {
    fn index_mut(&mut self, index: RangeToInclusive<usize>) -> &mut Self::Output {
        // Truncating the slice on the left doesn't affect the alignment
        unsafe { to_alignedslice_unchecked_mut(&mut self.data[index]) }
    }
}
impl<A: Alignment> IndexMut<Range<usize>> for AlignedSlice<A> {
    fn index_mut(&mut self, index: Range<usize>) -> &mut Self::Output {
        // Truncating the slice on the left can affect the alignment, so we
        // return an unaligned slice here
        self.data[index].as_aligned_mut()
    }
}
impl<A: Alignment> IndexMut<RangeInclusive<usize>> for AlignedSlice<A> {
    fn index_mut(&mut self, index: RangeInclusive<usize>) -> &mut Self::Output {
        // Truncating the slice on the left can affect the alignment, so we
        // return an unaligned slice here
        self.data[index].as_aligned_mut()
    }
}
impl<A: Alignment> IndexMut<RangeFrom<usize>> for AlignedSlice<A> {
    fn index_mut(&mut self, index: RangeFrom<usize>) -> &mut Self::Output {
        // Truncating the slice on the left can affect the alignment, so we
        // return an unaligned slice here
        self.data[index].as_aligned_mut()
    }
}
impl<A: Alignment> IndexMut<RangeFull> for AlignedSlice<A> {
    fn index_mut(&mut self, _: RangeFull) -> &mut Self::Output {
        self
    }
}

impl<A: Alignment> IndexMut<Range<AlignedOffset<A>>> for AlignedSlice<A> {
    fn index_mut(&mut self, index: Range<AlignedOffset<A>>) -> &mut Self::Output {
        // index.start and index.end are guaranteed to be a multiple of
        // A::ALIGNMENT, so this is safe:
        unsafe {
            to_alignedslice_unchecked_mut(
                &mut self.data[index.start.to_usize()..index.end.to_usize()],
            )
        }
    }
}
impl<A: Alignment> IndexMut<RangeInclusive<AlignedOffset<A>>> for AlignedSlice<A> {
    fn index_mut(&mut self, index: RangeInclusive<AlignedOffset<A>>) -> &mut Self::Output {
        // index.start and index.end are guaranteed to be a multiple of
        // A::ALIGNMENT, so this is safe:
        unsafe {
            to_alignedslice_unchecked_mut(
                &mut self.data[index.start().to_usize()..=index.end().to_usize()],
            )
        }
    }
}
impl<A: Alignment> IndexMut<RangeFrom<AlignedOffset<A>>> for AlignedSlice<A> {
    fn index_mut(&mut self, index: RangeFrom<AlignedOffset<A>>) -> &mut Self::Output {
        // index.start is guaranteed to be a multiple of A::ALIGNMENT, so this
        // is safe:
        unsafe { to_alignedslice_unchecked_mut(&mut self.data[index.start.to_usize()..]) }
    }
}

unsafe fn to_alignedslice_unchecked<A: Alignment>(value: &[u8]) -> &AlignedSlice<A> {
    debug_assert!(is_aligned_to::<A>(value));
    #[allow(unused_unsafe)]
    unsafe {
        &*(value as *const [u8] as *const AlignedSlice<A>)
    }
}
unsafe fn to_alignedslice_unchecked_mut<A: Alignment>(value: &mut [u8]) -> &mut AlignedSlice<A> {
    debug_assert!(is_aligned_to::<A>(value));
    #[allow(unused_unsafe)]
    unsafe {
        &mut *(value as *mut [u8] as *mut AlignedSlice<A>)
    }
}

fn is_aligned_to<A: Alignment>(value: &[u8]) -> bool {
    is_aligned(value, A::ALIGNMENT)
}
pub(crate) fn is_aligned(value: &[u8], alignment: usize) -> bool {
    value as *const [u8] as *const u8 as usize % alignment == 0
}

/// Error returned by [`TryAsAligned::try_as_aligned`] and
/// [`TryAsAlignedMut::try_as_aligned_mut`] when the passed in slice isn't
/// appropriately aligned.
#[derive(Debug)]
pub struct Misaligned {}
#[cfg(feature = "std")]
impl std::error::Error for Misaligned {}
impl core::fmt::Display for Misaligned {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "Misaligned")
    }
}

impl<'a> core::convert::From<&'a [u8]> for &'a AlignedSlice<A1> {
    fn from(value: &'a [u8]) -> Self {
        // Everything is 1B aligned, so this is safe:
        unsafe { to_alignedslice_unchecked(value) }
    }
}
impl<'a> core::convert::From<&'a mut [u8]> for &'a mut AlignedSlice<A1> {
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

fn align_bytes<A: Alignment>(value: &[u8]) -> &AlignedSlice<A> {
    let p = value as *const [u8] as *const u8 as usize;
    let offset = p.wrapping_neg() & (A::ALIGNMENT - 1);
    value[offset..].try_as_aligned().unwrap()
}

/// Get a static reference to an empty [`AlignedSlice`] with the given
/// alignment.
///
/// This is useful for implementing GVariant default values.
pub fn empty_aligned<A: Alignment>() -> &'static AlignedSlice<A> {
    &align_bytes(b"        ")[..0]
}

#[cfg(test)]
mod test {
    use super::{read_to_slice, AlignedSlice, A8};

    #[test]
    fn test_read_to_slice() {
        let mut d: Vec<u8> = vec![0; 16384];
        for x in 0..16384 {
            d[x] = (x % 256) as u8;
        }

        for size_hint in &[
            Some(11),
            None,
            Some(0),
            Some(1),
            Some(7999),
            Some(8000),
            Some(8001),
        ] {
            let s: Box<AlignedSlice<A8>> = read_to_slice(b"".as_ref(), *size_hint).unwrap();
            assert_eq!(**s, *b"");

            let s: Box<AlignedSlice<A8>> = read_to_slice(&d[..12], *size_hint).unwrap();
            assert_eq!(**s, d[..12]);

            let s: Box<AlignedSlice<A8>> = read_to_slice(&d[..8000], *size_hint).unwrap();
            assert_eq!(**s, d[..8000]);

            let s: Box<AlignedSlice<A8>> = read_to_slice(d.as_slice(), *size_hint).unwrap();
            assert_eq!(&**s, d.as_slice());
        }
    }
}
