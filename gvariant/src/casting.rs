//! Safe casting
//!
//! `unsafe` is employed quite a lot to handle all the casting between types.
//! I've attempted to build abstractions here to cover the invariants that must
//! be maintained for this casting to be safe and sound.  These are:
//!
//! * The bit pattern of the underlying memory must be valid for the target
//!   type. This is provided by the trait [`AllBitPatternsValid`].  It's
//!   implemented for the relevant primative types.
//! * The alignment requirements of the target type must be satisfied.  We use
//!   the [`AlignOf`] trait to determine the alignment of the target type.  We
//!   can't rely on [`std::mem::align_of`] because we want to use it to
//!   constrain input types, and we can't do that without const generics, which
//!   are still an unstable Rust feature.  We use [`AlignedSlice`] for byte
//!   slices with an alignment attached.  See [`aligned_bytes`].
//! * The size of the data must match.  We check this at run-time.  When casting
//!   from slices to [`Sized`] types we only perform the cast if the sizes
//!   match. Similarly when casting between slices we only perform the cast if
//!   the input slice size is a multiple of the element size

use core::{convert::TryInto, fmt::Display};

#[cfg(feature = "std")]
use std::error::Error;

use crate::aligned_bytes;
use crate::aligned_bytes::{is_aligned, AlignedSlice};

/// If a type implements this trait it's a promise that all representations of
/// underlying memory are valid for this type.  That means any struct must be
/// `repr(C)` or ``repr(transparent)` and be made up of members that are also
/// `AllBitPatternsValid` and have no padding.
pub unsafe trait AllBitPatternsValid {}

/// Get the alignment of a type as a [`aligned_bytes::Alignment`]
///
/// This trait is unsafe because we will be relying on the information from the
/// trait to do casting safely.  The alignment needs to be correct, or at least
/// conservative.
///
/// It would be nice to be able to use `std::mem::align_of<T>()`, but we need it
/// in trait bounds, so that'll have to wait until const generics:
pub unsafe trait AlignOf {
    type AlignOf: aligned_bytes::Alignment;
}
unsafe impl<T: AlignOf> AlignOf for [T] {
    type AlignOf = T::AlignOf;
}

/// Error returned by [`try_cast_slice_to`] and [`try_cast_slice_to_mut`] if the
/// size of the slice doesn't match the size of the desination type.
#[derive(Debug)]
pub struct WrongSize {}
#[cfg(feature = "std")]
impl Error for WrongSize {}
impl Display for WrongSize {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "Slice has wrong size")
    }
}

/// Safely cast an `&AlignedSlice` to `&[T]`
///
/// If the length of the input slice isn't an exact multiple of the size of `T`
/// this function will return [`Err(WrongSize)`][WrongSize].
pub(crate) fn cast_slice<'a, A: aligned_bytes::Alignment, T: AllBitPatternsValid>(
    a: &'a AlignedSlice<A>,
) -> Result<&'a [T], WrongSize> {
    // This function requires the data to already have suitable alignment.  This
    // should be compiled out:
    if A::ALIGNMENT < core::mem::align_of::<T>() {
        panic!()
    }
    // This assertion should be guaranteed by the above check:
    debug_assert!(is_aligned(a, core::mem::align_of::<T>()));
    if a.len() % core::mem::size_of::<T>() == 0 {
        // We know the alignment and size is ok now, and AllBitPatternsValid
        // means that the representation makes sense, so we go ahead with the
        // cast:
        Ok(unsafe {
            core::slice::from_raw_parts::<'a, T>(
                a.as_ptr() as *const T,
                a.len() / core::mem::size_of::<T>(),
            )
        })
    } else {
        Err(WrongSize {})
    }
}

/// Safely cast an `&AlignedSlice` to `&T` where `T: Sized`
///
/// If the length of the input slice isn't exactly the size of `T` this function
/// will return [`Err(WrongSize)`][WrongSize].
pub fn try_cast_slice_to<'a, T: AlignOf + AllBitPatternsValid>(
    s: &'a AlignedSlice<T::AlignOf>,
) -> Result<&'a T, WrongSize> {
    if core::mem::size_of::<T>() == s.len() {
        debug_assert!(is_aligned(s, core::mem::align_of::<T>()));
        Ok(unsafe { &*(s.as_ptr() as *const T) })
    } else {
        Err(WrongSize {})
    }
}

/// Safely cast a `&mut AlignedSlice` to `&mut T` where `T: Sized`
///
/// If the length of the input slice isn't exactly the size of `T` this function
/// will return [`Err(WrongSize)`][WrongSize].
pub fn try_cast_slice_to_mut<'a, T: AlignOf + AllBitPatternsValid>(
    s: &'a mut AlignedSlice<T::AlignOf>,
) -> Result<&'a mut T, WrongSize> {
    if core::mem::size_of::<T>() == s.len() {
        debug_assert!(is_aligned(s, core::mem::align_of::<T>()));
        Ok(unsafe { &mut *(s.as_mut_ptr() as *mut T) })
    } else {
        Err(WrongSize {})
    }
}

macro_rules! unsafe_fixed_bytes_to_type {
    // This macro is unsafe.  It's only permitted to call it with types where
    // all possible bit patterns are valid.  If you don't get the size and
    // alignment right the implemented trait methods will panic at run-time
    ($type:ident, $size:literal, $align:ty) => {
        unsafe impl AllBitPatternsValid for $type {}
        unsafe impl AlignOf for $type {
            type AlignOf = $align;
        }
        impl<'a> TryInto<&'a [$type]> for &'a AlignedSlice<$align> {
            type Error = WrongSize;
            fn try_into(self) -> Result<&'a [$type], Self::Error> {
                cast_slice(self)
            }
        }
    };
}

unsafe_fixed_bytes_to_type!(u8, 1, aligned_bytes::A1);
unsafe_fixed_bytes_to_type!(i16, 2, aligned_bytes::A2);
unsafe_fixed_bytes_to_type!(u16, 2, aligned_bytes::A2);
unsafe_fixed_bytes_to_type!(i32, 4, aligned_bytes::A4);
unsafe_fixed_bytes_to_type!(u32, 4, aligned_bytes::A4);
unsafe_fixed_bytes_to_type!(i64, 8, aligned_bytes::A8);
unsafe_fixed_bytes_to_type!(u64, 8, aligned_bytes::A8);
unsafe_fixed_bytes_to_type!(f64, 8, aligned_bytes::A8);
unsafe impl<T: AllBitPatternsValid> AllBitPatternsValid for [T] {}
