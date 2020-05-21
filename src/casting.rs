use std::{convert::TryInto, error::Error, fmt::Display};

use crate::aligned_bytes;
use crate::aligned_bytes::{is_aligned, AlignedSlice};

pub unsafe trait AllBitPatternsValid {
    // If a type implements this trait it's a promise that all representations
    // of underlying memory are valid for this type.  That means any struct must
    // be repr(C) and be made up of members that are also FixedSize and have
    // no padding.
}

// This trait is unsafe because we will be relying on the information from the
// trait to do casting safely.  The alignment needs to be correct, or at least
// conservative.
//
// It would be nice to be able to use std::mem::align_of<T>(), but we need it in
// trait bounds, so that'll have to wait until const generics:
pub unsafe trait AlignOf {
    type AlignOf: aligned_bytes::Alignment;
}

#[derive(Debug)]
pub struct WrongSize {}
impl Error for WrongSize {}
impl Display for WrongSize {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Slice has wrong size")
    }
}

pub(crate) fn cast_slice<'a, A: aligned_bytes::Alignment, T: AllBitPatternsValid>(
    a: &'a AlignedSlice<A>,
) -> Result<&'a [T], WrongSize> {
    // This function requires the data to already have suitable alignment.  This
    // should be compiled out:
    if A::ALIGNMENT < std::mem::align_of::<T>() {
        panic!()
    }
    // This assertion should be guaranteed by the above check:
    debug_assert!(is_aligned(a, std::mem::align_of::<T>()));
    if a.len() % std::mem::size_of::<T>() == 0 {
        // We know the alignment and size is ok now, and AllBitPatternsValid
        // means that the representation makes sense, so we go ahead with the
        // cast:
        Ok(unsafe {
            std::slice::from_raw_parts::<'a, T>(
                a.as_ptr() as *const T,
                a.len() / std::mem::size_of::<T>(),
            )
        })
    } else {
        Err(WrongSize {})
    }
}

pub(crate) fn try_cast_slice_to<
    'a,
    A: aligned_bytes::Alignment,
    T: AlignOf + AllBitPatternsValid,
>(
    s: &'a AlignedSlice<A>,
) -> Result<&'a T, WrongSize> {
    if std::mem::size_of::<T>() == s.len() {
        debug_assert!(is_aligned(s, std::mem::align_of::<T>()));
        Ok(unsafe { &*(s.as_ptr() as *const T) })
    } else {
        Err(WrongSize {})
    }
}

pub(crate) fn try_cast_slice_to_mut<
    'a,
    A: aligned_bytes::Alignment,
    T: AlignOf + AllBitPatternsValid,
>(
    s: &'a mut AlignedSlice<A>,
) -> Result<&'a mut T, WrongSize> {
    if std::mem::size_of::<T>() == s.len() {
        debug_assert!(is_aligned(s, std::mem::align_of::<T>()));
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
        /*impl AsRef<$type> for AlignedSlice<align::$align, [u8;$size]> {
            fn as_ref(&self) -> &$type {
                assert!(std::mem::align_of::<Self>() >= std::mem::align_of::<$type>());
                assert_eq!(std::mem::size_of::<Self>(), std::mem::size_of::<$type>());
                unsafe{&*(self as *const Self as *const $type)}
            }
        }
        impl AsMut<$type> for AlignedArray<align::$align, [u8;$size]> {
            fn as_mut(&mut self) -> &mut $type {
                assert!(std::mem::align_of::<Self>() >= std::mem::align_of::<$type>());
                assert_eq!(std::mem::size_of::<Self>(), std::mem::size_of::<$type>());
                unsafe{&mut *(self as *mut Self as *mut $type)}
            }
        }*/
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
