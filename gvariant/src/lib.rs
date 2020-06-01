//! A pure-rust implementation of the GVariant serialisation format intended for
//! fast reading of in-memory buffers.
//!
//! ```rust
//! # use gvariant::{aligned_bytes::copy_to_align, gv, Marker};
//! let data = copy_to_align(b"\x22\x00\x00\x00William\0");
//! let (age, name) = gv!("(is)").cast(data.as_ref()).into();
//! assert_eq!(
//!     format!("My name is {} and I am {} years old!", name, age),
//!     "My name is William and I am 34 years old!");
//! ```
//!
//! This library operates by reinterpreting byte buffers as a GVariant type. It
//! doesn't do any of its own allocations.  As a result proper alignment of byte
//! buffers is the responsibility of the user.  See [`aligned_bytes`].
//!
//! It's intended to conform to the [GVariant specification] and match the
//! behaviour of the [GLib implementation].  Exceptions to this are described in
//! ["Deviations from the Specification"](#deviations-from-the-specification)
//! below.
//!
//! This library assumes you know the types of the data you are dealing with at
//! compile time.  This is in contrast to the GLib implementation where you
//! could construct a GVariant type string dynamically.  This allows for a much
//! smaller and faster implementation, more in line with Alexander Larsson's
//! [GVariant Schema Compiler].  As a result GVariant structs are supported
//! through use of code generation via macros.  See the gvariant-macro
//! subdirectory.
//!
//! The library is intended to be sound and safe to run on untrusted input,
//! although the implementation does include use of `unsafe`.  See ["Use of
//! `unsafe`"](#use-of-unsafe) below. Help with validating the unsafe portions
//! of the library would be gratefully received.
//!
//! This library works Rust stable.  As a result we can't use const-generics,
//! which would make some of the code much more streightforward.  A future
//! version of this library may use const-generics, once they are available in
//! stable rust.
//!
//! [GLib implementation]: https://developer.gnome.org/glib/stable/glib-GVariant.html
//! [GVariant Schema Compiler]: https://gitlab.gnome.org/alexl/variant-schema-compiler/
//! [GVariant specification]: https://people.gnome.org/~desrt/gvariant-serialisation.pdf
//!
//! ## Status
//!
//! * Support for all GVariant types is implemented
//! * Behaviour is identical to GLib's implementation for all data in "normal
//!   form". This has been confirmed with fuzz testing.  There are some
//!   differences for data not in normal form.   See
//!   https://gitlab.gnome.org/GNOME/glib/-/issues/2121 for more information.
//! * Serialisation is not currently supported, but may be implemented in a
//!   future version, or possibly as a seperate crate.
//!
//! ### TODO
//!
//! * Fuzz testing of structure types - compare against the GLib version
//!
//! ## Features
//!
//! ### `std` - enabled by default
//!
//! Required for:
//!
//! * our errors to implement [`std::error::Error`]
//! * `Str`'s `to_cstr()` method
//!
//! Disable this feature for no-std support.
//!
//! ### `alloc` - enabled by default
//!
//! Required for:
//!
//! * Allocating [`AlignedSlice`]s with [`ToOwned`],
//!   [`copy_to_align`][aligned_bytes::copy_to_align] and
//!   [`alloc_aligned`][aligned_bytes::alloc_aligned].
//! * Correctly displaying non-utf-8 formatted strings
//! * The std feature
//!
//! ## Deviations from the Specification
//!
//! ### Maximum size of objects
//!
//! The spec says:
//!
//! > **2.3.6 Framing Offsets**
//! >
//! >There is no theoretical upper limit in how large a framing offset can be.
//! >This fact (along with the absence of other limitations in the serialisation
//! >format) allows for values of arbitrary size.
//!
//! In this implementation the maximum size of an object is [`usize`] (typically
//! 64-bits).  This should not be a problem in practice on 64-bit machines.
//!
//! ### Equality of Variant **v** type for non-normal form data
//!
//! See note under [`Variant`].
//!
//! ### Validation of non-normal form object path "o" and signature "g" types
//!
//! The spec says:
//!
//! > ### 2.7.3 Handling Non-Normal Serialised Data
//! >
//! > #### Invalid Object Path
//! >
//! > If the serialised form of an object path is not a valid object path
//! > followed by a zero byte then the default value is used.
//! >
//! > #### Invalid Signature
//! >
//! > If the serialised form of a signature string is not a valid DBus signature
//! > followed by a zero byte then the default value is used.
//!
//! We don't currently do any validation of the object path or signature types,
//! treating them as normal strings.
//!
//! ## Design
//!
//! The intention is to build abstractions that are transparent to the compiler,
//! such that they compile down to simple memory accesses, like reading the
//! fields of a struct.  For many of the GVariant types rust already has a type
//! with the same representation (such as `i32` for **i** or `[u8]` for **ay**).
//! For other types this library defines such types (such as
//! [`gvariant::Str`][Str] for **s** or
//! [`gvariant::NonFixedWidthArray<[i32]>`][NonFixedWidthArray] for **aai**).
//! For structure types this library provides a macro [`gv!`] to generate the
//! code for struct types.
//!
//! If we have a type with the same representation as the underlying bytes we
//! can just cast the data to the appropriate type and then read it.  The macro
//! [`gv!`] maps from GVariant typestrs to compatible Rust types returning a
//! [`Marker`].  This [`Marker`] can then be used to cast data into that type by
//! calling `Marker::cast`.
//!
//! So typically code might look like:
//!
//!     # use gvariant::{aligned_bytes::alloc_aligned, gv, Marker};
//!     # use std::io::Read;
//!     # fn a() -> std::io::Result<()> {
//!     # let mut file = std::fs::File::open("")?;
//!     let mut buf = alloc_aligned(4096);
//!     let len = file.read(&mut buf)?;
//!     let data = gv!("a(sia{sv})").cast(&buf[..len]);
//!     # todo!()
//!     # }
//!
//! For casting data to be valid and safe the byte buffer must be aligned...
//!
//! ### Use of `unsafe`
//!
//! I've tried to concentrate almost all of the unsafe in [`aligned_bytes`] and
//! [`casting`] to make it easier to review.  I also take advantage of the
//! [`ref_cast`] crate to avoid some unsafe casting that I'd otherwise require.
//!
//! A review of the use of `unsafe`, or advice on how the amount of unsafe could
//! be reduced would be greatly appreciated.
//!
//! ## Comparison to and relationship with other projects
//!
//! * [GVariant Schema Compiler] - Similar to this project the GSC generates
//!   code at compile time to represent the types the user is interested in. GSC
//!   targets the C language.  Unlike this project the types are generated from
//!   schema files, allowing structures to have named fields.  In gvariant-rs we
//!   generate our code just from the plain GVariant type strings using macros.
//!   This makes the build process simpler - there are no external tools, and it
//!   makes it easier to get started - there is no new schema format to learn.
//!   The cost is that the user is responsible for remember which field means
//!   what and what endianness should be used to interpret the data.
//!
//!   It might make sense in the future to extend GSC to generate rust code as
//!   well - in which case the generated code may depend on this library.
//! * [gtk-rs glib::variant](https://gtk-rs.org/docs/glib/variant/index.html) -
//!   This is a binding to the GLib GVariant implementation in C, so depends on
//!   glib. It's currently incomplete.  The docs say "Although `GVariant`
//!   supports arbitrarily complex types, this binding is currently limited to
//!   the basic ones: `bool`, `u8`, `i16`, `u16`, `i32`, `u32`, `i64`, `u64`,
//!   `f64` and `&str`/`String`."
//! * [zvariant](https://crates.io/crates/zvariant) - Implements the similar
//!   DBus serialisation format rather than GVariant.  Docs say: "GVariant ...
//!   will be supported by a future version of this crate."
//! * [serde_gvariant](https://github.com/lucab/serde_gvariant) - Implements the
//!   same format, but for serde integration.  Described as "WIP" and not
//!   published on crates.io

#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(feature = "alloc")]
extern crate alloc;
#[cfg(feature = "alloc")]
use alloc::string::String;

use core::{
    convert::TryInto,
    fmt::{Debug, Display},
    marker::PhantomData,
};

#[cfg(feature = "std")]
use std::ffi::CStr;

use ref_cast::RefCast;

pub mod aligned_bytes;
use offset::align_offset;

pub mod casting;
mod offset;

use aligned_bytes::{empty_aligned, AlignedSlice, AsAligned, A8};
use casting::{AlignOf, AllBitPatternsValid};

#[doc(hidden)]
pub use gvariant_macro::{define_gv as _define_gv, gv_type as _gv_type};

/// This is the return type of the `gv!` macro.
///
/// This acts as a kind of factory trait for GVariant types, creating them from
/// aligned data using the `cast` method.
///
/// Do not implement this trait yourself, `gv!` is responsible for creating the
/// marker structs that implement this.  Use that instead.
///
/// See the documentation of `gv!` for usage examples.
pub trait Marker {
    /// The typestr that was passed to the `gv!` macro.
    const TYPESTR: &'static [u8];

    /// This type has the same representation in memory as the GVariant type
    /// with the signature given by `Self::TYPESTR`, and implements `Cast` so it
    /// can be created from appropriately aligned data.
    type Type: Cast + ?Sized;

    // I'd like to remove the `&self` argument as it isn't used, but see comment
    // below in macro_rules! gv

    /// Cast `data` to the appropriate rust type `Self::Type` for the type
    /// string `Self::TYPESTR`.
    fn cast<'a>(&self, data: &'a AlignedSlice<<Self::Type as AlignOf>::AlignOf>) -> &'a Self::Type {
        Self::Type::from_aligned_slice(data)
    }

    /// Cast `data` to the appropriate rust type `Self::Type` for the type
    /// string `Self::TYPESTR`.
    fn try_cast_mut<'a>(
        data: &'a mut AlignedSlice<<Self::Type as AlignOf>::AlignOf>,
    ) -> Result<&'a mut Self::Type, casting::WrongSize> {
        Self::Type::try_from_aligned_slice_mut(data)
    }
}

/// Maps from GVariant typestrs to compatible Rust types returning a `Marker`.
/// This `Marker` can then be used to cast data into that type by calling
/// `Marker::cast`.
///
/// The signature is essentially `fn gv(typestr : &str) -> impl Marker`.
///
/// This is the main entrypoint to the library.
///
/// Given `data` that you want to interpret as a GVariant of type **as** you
/// write:
///
///     # use gvariant::{aligned_bytes::empty_aligned, gv, Marker};
///     # let data = empty_aligned();
///     gv!("as").cast(data);
///
/// Similarly if you want to interpret some data in a variant as an **as** you
/// write:
///
///     # use gvariant::{aligned_bytes::empty_aligned, gv, Marker, Variant};
///     # let v = gv!("v").cast(empty_aligned());
///     v.get(gv!("as"));
///
/// The returned marker has a automatically generated type.  `Marker::TYPESTR`
/// will equal the typestr passed into the `gv!` invocation.  `Marker::Type` is
/// a type that has the same bit representation as GVariant type passed in.
///
/// The types are mapped as follows:
///
/// | GVariant Type | Rust Type                                                                                   | Sized                             |
/// | ------------- | ------------------------------------------------------------------------------------------- | --------------------------------- |
/// | **b**         | [`Bool`]                                                                                    | Yes                               |
/// | **y**         | [`u8`]                                                                                      | Yes                               |
/// | **n**         | [`i16`]                                                                                     | Yes                               |
/// | **q**         | [`u16`]                                                                                     | Yes                               |
/// | **i**         | [`i32`]                                                                                     | Yes                               |
/// | **u**         | [`u32`]                                                                                     | Yes                               |
/// | **x**         | [`i64`]                                                                                     | Yes                               |
/// | **t**         | [`u64`]                                                                                     | Yes                               |
/// | **d**         | [`f64`]                                                                                     | Yes                               |
/// | **s**         | [`Str`]                                                                                     | No                                |
/// | **o**         | [`Str`]                                                                                     | No                                |
/// | **g**         | [`Str`]                                                                                     | No                                |
/// | **v**         | [`Variant`]                                                                                 | No                                |
/// | **m**s        | [`MaybeNonFixedSize<Str>`][MaybeNonFixedSize] - and similarly for all non-[`Sized`] types   | No                                |
/// | **m**i        | [`MaybeFixedSize<i32>`][MaybeFixedSize] - and similarly for all [`Sized`] types             | No                                |
/// | **a**s        | [`NonFixedWidthArray<Str>`][NonFixedWidthArray] - and similarly for all non-[`Sized`] types | No                                |
/// | **a**i        | `[i32]` and similarly for all [`Sized`] types                                               | No                                |
/// | **(sv)**      | Custom struct generated by this macro. Implements `.to_tuple()` method                      | Yes if all children are [`Sized`] |
/// | **{si}**      | Custom struct generated by this Macro. Implements `.to_tuple()` method                      | Yes if all children are [`Sized`] |
#[macro_export]
macro_rules! gv {
    ($typestr:literal) => {{
        use $crate::Marker;
        mod _m {
            #[macro_use]
            use $crate::aligned_bytes;
            use $crate::aligned_bytes::{
                align_offset, empty_aligned, AlignedOffset, AlignedSlice, AsAligned,
            };
            use $crate::casting::{AlignOf, AllBitPatternsValid};
            use $crate::get_child_elem;
            use $crate::{Cast, Structure, _define_gv, _gv_type};

            _define_gv!($typestr);
            pub(crate) struct Marker();
            impl $crate::Marker for Marker {
                type Type = _gv_type!($typestr);
                const TYPESTR: &'static [u8] = $typestr.as_bytes();
            }
        };
        // TODO: I'd much rather that this macro returns a type, rather than
        // a value.  That way getting a gvariant looks like:
        //
        //     let a : <gv!("as")> = v.get()?
        //     let b = <gv!("(yii)")>::cast(bytes);
        //
        // rather than:
        //
        //     let a = v.get(gv!("as"))?
        //     let b = gv!("(yii)").cast(bytes);
        //
        // The former makes it much clearer what's happening at compile time
        // and what's happening at run time.
        //
        // As it is, when I try to make this return a type I get the error
        // message
        _m::Marker()
    }};
}

/// Trait implemented by all our types that have the same representation as the
/// GVariant type
///
/// This allows casting appropriately aligned [`AlignedSlice`]s to rust types.
///
/// Don't implement this class for your own types.  It's already implemented for
/// all appropriate types.  It's automatically implemented for [`Structure`]
/// types generated by the [`gv!`] macro.
pub trait Cast:
    casting::AlignOf + casting::AllBitPatternsValid + 'static + PartialEq + Debug
{
    /// Cast `slice` to type `Self`.
    ///
    /// This always succeeds.  If the slice is the wrong size a defualt value is
    /// returned in accordance with the GVariant spec.
    fn from_aligned_slice(slice: &AlignedSlice<Self::AlignOf>) -> &Self {
        match Self::try_from_aligned_slice(slice) {
            Ok(x) => x,
            Err(_) => Self::default_ref(),
        }
    }

    /// Get a static reference to the default value for this type.
    ///
    /// In GVariant every type has a default value which is used in certian
    /// circumstances in-lieu of returning errors during deserialisation.  We're
    /// always dealing with references so [`std::default::Default`] isn't
    /// appropriate.
    fn default_ref() -> &'static Self;
    fn try_from_aligned_slice(
        slice: &AlignedSlice<Self::AlignOf>,
    ) -> Result<&Self, casting::WrongSize>;
    fn try_from_aligned_slice_mut(
        slice: &mut AlignedSlice<Self::AlignOf>,
    ) -> Result<&mut Self, casting::WrongSize>;
}

macro_rules! impl_cast_for {
    ($t:ty, $default:expr) => {
        impl Cast for $t {
            fn default_ref() -> &'static Self {
                &$default
            }
            fn try_from_aligned_slice(
                slice: &AlignedSlice<Self::AlignOf>,
            ) -> Result<&Self, casting::WrongSize> {
                casting::try_cast_slice_to::<Self>(slice)
            }
            fn try_from_aligned_slice_mut(
                slice: &mut AlignedSlice<Self::AlignOf>,
            ) -> Result<&mut Self, casting::WrongSize> {
                casting::try_cast_slice_to_mut::<Self>(slice)
            }
        }
    };
}

impl_cast_for!(Bool, Bool(0u8));
impl_cast_for!(u8, 0);
impl_cast_for!(u16, 0);
impl_cast_for!(i16, 0);
impl_cast_for!(u32, 0);
impl_cast_for!(i32, 0);
impl_cast_for!(u64, 0);
impl_cast_for!(i64, 0);
impl_cast_for!(f64, 0.);

/// Type with same representation as GVariant "s", "o" and "g" types
///
/// This is the type returned by:
///
///     # use gvariant::{aligned_bytes::empty_aligned, gv, Marker};
///     # let data = empty_aligned();
///     gv!("s").cast(data);
///
/// We can't use Rust's `str` type because, although UTF-8 is "expected and
/// encouraged" it is not guaranteed. We can't use `&[u8]` here because GVariant
/// strings always end with a NUL byte.
#[derive(RefCast, Eq)]
#[repr(transparent)]
pub struct Str {
    data: [u8],
}

impl Str {
    /// Convert `&Str` to `&[u8]`
    ///
    /// This will give the same result as `to_bytes()` for normal data, but
    /// unlike `to_bytes()` it should be 0-cost.
    ///
    /// The result of this function will deviate from the GVariant specification
    /// if the data contains embedded NULs.  The spec says:
    ///
    /// > **2.7.3 Handling Non-Normal Serialised Data**
    /// >
    /// >**String with Embedded Nul**
    /// >
    /// > If a string has a nul character as its final byte, but also contains
    /// > another nul character before this final terminator, the value of the
    /// > string is taken to be the part of the string that precedes the embedded
    /// > nul. This means that obtaining a C pointer to a string is still a
    /// > constant time operation.
    ///
    /// Instead this function will return the data with the embedded NULs intact
    /// (excluding the final NUL byte)
    pub fn to_bytes_non_conformant(&self) -> &[u8] {
        let d: &[u8] = self.data.as_ref();
        match d.last() {
            Some(b'\0') => &d[..d.len() - 1],
            _ => b"",
        }
    }
    /// Convert `&Str` to `&[u8]`
    ///
    /// To handle non-normal data we must scanning the contents of the buffer
    /// for NUL bytes.  So the performance of this function is linear with
    /// string length.  If this is unacceptable for your use-case and you know
    /// you'll be dealing with normal data use `to_bytes_non_conformant`.
    pub fn to_bytes(&self) -> &[u8] {
        match self.find_nul() {
            Some(n) => &self.data.as_ref()[..n],
            None => b"",
        }
    }
    /// Convert `&Str` to `&std::ffi::CStr`
    ///
    /// This currently requires scanning the contents of the buffer for NUL
    /// bytes. So the performance of this function is currently linear with
    /// string length.  This could be changed in the future to be 0-cost if
    /// `std::ffi::CStr::from_ptr` is changed similarly.
    #[cfg(feature = "std")]
    pub fn to_cstr(&self) -> &CStr {
        CStr::from_bytes_with_nul(match self.find_nul() {
            Some(n) => &self.data.as_ref()[..=n],
            None => b"\0",
        })
        .unwrap()
    }
    /// Convert `&Str` to `&str` assuming UTF-8 encoding
    ///
    /// The GVariant spec says that "the use of UTF-8 is expected and
    /// encouraged", but it is not guaranteed, so we return a [`Result`] here.
    pub fn to_str(&self) -> Result<&str, core::str::Utf8Error> {
        core::str::from_utf8(&self.to_bytes())
    }
    fn find_nul(&self) -> Option<usize> {
        let d: &[u8] = self.data.as_ref();
        match d.last() {
            Some(b'\0') => (),
            _ => return None,
        }
        Some(d.iter().position(|x| *x == b'\0').unwrap())
    }
}
unsafe impl AllBitPatternsValid for Str {}
unsafe impl AlignOf for Str {
    type AlignOf = aligned_bytes::A1;
}

impl Cast for Str {
    fn default_ref() -> &'static Self {
        unsafe { &*(b"" as *const [u8] as *const Str) }
    }
    fn try_from_aligned_slice(
        slice: &AlignedSlice<Self::AlignOf>,
    ) -> Result<&Self, casting::WrongSize> {
        Ok(Self::ref_cast(slice.as_ref()))
    }
    fn try_from_aligned_slice_mut(
        slice: &mut AlignedSlice<Self::AlignOf>,
    ) -> Result<&mut Self, casting::WrongSize> {
        Ok(Self::ref_cast_mut(slice.as_mut()))
    }
}

impl PartialEq for Str {
    fn eq(&self, other: &Self) -> bool {
        self.to_bytes() == other.to_bytes()
    }
}
impl PartialEq<[u8]> for Str {
    fn eq(&self, other: &[u8]) -> bool {
        self.to_bytes() == other
    }
}
impl PartialEq<Str> for [u8] {
    fn eq(&self, other: &Str) -> bool {
        self == other.to_bytes()
    }
}
impl Display for Str {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        core::fmt::Display::fmt(&DisplayUtf8Lossy(self.to_bytes()), f)
    }
}
impl Debug for Str {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        core::fmt::Debug::fmt(&DisplayUtf8Lossy(self.to_bytes()), f)
    }
}

// TODO: Replace this with core::str::lossy::Utf8Lossy if it's ever stabilised.
struct DisplayUtf8Lossy<'a>(&'a [u8]);
impl core::fmt::Display for DisplayUtf8Lossy<'_> {
    #[cfg(feature = "alloc")]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        core::fmt::Display::fmt(&String::from_utf8_lossy(self.0).as_ref(), f)
    }
    #[cfg(not(feature = "alloc"))]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        core::fmt::Display::fmt(
            match core::str::from_utf8(self.0) {
                Ok(x) => x,
                Err(_) => "<Error: Invalid Utf-8>",
            },
            f,
        )
    }
}
impl core::fmt::Debug for DisplayUtf8Lossy<'_> {
    #[cfg(feature = "alloc")]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        core::fmt::Debug::fmt(&String::from_utf8_lossy(self.0).as_ref(), f)
    }
    #[cfg(not(feature = "alloc"))]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match core::str::from_utf8(self.0) {
            Ok(x) => core::fmt::Debug::fmt(x, f),
            Err(_) => core::fmt::Display::fmt("<Error: Invalid Utf-8>", f),
        }
    }
}

/// The GVariant Variant **v** type
///
/// The Variant type can contain any GVariant value.
///
/// ### Non-spec conformant implementation of Equality with non-normal form data
///
/// While every value has a single canoncial byte representation ("normal form")
/// there other representations that have the same value.  For example: values
/// of type **(yi)** have 3B of padding between the **y** and the **i**.  In
/// normal form these bytes are 0, but they are irrelevant for the actual value.
/// Ignoring the value of the padding bytes is correct according to the spec.
///
/// This is handled correctly when comparing the values of two **(yi)**
/// instances in rust code.  What isn't correct is the handling of `Variant`
/// **v** types that contain non-normal data with respect to checking for
/// equality.  Correct checking of equality would require deserialising the data
/// according to the typestr contained within the variant, and then doing the
/// comparison.  We don't do run-time interpretation of typestrs in this crate,
/// prefering to do it at compile time.  Instead we just compare the underlying
/// data.  This gives correct results for data in normal form, but there will be
/// some false-negatives for non-normal form data.
///
/// Therefore [`Variant`] implements [`PartialEq`], but not [`Eq`] because the
/// comparison is not "reflexive".

#[derive(RefCast)]
#[repr(transparent)]
pub struct Variant(AlignedSlice<A8>);
unsafe impl AlignOf for Variant {
    type AlignOf = A8;
}
unsafe impl AllBitPatternsValid for Variant {}
impl Cast for Variant {
    fn default_ref() -> &'static Self {
        Self::ref_cast(empty_aligned())
    }
    fn try_from_aligned_slice(
        slice: &AlignedSlice<Self::AlignOf>,
    ) -> Result<&Self, casting::WrongSize> {
        Ok(Self::ref_cast(slice))
    }
    fn try_from_aligned_slice_mut(
        slice: &mut AlignedSlice<Self::AlignOf>,
    ) -> Result<&mut Self, casting::WrongSize> {
        Ok(Self::ref_cast_mut(slice))
    }
}
impl Debug for Variant {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let (gv_type, data) = self.split();
        write!(
            f,
            "Variant {{ type: {:?}, data: {:?} }}",
            DisplayUtf8Lossy(gv_type),
            data.as_ref() as &[u8]
        )
    }
}
impl Variant {
    /// Get the value from the variant, if it matches the type passed in.
    ///
    /// Example:
    ///
    ///     # use gvariant::{aligned_bytes::empty_aligned, gv, Marker, Variant};
    ///     # let v = gv!("v").cast(empty_aligned());
    ///     let a = v.get(gv!("ai"));
    ///     // a now has type &[i32]
    pub fn get<M: Marker>(&self, m: M) -> Option<&M::Type>
    where
        AlignedSlice<A8>: AsAligned<<M::Type as AlignOf>::AlignOf>,
    {
        let (typestr, data) = self.split();
        if typestr == M::TYPESTR {
            Some(m.cast(data.as_aligned()))
        } else {
            None
        }
    }
    /// Destructures the variant into (typestr, data).
    ///
    /// Note: typestr is not guaranteed to be a valid GVariant type.
    ///
    /// Example use:
    ///
    ///     # use gvariant::{aligned_bytes::{A8, copy_to_align, empty_aligned}, gv, Variant, Marker};
    ///     # let data = copy_to_align::<A8>(b"a\0(is)");
    ///     # let data = data.as_ref();
    ///     # let v = gv!("v").cast(data);
    ///     match v.split() {
    ///         (b"(is)", _) => {
    ///             let s = v.get(gv!("(is)"));
    ///             // Do something with s
    ///         }
    ///         (ty, _) => panic!("Unexpected variant type {:?}", ty)
    ///     }
    pub fn split(&self) -> (&[u8], &AlignedSlice<A8>) {
        // Variants are serialised by storing the serialised data of the child,
        // plus a zero byte, plus the type string of the child.
        let mut split_pos = None;
        for (n, c) in self.0.rchunks_exact(1).enumerate() {
            if c[0] == b'\0' {
                split_pos = Some(self.0.len() - n - 1);
                break;
            }
        }
        if let Some(mid) = split_pos {
            let (data, ty) = self.0.split_at(mid);
            (&ty[1..], data)
        } else {
            (b"()", empty_aligned())
        }
    }
}

impl PartialEq for Variant {
    /// Caveat: The current implementation has false negatives for data not in
    /// "normal form".  This may change in the future.
    fn eq(&self, other: &Self) -> bool {
        self.0.as_ref() as &[u8] == other.0.as_ref() as &[u8]
    }
}

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
//
// We implement this a normal rust slice.

impl<'a, T: Cast + 'static> Cast for [T] {
    fn default_ref() -> &'static Self {
        &[]
    }
    fn try_from_aligned_slice(
        slice: &AlignedSlice<Self::AlignOf>,
    ) -> Result<&Self, casting::WrongSize> {
        casting::cast_slice::<Self::AlignOf, T>(slice)
    }
    fn try_from_aligned_slice_mut(
        _: &mut AlignedSlice<Self::AlignOf>,
    ) -> Result<&mut Self, casting::WrongSize> {
        todo!()
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

#[doc(hidden)]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum OffsetSize {
    U0 = 0,
    U1 = 1,
    U2 = 2,
    U4 = 4,
    U8 = 8,
}

#[doc(hidden)]
pub fn offset_size(len: usize) -> OffsetSize {
    match len {
        0 => OffsetSize::U0,
        0x1..=0xFF => OffsetSize::U1,
        0x100..=0xFFFF => OffsetSize::U2,
        0x1_0000..=0xFFFF_FFFF => OffsetSize::U4,
        0x1_0000_0000..=0xFFFF_FFFF_FFFF_FFFF => OffsetSize::U8,
        _ => unreachable!(),
    }
}

#[doc(hidden)]
pub fn read_uint(data: &[u8], size: OffsetSize, n: usize) -> usize {
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
    if osz == OffsetSize::U0 {
        (OffsetSize::U1, 0)
    } else {
        let last = read_uint(&data[data.len() - osz as usize..], osz, 0);
        if last > data.len() {
            return (osz, data.len());
        }
        let size = data.len() - last;
        if size % osz as usize == 0 {
            // Normal form
            (osz, last)
        } else {
            // In the event that the final framing offset of a non-fixed-width
            // array ... indicates a non-integral number of framing offsets is
            // present in the array, the value is taken to be the empty array.
            (osz, data.len())
        }
    }
}

/// Type with same representation as GVariant "aX" type where X is any non-fixed
/// size type
///
/// This is similar to a [`slice`][std::slice], but for non-fixed width types,
/// and implements many of the same methods.  Items can be retrieved by indexing
/// or iterated over.
///
/// For fixed-width types a standard rust slice is used.
#[derive(RefCast)]
#[repr(transparent)]
pub struct NonFixedWidthArray<T: Cast + ?Sized> {
    data: AlignedSlice<T::AlignOf>,
}

impl<T: Cast + Debug + ?Sized> Debug for NonFixedWidthArray<T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "[")?;
        for child in self {
            write!(f, "{:?}, ", child)?;
        }
        write!(f, "]")
    }
}

unsafe impl<T: Cast + ?Sized> AlignOf for NonFixedWidthArray<T> {
    type AlignOf = T::AlignOf;
}
unsafe impl<T: Cast + ?Sized> AllBitPatternsValid for NonFixedWidthArray<T> {}
impl<T: Cast + ?Sized> Cast for NonFixedWidthArray<T> {
    fn default_ref() -> &'static Self {
        Self::ref_cast(empty_aligned())
    }
    fn try_from_aligned_slice(
        slice: &AlignedSlice<Self::AlignOf>,
    ) -> Result<&Self, casting::WrongSize> {
        Ok(Self::ref_cast(slice))
    }
    fn try_from_aligned_slice_mut(
        slice: &mut AlignedSlice<Self::AlignOf>,
    ) -> Result<&mut Self, casting::WrongSize> {
        Ok(Self::ref_cast_mut(slice))
    }
}

impl<T: Cast + ?Sized> NonFixedWidthArray<T> {
    /// Returns the number of elements in the array.
    pub fn len(&self) -> usize {
        // Since determining the length of the array relies on our ability
        // to count the number of framing offsets and since the number of
        // framing offsets is determined from how much space they take up,
        // zero byte framing offsets are not permitted in arrays, even in
        // the case where all other serialised data has a size of zero. This
        // special exception avoids having to divide zero by zero and wonder
        // what the answer is.
        let (osz, lfo) = read_last_frame_offset(&self.data);
        (self.data.len() - lfo) / osz as usize
    }
    /// Returns `true` if the array has a length of 0.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
    /// Returns an iterator over the array.
    pub fn iter(&self) -> NonFixedWidthArrayIterator<T> {
        self.into_iter()
    }
    /// Returns the first element of the array, or [`None`] if it is empty.
    pub fn first(&self) -> Option<&T> {
        if self.is_empty() {
            None
        } else {
            Some(&self[0])
        }
    }
    /// Returns the last element of the array, or [`None`] if it is empty.
    pub fn last(&self) -> Option<&T> {
        if self.is_empty() {
            None
        } else {
            Some(&self[self.len() - 1])
        }
    }
}

impl<T: Cast + PartialEq + ?Sized> PartialEq for NonFixedWidthArray<T> {
    fn eq(&self, other: &Self) -> bool {
        if self.len() != other.len() {
            return false;
        }
        for (a, b) in self.iter().zip(other) {
            if a != b {
                return false;
            }
        }
        true
    }
}
impl<T: Cast + PartialEq + Eq + ?Sized> Eq for NonFixedWidthArray<T> {}

/// A iterator over the items of a [`NonFixedWidthArray`]
///
/// This struct is created by the [`iter`] method on [`NonFixedWidthArray`].
/// See its documentation for more.
///
/// [`iter`]: NonFixedWidthArray::iter
pub struct NonFixedWidthArrayIterator<'a, Item: Cast + ?Sized> {
    slice: &'a NonFixedWidthArray<Item>,
    next_start: usize,
    offset_idx: usize,
    offset_size: OffsetSize,
}
impl<'a, Item: Cast + 'static + ?Sized> Iterator for NonFixedWidthArrayIterator<'a, Item> {
    type Item = &'a Item;
    fn next(&mut self) -> Option<Self::Item> {
        if self.offset_idx >= self.slice.data.len() {
            None
        } else {
            let start = align_offset::<Item::AlignOf>(self.next_start);
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
                Some(Item::try_from_aligned_slice(aligned_bytes::empty_aligned()).unwrap())
            } else {
                Some(Item::try_from_aligned_slice(&self.slice.data[..end][start..]).unwrap())
            }
        }
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        let l = match self.offset_size {
            OffsetSize::U0 => 0,
            _ => (self.slice.data.len() - self.offset_idx) / self.offset_size as usize,
        };
        (l, Some(l))
    }
}

impl<'a, Item: Cast + 'static + ?Sized> IntoIterator for &'a NonFixedWidthArray<Item> {
    type Item = &'a Item;
    type IntoIter = NonFixedWidthArrayIterator<'a, Item>;
    fn into_iter(self) -> Self::IntoIter {
        let (osz, lfo) = read_last_frame_offset(&self.data);
        NonFixedWidthArrayIterator {
            slice: self,
            next_start: 0,
            offset_idx: lfo,
            offset_size: osz,
        }
    }
}
impl<Item: Cast + 'static + ?Sized> core::ops::Index<usize> for NonFixedWidthArray<Item> {
    type Output = Item;
    fn index(&self, index: usize) -> &Self::Output {
        let (osz, lfo) = read_last_frame_offset(&self.data);
        let frame_offsets = &self.data.as_ref()[lfo..];
        let end = read_uint(frame_offsets, osz, index);
        let start = align_offset::<Item::AlignOf>(match index {
            0 => 0,
            x => read_uint(frame_offsets, osz, x - 1),
        });
        if start < self.data.len() && end < self.data.len() && start <= end {
            Item::try_from_aligned_slice(&self.data[..end][start..]).unwrap()
        } else {
            // Start or End Boundary of a Child Falls Outside the Container
            //
            // If the framing offsets (or calculations based on them) indicate
            // that any part of the byte sequence of a child value would fall
            // outside of the byte sequence of the parent then the child is given
            // the default value for its type.
            Item::try_from_aligned_slice(aligned_bytes::empty_aligned()).unwrap()
        }
    }
}

// 2.5.2 Maybes
//
// Maybes are encoded differently depending on if their element type is
// fixed-sized or not.
//
// The alignment of a maybe type is always equal to the alignment of its element
// type.

/// Type with same representation as GVariant "mX" type where X is any fixed
/// size type
///
/// This is the type returned by:
///
///     # use gvariant::{aligned_bytes::empty_aligned, gv, Marker};
///     # let data = empty_aligned();
///     gv!("mb").cast(data);
///     # let data = empty_aligned();
///     gv!("mi").cast(data);
///     gv!("m(yi)").cast(data);
///
/// Rust's built in [`Option`] doesn't have any specified byte representation so
/// we need our own type here.
///
/// Maybes are encoded differently depending on if their element type is
/// fixed-sized or not.  [`MaybeNonFixedSize`] is used when the contained size
/// is non-fixed, but it implements the same interface as this type
///
/// You probably just want to call `.to_option()` on this type.

#[repr(transparent)]
#[derive(RefCast)]
pub struct MaybeFixedSize<T: Cast> {
    marker: PhantomData<T>,
    data: AlignedSlice<T::AlignOf>,
}
impl<T: Cast + Debug> Debug for MaybeFixedSize<T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        self.to_option().fmt(f)
    }
}
impl<T: Cast> MaybeFixedSize<T> {
    /// Convert to a rust native [`Option`] type.
    ///
    /// Note: this doesn't copy the data, it returns an option to a reference to
    /// the underlying data.
    pub fn to_option(&self) -> Option<&T> {
        // 2.5.2.1 Maybe of a Fixed-Sized Element
        //
        // For the `Nothing` case, the serialised data is the empty byte
        // sequence.  For the `Just` case, the serialised data is exactly
        // equal to the serialised data of the child.  This is always
        // distinguishable from the `Nothing` case because all fixed-sized
        // values have a non-zero size.
        //
        // Wrong Size for Fixed Sized Maybe
        //
        // In the event that a maybe instance with a fixed element size
        // is not exactly equal to the size of that element, then the
        // value is taken to be `Nothing`.
        T::try_from_aligned_slice(&self.data).ok()
    }
}

impl<'a, T: Cast> From<&'a MaybeFixedSize<T>> for Option<&'a T> {
    fn from(m: &'a MaybeFixedSize<T>) -> Self {
        m.to_option()
    }
}

impl<T: Cast + PartialEq> PartialEq for MaybeFixedSize<T> {
    fn eq(&self, other: &Self) -> bool {
        self.to_option() == other.to_option()
    }
}
impl<T: Cast + Eq> Eq for MaybeFixedSize<T> {}
impl<T: Cast + PartialEq> PartialEq<Option<&T>> for &MaybeFixedSize<T> {
    fn eq(&self, other: &Option<&T>) -> bool {
        self.to_option() == *other
    }
}
impl<T: Cast + PartialEq> PartialEq<&MaybeFixedSize<T>> for Option<&T> {
    fn eq(&self, other: &&MaybeFixedSize<T>) -> bool {
        other == self
    }
}

unsafe impl<T: Cast> AlignOf for MaybeFixedSize<T> {
    type AlignOf = T::AlignOf;
}
unsafe impl<T: Cast> AllBitPatternsValid for MaybeFixedSize<T> {}

impl<T: Cast> Cast for MaybeFixedSize<T> {
    fn default_ref() -> &'static Self {
        Self::ref_cast(empty_aligned())
    }
    fn try_from_aligned_slice(
        slice: &AlignedSlice<Self::AlignOf>,
    ) -> Result<&Self, casting::WrongSize> {
        Ok(Self::ref_cast(slice))
    }
    fn try_from_aligned_slice_mut(
        slice: &mut AlignedSlice<Self::AlignOf>,
    ) -> Result<&mut Self, casting::WrongSize> {
        Ok(Self::ref_cast_mut(slice))
    }
}

/// Type with same representation as GVariant "mX" type where X is any non-fixed
/// size type
///
/// This is the type returned by:
///
///     # use gvariant::{aligned_bytes::empty_aligned, gv, Marker};
///     # let data = empty_aligned();
///     gv!("ms").cast(data);
///     # let data = empty_aligned();
///     gv!("mmi").cast(data);
///     gv!("m(ias)").cast(data);
///
/// Rust's built in [`Option`] doesn't have any specified byte representation so
/// we need our own type here.
///
/// Maybes are encoded differently depending on if their element type is
/// fixed-sized or not.  [`MaybeFixedSize`] is used when the contained size is
/// fixed, but it implements the same interface as this type.

#[derive(RefCast)]
#[repr(transparent)]
pub struct MaybeNonFixedSize<T: Cast + ?Sized> {
    marker: PhantomData<T>,
    data: AlignedSlice<T::AlignOf>,
}
impl<T: Cast + Debug + ?Sized> Debug for MaybeNonFixedSize<T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        self.to_option().fmt(f)
    }
}
impl<T: Cast + ?Sized> MaybeNonFixedSize<T> {
    /// Convert to a rust native [`Option`] type.
    ///
    /// Note: this doesn't copy the data, it returns an option to a reference to
    /// the underlying data.
    pub fn to_option(&self) -> Option<&T> {
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
            Some(T::try_from_aligned_slice(&self.data[..self.data.len() - 1]).unwrap())
        }
    }
}

unsafe impl<T: Cast + ?Sized> AlignOf for MaybeNonFixedSize<T> {
    type AlignOf = T::AlignOf;
}
unsafe impl<T: Cast + ?Sized> AllBitPatternsValid for MaybeNonFixedSize<T> {}

impl<T: Cast + ?Sized> Cast for MaybeNonFixedSize<T> {
    fn default_ref() -> &'static Self {
        Self::ref_cast(empty_aligned())
    }
    fn try_from_aligned_slice(
        slice: &AlignedSlice<Self::AlignOf>,
    ) -> Result<&Self, casting::WrongSize> {
        Ok(Self::ref_cast(slice))
    }
    fn try_from_aligned_slice_mut(
        slice: &mut AlignedSlice<Self::AlignOf>,
    ) -> Result<&mut Self, casting::WrongSize> {
        Ok(Self::ref_cast_mut(slice))
    }
}

impl<'a, T: Cast + ?Sized> From<&'a MaybeNonFixedSize<T>> for Option<&'a T> {
    fn from(m: &'a MaybeNonFixedSize<T>) -> Self {
        m.to_option()
    }
}

impl<T: Cast + PartialEq + ?Sized> PartialEq for MaybeNonFixedSize<T> {
    fn eq(&self, other: &Self) -> bool {
        self.to_option() == other.to_option()
    }
}
impl<T: Cast + Eq + ?Sized> Eq for MaybeNonFixedSize<T> {}
impl<T: Cast + PartialEq> PartialEq<Option<&T>> for MaybeNonFixedSize<T> {
    fn eq(&self, other: &Option<&T>) -> bool {
        self.to_option() == *other
    }
}
impl<T: Cast + PartialEq> PartialEq<MaybeNonFixedSize<T>> for Option<&T> {
    fn eq(&self, other: &MaybeNonFixedSize<T>) -> bool {
        other == self
    }
}

/// Type with same representation as GVariant "b" type
///
/// This is the type returned by:
///
///     # use gvariant::{aligned_bytes::AsAligned, gv, Marker};
///     gv!("b").cast(b"\0".as_aligned());
///
/// Rust's built in [`bool`] doesn't have the same representation as GVariant's,
/// so we need our own type here.  Rust's must either be `0x00` (`false`) or
/// `0x01` (`true`), while with GVariant any value in the range `0x01..=0xFF` is
/// `true`.
#[derive(RefCast, Eq)]
#[repr(transparent)]
pub struct Bool(u8);
impl Bool {
    pub fn to_bool(&self) -> bool {
        self.0 > 0
    }
}
impl Debug for Bool {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        core::fmt::Debug::fmt(&self.to_bool(), f)
    }
}
unsafe impl AllBitPatternsValid for Bool {}
unsafe impl AlignOf for Bool {
    type AlignOf = aligned_bytes::A1;
}
impl From<Bool> for bool {
    fn from(b: Bool) -> Self {
        b.to_bool()
    }
}
impl PartialEq for Bool {
    fn eq(&self, other: &Self) -> bool {
        self.to_bool() == other.to_bool()
    }
}

/// A trait that all generated structure types implement.
///
/// This exists mostly to document the interface of the generated types.  Don't
/// implement this for your own types.
///
/// All structures also implement `Into<Self::RefTuple>`.
pub trait Structure<'a>: Cast + Debug + casting::AlignOf + casting::AllBitPatternsValid {
    /// This a tuple of refs, one for each structure element
    ///
    /// For **(is)** this will be `(&'a i32, &'a Str)`.
    type RefTuple;

    /// Convert this struct to a rust tuple
    fn to_tuple(&'a self) -> Self::RefTuple;
}

#[inline]
fn nth_last_frame_offset(data: &[u8], osz: crate::OffsetSize, n: usize) -> Option<usize> {
    if n == 0 {
        Some(0)
    } else if let Some(off) = usize::checked_sub(data.len(), n * osz as usize) {
        Some(read_uint(&data[off..], osz, 0))
    } else {
        None
    }
}

#[inline]
fn calc_offsets<ChildAlign: aligned_bytes::Alignment, B: aligned_bytes::Alignment>(
    data: &[u8],
    i: isize,
    a: usize,
    c: usize,
    size: Option<usize>,
    last_child: bool,
    n_frame_offsets: usize,
) -> Option<(aligned_bytes::AlignedOffset<ChildAlign>, usize)>
where
    aligned_bytes::AlignedOffset<B>: Into<aligned_bytes::AlignedOffset<ChildAlign>>,
{
    let osz = offset_size(data.len());
    let fo = nth_last_frame_offset(data, osz, (i + 1) as usize)?;

    let start: aligned_bytes::AlignedOffset<ChildAlign> = align_offset::<B>(fo + a).into()
        | aligned_bytes::AlignedOffset::<ChildAlign>::try_new(c).unwrap();
    let end = if let Some(size) = size {
        start.to_usize() + size
    } else if last_child {
        usize::checked_sub(data.len(), osz as usize * n_frame_offsets)?
    } else {
        nth_last_frame_offset(data, osz, (i + 2) as usize)?
    };
    if start <= end && end <= data.len() {
        Some((start, end))
    } else {
        None
    }
}

/// Used for getting children of structures
///
/// `i`, `a`, `B` and `c` are described in the GVariant spec section "3.2.2
/// Computing the Table".  See gvariant_macro::generate_impl::generate_table.
///
/// This is not really public, it's only for use by the code generated by our
/// macro.
#[doc(hidden)]
#[inline]
pub fn get_child_elem<T: Cast + ?Sized, B: aligned_bytes::Alignment>(
    data: &AlignedSlice<<T as AlignOf>::AlignOf>,
    i: isize,
    a: usize,
    c: usize,
    child_size: Option<usize>,
    last_child: bool,
    n_frame_offsets: usize,
) -> &T
where
    aligned_bytes::AlignedOffset<B>: Into<aligned_bytes::AlignedOffset<T::AlignOf>>,
{
    match calc_offsets::<<T as AlignOf>::AlignOf, B>(
        data,
        i,
        a,
        c,
        child_size,
        last_child,
        n_frame_offsets,
    ) {
        Some((start, end)) => &T::from_aligned_slice(&data[..end][start..]),
        None => &T::default_ref(),
    }
}

#[cfg(test)]
#[cfg(feature = "alloc")]
mod tests {
    use super::*;
    use aligned_bytes::{copy_to_align, AlignedSlice, AsAligned, A8};

    #[test]
    fn test_numbers() {
        let data = copy_to_align(&[1, 2, 3, 4, 5, 6, 7, 8, 9]);
        let aligned_slice: &AlignedSlice<A8> = data.as_ref();

        // If the size doesn't match exactly it should default to 0:
        assert_eq!(
            *i32::from_aligned_slice(&aligned_slice[..0].as_aligned()),
            0
        );
        assert_eq!(
            *i32::from_aligned_slice(&aligned_slice[..3].as_aligned()),
            0
        );
        assert_eq!(
            *i32::from_aligned_slice(&aligned_slice[..5].as_aligned()),
            0
        );
        assert_eq!(
            *i32::from_aligned_slice(&aligned_slice[..8].as_aligned()),
            0
        );

        // Common case (Little endian):
        assert_eq!(
            Bool::from_aligned_slice(&aligned_slice[..1].as_aligned()).to_bool(),
            true
        );
        assert_eq!(
            *u8::from_aligned_slice(&aligned_slice[..1].as_aligned()),
            0x01
        );
        assert_eq!(
            *i16::from_aligned_slice(&aligned_slice[..2].as_aligned()),
            0x0201
        );
        assert_eq!(
            *u16::from_aligned_slice(&aligned_slice[..2].as_aligned()),
            0x0201
        );
        assert_eq!(
            *i32::from_aligned_slice(&aligned_slice[..4].as_aligned()),
            0x04030201
        );
        assert_eq!(
            *u32::from_aligned_slice(&aligned_slice[..4].as_aligned()),
            0x04030201
        );
        assert_eq!(
            *i64::from_aligned_slice(&aligned_slice[..8]),
            0x0807060504030201
        );
        assert_eq!(
            *u64::from_aligned_slice(&aligned_slice[..8]),
            0x0807060504030201
        );
        assert_eq!(
            *f64::from_aligned_slice(&aligned_slice[..8]),
            f64::from_bits(0x0807060504030201)
        );
    }
    #[test]
    fn test_non_fixed_width_maybe() {
        assert_eq!(
            MaybeNonFixedSize::<Str>::from_aligned_slice(b"".as_aligned()).to_option(),
            None
        );
        assert_eq!(
            MaybeNonFixedSize::<Str>::from_aligned_slice(b"\0".as_aligned())
                .to_option()
                .unwrap()
                .to_bytes(),
            b""
        );
        assert_eq!(
            MaybeNonFixedSize::<Str>::from_aligned_slice(b"hello world\0\0".as_aligned())
                .to_option()
                .unwrap()
                .to_bytes(),
            b"hello world"
        );
    }
    #[test]
    fn test_fixed_width_maybe() {
        assert_eq!(
            MaybeFixedSize::<u8>::from_aligned_slice(b"".as_aligned()),
            None
        );
        assert_eq!(
            MaybeFixedSize::<u8>::from_aligned_slice(b"\x43".as_aligned()),
            Some(&0x43)
        );
        assert_eq!(
            MaybeFixedSize::<u8>::from_aligned_slice(b"\x43\0".as_aligned()),
            None
        );
    }

    #[test]
    fn test_non_fixed_width_array() {
        let a_s = NonFixedWidthArray::<Str>::from_aligned_slice(b"".as_aligned());
        assert_eq!(a_s.len(), 0);
        assert!(a_s.is_empty());
        assert_eq!(a_s.first(), None);
        assert_eq!(a_s.last(), None);
        assert!(a_s.into_iter().collect::<Vec<_>>().is_empty());
        assert_eq!(a_s.iter().size_hint(), (0, Some(0)));

        let a_s =
            NonFixedWidthArray::<Str>::from_aligned_slice(b"hello\0world\0\x06\x0c".as_aligned());
        assert_eq!(a_s.len(), 2);
        assert_eq!(
            a_s.into_iter().map(|x| x.to_bytes()).collect::<Vec<_>>(),
            &[b"hello", b"world"]
        );
        assert_eq!(a_s[0].to_bytes(), b"hello");
        assert_eq!(a_s[1].to_bytes(), b"world");
        assert!(!a_s.is_empty());
        assert_eq!(a_s.first().unwrap(), b"hello".as_ref());
        assert_eq!(a_s.last().unwrap(), b"world".as_ref());

        let mut it = a_s.iter();
        assert_eq!(it.size_hint(), (2, Some(2)));
        it.next();
        assert_eq!(it.size_hint(), (1, Some(1)));
        it.next();
        assert_eq!(it.size_hint(), (0, Some(0)));

        // Non-normal regression test found by fuzzing:
        let nfwa = NonFixedWidthArray::<[u8]>::from_aligned_slice(b"\x08".as_aligned());
        let v = assert_array_self_consistent(nfwa);
        assert_eq!(v.as_slice(), &[] as &[&[u8]]);

        // Non-normal regression test found by fuzzing:
        let nfwa = NonFixedWidthArray::<[u8]>::from_aligned_slice(b"\x01\x00".as_aligned());
        let v = assert_array_self_consistent(nfwa);
        assert_eq!(v, [&[1u8] as &[u8], &[]]);

        // Non-normal regression test found by fuzzing. There are a non-integral
        // number of array elements indicated.  1.5 in this case:
        let mut data = [0u8; 258];
        data[256..].copy_from_slice(&255u16.to_le_bytes());
        let cow = copy_to_align(&data);
        let nfwa = NonFixedWidthArray::<[u8]>::from_aligned_slice(cow.as_ref());
        let v = assert_array_self_consistent(nfwa);
        assert_eq!(v.as_slice(), &[] as &[&[u8]]);
    }

    fn assert_array_self_consistent<T: Cast + ?Sized>(a: &NonFixedWidthArray<T>) -> Vec<&T> {
        let v: Vec<_> = a.iter().collect();
        assert_eq!(a.len(), v.len());
        for (n, elem) in v.iter().enumerate() {
            assert_eq!(**elem, a[n]);
        }
        v
    }

    #[test]
    #[should_panic]
    fn test_non_fixed_width_array_panic() {
        // Non-normal regression test found by fuzzing:
        let nfwa = NonFixedWidthArray::<[u8]>::from_aligned_slice(b"\x08".as_aligned());
        &nfwa[0];
    }

    #[test]
    fn test_spec_examples() {
        assert_eq!(
            Str::from_aligned_slice(b"hello world\0".as_aligned()).to_bytes(),
            b"hello world"
        );
        assert_eq!(
            MaybeNonFixedSize::<Str>::from_aligned_slice(b"hello world\0\0".as_aligned())
                .to_option()
                .unwrap()
                .to_bytes(),
            b"hello world"
        );
        let aob = <[Bool]>::from_aligned_slice([1u8, 0, 0, 1, 1].as_aligned());
        assert_eq!(
            aob.iter().map(|x| x.to_bool()).collect::<Vec<_>>(),
            [true, false, false, true, true]
        );

        // String Array Example
        //
        // With type 'as':
        let v: Vec<_> = NonFixedWidthArray::<Str>::from_aligned_slice(
            b"i\0can\0has\0strings?\0\x02\x06\x0a\x13".as_aligned(),
        )
        .into_iter()
        .map(|x| x.to_bytes())
        .collect();
        assert_eq!(v, [b"i".as_ref(), b"can", b"has", b"strings?"]);

        // Array of Bytes Example
        //
        // With type 'ay':
        let aob = <[u8]>::from_aligned_slice([0x04u8, 0x05, 0x06, 0x07].as_aligned());
        assert_eq!(aob, &[0x04u8, 0x05, 0x06, 0x07]);

        // Array of Integers Example
        //
        // With type 'ai':
        let data = copy_to_align(b"\x04\0\0\0\x02\x01\0\0");
        let aoi = <[i32]>::from_aligned_slice(data.as_ref());
        assert_eq!(aoi, [4, 258]);

        // Dictionary Entry Example
        //
        // With type '{si}':
        //    'a sp 'k 'e  'y \0 -- --   02 02 00 00 06has a value of {'a key', 514}
    }

    #[test]
    fn test_gvariantstr() {
        assert_eq!(Str::from_aligned_slice(b"".as_aligned()).to_bytes(), b"");
        assert_eq!(Str::from_aligned_slice(b"\0".as_aligned()).to_bytes(), b"");
        assert_eq!(
            Str::from_aligned_slice(b"hello world\0".as_aligned()).to_bytes(),
            b"hello world"
        );
        assert_eq!(
            Str::from_aligned_slice(b"hello world\0".as_aligned()),
            b"hello world".as_ref()
        );
    }

    #[test]
    fn test_variant() {
        let data = copy_to_align(b"\x04\x00\x00n");
        let v = Variant::from_aligned_slice(data.as_ref());
        match v.split() {
            (b"n", d) => assert_eq!(*i16::from_aligned_slice(d.as_aligned()), 4),
            (ty, _) => panic!("Incorrect type {:?}", ty),
        }
    }
}
