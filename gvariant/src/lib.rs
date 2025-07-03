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
//! behaviour of the reference [GLib implementation], preferring the latter
//! rather than the former where they disagree.  Exceptions to this are
//! described in ["Deviations from the Specification and reference
//! implementation"](#deviations-from-the-specification-and-reference-implementation)
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
//! [GVariant specification]: https://developer.gnome.org/documentation/specifications/gvariant-specification-1.0.html
//!
//! ## Status
//!
//! * Serialization and Deserialization is supported
//! * Support for all GVariant types is implemented
//! * Behaviour is identical to GLib's implementation for all data in "normal
//!   form". This has been confirmed with fuzz testing.  There are some
//!   differences for data not in normal form.   See [GNOME/glib#2121] for more
//!   information.
//!
//! [GNOME/glib#2121]: https://gitlab.gnome.org/GNOME/glib/-/issues/2121
//!
//! ### TODO
//!
//! * Benchmarking and performance improvements
//! * Ensure that deserialisation of non-normal structures matches GLib in all
//!   cases.
//!
//! ## Features
//!
//! ### `std` - enabled by default
//!
//! Required for:
//!
//! * our errors to implement [`std::error::Error`]
//! * [`Marker::deserialize`]
//! * Some CPU dependent string handling optimisations in the memchr crate
//! * Serialisation: although this requirement could be relaxed in the future
//!
//! Disable this feature for no-std support.
//!
//! ### `alloc` - enabled by default
//!
//! Required for:
//!
//! * Allocating [`AlignedSlice`]s with [`ToOwned`],
//!   [`copy_to_align`][aligned_bytes::copy_to_align] and
//!   [`AlignedBuf`].
//! * The convenience API `Marker::from_bytes` - use `Marker::cast` instead
//! * Correctly displaying non-utf-8 formatted strings
//! * Copying unsized GVariant objects with `to_owned()`
//! * The std feature
//!
//! ## Deviations from the Specification and reference implementation
//!
//! This implementation is intended to conform to the [GVariant specification]
//! and match the behaviour of the reference [GLib implementation], preferring
//! the latter rather than the former where they disagree.
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
//! ### Data that overlaps framing offsets (non-normal form)
//!
//! This applies to arrays of non-fixed size type in non-normal form and to
//! structures in non-normal form.  We follow the behaviour of GLib reference
//! implementation rather than the GVariant spec in this instance.
//!
//! The spec says:
//!
//! > #### Child Values Overlapping Framing Offsets
//! >
//! > If the byte sequence of a child value overlaps the framing offsets of the
//! > container it resides within then this error is ignored. The child is given
//! > a value that corresponds to the normal deserialisation process performed
//! > on this byte sequence (including the bytes from the framing offsets) with
//! > the type of the child.
//!
//! Whereas we give the child value the default value for the type consistent
//! with the GLib implementation.  This is the behaviour in GLib since 2.60,
//! 2.58.2 and 2.56.4.
//!
//! There are still some differences to GLib in the way we handle non-normal,
//! non-fixed size structures.  These will be fixed.
//!
//! See [GNOME/glib#2121] for more information.
//!
//! ### Handling of non-normal form strings
//!
//! We are consistent with the GLib implementation in this regard rather than
//! the spec. See the note under [`Str::to_str`]
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
//!     # use gvariant::{aligned_bytes::AlignedBuf, gv, Marker};
//!     # use std::io::Read;
//!     # fn a() -> std::io::Result<()> {
//!     # let mut file = std::fs::File::open("")?;
//!     let mut buf = vec![];
//!     file.read_to_end(&mut buf)?;
//!     let mut buf : AlignedBuf = buf.into();
//!     let data = gv!("a(sia{sv})").cast(&buf);
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
//!
//! # Release Notes
//!
//! ## 0.5.0
//!
//! ### Breaking changes
//!
//! * The owned equivalent of [Str] is [GString].  In 0.4 it was [Box<Str>].
//!   This affects the return type of `gv!("s").from_bytes(...)`.
//! * [Owned<T>] replaces [Box<T>] as the owned equivalent of [Variant],
//!   [NonFixedWidthArray], [MaybeFixedSize], [MaybeNonFixedSize] and the
//!   macro-generated GVariant structs that implement the [Structure] trait.
//! * Removed `aligned_bytes::read_to_slice` in favour of using [AlignedBuf].
//!   [AlignedBuf] is more convenient as it interoperates with [Vec<u8>].  So
//!   instead of writing:
//!
//!   ```compile_fail
//!   use gvariant::aligned_bytes::{AlignedSlice, read_to_slice, A8};
//!   let b : Box<AlignedSlice<A8>> = read_to_slice(file)?;
//!   ```
//!
//!   you write:
//!
//!   ```
//!   use gvariant::aligned_bytes::AlignedBuf;
//!   use std::io::Read;
//!   # fn main() -> std::io::Result<()> {
//!   #     let mut file : &[u8] = b"";
//!         let mut v = vec![];
//!         file.read_to_end(&mut v)?;
//!         let b : AlignedBuf = v.into();
//!   #     Ok(())
//!   # }
//!   ```
//! * Removed `aligned_bytes::alloc_aligned`.  Use [AlignedBuf] instead.
//!   According to miri `alloc_aligned` was unsound.  [AlignedBuf] is sound and
//!   more convenient to use.
//!
//! ### New features
//!
//! * New struct [GString] introduced.  It replaces [Box<Str>] as the owned
//!   equivalent of [Str].  Unlike [Box<Str>] it can be extended and written
//!   to in-place.
//! * New struct [AlignedBuf] introduced.  This is to [AlignedSlice] as
//!   [Vec<u8>] is to [[u8]].  There is cheap conversion to/from [Vec<u8>]
//!   which will make it much easier to integrate with the broader Rust
//!   ecosystem - including reading from files, async, etc.
//! * New struct [Owned<T>] introduced.  It replaces [Box<T>] as the owned
//!   eqivalent of most of our unsized types.  Namely: [Variant],
//!   [NonFixedWidthArray], [MaybeFixedSize], [MaybeNonFixedSize] and the
//!   macro-generated GVariant structs that implement the [Structure] trait.

#![allow(clippy::manual_map)]
#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(feature = "alloc")]
extern crate alloc;
#[cfg(feature = "alloc")]
use alloc::{borrow::ToOwned, string::String};

use core::{
    borrow::Borrow,
    convert::TryInto,
    fmt::{Debug, Display},
    hash::Hash,
    marker::PhantomData,
    ops::Deref,
};

#[cfg(feature = "std")]
use std::io::Write;

use ref_cast::RefCast;

pub mod aligned_bytes;
use offset::align_offset;

pub mod casting;
mod offset;

#[cfg(feature = "alloc")]
pub(crate) mod buf;

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
pub trait Marker: Copy {
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
    ///
    /// Use this in preference to `deserialize` if you already have the data in
    /// (properly aligned) memory.  This makes no allocations and has no
    /// dependency on `std` or `alloc`.
    ///
    /// Example
    ///
    ///     # use gvariant::{gv, Marker, Structure};
    ///     # let aligned_data = gvariant::aligned_bytes::empty_aligned();
    ///     let (my_int, my_str) = gv!("(ias)").cast(aligned_data).to_tuple();
    fn cast<'a>(&self, data: &'a AlignedSlice<<Self::Type as AlignOf>::AlignOf>) -> &'a Self::Type {
        Self::Type::from_aligned_slice(data)
    }

    /// Cast `data` to the appropriate rust type `Self::Type` for the type
    /// string `Self::TYPESTR`.
    fn try_cast_mut(
        data: &mut AlignedSlice<<Self::Type as AlignOf>::AlignOf>,
    ) -> Result<&mut Self::Type, casting::WrongSize> {
        Self::Type::try_from_aligned_slice_mut(data)
    }

    /// Read the data from r returning an owned deserialised GVariant object
    ///
    /// Example
    ///
    ///     # use gvariant::{gv, Marker};
    ///     # fn moo(myfile: &str) -> std::io::Result<()> {
    ///     # let myfile = "";
    ///     let v = gv!("s").deserialize(std::fs::File::open(myfile)?)?;
    ///     assert_eq!(&*v, "An example string");
    ///     # Ok(())
    ///     # }
    ///
    /// This requires the features std and alloc be enabled on the gvariant
    /// crate.
    #[cfg(feature = "std")]
    fn deserialize(
        &self,
        mut r: impl std::io::Read,
    ) -> std::io::Result<<Self::Type as ToOwned>::Owned> {
        let mut buf = vec![];
        r.read_to_end(&mut buf)?;
        let buf: buf::AlignedBuf = buf.into();
        Ok(self.cast(buf.as_aligned()).to_owned())
    }

    /// Deserialise the given `data`, making a copy in the process.
    ///
    /// This is a convenience API wrapper around `copy_to_align` and `cast`
    /// allowing users to not have to think about the alignment of their data.
    /// It is usually better to ensure the data you have is aligned, for example
    /// using `copy_to_align` or `AlignedBuf`, and then use `cast` directly.
    /// This way you can avoid additional allocations, avoid additional copying,
    /// and work in noalloc contexts.
    ///
    /// Example
    ///
    ///     # use gvariant::{gv, Marker};
    ///     let v = gv!("s").from_bytes(b"An example string\0");
    ///     assert_eq!(&*v, "An example string");
    #[allow(clippy::wrong_self_convention)]
    #[cfg(feature = "alloc")]
    fn from_bytes(&self, data: impl AsRef<[u8]>) -> <Self::Type as ToOwned>::Owned {
        let cow = aligned_bytes::copy_to_align(data.as_ref());
        self.cast(cow.as_ref()).to_owned()
    }

    /// Serialize the data to the given stream as a GVariant
    ///
    /// To be serialized as a GVariant the passed type must implement
    /// `SerializeTo<>` for the relevant GVariant type.
    ///
    /// Example
    ///
    ///     # use gvariant::{gv, Marker};
    ///     # fn m(mut myfile: impl std::io::Write) -> std::io::Result<()> {
    ///     let comment = Some("It's great!");
    ///     gv!("ms").serialize(&comment, &mut myfile)?;
    ///     # Ok(())
    ///     # }
    ///
    /// For information on how to serialize to the variant type see [VariantWrap].
    #[cfg(feature = "std")]
    fn serialize(
        &self,
        data: impl SerializeTo<Self::Type>,
        out: &mut impl Write,
    ) -> std::io::Result<usize> {
        data.serialize(out)
    }

    /// Convenience method for in-memory serialization
    ///
    /// Used by our tests.  You probably want to use the more flexible
    /// `serialize` instead which can be used to write to files/sockets.
    #[cfg(feature = "std")]
    fn serialize_to_vec(&self, data: impl SerializeTo<Self::Type>) -> Vec<u8> {
        let mut out = vec![];
        self.serialize(data, &mut out)
            .expect("Serialization to Vec should be infallible");
        out
    }
}

/// Trait to enable Serialization to GVariant
///
/// T in this instance is the type to be serialised to.  For example: for
/// GVariant type "ai" this will be `[i32]`.  This means that there is a
/// different SerializeTo trait for every GVariant type, and any rust type may
/// implement serialization to any GVariant type.  For example `&[u8]` can be
/// serialized as a GVariant string "s" with `SerializeTo<Str>` or as a byte
/// array with `SerializeTo<[u8]>`.
///
/// `SerializeTo<>` is implemented for appropriate built-in types, and you may
/// wish to implement it for your own types as well.
pub trait SerializeTo<T: Cast + ?Sized> {
    fn serialize(self, f: &mut impl Write) -> std::io::Result<usize>;
}

/// Owned version of unsized types [Variant], [NonFixedWidthArray],
/// [MaybeFixedSize], [MaybeNonFixedSize] and the macro-generated GVariant
/// structs that implement the [Structure] trait.
///
/// Much like [Box<T>], [Owned<T>] dereferences to `T`.
///
/// Can be converted to/from a [Vec<u8>] for free[^1].
///
/// [^1]: See notes in [AlignedBuf].
pub struct Owned<T: Cast + ?Sized> {
    data: buf::AlignedBuf,
    ty: PhantomData<T>,
}

impl<T: Cast + ?Sized> Debug for Owned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.deref().fmt(f)
    }
}

impl<T: Cast + ?Sized> Display for Owned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.deref().fmt(f)
    }
}

impl<T: Cast + ?Sized> Owned<T> {
    /// Copies the bytes
    pub fn from_bytes(s: &[u8]) -> Self {
        Self {
            data: s.to_owned().into(),
            ty: PhantomData::<T> {},
        }
    }
    pub fn from_vec(data: Vec<u8>) -> Self {
        Self {
            data: data.into(),
            ty: PhantomData::<T> {},
        }
    }
}

impl<T: Cast + ?Sized> Borrow<T> for Owned<T> {
    fn borrow(&self) -> &T {
        &*self
    }
}

impl<T: Cast + ?Sized> Deref for Owned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        T::from_aligned_slice(self.data.as_aligned())
    }
}

impl<T: ?Sized + Cast> From<Vec<u8>> for Owned<T> {
    fn from(data: Vec<u8>) -> Self {
        Self::from_vec(data)
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
        #[allow(unused_imports)]
        mod _m {
            use $crate::aligned_bytes::{
                align_offset, empty_aligned, AlignedOffset, AlignedSlice, AsAligned, A1, A2, A4, A8,
            };
            use $crate::casting::{AlignOf, AllBitPatternsValid};
            use $crate::*;

            _define_gv!($typestr);
            #[derive(Copy, Clone)]
            pub(crate) struct Marker();
            impl $crate::Marker for Marker {
                type Type = _gv_type!($typestr);
                #[allow(clippy::string_lit_as_bytes)]
                const TYPESTR: &'static [u8] = $typestr.as_bytes();
            }
        }
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

// Not really pub, just for use by the code generated by our macro.
#[doc(hidden)]
pub fn write_padding<A: aligned_bytes::Alignment, W: Write>(
    offset: usize,
    f: &mut W,
) -> std::io::Result<usize> {
    let len = align_offset::<A>(offset).to_usize() - offset;
    f.write_all(&b"\0\0\0\0\0\0\0"[..len])?;
    Ok(len)
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
    casting::AlignOf + casting::AllBitPatternsValid + 'static + PartialEq + Debug + ToOwned
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
        impl SerializeTo<$t> for $t {
            fn serialize(self, f: &mut impl std::io::Write) -> std::io::Result<usize> {
                f.write_all(self.to_ne_bytes().as_ref())?;
                Ok(std::mem::size_of::<$t>())
            }
        }
        impl SerializeTo<$t> for &$t {
            fn serialize(self, f: &mut impl std::io::Write) -> std::io::Result<usize> {
                (*self).serialize(f)
            }
        }
    };
}

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
/// We can't use Rust's `str` type here because GVariant strings always end with
/// a NUL byte.
#[derive(RefCast, Eq)]
#[repr(transparent)]
pub struct Str {
    data: [u8],
}

impl Str {
    /// Convert `&Str` to `&[u8]`
    ///
    /// This will give the same result as `s.to_str().as_bytes()` for normal
    /// data, but unlike using `to_str()` it should be 0-cost as it doesn't
    /// require scanning the underlying data.
    ///
    /// The result of this function will deviate from the GLib GVariant
    /// implementation if the data contains embedded NULs or non-utf8 data.  The
    /// spec says:
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
    pub fn as_bytes_non_conformant(&self) -> &[u8] {
        let d: &[u8] = self.data.as_ref();
        match d.last() {
            Some(b'\0') => &d[..d.len() - 1],
            _ => b"",
        }
    }
    /// Convert `&Str` to `&str`
    ///
    /// For consistency with the GLib implementation this will return a empty
    /// string if the underlying data is not utf-8 encoded or contains embedded
    /// NULs.  This differs from the wording of the GVariant specification which
    /// says that "the use of UTF-8 is expected and encouraged", but it is not
    /// guaranteed.
    ///
    /// This function executes in linear time with the length of the data.  If
    /// you know that your data is in normal form you can use
    /// `self.to_bytes_non_conformant()` instead which executes in constant
    /// time.
    pub fn to_str(&self) -> &str {
        let b = self.as_bytes_non_conformant();
        if memchr::memchr(b'\0', b).is_some() {
            ""
        } else {
            match core::str::from_utf8(self.as_bytes_non_conformant()) {
                Ok(x) => x,
                Err(_) => "",
            }
        }
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
impl SerializeTo<Str> for &Str {
    fn serialize(self, f: &mut impl Write) -> std::io::Result<usize> {
        let b = self.to_str().as_bytes();
        f.write_all(b)?;
        f.write_all(b"\0")?;
        Ok(b.len() + 1)
    }
}
impl SerializeTo<Str> for &str {
    fn serialize(self, f: &mut impl Write) -> std::io::Result<usize> {
        let b = self.as_bytes();
        if memchr::memchr(b'\0', b).is_some() {
            // GVariant can't represent strings with embedded NULs.  We don't
            // want to silently encode something that won't round-trip, so fail
            // here:
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                "Strings may not contain NULs",
            ));
        }
        f.write_all(self.as_bytes())?;
        f.write_all(b"\0")?;
        Ok(self.len() + 1)
    }
}
impl<T: SerializeTo<Str> + Copy> SerializeTo<Str> for &T {
    fn serialize(self, f: &mut impl Write) -> std::io::Result<usize> {
        (*self).serialize(f)
    }
}
impl SerializeTo<Str> for &Box<Str> {
    fn serialize(self, f: &mut impl Write) -> std::io::Result<usize> {
        self.to_str().serialize(f)
    }
}
impl SerializeTo<Str> for &String {
    fn serialize(self, f: &mut impl Write) -> std::io::Result<usize> {
        self.as_str().serialize(f)
    }
}
impl PartialEq for Str {
    fn eq(&self, other: &Self) -> bool {
        self.as_bytes_non_conformant() == other.as_bytes_non_conformant()
            || self.to_str() == other.to_str()
    }
}
impl PartialEq<Str> for str {
    fn eq(&self, other: &Str) -> bool {
        self == other.to_str()
    }
}
impl PartialEq<str> for Str {
    fn eq(&self, other: &str) -> bool {
        self.to_str() == other
    }
}
impl Display for Str {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        core::fmt::Display::fmt(self.to_str(), f)
    }
}
impl Debug for Str {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        core::fmt::Debug::fmt(self.to_str(), f)
    }
}
impl<'a> From<&'a Str> for &'a str {
    fn from(x: &'a Str) -> Self {
        x.to_str()
    }
}
#[cfg(feature = "alloc")]
impl From<&Str> for String {
    fn from(x: &Str) -> Self {
        x.to_str().into()
    }
}
#[cfg(feature = "alloc")]
impl ToOwned for Str {
    type Owned = GString;
    fn to_owned(&self) -> Self::Owned {
        GString::from_str_unchecked(self.to_str())
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

#[cfg(feature = "alloc")]
mod gstring {
    use super::*;

    use core::{borrow::Borrow, convert::TryFrom, ops::Deref};
    #[cfg(feature = "std")]
    use std::ffi::CStr;

    /// Owned version of `Str`
    ///
    /// Invariants:
    ///
    /// * must be UTF-8 encoded (much like [std::string::String])
    /// * must not contain embedded NUL bytes (much like [std::ffi::CString])
    ///
    /// Requirements:
    ///
    /// * Must be convertable to Str without mutation - so internally it is
    ///   stored with a terminating NUL byte.
    ///
    /// ```
    /// use gvariant::{gv, GString, Marker};
    /// use core::fmt::Write;
    /// let mut s = GString::new();
    /// write!(s, "I love the number {}, it's the best", 5);
    /// assert_eq!(s.as_str(), "I love the number 5, it's the best");
    ///
    /// let s: GString = gv!("s").from_bytes("Bloo blah\0");
    /// assert_eq!(s.as_str(), "Bloo blah");
    /// ```
    ///
    /// New in 0.5.0
    #[derive(PartialEq, Eq, PartialOrd, Ord, Clone)]
    pub struct GString {
        // Invariant: data contains a single embedded NUL byte at the end and no
        // other NUL bytes anywhere else
        data: String,
    }
    impl Default for GString {
        fn default() -> Self {
            Self::new()
        }
    }
    impl GString {
        /// Extracts a string slice containing the entire string.  Doesn't include
        /// the trailing NUL byte.
        pub fn as_str(&self) -> &str {
            self.as_ref()
        }
        /// Returns this [String]’s capacity, in bytes.
        pub fn capacity(&self) -> usize {
            self.data.capacity() - 1
        }
        /// Ensures that this string’s capacity is at least `additional` bytes
        /// larger than its length.
        ///
        /// See [String::reserve]
        pub fn reserve(&mut self, additional: usize) {
            self.data.reserve(additional)
        }
        /// Truncates this string, removing all contents.
        ///
        /// See [String::clear]
        pub fn clear(&mut self) {
            self.data.clear();
            self.data.push('\0');
        }
        /// Returns the length of this String, in bytes not including terminating
        /// NUL.
        ///
        /// See [String::len].
        pub fn len(&self) -> usize {
            self.data.len() - 1
        }
        pub fn is_empty(&self) -> bool {
            self.len() == 0
        }
        /// Creates a new empty string.
        pub fn new() -> Self {
            Self {
                data: "\0".to_owned(),
            }
        }
        /// Creates a new empty String with a particular capacity.
        ///
        /// See [String::with_capacity].
        pub fn with_capacity(capacity: usize) -> Self {
            let mut data = String::with_capacity(capacity + 1);
            data.push('\0');
            Self {
                data: "\0".to_owned(),
            }
        }
        /// Appends a given string slice onto the end of this string.
        ///
        /// Returns error if the passed string contains a NUL byte.
        pub fn try_push_str(&mut self, s: &str) -> Result<(), ContainsNulBytesError> {
            if memchr::memchr(b'\0', s.as_bytes()).is_some() {
                Err(ContainsNulBytesError())
            } else {
                self.data.reserve(s.len());
                self.data.pop();
                self.data += s;
                self.data.push('\0');
                Ok(())
            }
        }
        pub(crate) fn from_str_unchecked(s: &str) -> Self {
            let mut data = String::with_capacity(s.len() + 1);
            data.push_str(s);
            data.push('\0');
            Self { data }
        }
    }
    impl Display for GString {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let s: &str = self.as_ref();
            Display::fmt(s, f)
        }
    }
    impl Debug for GString {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let s: &str = self.as_ref();
            Debug::fmt(s, f)
        }
    }
    impl Borrow<Str> for GString {
        fn borrow(&self) -> &Str {
            self.deref()
        }
    }
    impl Deref for GString {
        type Target = Str;

        fn deref(&self) -> &Self::Target {
            Str::try_from_aligned_slice(self.data.as_bytes().as_aligned()).unwrap()
        }
    }
    impl core::fmt::Write for GString {
        fn write_str(&mut self, s: &str) -> core::fmt::Result {
            self.try_push_str(s).map_err(|_| core::fmt::Error)
        }
    }

    pub struct ContainsNulBytesError();

    impl TryFrom<String> for GString {
        type Error = ContainsNulBytesError;

        fn try_from(mut value: String) -> Result<Self, Self::Error> {
            if memchr::memchr(b'\0', value.as_bytes()).is_some() {
                Err(ContainsNulBytesError())
            } else {
                value.push('\0');
                Ok(GString { data: value })
            }
        }
    }
    impl From<GString> for String {
        fn from(mut s: GString) -> Self {
            // Remove trailing NUL:
            s.data.pop();
            s.data
        }
    }
    #[cfg(feature = "std")]
    impl From<GString> for std::ffi::CString {
        fn from(s: GString) -> Self {
            // Unwrap is ok: We don't contain embedded NUL bytes
            Self::from_vec_with_nul(s.data.into()).unwrap()
        }
    }
    #[cfg(feature = "std")]
    impl AsRef<CStr> for GString {
        fn as_ref(&self) -> &CStr {
            CStr::from_bytes_with_nul(self.data.as_bytes()).unwrap()
        }
    }
    impl AsRef<str> for GString {
        fn as_ref(&self) -> &str {
            &self.data[..self.data.len() - 1]
        }
    }
}
#[cfg(feature = "alloc")]
pub use gstring::{ContainsNulBytesError, GString};

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
impl SerializeTo<Variant> for &Variant {
    fn serialize(self, f: &mut impl Write) -> std::io::Result<usize> {
        // Our ability to normalise variants is limited because we only deal
        // with typestrs at compile time and this could be anything.
        let (typestr, data) = self.split();
        f.write_all(data)?;
        f.write_all(b"\0")?;
        f.write_all(typestr)?;
        Ok(data.len() + typestr.len() + 1)
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
#[cfg(feature = "alloc")]
impl ToOwned for Variant {
    type Owned = Owned<Self>;
    fn to_owned(&self) -> Self::Owned {
        Owned::from_bytes(&*self.0)
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
        let split_pos = memchr::memrchr(b'\0', &self.0);
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
        self.split() == other.split()
    }
}

/// Mark a value to be serialised as type **v**
///
/// GVariant has a variant type which can contain a value of any GVariant type.
/// This can be used to implment enums.  This is a wrapper type that helps
/// serialising to those types.
///
/// For example:  Instead of serialising this as type **i**:
///
///     use gvariant::{gv, Marker, VariantWrap};
///     let x: i32 = 5;
///     let serialized_i = gv!("i").serialize_to_vec(x);
///
/// This code will serialised to a value of type **v** that contains a value of
/// type **i**:
///
///     # use gvariant::{gv, Marker, VariantWrap};
///     # let x: i32 = 5;
///     let serialized_vi = gv!("v").serialize_to_vec(VariantWrap(gv!("i"), x));
///
/// Similarly you can wrap an **i** in a **v** in another **v**:
///
///     # use gvariant::{gv, Marker, VariantWrap};
///     # let x: i32 = 5;
///     let serialized_vvi = gv!("v").serialize_to_vec(
///         VariantWrap(gv!("v"), VariantWrap(gv!("i"), x)));
///
/// Typically you'd represent rust enums as GVariant variants.  The best way to
/// serialize enums as variants is to implement `SerializeTo` for the enum.
/// Example:
///
///     # use gvariant::{gv, Marker, SerializeTo, VariantWrap};
///     enum MyEnum {
///         Bool(bool),
///         String(String),
///     }
///     impl SerializeTo<gvariant::Variant> for &MyEnum {
///         fn serialize(self, f: &mut impl std::io::Write) -> std::io::Result<usize> {
///             match self {
///                 MyEnum::Bool(x) => VariantWrap(gv!("b"), x).serialize(f),
///                 MyEnum::String(x) => VariantWrap(gv!("s"), x).serialize(f),
///             }
///         }
///     }
///
/// A common type type seen in the wild is the "bag of properties" **a{sv}**.
#[derive(Debug, Copy, Clone)]
pub struct VariantWrap<M: Marker, T: SerializeTo<M::Type>>(pub M, pub T);
impl<M: Marker, T: SerializeTo<M::Type>> SerializeTo<Variant> for VariantWrap<M, T> {
    fn serialize(self, f: &mut impl Write) -> std::io::Result<usize> {
        let len = self.0.serialize(self.1, f)?;
        f.write_all(b"\0")?;
        f.write_all(M::TYPESTR)?;
        Ok(len + 1 + M::TYPESTR.len())
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

impl<'a, T: Cast + 'static + Copy> Cast for [T] {
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

impl<GvT: Cast + Copy, It: IntoIterator> SerializeTo<[GvT]> for It
where
    It::Item: SerializeTo<GvT>,
{
    fn serialize(self, f: &mut impl Write) -> std::io::Result<usize> {
        let mut bytes_written = 0;
        for x in self.into_iter() {
            bytes_written += x.serialize(f)?;
        }
        Ok(bytes_written)
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
#[cfg(feature = "alloc")]
impl<T: Cast + ?Sized> ToOwned for NonFixedWidthArray<T> {
    type Owned = Owned<Self>;
    fn to_owned(&self) -> Self::Owned {
        Self::Owned::from_bytes(&*self.data)
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
    data: &'a AlignedSlice<Item::AlignOf>,
    offsets: &'a [u8],

    next_start: usize,
    offset_idx: usize,
    offset_size: OffsetSize,
}
impl<Item: Cast + ?Sized + PartialEq<T>, T: ?Sized> PartialEq<[&T]> for NonFixedWidthArray<Item> {
    fn eq(&self, other: &[&T]) -> bool {
        if self.len() != other.len() {
            return false;
        }
        for (a, b) in self.iter().zip(other.iter()) {
            if a != *b {
                return false;
            }
        }
        true
    }
}
impl<Item: Cast + ?Sized + PartialEq<T>, T: ?Sized> PartialEq<NonFixedWidthArray<Item>> for [&T] {
    fn eq(&self, other: &NonFixedWidthArray<Item>) -> bool {
        other == self
    }
}
impl<'a, Item: Cast + 'static + ?Sized> Iterator for NonFixedWidthArrayIterator<'a, Item> {
    type Item = &'a Item;
    fn next(&mut self) -> Option<Self::Item> {
        if self.offset_idx >= self.offsets.len() {
            None
        } else {
            let start = align_offset::<Item::AlignOf>(self.next_start);
            let end = read_uint(&self.offsets[self.offset_idx..], self.offset_size, 0);
            self.offset_idx += self.offset_size as usize;
            self.next_start = end;
            if end < start || end > self.data.len() {
                // If the framing offsets (or calculations based on them)
                // indicate that any part of the byte sequence of a child value
                // would fall outside of the byte sequence of the parent then
                // the child is given the default value for its type.
                Some(Item::try_from_aligned_slice(aligned_bytes::empty_aligned()).unwrap())
            } else {
                Some(Item::try_from_aligned_slice(&self.data[..end][start..]).unwrap())
            }
        }
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        let l = match self.offset_size {
            OffsetSize::U0 => 0,
            _ => (self.offsets.len() - self.offset_idx) / self.offset_size as usize,
        };
        (l, Some(l))
    }
}

impl<'a, Item: Cast + ?Sized> ExactSizeIterator for NonFixedWidthArrayIterator<'a, Item> {}

impl<'a, Item: Cast + 'static + ?Sized> IntoIterator for &'a NonFixedWidthArray<Item> {
    type Item = &'a Item;
    type IntoIter = NonFixedWidthArrayIterator<'a, Item>;
    fn into_iter(self) -> Self::IntoIter {
        let (osz, lfo) = read_last_frame_offset(&self.data);
        let (data, offsets) = self.data.split_at(lfo);
        NonFixedWidthArrayIterator {
            data,
            offsets,
            next_start: 0,
            offset_idx: 0,
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
        if start < self.data.len() && end <= lfo && start <= end {
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

impl<GvT: Cast + ?Sized, It: IntoIterator> SerializeTo<NonFixedWidthArray<GvT>> for It
where
    It::Item: SerializeTo<GvT>,
{
    fn serialize(self, f: &mut impl Write) -> std::io::Result<usize> {
        let mut bytes_written = 0;
        let mut offsets = vec![];
        for x in self.into_iter() {
            let padding = align_offset::<GvT::AlignOf>(bytes_written).to_usize() - bytes_written;
            f.write_all(&b"\0\0\0\0\0\0\0"[..padding])?;
            bytes_written += padding;
            bytes_written += x.serialize(f)?;
            offsets.push(bytes_written);
        }
        write_offsets(bytes_written, offsets.as_ref(), f)
    }
}

// Not really pub, just for use by the code generated by our macro.
#[doc(hidden)]
pub fn write_offsets(
    mut bytes_written: usize,
    offsets: &[usize],
    f: &mut impl Write,
) -> std::io::Result<usize> {
    if bytes_written + offsets.len() <= 0xff {
        bytes_written += offsets.len();
        for offset in offsets {
            f.write_all((*offset as u8).to_le_bytes().as_ref())?;
        }
    } else if bytes_written + offsets.len() * 2 <= 0xffff {
        bytes_written += offsets.len() * 2;
        for offset in offsets {
            f.write_all((*offset as u16).to_le_bytes().as_ref())?;
        }
    } else if bytes_written + offsets.len() * 4 <= 0xffff_ffff {
        bytes_written += offsets.len() * 4;
        for offset in offsets {
            f.write_all((*offset as u32).to_le_bytes().as_ref())?;
        }
    } else {
        bytes_written += offsets.len() * 8;
        for offset in offsets {
            f.write_all((*offset as u64).to_le_bytes().as_ref())?;
        }
    }
    Ok(bytes_written)
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

#[cfg(feature = "alloc")]
impl<T: Cast> ToOwned for MaybeFixedSize<T> {
    // I'd like this to be Option<T>, but ToOwned requires Owned to be
    // Borrow<Self>, and we can't implement this because while we're guaranteed
    // that `T::AlignOf >= mem::align_of<T>`, the inverse is not true.  We could
    // make that guarantee, but then this wouldn't be valid on architectures
    // where i64 is 32-bit aligned for example.
    type Owned = Owned<Self>;
    fn to_owned(&self) -> Self::Owned {
        Owned::from_bytes(&*self.data)
    }
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
impl<T: Cast + PartialOrd> PartialOrd for MaybeFixedSize<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.to_option().partial_cmp(&other.to_option())
    }
}
impl<T: Cast + Hash> Hash for MaybeFixedSize<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.to_option().hash(state)
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

impl<'a, T: Cast> IntoIterator for &'a MaybeFixedSize<T> {
    type Item = &'a T;
    type IntoIter = core::option::IntoIter<&'a T>;
    fn into_iter(self) -> Self::IntoIter {
        self.to_option().into_iter()
    }
}

impl<'a, GvT: Cast + SerializeTo<GvT>> SerializeTo<MaybeFixedSize<GvT>> for &'a MaybeFixedSize<GvT>
where
    &'a GvT: SerializeTo<GvT>,
{
    fn serialize(self, f: &mut impl Write) -> std::io::Result<usize> {
        SerializeTo::<MaybeFixedSize<GvT>>::serialize(&self.to_option(), f)
    }
}

impl<GvT: Cast, T: SerializeTo<GvT> + Copy> SerializeTo<MaybeFixedSize<GvT>> for &Option<T>
where
    T: SerializeTo<GvT>,
{
    fn serialize(self, f: &mut impl Write) -> std::io::Result<usize> {
        if let Some(x) = self {
            x.serialize(f)
        } else {
            Ok(0)
        }
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
#[cfg(feature = "alloc")]
impl<T: Cast + ?Sized> ToOwned for MaybeNonFixedSize<T> {
    type Owned = Owned<Self>;
    fn to_owned(&self) -> Self::Owned {
        Owned::from_bytes(&*self.data)
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

impl<'a, T: Cast + ?Sized> IntoIterator for &'a MaybeNonFixedSize<T> {
    type Item = &'a T;
    type IntoIter = core::option::IntoIter<&'a T>;
    fn into_iter(self) -> Self::IntoIter {
        self.to_option().into_iter()
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

impl<GvT: Cast + ?Sized, T: IntoIterator> SerializeTo<MaybeNonFixedSize<GvT>> for T
where
    T::Item: SerializeTo<GvT>,
{
    fn serialize(self, f: &mut impl Write) -> std::io::Result<usize> {
        if let Some(x) = self.into_iter().next() {
            let len = x.serialize(f)?;
            f.write_all(b"\0")?;
            Ok(len + 1)
        } else {
            Ok(0)
        }
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
#[derive(RefCast, Eq, Copy, Clone)]
#[repr(transparent)]
pub struct Bool(u8);
impl Bool {
    pub fn to_bool(self) -> bool {
        self.0 > 0
    }
}
impl Cast for Bool {
    fn default_ref() -> &'static Self {
        &Bool(0u8)
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
impl PartialEq<bool> for Bool {
    fn eq(&self, other: &bool) -> bool {
        self.to_bool() == *other
    }
}
impl PartialEq<Bool> for bool {
    fn eq(&self, other: &Bool) -> bool {
        other == self
    }
}
impl SerializeTo<Bool> for &Bool {
    fn serialize(self, f: &mut impl Write) -> std::io::Result<usize> {
        self.to_bool().serialize(f)
    }
}

impl SerializeTo<Bool> for &bool {
    fn serialize(self, f: &mut impl Write) -> std::io::Result<usize> {
        f.write_all(if *self { b"\x01" } else { b"\x00" })?;
        Ok(1)
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
    let data_end = usize::checked_sub(data.len(), osz as usize * n_frame_offsets)?;

    let start: aligned_bytes::AlignedOffset<ChildAlign> = align_offset::<B>(fo + a).into()
        + aligned_bytes::AlignedOffset::<ChildAlign>::try_new(c).unwrap();
    let end = if let Some(size) = size {
        start.to_usize() + size
    } else if last_child {
        data_end
    } else {
        nth_last_frame_offset(data, osz, (i + 2) as usize)?
    };
    if start <= end && end <= data_end {
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
        Some((start, end)) => T::from_aligned_slice(&data[..end][start..]),
        None => T::default_ref(),
    }
}

#[cfg(test)]
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
                .unwrap(),
            ""
        );
        assert_eq!(
            MaybeNonFixedSize::<Str>::from_aligned_slice(b"hello world\0\0".as_aligned())
                .to_option()
                .unwrap(),
            "hello world"
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
            a_s.into_iter().map(|x| x.to_str()).collect::<Vec<_>>(),
            &["hello", "world"]
        );
        assert_eq!(&a_s[0], "hello");
        assert_eq!(&a_s[1], "world");
        assert!(!a_s.is_empty());
        assert_eq!(a_s.first().unwrap(), "hello");
        assert_eq!(a_s.last().unwrap(), "world");

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
        assert_eq!(v, [&[] as &[u8], &[]]);

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
        assert!(a.iter().len() == a.len());
        v
    }

    #[test]
    #[should_panic]
    fn test_non_fixed_width_array_panic() {
        // Non-normal regression test found by fuzzing:
        let nfwa = NonFixedWidthArray::<[u8]>::from_aligned_slice(b"\x08".as_aligned());
        _ = &nfwa[0];
    }

    #[test]
    fn test_spec_examples() {
        assert_eq!(
            gv!("s").from_bytes(b"hello world\0").as_str(),
            "hello world"
        );
        assert_eq!(gv!("s").serialize_to_vec("hello world"), b"hello world\0");

        assert_eq!(
            gv!("ms")
                .from_bytes(b"hello world\0\0")
                .to_option()
                .unwrap(),
            "hello world"
        );
        assert_eq!(
            gv!("ms").serialize_to_vec(&Some("hello world")),
            b"hello world\0\0"
        );

        assert_eq!(
            gv!("ab").cast(b"\x01\x00\x00\x01\x01".as_aligned()),
            [true, false, false, true, true]
        );
        assert_eq!(
            gv!("ab").serialize_to_vec(&[true, false, false, true, true][..]),
            b"\x01\x00\x00\x01\x01"
        );

        // String Array Example
        //
        // With type 'as':
        let a = gv!("as").from_bytes(b"i\0can\0has\0strings?\0\x02\x06\x0a\x13");
        assert_array_self_consistent(&*a);
        assert_eq!(*a, ["i", "can", "has", "strings?"][..]);
        assert_eq!(
            gv!("as")
                .serialize_to_vec(&["i", "can", "has", "strings?"][..])
                .as_slice(),
            b"i\0can\0has\0strings?\0\x02\x06\x0a\x13"
        );

        // Array of Bytes Example
        //
        // With type 'ay':
        let aob = gv!("ay").cast([0x04u8, 0x05, 0x06, 0x07].as_aligned());
        assert_eq!(aob, &[0x04u8, 0x05, 0x06, 0x07]);
        assert_eq!(
            gv!("ay")
                .serialize_to_vec(&[0x04u8, 0x05, 0x06, 0x07])
                .as_slice(),
            &[0x04u8, 0x05, 0x06, 0x07]
        );

        // Array of Integers Example
        //
        // With type 'ai':
        assert_eq!(gv!("ai").from_bytes(b"\x04\0\0\0\x02\x01\0\0"), [4, 258]);
        assert_eq!(
            gv!("ai").serialize_to_vec(&[4, 258]).as_slice(),
            b"\x04\0\0\0\x02\x01\0\0"
        );

        // Dictionary Entry Example
        //
        // With type '{si}':
        //    'a sp 'k 'e  'y \0 -- --   02 02 00 00 06has a value of {'a key', 514}
    }

    #[test]
    fn test_gvariantstr() {
        assert_eq!(Str::from_aligned_slice(b"".as_aligned()).to_str(), "");
        assert_eq!(Str::from_aligned_slice(b"\0".as_aligned()).to_str(), "");
        assert_eq!(
            Str::from_aligned_slice(b"hello world\0".as_aligned()).to_str(),
            "hello world"
        );
        assert_eq!(
            Str::from_aligned_slice(b"hello world\0".as_aligned()),
            "hello world"
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
        assert_eq!(v, v);

        let data_1 = copy_to_align(b"\x00()");
        let data_2 = copy_to_align(b"");
        assert_eq!(
            Variant::from_aligned_slice(data_1.as_ref()),
            Variant::from_aligned_slice(data_2.as_ref())
        );

        let non_normal = Variant::from_aligned_slice(data_1.as_ref());
        assert_ne!(non_normal, v);

        // Encode an **as** as a variant
        let x = VariantWrap(gv!("as"), ["hello", "goodbye"].as_ref());
        let v = gv!("v").serialize_to_vec(x);
        assert_eq!(v, b"hello\0goodbye\0\x06\x0e\0as");

        // Wrap in another layer of variant
        let xv = VariantWrap(gv!("v"), x);
        let vv = gv!("v").serialize_to_vec(xv);
        assert_eq!(vv, b"hello\0goodbye\0\x06\x0e\0as\0v");

        // Deserialize and unwrap those variants to get the original **as** back:
        let de_vv: Owned<Variant> = gv!("v").from_bytes(vv);
        let de_v = de_vv.get(gv!("v")).unwrap();
        let de = de_v.get(gv!("as")).unwrap();
        assert_eq!(de, ["hello", "goodbye"].as_ref())
    }
}
