# GVariant-rs

A pure-rust implementation of the GVariant serialisation format intended for
fast reading of in-memory buffers.

```rust
    let data = b"It works!\0";
    let string : &[u8] = gv!("s")::mark(data).into();
```

This library operates by reinterpreting byte buffers as a GVariant type. It
doesn't do any of its own allocations.  As a result proper alignment of byte
buffers is the responsibility of the user.  See "Alignment of data" below.

It's intended to conform to the GVariant specification and match the behaviour
of the GLib implementation.  Exceptions to this are described in "Deviations
from the Specification" below.

This library assumes you know the types of the data you are dealing with at
compile time.  This is in contrast to the GLib implementation where you could
construct a GVariant type string dynamically.  This allows for a much smaller
and faster implementation, more in line with Alexander Larsson's [GVariant
Schema Compiler].  As a result GVariant structs are supported through use of
code generation via macros.  See the gvariant-macro subdirectory.

The library is intended to be sound and safe to run on untrusted input, although
the implementation does include use of `unsafe`.  See "Use of `unsafe`" below.
Help with validating the unsafe portions of the library would be gratefully
received.

This library works Rust stable.  As a result we can't use const-generics, which
would make some of the code much more streightforward.  A future version of this
library may use const-generics, once they are available in stable rust.

[GVariant Schema Compiler]: https://gitlab.gnome.org/alexl/variant-schema-compiler/

## Status

* Support for all GVariant types is implemented apart from v (variant) and
  structures.
* Serialisation is not currently supported, but may be implemented in a future
  version, or possibly as a seperate crate.

### TODO

* Implement support for the variant type
* Implement Structure generation macro
* Fuzz testing - compare against the GLib version

## Deviations from the Specification

### Maximum size of objects

The spec says:

> **2.3.6 Framing Offsets**
>
> There is no theoretical upper limit in how large a framing offset can be. This
> fact (along with the absence of other limitations in the serialisation format)
> allows for values of arbitrary size.

In this implementation the maximum size of an object is usize (typically
64-bits).  This should not be a problem in practice on 64-bit machines.

### Handling of non-normal strings

The spec says:

> **2.7.3 Handling Non-Normal Serialised Data**
>
> **String with Embedded Nul**
>
> If a string has a nul character as its final byte, but also contains another
> nul character before this final terminator, the value of the string is taken to
> be the part of the string that precedes the embedded nul. This means that
> obtaining a C pointer to a string is still a constant time operation.

In this implementation embedded NULs are preserved in strings when converting to
&[u8] with to_bytes().  This is because rust represents strings as `&[u8]` and
conforming to the spec would involve scanning the data for the NUL terminator.
For conformant behaviour use `.to_cstr().to_bytes()`.  Note however this is not
a constant-time operation and a scan of the string is necessary.

## Design

The intention is to build abstractions that are transparent to the compiler,
such that they compile down to simple memory accesses, like reading the fields
of a struct.

The GVariant spec concerns itself with Byte Sequences:

> 2.3.1 Byte Sequence
>
> A byte sequence is defined as a sequence of bytes which has a known length. In
> all cases, in GVariant, knowing the length is essential to being able to
> successfully deserialise a value.

In Rust a byte sequence is typically descibed as `&[u8]`.  We 

### Alignment of data

This library operates by reinterpreting byte buffers as a GVariant type.  For
these casts to be valid the alignment of the underlying data must be sufficient
for the target type.  We don't perform any of our own allocations, relying on
data passed from the user, as a result proper alignment of byte buffers is the
responsibility of the user.

This library defines a type `AlignedSlice<A>` which represents an aligned byte
buffer aligned to the alignment given by `A`.  `A` may be:

* `A1` - 1B aligned aka unaligned
* `A2` - 2B aligned
* `A4` - 4B aligned
* `A8` - 8B aligned

As a type parameter the alignment is known statically at compile time.  This
allows eliding runtime checks.

You can convert `AlignedSlice`s to lower alignments infallibly and for free
(using `as_aligned` and `as_aligned_mut`).  Going in the opposite direction and
coming from a plain `&[u8]` slice requires runtime checks (`try_as_aligned` and
`try_as_aligned_mut`).

The `AsAligned` trait is provided to make accepting any data of the appropriate
alignment convenient.  For example: use a `&impl AsAligned<A2>` parameter to
accept any data with 2B or greater alignment.

`alloc_aligned` is provided to make it easy and safe to create aligned buffers.
Example reading data from file into aligned buffer:

```rust
let buf = alloc_aligned::<A8>(4096);
let len = file.read(buf.as_mut())?;
let aligned_data = &buf[..len];
```

I've not yet implemented it, but it may become necessary to create an equivalent
to `Vec<>` but for aligned memory.  We'll see how we get on.

#### Efficiency of statically known alignment

Every GVariant container type has alignment >= any of its children.  This means
that if a byte slice is aligned for the parent it will still be aligned when we
access the children, so we only need to check the alignment once when
constructing the buffer rather than on every access.

For example: The structure `(nu)` (or `(i16, u32)` in rust terms) has alignment
4: So we create an `AlignedSlice<A4>` containing the complete 8B structure.  The
alignment is checked at run-time when we create the slice.  The `i16` is
extracted with `data[..2]`, which still has type `AlignedSlice<A4>`. The
conversion to `AlignedSlice<A2>` as required by the `i16`'s doesn't require any
runtime checks.

#### Serialisation alignment vs. platform alignment requirements

Note: there are two related, but subtly different concepts of alignment at use
in this library:

1. The GVariant serialisation format has a concept of alignment of data types
   which informs where padding should be placed and affects the size of fixed
   size structures.
2. There are the alignment requirements of the types we're casting to that Rust
   and the underlying platform requires of us when we cast.  These are the
   values that `std::mem::align_of<>()` returns.

These alignments are usually the same, but in some circumstances can differ. For
example: I believe that on 32-bit x86 `align_of<u64>() == 4` - while of course
the serialisation format doesn't depend on the platform in use.  This library
assumes (and asserts in code) that the former alignment is always >= the latter.

#### Use of `unsafe`

`unsafe` is employed quite a lot to handle all the casting between types.  I've
attempted to build abstractions to cover the invariants that must be maintained
for this casting to be safe and sound.  These are:

* The bit pattern of the underlying memory must be valid for the target type.
  This is provided by the `unsafe trait AllBitPatternsValid`.  It's implemented
  for the relevant primative types.
* The alignment requirements of the target type must be satisfied.  We use the
  `pub trait AlignOf` to determine the alignment of the target type.  We can't
  rely on `std::mem::align_of` because we want to use it to constrain input
  types, and we can't do that without const generics, which are still an
  unstable Rust feature.  We use `AlignedSlice<A>` for byte slices with an
  alignment attached.  See "Alignment of Data" above.  
* The size of the data must match.  We're always casting from slices to `Sized`
  types so we only perform the cast if the sizes match.

I've tried to concentrate almost all of the unsafe in `aligned_bytes.rs` and
`casting.rs` to make it easier to review.  I also take advantage of the ref_cast
crate to avoid some unsafe casting that I'd otherwise require.

A review of the use of `unsafe`, or advice on how the amount of unsafe could be
reduced would be greatly appreciated.

## Comparison to and relationship with other projects

* [GVariant Schema Compiler] - Similar to this project the GSC generates code at
  compile time to represent the types the user is interested in.  GSC targets
  the C language.  Unlike this project the types are generated from schema
  files, allowing structures to have named fields.  In gvariant-rs we generate
  our code just from the plain GVariant type strings using macros.  This makes
  the build process simpler - there are no external tools, and it makes it
  easier to get started - there is no new schema format to learn.  The cost is
  that the user is responsible for remember which field means what and what
  endianness should be used to interpret the data.

  It might make sense in the future to extend GSC to generate rust code as well
  - in which case the generated code may depend on this library.
* [gtk-rs glib::variant](https://gtk-rs.org/docs/glib/variant/index.html) - This
  is a binding to the GLib GVariant implementation in C, so depends on glib.
  It's currently incomplete.  The docs say "Although `GVariant` supports
  arbitrarily complex types, this binding is currently limited to the basic
  ones: `bool`, `u8`, `i16`, `u16`, `i32`, `u32`, `i64`, `u64`, `f64` and
  `&str`/`String`."
* [zvariant](https://crates.io/crates/zvariant) - Implements the similar DBus
  serialisation format rather than GVariant.  Docs say: "GVariant ... will be
  supported by a future version of this crate."
* [serde_gvariant](https://github.com/lucab/serde_gvariant) - Implements the
  same format, but for serde integration.  Described as WIP and not published on
  crates.io
