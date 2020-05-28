GVariant-rs
===========

A pure-rust implementation of the GVariant serialisation format intended for
fast reading of in-memory buffers.

```rust
    let data = b"It works!\0";
    let string = gv!("s")::from_aligned_bytes(data.as_aligned).to_bytes();
    assert_eq!(string, b"It works!");
```

### Documentation

[Module documentation with examples](https://docs.rs/gvariant).

### Usage

Add this to your `Cargo.toml`:

```toml
[dependencies]
gvariant = "0.1"
```

Example: read an ostree dirtree file and print the listing:

```rust
use gvariant;

const FILENAME : &str = "ostree/objects/6e/fbd2d0db263cec96463b45fdb68a803aed7aef9181f8d91086b01b8f7c9c83.dirtree"

fn main () {
    // Allocate a buffer for the data:
    let buf = gvariant::aligned_bytes::alloc_aligned(std::fs:metadata(FILENAME).len());

    // Read the data into the buffer
    std::fs::File::open(FILENAME).read_exact(buf)?;

    // Intepret as a tree
    let files, dirs = gv!("(a(say)a(sayay))").cast(buf).to_tuple();

    for s in dirs {
      let (filename, tree_checksum, meta_checksum) = s.to_tuple();
      println!("{:x} {:x} {}/", tree_checksum, meta_checksum, filename)
    }

    for f in files {
      let (filename, checksum) = f.to_tuple();
      println!("{:x} {}", checksum, filename)
    }
}
```

## Status

* Support for all GVariant types is implemented apart from dict entries
* Serialisation is not currently supported, but may be implemented in a future
  version, or possibly as a seperate crate.

### TODO

* Implement support for dict items
* Correct handling of non-normal structs
* Add no-std and no-alloc support
* Fuzz testing - compare against the GLib version
* Publish version 1.0

### License

This project is licensed under either of

 * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or
   http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or
   http://opensource.org/licenses/MIT)

at your option.