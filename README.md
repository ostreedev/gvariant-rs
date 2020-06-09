GVariant-rs
===========

A pure-rust implementation of the GVariant serialisation format intended for
fast reading of in-memory buffers.

```rust
    let string = gv!("s").from_bytes("It works!\0");
    assert_eq!(string, "It works!");
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
use gvariant::{gv, Marker, Structure};
use std::error::Error;

fn ostree_ls(filename: &std::path::Path) -> Result<(), Box<dyn Error>> {
    // Allocate an aligned buffer for the data:
    let mut buf =
        gvariant::aligned_bytes::alloc_aligned(std::fs::metadata(filename)?.len() as usize);

    // Read the data into the buffer
    std::fs::File::open(filename)?.read_exact(&mut buf)?;

    // Interpret as a tree
    let (files, dirs) = gv!("(a(say)a(sayay))").cast(&buf).to_tuple();

    // Print the contents
    for s in dirs {
        let (filename, tree_checksum, meta_checksum) = s.to_tuple();
        println!(
            "{} {} {}/",
            hex::encode(tree_checksum),
            hex::encode(meta_checksum),
            filename
        )
    }

    for f in files {
        let (filename, checksum) = f.to_tuple();
        println!("{} {}", hex::encode(checksum), filename)
    }
    Ok(())
}
```

## Status

* Support for all GVariant types is implemented
* Behaviour is identical to GLib's implementation for all data in "normal
  form". This has been confirmed with fuzz testing.  There are some differences
  for data not in normal form.   See
  https://gitlab.gnome.org/GNOME/glib/-/issues/2121 for more information.

### TODO

* Fuzz testing of structure types
* Publish version 1.0
* Fix or confirm inconsistencies with the GLib implementation discovered by
  fuzzing.

### License

This project is licensed under either of

 * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or
   http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or
   http://opensource.org/licenses/MIT)

at your option.