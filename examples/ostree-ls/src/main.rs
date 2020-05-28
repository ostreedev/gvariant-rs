use gvariant::{gv, Marker, Structure};
use hex;
use std::error::Error;
use std::io::Read;

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

fn main() {
    ostree_ls(
        "ostree/objects/6e/fbd2d0db263cec96463b45fdb68a803aed7aef9181f8d91086b01b8f7c9c83.dirtree"
            .as_ref(),
    )
    .unwrap()
}
