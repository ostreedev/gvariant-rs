use gvariant::{gv, Marker, Structure};
use std::error::Error;

fn ostree_ls(filename: &std::path::Path) -> Result<(), Box<dyn Error>> {
    // Read the data into the buffer and interpret as an OSTree tree:
    let tree = gv!("(a(say)a(sayay))").deserialize(std::fs::File::open(filename)?)?;

    // (a(say)a(sayay)) is a structure, so tree implements gvariant::Structure,
    // and we can turn it into a tuple:
    let (files, dirs) = tree.to_tuple();

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
