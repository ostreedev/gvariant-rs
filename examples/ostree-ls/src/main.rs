use gvariant::{Cast, NonFixedWidthArray, Structure, aligned_bytes::AsAligned, decl_gv};
use std::{io::Read, error::Error};

decl_gv!(
    type GVTreeMeta = "(a(say)a(sayay))";
    type GVTreeFile = "(say)";
    type GVTreeDir = "(sayay)";
);

#[repr(transparent)]
#[derive(Debug,PartialEq)]
struct TreeFile<'a>(<<gv_decls::MarkerCsay7 as gvariant::Marker>::Type as Structure<'a>>::RefTuple);

#[derive(Clone, Copy, Debug)]
struct DirTreeRef<'a>(<<GVTreeMeta as gvariant::Marker>::Type as Structure<'a>>::RefTuple);
impl<'a> DirTreeRef<'a> {
    fn cast(data: &'a [u8]) -> Self {
        Self(GVTreeMeta::cast(data.as_aligned()).to_tuple())
    }
    fn dirs(self) -> NonFixedWidthArray<TreeFile<'a>> {
        self.0.0.into_iter().map(|x| {let t = x.to_tuple(); (t.0.into(), t.1.into()) })
    }
}

fn ostree_ls(filename: &std::path::Path) -> Result<(), Box<dyn Error>> {
    // Read the data into the buffer and interpret as an OSTree tree:
    let mut buf = vec![];
    std::fs::File::open(filename)?.read_to_end(&mut buf);

    let tree = DirTreeRef::cast(buf.as_ref());

    // Print the contents
    for s in tree.dirs() {
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
