use gvariant::{Cast, NonFixedWidthArray, Structure, aligned_bytes::AsAligned, casting::AlignOf, decl_gv};
use std::{io::Read, error::Error};

decl_gv!(
    type GVTreeMeta = "(a(say)a(sayay))";
    type GVTreeFile = "(say)";
    type GVTreeDir = "(sayay)";
);

#[repr(transparent)]
#[derive(Debug,PartialEq)]
struct TreeFile<'a>(<GVTreeFile as Structure<'a>>::RefTuple);
impl<'a> TreeFile<'a> {
    fn filename(&self) -> &'a str {
        self.0.0.to_str()
    }
    fn checksum(&self) -> &'a [u8] {
        self.0.1
    }
}
impl<'a> From<&'a GVTreeFile> for TreeFile<'a> {
    fn from(x: &'a GVTreeFile) -> Self {
        TreeFile(x.to_tuple())
    }
}

#[repr(transparent)]
#[derive(Debug,PartialEq)]
struct TreeDir<'a>(<GVTreeDir as Structure<'a>>::RefTuple);

impl<'a> TreeDir<'a> {
    fn filename(&self) -> &'a str {
        self.0.0.to_str()
    }
    fn tree_checksum(&self) -> &'a [u8] {
        self.0.1
    }
    fn meta_checksum(&self) -> &'a [u8] {
        self.0.2
    }
}
impl<'a> From<&'a GVTreeDir> for TreeDir<'a> {
    fn from(x: &'a GVTreeDir) -> Self {
        Self(x.to_tuple())
    }
}

#[derive(Clone, Copy, Debug)]
struct DirTreeRef<'a>(<GVTreeMeta as Structure<'a>>::RefTuple);
impl<'a> DirTreeRef<'a> {
    fn files(self) -> impl Iterator<Item = TreeFile<'a>> {
        self.0.0.into_iter().map(|x| x.into())
    }
    fn dirs(self) -> impl Iterator<Item = TreeDir<'a>> {
        self.0.1.into_iter().map(|x| x.into())
    }
}

impl<'a> From<&'a GVTreeMeta> for DirTreeRef<'a> {
    fn from(x: &'a GVTreeMeta) -> Self {
        Self(x.to_tuple())
    }
}

fn ostree_ls(filename: &std::path::Path) -> Result<(), Box<dyn Error>> {
    // Read the data into the buffer and interpret as an OSTree tree:
    let mut buf = vec![];
    std::fs::File::open(filename)?.read_to_end(&mut buf).unwrap();

    let tree : DirTreeRef = GVTreeMeta::from_aligned_slice(buf.as_aligned()).into();

    // Print the contents
    for s in tree.dirs() {
        println!(
            "{} {} {}/",
            hex::encode(s.tree_checksum()),
            hex::encode(s.meta_checksum()),
            s.filename()
        )
    }

    for f in tree.files() {
        println!("{} {}", hex::encode(f.checksum()), f.filename())
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
