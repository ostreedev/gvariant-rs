use gvariant_macro::gv;
use gvariant::marker::GVariantMarker;
use gvariant::aligned_bytes::A4;
use gvariant::aligned_bytes::copy_to_align;

#[test]
fn test_basic_types() {
    assert_eq!(<gv!("i")>::_mark(&*copy_to_align(&[0x00, 0x00, 0x00, 0x00])).to_rs(), 0);
}


#[test]
fn test_non_normal_values() {
    // Examples of non-normal data from the GVariant paper:

    // Wrong Size for Fixed Size Value
    assert_eq!(<gv!("i")>::_mark(&*copy_to_align(&[0x7u8, 0x33, 0x90]).as_ref()).to_rs(), 0);

    // Non-zero Padding Bytes
    //assert_eq!(<gv!("(yi)")>::mark(&[0x55, 0x66, 0x77, 0x88, 0x02, 0x01, 0x00, 0x00]).split(), (0x55, 258));

    // Boolean Out of Range
    assert_eq!(<gv!("ab")>::mark(&[0x01u8, 0x00, 0x03, 0x04, 0x00, 0x01, 0xff, 0x80, 0x00])
        .to_rs().iter().map(|x| x.to_bool()).collect::<Vec<_>>(),
        [true, false, true, true, false, true, true, true, false]);

    // Unterminated String
    assert_eq!(<gv!("as")>::mark(b"hello world\0\x0b\x0c").into_iter().map(|x| x.to_bytes()).collect::<Vec<_>>(), [b"".as_ref(), b""]);

    // String with Embedded Nul
    assert_eq!(<gv!("s")>::mark(b"foo\0bar\0").to_cstr().to_bytes(), b"foo");

    // String with embedded nul but none at end
    assert_eq!(<gv!("s")>::mark(b"foo\0bar").to_rs(), b"");

    // Wrong size for fixed-size maybe
    assert!(<gv!("mi")>::_mark(&*copy_to_align(&[0x33u8, 0x44, 0x55, 0x66, 0x77, 0x88])).to_option().is_none());

    // Wrong size for fixed-width array
    //assert_eq!(<gv!("a(yy)")>::mark(&[0x03, 0x04, 0x05, 0x06, 0x07]), []);

    // Start or end boundary of child falls outside the container
    let gv_as = <gv!("as")>::mark(b"foo\0bar\0baz\0\x04\x10\x0c");
    assert_eq!(gv_as.into_iter().map(|x| x.to_bytes()).collect::<Vec<_>>(), [b"foo".as_ref(), b"", b""]);
    assert_eq!(gv_as[0].to_bytes(), b"foo");
    assert_eq!(gv_as[1].to_bytes(), b"");
    assert_eq!(gv_as[2].to_bytes(), b"");
    assert_eq!(gv_as.len(), 3);

    // End boundary precedes start boundary
    let gv_as = <gv!("as")>::mark(b"foo\0bar\0baz\0\x04\x00\x0c");
    assert_eq!(gv_as.into_iter().map(|x| x.to_cstr().to_bytes()).collect::<Vec<_>>(), [b"foo".as_ref(), b"", b"foo"]);
    assert_eq!(gv_as[0].to_bytes(), b"foo");
    assert_eq!(gv_as[1].to_bytes(), b"");
    assert_eq!(gv_as[2].to_cstr().to_bytes(), b"foo");
    assert_eq!(gv_as.len(), 3);

    // Insufficient space for structure framing offsets
    //assert_eq!(<gv!("(ayayayayay)")>::mark([0x03, 0x02, 0x01]), [[3], [2], [1], [], []]);
}
