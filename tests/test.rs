use gvariant::aligned_bytes::copy_to_align;
use gvariant::marker::GVariantMarker;
use gvariant_macro::{define_gv, gv};

#[test]
fn test_basic_types() {
    assert_eq!(
        <gv!("i")>::_mark(&*copy_to_align(&[0x00, 0x00, 0x00, 0x00])).to_rs(),
        0
    );
}
define_gv!("(si)");

#[test]
fn test_spec_examples() {
    let data = copy_to_align(b"foo\0\xff\xff\xff\xff\x04");
    let (s, i) = <gv!("(si)")>::_mark(data.as_ref()).split();
    assert_eq!(s, &*b"foo");
    assert_eq!(*i, -1);

    // Structure Array Example
    //
    // With type 'a(si)'.
    //
    // The example in the spec is missing the second array frame offset
    // `21`.  I've added it here giving me consistent results with the GLib
    // implmentation
    let data = copy_to_align(&[
        b'h', b'i', 0, 0, 0xfe, 0xff, 0xff, 0xff, 3, 0, 0, 0, b'b', b'y', b'e', 0, 0xff, 0xff,
        0xff, 0xff, 4, 9, 21,
    ]);
    let a = <gv!("a(si)")>::_mark(data.as_ref());
    assert_eq!(a.len(), 2);
    assert_eq!(&a[0].split().0, b"hi");
    assert_eq!(*a[0].split().1, -2);
    assert_eq!(&a[1].split().0, b"bye");
    assert_eq!(*a[1].split().1, -1);

    // Nested Structure Example
    //
    // With type '((ys)as)'
    //
    // Note: This is another example where I think there is a bug in the
    // spec. I've added \x0d here as an additional framing offset of the
    // `as`. This gives consistent results with the GLib implementation.
    /*
    let ns = <gv!("((ys)as)")>::mark(b"ican\0has\0strings?\0\x04\x0d\x05");
    assert_eq!(*ns.split().0.split().0, b'i');
    assert_eq!(ns.split().0.split().1, b"can");
    let v : Vec<_> = ns.split().1.into_iter().map(|x| x.to_bytes()).collect();
    assert_eq!(v, &[b"has".as_ref(), b"strings?"]);
    */
    // Simple Structure Example
    //
    // With type '(yy)':
    /*
    let ss = <gv!("(yy)")>::mark([0x70u8, 0x80]);
    assert_eq!(ss.split(), (0x80, 0x80));
    */
    // Padded Structure Example 1
    //
    // With type '(iy)':
    /*
    let ps = <gv!("(iy)")>::mark([0x60, 0x00, 0x00, 0x00, 0x70, 0x00, 0x00, 0x00]);
    assert_eq!(ps.split(), (96, 0x70));
    */

    // Padded Structure Example 2
    //
    // With type '(yi)':
    /*
    let ps = <gv!("(yi)")>::mark([0x70, 0x00, 0x00, 0x00, 0x60, 0x00, 0x00, 0x00]);
    assert_eq!(ps.split(), (0x70, 96));
    */

    // Array of Structures Example
    //
    // With type 'a(iy)':
    /*
    let aos = <gv!("a(iy)")>::mark(
        b"\x60\0\0\0\x70\0\0\0\x88\x02\0\0\xf7\0\0\0");
    let v : Vec<_> = aos.into_iter().map(|x|x.split()).collect();
    assert_eq!(v, [(96, 0x70), (648, 0xf7)]);
    */
}

#[test]
fn test_non_normal_values() {
    // Examples of non-normal data from the GVariant paper:

    // Wrong Size for Fixed Size Value
    assert_eq!(
        <gv!("i")>::_mark(&*copy_to_align(&[0x7u8, 0x33, 0x90]).as_ref()).to_rs(),
        0
    );

    // Non-zero Padding Bytes
    //assert_eq!(<gv!("(yi)")>::mark(&[0x55, 0x66, 0x77, 0x88, 0x02, 0x01, 0x00, 0x00]).split(), (0x55, 258));

    // Boolean Out of Range
    assert_eq!(
        <gv!("ab")>::mark(&[0x01u8, 0x00, 0x03, 0x04, 0x00, 0x01, 0xff, 0x80, 0x00])
            .to_slice()
            .iter()
            .map(|x| x.to_bool())
            .collect::<Vec<_>>(),
        [true, false, true, true, false, true, true, true, false]
    );

    // Unterminated String
    assert_eq!(
        <gv!("as")>::mark(b"hello world\0\x0b\x0c")
            .into_iter()
            .map(|x| x.to_bytes())
            .collect::<Vec<_>>(),
        [b"".as_ref(), b""]
    );

    // String with Embedded Nul
    assert_eq!(<gv!("s")>::mark(b"foo\0bar\0").to_cstr().to_bytes(), b"foo");

    // String with embedded nul but none at end
    assert_eq!(<gv!("s")>::mark(b"foo\0bar").to_rs(), b"");

    // Wrong size for fixed-size maybe
    assert!(
        <gv!("mi")>::_mark(&*copy_to_align(&[0x33u8, 0x44, 0x55, 0x66, 0x77, 0x88]))
            .to_option()
            .is_none()
    );

    // Wrong size for fixed-width array
    //assert_eq!(<gv!("a(yy)")>::mark(&[0x03, 0x04, 0x05, 0x06, 0x07]), []);

    // Start or end boundary of child falls outside the container
    let gv_as = <gv!("as")>::mark(b"foo\0bar\0baz\0\x04\x10\x0c");
    assert_eq!(
        gv_as.into_iter().map(|x| x.to_bytes()).collect::<Vec<_>>(),
        [b"foo".as_ref(), b"", b""]
    );
    assert_eq!(gv_as[0].to_bytes(), b"foo");
    assert_eq!(gv_as[1].to_bytes(), b"");
    assert_eq!(gv_as[2].to_bytes(), b"");
    assert_eq!(gv_as.len(), 3);

    // End boundary precedes start boundary
    let gv_as = <gv!("as")>::mark(b"foo\0bar\0baz\0\x04\x00\x0c");
    assert_eq!(
        gv_as
            .into_iter()
            .map(|x| x.to_cstr().to_bytes())
            .collect::<Vec<_>>(),
        [b"foo".as_ref(), b"", b"foo"]
    );
    assert_eq!(gv_as[0].to_bytes(), b"foo");
    assert_eq!(gv_as[1].to_bytes(), b"");
    assert_eq!(gv_as[2].to_cstr().to_bytes(), b"foo");
    assert_eq!(gv_as.len(), 3);

    // Insufficient space for structure framing offsets
    //assert_eq!(<gv!("(ayayayayay)")>::mark([0x03, 0x02, 0x01]), [[3], [2], [1], [], []]);
}
