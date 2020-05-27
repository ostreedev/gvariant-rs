use gvariant::aligned_bytes::{copy_to_align, AsAligned};
use gvariant::{gv, Marker};

#[test]
fn test_basic_types() {
    assert_eq!(
        *gv!("i").cast(&*copy_to_align(&[0x00, 0x00, 0x00, 0x00])),
        0
    );
}

#[test]
fn test_spec_examples() {
    let data = copy_to_align(b"foo\0\xff\xff\xff\xff\x04");
    let (s, i) = gv!("(si)").cast(data.as_ref()).to_tuple();
    assert_eq!(s.to_bytes(), &*b"foo");
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
    let a = gv!("a(si)").cast(data.as_ref());
    assert_eq!(a.len(), 2);
    assert_eq!(a[0].to_tuple().0.to_bytes(), b"hi");
    assert_eq!(*a[0].to_tuple().1, -2);
    assert_eq!(a[1].to_tuple().0.to_bytes(), b"bye");
    assert_eq!(*a[1].to_tuple().1, -1);

    // Nested Structure Example
    //
    // With type '((ys)as)'
    //
    // Note: This is another example where I think there is a bug in the
    // spec. I've added \x0d here as an additional framing offset of the
    // `as`. This gives consistent results with the GLib implementation.
    let ns = gv!("((ys)as)").cast(b"ican\0has\0strings?\0\x04\x0d\x05".as_aligned());
    assert_eq!(*ns.to_tuple().0.to_tuple().0, b'i');
    assert_eq!(ns.to_tuple().0.to_tuple().1.to_bytes(), b"can");
    let v: Vec<_> = ns.to_tuple().1.into_iter().map(|x| x.to_bytes()).collect();
    assert_eq!(v, &[b"has".as_ref(), b"strings?"]);

    // Simple Structure Example
    //
    // With type '(yy)':
    let s = gv!("(yy)").cast([0x70u8, 0x80].as_aligned());
    assert_eq!((s.field_0, s.field_1), (0x70, 0x80));
    assert_eq!(s.to_tuple(), (0x70, 0x80));

    // Padded Structure Example 1
    //
    // With type '(iy)':
    let data = copy_to_align(&[0x60u8, 0x00, 0x00, 0x00, 0x70, 0x00, 0x00, 0x00]);
    let s = gv!("(iy)").cast(data.as_ref());
    assert_eq!((s.field_0, s.field_1), (96, 0x70));
    assert_eq!(s.to_tuple(), (96, 0x70));

    // Padded Structure Example 2
    //
    // With type '(yi)':
    let data = copy_to_align(&[0x70, 0x00, 0x00, 0x00, 0x60, 0x00, 0x00, 0x00]);
    let s = gv!("(yi)").cast(data.as_ref());
    assert_eq!((s.field_0, s.field_1), (0x70, 96));
    assert_eq!(s.to_tuple(), (0x70, 96));

    // Array of Structures Example
    //
    // With type 'a(iy)':
    let data = copy_to_align(b"\x60\0\0\0\x70\0\0\0\x88\x02\0\0\xf7\0\0\0");
    let v: Vec<_> = gv!("a(iy)")
        .cast(&*data)
        .iter()
        .map(|x| x.to_tuple())
        .collect();
    assert_eq!(v, [(96, 0x70), (648, 0xf7)]);
}

#[test]
fn test_non_normal_values() {
    // Examples of non-normal data from the GVariant paper:

    // Wrong Size for Fixed Size Value
    assert_eq!(
        *gv!("i").cast(&*copy_to_align(&[0x7u8, 0x33, 0x90]).as_ref()),
        0
    );

    // Non-zero Padding Bytes
    let data = copy_to_align(&[0x55u8, 0x66, 0x77, 0x88, 0x02, 0x01, 0x00, 0x00]);
    let yi = gv!("(yi)").cast(&*data);
    assert_eq!(yi.to_tuple(), (0x55, 258));

    // Boolean Out of Range
    assert_eq!(
        gv!("ab")
            .cast([0x01u8, 0x00, 0x03, 0x04, 0x00, 0x01, 0xff, 0x80, 0x00].as_aligned())
            .iter()
            .map(|x| x.to_bool())
            .collect::<Vec<_>>(),
        [true, false, true, true, false, true, true, true, false]
    );

    // Unterminated String
    assert_eq!(
        gv!("as")
            .cast(b"hello world\0\x0b\x0c".as_aligned())
            .into_iter()
            .map(|x| x.to_bytes())
            .collect::<Vec<_>>(),
        [b"".as_ref(), b""]
    );

    // String with Embedded Nul
    assert_eq!(
        gv!("s")
            .cast(b"foo\0bar\0".as_aligned())
            .to_cstr()
            .to_bytes(),
        b"foo"
    );

    // String with embedded nul but none at end
    assert_eq!(gv!("s").cast(b"foo\0bar".as_aligned()).to_bytes(), b"");

    // Wrong size for fixed-size maybe
    assert!(gv!("mi")
        .cast(&*copy_to_align(&[0x33u8, 0x44, 0x55, 0x66, 0x77, 0x88]))
        .to_option()
        .is_none());

    // Wrong size for fixed-width array
    assert_eq!(
        gv!("a(yy)").cast([0x03, 0x04, 0x05, 0x06, 0x07].as_aligned()),
        []
    );

    // Start or end boundary of child falls outside the container
    let gv_as = gv!("as").cast(b"foo\0bar\0baz\0\x04\x10\x0c".as_aligned());
    assert_eq!(
        gv_as.into_iter().map(|x| x.to_bytes()).collect::<Vec<_>>(),
        [b"foo".as_ref(), b"", b""]
    );
    assert_eq!(gv_as[0].to_bytes(), b"foo");
    assert_eq!(gv_as[1].to_bytes(), b"");
    assert_eq!(gv_as[2].to_bytes(), b"");
    assert_eq!(gv_as.len(), 3);

    // End boundary precedes start boundary
    let gv_as = gv!("as").cast(b"foo\0bar\0baz\0\x04\x00\x0c".as_aligned());
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
    //assert_eq!(<gv!("(ayayayayay)")>::from_aligned_slice([0x03, 0x02, 0x01]), [[3], [2], [1], [], []]);
}
