use gvariant::aligned_bytes::{copy_to_align, empty_aligned, AsAligned};
use gvariant::{gv, Marker, Structure};
use ref_cast::RefCast;
use std::collections::HashMap;

#[test]
fn test_basic_types() {
    assert_eq!(
        *gv!("i").cast(&*copy_to_align(&[0x00, 0x00, 0x00, 0x00])),
        0
    );
}

#[test]
fn test_struct_into_tuple() {
    let t = gv!("()").cast(empty_aligned()).to_tuple();
    assert_eq!(t, ());

    let t = gv!("(i)").cast(empty_aligned()).to_tuple();
    assert_eq!(t, (&0,));

    let t = gv!("(s)").cast(empty_aligned()).to_tuple();
    assert_eq!(t.0.to_str(), "");

    let buf = copy_to_align(b"\x06\x00\x00\x00\x03\x00\x00\x00");
    let t: (&i32, &i32) = gv!("(ii)").cast(buf.as_ref()).into();
    assert_eq!(t, (&6, &3));

    let buf = copy_to_align(b"\x06\x00\x00\x00super\x00");
    let t: (&i32, &gvariant::Str) = gv!("(is)").cast(buf.as_ref()).to_tuple();
    assert_eq!(t.0, &6);
    assert_eq!(t.1, "super");
}

#[test]
fn test_struct_serialisation() {
    // () - Unit type
    let v = gv!("()").serialize_to_vec(&());
    assert_eq!(v, b"\0");

    // (i) - Fixed size
    let v = gv!("(i)").serialize_to_vec(&(87654321,));
    assert_eq!(v, b"\xb1\x7f\x39\x05");

    // (iy) - Fixed size with padding at end
    let v = gv!("(iy)").serialize_to_vec(&(87654321, 6));
    assert_eq!(v, b"\xb1\x7f\x39\x05\x06\x00\x00\x00");

    // (yi) - Fixed size with padding in middle
    let v = gv!("(yi)").serialize_to_vec(&(6, 87654321));
    assert_eq!(v, b"\x06\x00\x00\x00\xb1\x7f\x39\x05");

    // (yyyyi) - Fixed size no padding
    let v = gv!("(yyyyi)").serialize_to_vec(&(6, 5, 4, 3, 87654321));
    assert_eq!(v, b"\x06\x05\x04\x03\xb1\x7f\x39\x05");

    // (s) - Non fixed size
    let v = gv!("(s)").serialize_to_vec(&("Hello!",));
    assert_eq!(v, b"Hello!\0");

    // (si) - Non fixed size ends with fixed aligned value
    let v = gv!("(si)").serialize_to_vec(&("Hello!!", 87654321));
    assert_eq!(v, b"Hello!!\0\xb1\x7f\x39\x05\x08");
    let v = gv!("(si)").serialize_to_vec(&("Hello!", 87654321));
    assert_eq!(v, b"Hello!\0\0\xb1\x7f\x39\x05\x07");
    let v = gv!("(si)").serialize_to_vec(&("Hello", 87654321));
    assert_eq!(v, b"Hello\0\0\0\xb1\x7f\x39\x05\x06");
    let v = gv!("(si)").serialize_to_vec(&("Hell", 87654321));
    assert_eq!(v, b"Hell\0\0\0\0\xb1\x7f\x39\x05\x05");

    // (is) - Non fixed size starts with fixed aligned value
    let v = gv!("(is)").serialize_to_vec(&(87654321, "Hello"));
    assert_eq!(v, b"\xb1\x7f\x39\x05Hello\0");

    // (isis) - 2 non-fixed size elements, 1 framing offset
    let v = gv!("(isis)").serialize_to_vec(&(87654321, "Hello", 0x12345678, "Byee"));
    assert_eq!(v, b"\xb1\x7f\x39\x05Hello\0\0\0\x78\x56\x34\x12Byee\0\x0a");

    // (sisi) - 2 non-fixed size elements, 2 framing offsets
    let v = gv!("(sisi)").serialize_to_vec(&("Hello", 87654321, "Byee", 0x12345678));
    assert_eq!(
        v,
        b"Hello\0\0\0\xb1\x7f\x39\x05Byee\0\0\0\0\x78\x56\x34\x12\x11\x06"
    );

    // (sututy) - Complex aligment of fixed size elements at end - 2 dynamic alignments, 2 static padding
    let mut input = (
        "Hello",
        0x1111_1111,
        0x2222_2222_2222_2222,
        0x3333_3333,
        0x4444_4444_4444_4444,
        0x55,
    );
    let v = gv!("(sututy)").serialize_to_vec(&input);
    let expected: &[u8] =
        b"Hello\0\0\0\x11\x11\x11\x11\0\0\0\0\x22\x22\x22\x22\x22\x22\x22\x22\x33\x33\x33\x33\0\0\0\0\x44\x44\x44\x44\x44\x44\x44\x44\x55\x06";
    assert_eq!(&*v, expected);

    // Removes the padding between the first u and the first t:
    input.0 = "He";
    let v = gv!("(sututy)").serialize_to_vec(&input);
    let expected: &[u8] =
        b"He\0\0\x11\x11\x11\x11\x22\x22\x22\x22\x22\x22\x22\x22\x33\x33\x33\x33\0\0\0\0\x44\x44\x44\x44\x44\x44\x44\x44\x55\x03";
    assert_eq!(&*v, expected);

    // Regression test
    let empty = gvariant::Str::ref_cast(b"\0");
    let m = gv!("(sututysis)");
    let v = m.serialize_to_vec(&(
        empty,
        &0x11111111,
        &0x2222222222222222,
        &0x33333333,
        &0x4444444444444444,
        &0x55,
        empty,
        &0x66666666,
        empty,
    ));
    let expected: &[u8] = b"\0\0\0\0\x11\x11\x11\x11\x22\x22\x22\x22\x22\x22\x22\x22\x33\x33\x33\x33\0\0\0\0\x44\x44\x44\x44\x44\x44\x44\x44\x55\0\0\0\x66\x66\x66\x66\0\x22\x01";
    assert_eq!(&*v, expected);
}

#[test]
fn test_complex_types() {
    // Data created in Python with:
    //
    //     from gi.repository import GLib
    //     v = GLib.Variant('(a(say)a(sayay))', ([('hello', [1,2,3,4]), ('world', [4,3,2,1])], [('my-dir', [0x3, 0x14, 0x15, 0x92], [0x65, 0x35])]))
    //     v.get_data_as_bytes().get_data()
    let buf = copy_to_align(
        b"hello\x00\x01\x02\x03\x04\x06world\x00\x04\x03\x02\x01\x06\x0b\x16my-dir\x00\x03\x14\x15\x92e5\x0b\x07\x0f\x18");
    let (files, dirs) = gv!("(a(say)a(sayay))").cast(buf.as_ref()).into();
    let expected: (&[(&str, &[u8])], &[(&str, &[u8], &[u8])]) = (
        &[
            ("hello", b"\x01\x02\x03\x04"),
            ("world", b"\x04\x03\x02\x01"),
        ],
        &[("my-dir", b"\x03\x14\x15\x92", b"\x65\x35")],
    );
    assert_eq!(files.len(), 2);
    assert_eq!(files[0].to_tuple().0, expected.0[0].0);
    assert_eq!(files[0].to_tuple().1, expected.0[0].1);
    assert_eq!(files[1].to_tuple().0, expected.0[1].0);
    assert_eq!(files[1].to_tuple().1, expected.0[1].1);

    assert_eq!(dirs.len(), 1);
    let d = dirs[0].to_tuple();
    assert_eq!(d.0, expected.1[0].0);
    assert_eq!(d.1, expected.1[0].1);
    assert_eq!(d.2, expected.1[0].2);
    let reserialized = gv!("(a(say)a(sayay))").serialize_to_vec(&expected);
    assert_eq!(*reserialized, **buf);

    let buf = copy_to_align(include_bytes!(
        "0bf6200211dd4fd63be6e9bc5c90bea645e2696c0117b05f83562081813a5b94.commit"
    ));
    let commit = gv!("(a{sv}aya(say)sstayay)").cast(buf.as_ref());
    let (
        metadata,
        _parent_checksum,
        _related_objects,
        _subject,
        _body,
        timestamp,
        _root_tree,
        _root_tree_meta,
    ) = commit.into();
    let metadata: HashMap<_, _> = metadata
        .iter()
        .map(|x| x.to_tuple())
        .map(|x| (x.0.to_str(), x.1))
        .collect();
    assert_eq!(
        metadata["version"].get(gv!("s")).unwrap().to_str(),
        "7.1707"
    );
    assert_eq!(*timestamp, 15444671992342511616);

    let buf = copy_to_align(
        b"\0\0\0\0\x11\x11\x11\x11\x22\x22\x22\x22\x22\x22\x22\x22\x33\x33\x33\x33\0\0\0\0\x44\x44\x44\x44\x44\x44\x44\x44\x55\0\0\0\x66\x66\x66\x66\0\x22\x01");
    let t = gv!("(sututysis)").cast(buf.as_ref()).to_tuple();
    assert_eq!(t.0, "");
    assert_eq!(*t.1, 0x11111111);
    assert_eq!(*t.2, 0x2222222222222222);
    assert_eq!(*t.3, 0x33333333);
    assert_eq!(*t.4, 0x4444444444444444);
    assert_eq!(*t.5, 0x55);
    assert_eq!(t.6, "");
    assert_eq!(*t.7, 0x66666666);
    assert_eq!(t.8, "");
}

#[test]
fn test_repeated_types() {
    let foo = [("hi", 3)];
    let data = gv!("(a{si}a{si})").serialize_to_vec(&(&foo, &foo));
    let aligned = copy_to_align(&*data);

    assert_eq!(
        &data,
        b"hi\x00\x00\x03\x00\x00\x00\x03\x09\x00\x00hi\x00\x00\x03\x00\x00\x00\x03\x09\x0a"
    );

    let s = gv!("(a{si}a{si})").cast(aligned.as_ref()).to_tuple();
    println!("{:?}", s);
    assert_eq!(s.0, s.1);
    assert_eq!(s.0.len(), 1);
    assert_eq!(s.0[0].to_tuple().0, "hi");
    assert_eq!(*s.0[0].to_tuple().1, 3);
}

#[test]
fn test_spec_examples() {
    let data = copy_to_align(b"foo\0\xff\xff\xff\xff\x04");
    let (s, i) = gv!("(si)").cast(data.as_ref()).to_tuple();
    assert_eq!(s, "foo");
    assert_eq!(*i, -1);
    assert_eq!(*gv!("(si)").serialize_to_vec(&("foo", -1)), **data.as_ref());

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
    assert_eq!(a[0].to_tuple().0, "hi");
    assert_eq!(*a[0].to_tuple().1, -2);
    assert_eq!(a[1].to_tuple().0, "bye");
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
    assert_eq!(ns.to_tuple().0.to_tuple().1.to_str(), "can");
    let v: Vec<_> = ns.to_tuple().1.into_iter().map(|x| x.to_str()).collect();
    assert_eq!(v, &["has", "strings?"]);

    // Simple Structure Example
    //
    // With type '(yy)':
    let s = gv!("(yy)").cast([0x70u8, 0x80].as_aligned());
    assert_eq!((s.field_0, s.field_1), (0x70, 0x80));
    assert_eq!(s.to_tuple(), (&0x70, &0x80));

    // Padded Structure Example 1
    //
    // With type '(iy)':
    let data = copy_to_align(&[0x60u8, 0x00, 0x00, 0x00, 0x70, 0x00, 0x00, 0x00]);
    let s = gv!("(iy)").cast(data.as_ref());
    assert_eq!((s.field_0, s.field_1), (96, 0x70));
    assert_eq!(s.to_tuple(), (&96, &0x70));

    // Padded Structure Example 2
    //
    // With type '(yi)':
    let data = copy_to_align(&[0x70, 0x00, 0x00, 0x00, 0x60, 0x00, 0x00, 0x00]);
    let s = gv!("(yi)").cast(data.as_ref());
    assert_eq!((s.field_0, s.field_1), (0x70, 96));
    assert_eq!(s.to_tuple(), (&0x70, &96));

    // Array of Structures Example
    //
    // With type 'a(iy)':
    let data = copy_to_align(b"\x60\0\0\0\x70\0\0\0\x88\x02\0\0\xf7\0\0\0");
    let v: Vec<_> = gv!("a(iy)")
        .cast(&*data)
        .iter()
        .map(|x| x.to_tuple())
        .collect();
    assert_eq!(v, [(&96, &0x70), (&648, &0xf7)]);
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
    assert_eq!(yi.to_tuple(), (&0x55, &258));

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
            .map(|x| x.to_str())
            .collect::<Vec<_>>(),
        ["", ""]
    );

    // String with Embedded Nul
    assert_eq!(gv!("s").cast(b"foo\0bar\0".as_aligned()), "");

    // String with embedded nul but none at end
    assert_eq!(gv!("s").cast(b"foo\0bar".as_aligned()), "");

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
        gv_as.into_iter().map(|x| x.to_str()).collect::<Vec<_>>(),
        ["foo", "", ""]
    );
    assert_eq!(&gv_as[0], "foo");
    assert_eq!(&gv_as[1], "");
    assert_eq!(&gv_as[2], "");
    assert_eq!(gv_as.len(), 3);

    // End boundary precedes start boundary
    let gv_as = gv!("as").cast(b"foo\0bar\0baz\0\x04\x00\x04\x0c".as_aligned());
    assert_eq!(
        gv_as.into_iter().map(|x| x.to_str()).collect::<Vec<_>>(),
        ["foo", "", "foo", ""]
    );
    assert_eq!(&gv_as[0], "foo");
    assert_eq!(&gv_as[1], "");
    assert_eq!(&gv_as[2], "foo");
    assert_eq!(&gv_as[3], "");
    assert_eq!(gv_as.len(), 4);

    // Insufficient space for structure framing offsets
    let t = gv!("(ayayayayay)")
        .cast([0x03u8, 0x02, 0x01].as_ref().as_aligned())
        .to_tuple();
    assert_eq!(
        t,
        (
            [].as_ref(),
            [].as_ref(),
            [].as_ref(),
            [].as_ref(),
            [].as_ref()
        )
    );
}
