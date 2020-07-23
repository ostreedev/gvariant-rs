#![no_main]

use gvariant::{
    aligned_bytes::{copy_to_align, AsAligned, A8},
    casting::AlignOf,
    gv, Bool, Cast, MaybeFixedSize, MaybeNonFixedSize, NonFixedWidthArray, SerializeTo, Str,
    Variant,
};
use libfuzzer_sys::fuzz_target;
use std::{
    ffi::{CStr, CString},
    fmt::Debug,
};

struct GLibVariantType {
    ptr: *mut glib_sys::GVariantType,
}
impl GLibVariantType {
    fn new(ty: &str) -> GLibVariantType {
        let cs = CString::new(ty).unwrap();
        GLibVariantType {
            ptr: unsafe { glib_sys::g_variant_type_new(cs.as_ptr()) },
        }
    }
}
impl Drop for GLibVariantType {
    fn drop(&mut self) {
        unsafe { glib_sys::g_variant_type_free(self.ptr) };
    }
}

struct GLibVariant {
    variant: *mut glib_sys::GVariant,
}
impl GLibVariant {
    fn new(data: &[u8], ty: &GLibVariantType) -> GLibVariant {
        unsafe {
            let bytes = glib_sys::g_bytes_new(data.as_ptr() as *const std::ffi::c_void, data.len());
            let out = Self::new_from_gvariant(glib_sys::g_variant_new_from_bytes(ty.ptr, bytes, 0));
            glib_sys::g_bytes_unref(bytes);
            out.unwrap()
        }
    }
    unsafe fn new_from_gvariant(variant: *mut glib_sys::GVariant) -> Option<GLibVariant> {
        if variant.is_null() {
            None
        } else {
            Some(GLibVariant { variant })
        }
    }
    unsafe fn get_data(&self) -> &[u8] {
        let data = glib_sys::g_variant_get_data(self.variant);
        std::slice::from_raw_parts(
            data as *const u8,
            glib_sys::g_variant_get_size(self.variant),
        )
    }
    fn is_normal_form(&self) -> bool {
        assert!(!self.variant.is_null());
        (unsafe { glib_sys::g_variant_is_normal_form(self.variant) }) > 0
    }
}

impl Drop for GLibVariant {
    fn drop(&mut self) {
        unsafe {
            glib_sys::g_variant_unref(self.variant);
        }
    }
}

impl PartialEq for GLibVariant {
    fn eq(&self, other: &Self) -> bool {
        (unsafe {
            glib_sys::g_variant_equal(
                self.variant as *const std::ffi::c_void,
                other.variant as *const std::ffi::c_void,
            )
        }) != 0
    }
}

impl core::fmt::Debug for GLibVariant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), core::fmt::Error> {
        if self.variant.is_null() {
            write!(f, "GLibVariant(NULL)")
        } else {
            unsafe {
                let s = glib_sys::g_variant_print(self.variant, 1);
                let out = write!(f, "{}", CStr::from_ptr(s).to_string_lossy());
                glib_sys::g_free(s as *mut std::ffi::c_void);
                out
            }
        }
    }
}

macro_rules! int_eq {
    ($i_ty:ty, $get:expr) => {
        impl PartialEq<GLibVariant> for $i_ty {
            fn eq(&self, rhs: &GLibVariant) -> bool {
                *self == unsafe { $get(rhs.variant) }
            }
        }
        impl PartialEq<GLibVariant> for [$i_ty] {
            fn eq(&self, rhs: &GLibVariant) -> bool {
                let mut len: usize = 0;
                let s = unsafe {
                    let p = glib_sys::g_variant_get_fixed_array(
                        rhs.variant,
                        &mut len as *mut usize,
                        std::mem::size_of::<$i_ty>(),
                    );
                    std::slice::from_raw_parts(p as *const $i_ty, len)
                };
                self == s
            }
        }
    };
}

int_eq!(u8, glib_sys::g_variant_get_byte);
int_eq!(u16, glib_sys::g_variant_get_uint16);
int_eq!(i16, glib_sys::g_variant_get_int16);
int_eq!(u32, glib_sys::g_variant_get_uint32);
int_eq!(i32, glib_sys::g_variant_get_int32);
int_eq!(u64, glib_sys::g_variant_get_uint64);
int_eq!(i64, glib_sys::g_variant_get_int64);

impl PartialEq<GLibVariant> for f64 {
    fn eq(&self, rhs: &GLibVariant) -> bool {
        let g = unsafe { glib_sys::g_variant_get_double(rhs.variant) };
        if self.is_nan() {
            // f64 only implements PartialEq and not Eq because NaN != NaN, but
            // for our purposes it does.
            g.is_nan()
        } else {
            *self == g
        }
    }
}

impl PartialEq<GLibVariant> for Str {
    fn eq(&self, rhs: &GLibVariant) -> bool {
        // Internal consistency:
        if rhs.is_normal_form() {
            assert_eq!(self.to_str().as_bytes(), self.as_bytes_non_conformant());
        }

        let mut len: usize = 0;
        let string = unsafe {
            CStr::from_ptr(glib_sys::g_variant_get_string(
                rhs.variant,
                &mut len as *mut usize,
            ))
        };
        self.to_str().len() == len && self.to_str().as_bytes() == string.to_bytes()
    }
}

impl<T: ?Sized + Cast> PartialEq<GLibVariant> for NonFixedWidthArray<T>
where
    T: PartialEq<GLibVariant>,
{
    fn eq(&self, rhs: &GLibVariant) -> bool {
        // Self-consistency check:
        let v: Vec<_> = self.iter().collect();
        assert_eq!(self.len(), v.len());
        for (n, elem) in v.iter().enumerate() {
            assert_eq!(**elem, self[n]);
        }

        let len = unsafe { glib_sys::g_variant_n_children(rhs.variant) };
        if len != self.len() {
            return false;
        }
        for (n, elem) in self.iter().enumerate() {
            let child = unsafe {
                GLibVariant::new_from_gvariant(glib_sys::g_variant_get_child_value(rhs.variant, n))
                    .unwrap()
            };
            if *elem != child {
                return false;
            }
        }
        true
    }
}

impl PartialEq<GLibVariant> for Bool {
    fn eq(&self, rhs: &GLibVariant) -> bool {
        let g = (unsafe { glib_sys::g_variant_get_boolean(rhs.variant) }) > 0;
        self.to_bool() == g
    }
}

impl<T: Cast> PartialEq<GLibVariant> for MaybeFixedSize<T>
where
    T: PartialEq<GLibVariant>,
{
    fn eq(&self, rhs: &GLibVariant) -> bool {
        let g =
            unsafe { GLibVariant::new_from_gvariant(glib_sys::g_variant_get_maybe(rhs.variant)) };
        let o = self.to_option();
        match (g, o) {
            (Some(g), Some(o)) => *o == g,
            (None, None) => true,
            _ => false,
        }
    }
}

impl<T: Cast + ?Sized> PartialEq<GLibVariant> for MaybeNonFixedSize<T>
where
    T: PartialEq<GLibVariant>,
{
    fn eq(&self, rhs: &GLibVariant) -> bool {
        let g =
            unsafe { GLibVariant::new_from_gvariant(glib_sys::g_variant_get_maybe(rhs.variant)) };
        let o = self.to_option();
        match (g, o) {
            (Some(g), Some(o)) => *o == g,
            (None, None) => true,
            _ => false,
        }
    }
}

impl PartialEq<GLibVariant> for Variant {
    fn eq(&self, rhs: &GLibVariant) -> bool {
        let g = unsafe {
            GLibVariant::new_from_gvariant(glib_sys::g_variant_get_variant(rhs.variant)).unwrap()
        };
        let g_ty = unsafe { CStr::from_ptr(glib_sys::g_variant_get_type_string(g.variant)) };
        let (ty, data) = self.split();
        ty == g_ty.to_bytes() && data.as_ref() as &[u8] == unsafe { g.get_data() }
    }
}

fn test_cmp<'data, T: gvariant::Marker>(
    m: T,
    data: &'data gvariant::aligned_bytes::AlignedSlice<<T::Type as AlignOf>::AlignOf>,
) where
    T::Type: PartialEq<GLibVariant> + Debug + 'data,
    &'data T::Type: SerializeTo<T::Type>,
{
    let gvt = GLibVariantType::new(&std::str::from_utf8(T::TYPESTR).unwrap());
    let gv = GLibVariant::new(data, &gvt);
    let v = m.cast(data);

    let mut reserialized = vec![];
    m.serialize(v, &mut reserialized).unwrap();
    let rs = copy_to_align(&reserialized);

    // Round-tripping serialization should give the same result:
    // println!("{:?} {:?}", reserialized.as_slice(), data.as_ref());
    if T::TYPESTR != b"d" {
        // f64 doesn't implement Eq because of NaNs, so we only do this check
        // for non-f64s
        assert_eq!(m.cast(rs.as_ref()), v);
    }

    //println!("{}: {:?} == {:?}", &std::str::from_utf8(T::TYPESTR).unwrap(), gv, v);
    if gv.is_normal_form() {
        assert_eq!(*v, gv);

        if data.len() >= 256 && data.len() < 512 && T::TYPESTR == b"aay" {
            // In theory there is exactly 1 normal form for data, but
            // `g_variant_is_normal_form` is buggy, so we can't do the check
            // from the else clause.  This performs a weaker version of that
            // check instead:
            assert!(GLibVariant::new(&reserialized, &gvt).is_normal_form());
        } else {
            assert_eq!(reserialized.as_slice(), data.as_ref());
        }
    } else {
        // Just do some consistency checks:
        #[allow(unused_must_use, clippy::unnecessary_operation)]
        {
            *v == gv;
        }
    }
}

fuzz_target!(|data: &[u8]| {
    let data_cow = copy_to_align::<A8>(data);
    let data = data_cow.as_ref();
    test_cmp(gv!("b"), data.as_aligned());
    test_cmp(gv!("y"), data.as_aligned());
    test_cmp(gv!("n"), data.as_aligned());
    test_cmp(gv!("q"), data.as_aligned());
    test_cmp(gv!("i"), data.as_aligned());
    test_cmp(gv!("u"), data.as_aligned());
    test_cmp(gv!("x"), data.as_aligned());
    test_cmp(gv!("t"), data.as_aligned());
    test_cmp(gv!("d"), data.as_aligned());
    test_cmp(gv!("s"), data.as_aligned());
    test_cmp(gv!("o"), data.as_aligned());
    test_cmp(gv!("g"), data.as_aligned());
    test_cmp(gv!("v"), data.as_aligned());
    test_cmp(gv!("ay"), data.as_aligned());
    test_cmp(gv!("ai"), data.as_aligned());
    test_cmp(gv!("as"), data.as_aligned());
    test_cmp(gv!("aay"), data.as_aligned());
    test_cmp(gv!("my"), data.as_aligned());
    test_cmp(gv!("mi"), data.as_aligned());
    test_cmp(gv!("ms"), data.as_aligned());
    test_cmp(gv!("may"), data.as_aligned());
});
