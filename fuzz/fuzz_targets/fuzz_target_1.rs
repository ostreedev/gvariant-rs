#![no_main]

use glib_sys;
use gvariant::{
    aligned_bytes::copy_to_align, gv, Bool, Cast, Marker, MaybeFixedSize, MaybeNonFixedSize,
    NonFixedWidthArray, Str, Variant,
};
use libfuzzer_sys::fuzz_target;
use std::ffi::CStr;

struct GLibVariant {
    bytes: *mut glib_sys::GBytes,
    variant_type: *mut glib_sys::GVariantType,
    variant: *mut glib_sys::GVariant,
}

impl GLibVariant {
    fn new(data: &[u8], ty: &str) -> GLibVariant {
        unsafe {
            let bytes = glib_sys::g_bytes_new(data.as_ptr() as *const std::ffi::c_void, data.len());
            let gvtype = glib_sys::g_variant_type_new(ty.as_ptr() as *const i8);
            let glib_variant = glib_sys::g_variant_new_from_bytes(gvtype, bytes, 0);
            GLibVariant {
                bytes: bytes,
                variant_type: gvtype,
                variant: glib_variant,
            }
        }
    }
    unsafe fn new_from_gvariant(variant: *mut glib_sys::GVariant) -> GLibVariant {
        GLibVariant {
            bytes: 0 as *mut glib_sys::GBytes,
            variant_type: 0 as *mut glib_sys::GVariantType,
            variant: variant,
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
            if !self.variant.is_null() {
                glib_sys::g_variant_unref(self.variant);
            }
            if !self.variant_type.is_null() {
                glib_sys::g_variant_type_free(self.variant_type);
            }
            if !self.bytes.is_null() {
                glib_sys::g_bytes_unref(self.bytes);
            }
        }
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
        assert_eq!(self.to_cstr().to_bytes(), self.to_bytes());

        let mut len: usize = 0;
        let string = unsafe {
            CStr::from_ptr(glib_sys::g_variant_get_string(
                rhs.variant,
                &mut len as *mut usize,
            ))
        };
        self.to_bytes().len() == len && self.to_bytes() == string.to_bytes()
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
        if let Some(val) = o {
            if g.variant.is_null() {
                false
            } else {
                *val == g
            }
        } else {
            g.variant.is_null()
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
        if let Some(val) = o {
            if g.variant.is_null() {
                false
            } else {
                *val == g
            }
        } else {
            g.variant.is_null()
        }
    }
}

impl PartialEq<GLibVariant> for Variant {
    fn eq(&self, rhs: &GLibVariant) -> bool {
        let g =
            unsafe { GLibVariant::new_from_gvariant(glib_sys::g_variant_get_variant(rhs.variant)) };
        let g_ty = unsafe { CStr::from_ptr(glib_sys::g_variant_get_type_string(g.variant)) };
        let (ty, data) = self.split();
        ty == g_ty.to_bytes() && data.as_ref() as &[u8] == unsafe { g.get_data() }
    }
}

macro_rules! test_cmp {
    ($ty:literal, $data:expr) => {
        let gv = GLibVariant::new($data, concat!($ty, "\0"));
        let data = copy_to_align($data);
        let v = gv!($ty).cast(data.as_ref());
        //println!("{:?} == {:?}", gv, v);
        if gv.is_normal_form() {
            assert_eq!(*v, gv);
        } else {
            // Just do some consistency checks:
            #[allow(unused_must_use)]
            {
                *v == gv;
            }
        }
    };
}

fuzz_target!(|data: &[u8]| {
    test_cmp!("b", data);
    test_cmp!("y", data);
    test_cmp!("n", data);
    test_cmp!("q", data);
    test_cmp!("i", data);
    test_cmp!("u", data);
    test_cmp!("x", data);
    test_cmp!("t", data);
    test_cmp!("d", data);
    test_cmp!("s", data);
    test_cmp!("o", data);
    test_cmp!("g", data);
    test_cmp!("v", data);
    test_cmp!("ay", data);
    test_cmp!("ai", data);
    test_cmp!("as", data);
    test_cmp!("aay", data);
    test_cmp!("my", data);
    test_cmp!("mi", data);
    test_cmp!("ms", data);
});
