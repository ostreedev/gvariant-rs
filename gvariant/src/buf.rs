use core::borrow::Borrow;
use core::fmt::Debug;
use core::ops::Deref;
use core::ops::DerefMut;

use crate::aligned_bytes::{
    to_alignedslice_unchecked, to_alignedslice_unchecked_mut, AlignedSlice, Alignment, AsAligned,
    A8,
};

/// A buffer where the pointed to data is guaranteed to have 8B alignment.  The
/// length of the data needn't be a multiple of 8.
///
/// This is implemented in terms of a [Vec<u8>].  Alignment is maintained by
/// adding padding at the beginning if the underlying allocation is not 8B
/// aligned to start with.  In practice the system allocator will always
/// provide 8B alignment anyway[^1] so conversion between [AlignedBuf] and
/// [Vec<u8>] will in practice be a noop.
///
/// [^1]: except when running under miri.  Please let me know if you come
///       across an architecture where this is not true.
pub struct AlignedBuf {
    data: Vec<u8>,
}

impl Debug for AlignedBuf {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let x: &[u8] = self.as_ref();
        std::fmt::Debug::fmt(x, f)
    }
}

impl AlignedBuf {
    /// Create new empty [AlignedBuf].  This does not allocate.
    pub fn new() -> Self {
        AlignedBuf { data: vec![] }
    }
    /// Run the provided function passing in (a potentially unaligned) vec
    /// to be modified in-place.
    ///
    /// Example: Write into an [AlignedBuf] from file:
    ///
    /// ```
    /// # fn moo(mut file: impl std::io::Read) -> std::io::Result<()> {
    /// use gvariant::aligned_bytes::AlignedBuf;
    /// let mut a = AlignedBuf::new();
    /// a.with_vec(|v| file.read_to_end(v))?;
    /// # let r : &[u8] = a.as_ref();
    /// # assert_eq!(r, b"Hello there");
    /// # Ok(())
    /// # }
    /// # moo(b"Hello there".as_ref()).unwrap();
    /// ```
    pub fn with_vec<T>(&mut self, f: impl FnOnce(&mut Vec<u8>) -> T) -> T {
        unalign(&mut self.data);
        // Panic safety: We align the data again in the
        // [RealignGuard::drop] so even if the user-provided callback
        // panics the data will be aligned.  This isn't a safety issue, but
        // is a correctness one.
        let guard = RealignGuard(&mut self.data);
        f(guard.0)
    }
}
/// Implementation detail of with_vec above
struct RealignGuard<'a>(&'a mut Vec<u8>);
impl<'a> Drop for RealignGuard<'a> {
    fn drop(&mut self) {
        align(self.0)
    }
}
impl Default for AlignedBuf {
    fn default() -> Self {
        Self::new()
    }
}
fn offset_to_align(v: &[u8]) -> usize {
    if v.is_empty() {
        0
    } else {
        let p = &v[0] as *const u8 as usize;
        let out = p.wrapping_neg() & 7;
        debug_assert!((p + out) & 7 == 0);
        out
    }
}
#[inline]
fn is_aligned(v: &[u8]) -> bool {
    v.is_empty() || (&v[0] as *const u8 as usize & 7) == 0
}
/// Shift the data in Vec to the right such that the data at the start of
/// the vec is now 8B aligned.
fn align(data: &mut Vec<u8>) {
    if !is_aligned(data) {
        align_cold(data)
    }
}
#[cold]
#[inline(never)]
fn align_cold(data: &mut Vec<u8>) {
    data.reserve(7);
    let off = offset_to_align(data);
    data.resize(data.len() + off, 0);
    data.rotate_right(off)
}
/// Shift the data in Vec to the left so as to undo the effects of [wind].
fn unalign(data: &mut Vec<u8>) {
    if !is_aligned(data) {
        unalign_cold(data)
    }
}
#[cold]
#[inline(never)]
fn unalign_cold(data: &mut Vec<u8>) {
    let off = offset_to_align(data);
    data.rotate_left(off);
    data.truncate(data.len() - off);
}
impl From<Vec<u8>> for AlignedBuf {
    fn from(mut data: Vec<u8>) -> Self {
        align(&mut data);
        Self { data }
    }
}
impl From<AlignedBuf> for Vec<u8> {
    fn from(mut b: AlignedBuf) -> Self {
        unalign(&mut b.data);
        b.data
    }
}

impl Deref for AlignedBuf {
    type Target = AlignedSlice<A8>;

    fn deref(&self) -> &Self::Target {
        let s = &self.data[offset_to_align(&self.data)..];
        unsafe {
            // SAFETY: `s` is guaranteed to be 8B aligned because that's what `offset_to_align` is
            // for.
            to_alignedslice_unchecked(s)
        }
    }
}
impl DerefMut for AlignedBuf {
    fn deref_mut(&mut self) -> &mut Self::Target {
        let off = offset_to_align(&self.data);
        let s = &mut self.data[off..];
        unsafe {
            // SAFETY: `s` is guaranteed to be 8B aligned because that's what `offset_to_align` is
            // for.
            to_alignedslice_unchecked_mut(s)
        }
    }
}

impl<A: Alignment> Borrow<AlignedSlice<A>> for AlignedBuf {
    fn borrow(&self) -> &AlignedSlice<A> {
        (**self).as_aligned()
    }
}

#[cfg(test)]
mod test {
    use super::AlignedBuf;

    fn read_to_buf(mut file: impl std::io::Read) -> AlignedBuf {
        let mut v = vec![];
        file.read_to_end(&mut v).unwrap();
        v.into()
    }

    #[test]
    fn test_read_to_buf() {
        let mut d: Vec<u8> = vec![0; 16384];
        for x in 0..16384 {
            d[x] = (x % 256) as u8;
        }

        let s = read_to_buf(b"".as_ref());
        assert_eq!(**s, *b"");

        let s = read_to_buf(&d[..12]);
        assert_eq!(**s, d[..12]);

        let s = read_to_buf(&d[..8000]);
        assert_eq!(**s, d[..8000]);

        let s = read_to_buf(d.as_slice());
        assert_eq!(&**s, d.as_slice());
    }
}
