#![feature(specialization, associated_type_defaults, concat_idents)]

use std::iter::ExactSizeIterator;

pub trait RangeCmp<Y, RHS: ?Sized = Self> {
    type Item = Y;

    fn eq_to(self, t: RHS) -> bool;
}

fn cmp_by_next<T: Eq>(mut a: impl Iterator<Item = T>, mut b: impl Iterator<Item = T>) -> bool {
    loop {
        match (a.next(), b.next()) {
            (None, None) => return true,
            (None, _) | (_, None) => return false,
            (Some(x), Some(y)) if x != y => return false,
            (Some(_), Some(_)) => {}
        }
    }
}

impl<Y, RHS, LHS> RangeCmp<Y, RHS> for LHS
where
    Y: Eq,
    RHS: IntoIterator<Item = Y>,
    LHS: IntoIterator<Item = Y>,
{
    default fn eq_to(self, other: RHS) -> bool {
        cmp_by_next(self.into_iter(), other.into_iter())
    }
}

impl<Y, RHS, LHS> RangeCmp<Y, RHS> for LHS
where
    Y: Eq,
    RHS: Iterator<Item = Y>,
    LHS: Iterator<Item = Y>,
{
    default fn eq_to(self, other: RHS) -> bool {
        cmp_by_next(self, other)
    }
}

impl<Y, RHS, LHS> RangeCmp<Y, RHS> for LHS
where
    Y: Eq,
    RHS: ExactSizeIterator<Item = Y>,
    LHS: ExactSizeIterator<Item = Y>,
{
    fn eq_to(self, other: RHS) -> bool {
        if self.len() != other.len() {
            return false;
        }

        self.zip(other).all(|(a, b)| a == b)
    }
}

#[macro_export]
macro_rules! impl_eq_to_for_pod_slice {
    ($head:ident, $($tail:ident),*) => {
        impl_eq_to_for_pod_slice!($head);
        impl_eq_to_for_pod_slice!($($tail),+);
    };
    ($t:ident) => {
        pub(crate) mod $t {
            use super::*;

            pub(crate) struct WrappedSlice<'a>(&'a [$t]);

            impl<'a> From<&'a [$t]> for WrappedSlice<'a> {
                fn from(slice: &'a [$t]) -> Self {
                    Self(slice)
                }
            }

            #[allow(unused)]
            pub(crate) fn wrap<'a>(slice: &'a [$t]) -> WrappedSlice<'a> {
                slice.into()
            }

            // hacky way for pretending to be more precize type
            // https://github.com/rust-lang/rust/issues/45542
            impl<'a> IntoIterator for WrappedSlice<'a> {
                type Item = &'a $t;
                type IntoIter = std::slice::Iter<'a, $t>;

                fn into_iter(self) -> Self::IntoIter {
                    self.0.into_iter()
                }
            }

            impl<'a, 'b> RangeCmp<$t, WrappedSlice<'a>> for WrappedSlice<'b> {
                fn eq_to(self, other: WrappedSlice<'a>) -> bool {
                    let (a, b) = (self.0.as_ref(), other.0.as_ref());
                    if a.len() != b.len() {
                        return false;
                    }

                    0 == unsafe {
                        libc::memcmp(a.as_ptr() as *const _, b.as_ptr() as *const _, a.len())
                    }
                }
            }
        }
    };
}

impl_eq_to_for_pod_slice!(
    bool, char, u8, u16, u32, u64, u128, usize, i8, i16, i32, i64, i128, isize
);

#[cfg(test)]
mod tests {
    use super::*;

    fn exact_iter<Y>(
        t: impl IntoIterator<Item = Y, IntoIter = impl ExactSizeIterator<Item = Y>>,
    ) -> impl ExactSizeIterator<Item = Y> {
        t.into_iter()
    }

    macro_rules! test_pair {
        ($is_eq:expr, $a:expr, $b:expr) => {{
            assert_eq!($is_eq, $a.clone().eq_to($b.clone()));
            assert_eq!($is_eq, exact_iter($a.clone()).eq_to(exact_iter($b.clone())));
        }};
    }

    macro_rules! test_slices {
        ($is_eq:expr, $t:ident, $a:expr, $b:expr) => {
            assert_eq!(
                $is_eq,
                RangeCmp::<$t, $t::WrappedSlice>::eq_to($t::wrap($a), $t::wrap($b))
            );
        };
    }

    mod simple {
        use super::super::*;
        use super::exact_iter;

        #[test]
        fn iters_cmp() {
            test_pair!(true, vec![1, 2, 3], vec![1, 2, 3]);
            test_pair!(true, Vec::<()>::new(), vec![]);
            test_pair!(false, Vec::<()>::new(), vec![()]);
            test_pair!(false, vec![1, 2], vec![1, 3]);
            test_pair!(false, vec![1, 2, 3], vec![1, 2]);
        }

        #[test]
        fn slices_cmp() {
            test_slices!(true, u8, &[1, 2, 3], &[1, 2, 3]);
            test_slices!(true, u8, &[3, 2, 1], &[3, 2, 1]);
            test_slices!(true, u8, &[], &[]);
            test_slices!(false, u8, &[], &[1]);
            test_slices!(false, u8, &[1, 2], &[]);
            test_slices!(false, u8, &[3, 1], &[3, 2, 1]);
            test_slices!(false, u8, &[3, 2, 1], &[3, 1]);
        }
    }

    macro_rules! vec_cmp_fn {
        ($ty:ident, $($tys:ident),*) => {
            vec_cmp_fn!($ty);
            vec_cmp_fn!($($tys),*);
        };
        ($ty:ident) => {
            mod $ty {
                use super::super::*;
                use super::exact_iter;
                use quickcheck_macros::quickcheck;

                #[quickcheck]
                fn vec_cmp(a: Vec<$ty>, b: Vec<$ty>) {
                    let is_eq = a == b;
                    test_pair!(is_eq, a, b);
                    test_slices!(is_eq, $ty, &a, &b);
                }
            }
        };
    }

    vec_cmp_fn!(bool, char, u8, u16, u32, u64, u128, usize, i8, i16, i32, i64, i128, isize);
}
