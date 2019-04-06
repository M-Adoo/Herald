//! Type of witch can identified as a UID

use std::mem;
use std::raw::TraitObject;

pub trait Ident {
    // get the uid of this object, the uid assigned on the first call.
    fn uid(&self) -> UID
    where
        Self: Sized,
    {
        UID::from(self as &dyn Ident)
    }
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct UID(usize, usize);

impl From<&dyn Ident> for UID {
    fn from(ident: &dyn Ident) -> Self {
        let raw_obj: TraitObject = unsafe { mem::transmute(ident) };
        UID(raw_obj.data as usize, raw_obj.vtable as usize)
    }
}

impl UID {
    /// unsafe downcast an UID to a reference of `T`, this is dangerous
    /// use it only you can promise this is an uid of type `T`, and the host
    /// object of the uid has a valid lifetime.
    /// notice: always avoid to use this method directly.
    #[inline(always)]
    pub unsafe fn downcast_ref<T>(&self) -> &T {
        &*(self.0 as *mut T)
    }

    /// unsafe downcast an UID to a reference of `T`, this is dangerous
    /// use it only you can promise this is an uid of type `T`, and the host
    /// object of the uid has a valid lifetime.
    /// notice: always avoid to use this method directly.
    #[inline(always)]
    pub unsafe fn downcast_mut_ref<T>(&self) -> &mut T {
        &mut *(self.0 as *mut T)
    }
}

#[cfg(test)]
mod uid_test {
    use super::UID;
    use crate::prelude::*;

    struct Button {
        size: usize,
    }

    impl Ident for Button {}

    #[test]
    fn key_eq() {
        let btn = Button { size: 1 };
        let key1 = UID::from(&btn as &dyn Ident);
        let key2 = UID::from(&btn as &dyn Ident);
        let key3 = UID::from(&btn as &dyn Ident);
        assert_eq!(key1, key2);
        assert_eq!(key2, key3);
        assert_eq!(key3, key1);
    }

    #[test]
    fn downcast() {
        let btn = Button { size: 1 };
        unsafe {
            let btn_ref: &Button = btn.uid().downcast_ref();
            assert_eq!(btn.size, 1);
        }
    }
}
