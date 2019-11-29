use super::{PineStaticType, PineType, RuntimeErr};

pub fn downcast<'a, T: PineStaticType + 'a>(
    item: Box<dyn PineType<'a> + 'a>,
) -> Result<Box<T>, RuntimeErr> {
    if T::static_type() == item.get_type() {
        unsafe {
            let raw: *mut dyn PineType<'a> = Box::into_raw(item);
            Ok(Box::from_raw(raw as *mut T))
        }
    } else {
        Err(RuntimeErr::NotCompatible)
    }
}

pub fn downcast_ref<'a, 'b, T: PineStaticType + 'a>(
    item: &'b dyn PineType<'a>,
) -> Result<&'b T, RuntimeErr> {
    if T::static_type() == item.get_type() {
        unsafe {
            let raw: *const dyn PineType<'a> = item;
            let t = raw as *const T;
            Ok(t.as_ref().unwrap())
        }
    } else {
        Err(RuntimeErr::NotCompatible)
    }
}

pub fn downcast_mut<'a, 'b, T: PineStaticType + 'a>(
    item: &'b mut dyn PineType<'a>,
) -> Result<&'b mut T, RuntimeErr> {
    if T::static_type() == item.get_type() {
        unsafe {
            let raw: *mut dyn PineType<'a> = item;
            let t = raw as *mut T;
            Ok(t.as_mut().unwrap())
        }
    } else {
        Err(RuntimeErr::NotCompatible)
    }
}
