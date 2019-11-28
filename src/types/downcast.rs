use super::{ConvertErr, PineStaticType, PineType};

pub fn downcast<'a, T: PineStaticType + 'a>(
    item: Box<dyn PineType<'a> + 'a>,
) -> Result<Box<T>, ConvertErr> {
    if T::static_type() == item.get_type() {
        unsafe {
            let raw: *mut dyn PineType<'a> = Box::into_raw(item);
            Ok(Box::from_raw(raw as *mut T))
        }
    } else {
        Err(ConvertErr::NotCompatible)
    }
}

pub fn downcast_ref<'a, T: PineStaticType + 'a>(
    item: &'a mut dyn PineType<'a>,
) -> Result<&mut T, ConvertErr> {
    if T::static_type() == item.get_type() {
        unsafe {
            let raw: *mut dyn PineType<'a> = item;
            let t = raw as *mut T;
            Ok(t.as_mut().unwrap())
        }
    } else {
        Err(ConvertErr::NotCompatible)
    }
}
