use super::{PineRef, PineStaticType, PineType, RefData, RuntimeErr};
use std::cell::RefCell;
use std::fmt::Debug;
use std::rc::Rc;

pub fn downcast_pf<'a, T>(item: PineRef<'a>) -> Result<RefData<T>, RuntimeErr>
where
    T: PineStaticType + PartialEq + Debug + 'a,
{
    match item {
        PineRef::Box(item) => Ok(RefData::Box(downcast::<'a, T>(item)?)),
        PineRef::Rc(item) => Ok(RefData::Rc(downcast_rc::<'a, T>(item)?)),
    }
}

pub fn downcast_pf_ref<'a, 'b, T>(item: &'b PineRef<'a>) -> Result<&'b T, RuntimeErr>
where
    T: PineStaticType + PartialEq + Debug + 'a,
{
    match *item {
        PineRef::Box(ref item) => downcast_ref(&**item),
        PineRef::Rc(ref item) => unsafe {
            let ptr = item.as_ptr() as *mut T;
            Ok(ptr.as_ref().unwrap())
        },
    }
}

pub fn downcast_pf_mut<'a, 'b, T>(item: &'b mut PineRef<'a>) -> Result<&'b mut T, RuntimeErr>
where
    T: PineStaticType + PartialEq + Debug + 'a,
{
    match *item {
        PineRef::Box(ref mut item) => downcast_mut(&mut **item),
        PineRef::Rc(ref item) => unsafe {
            let ptr = item.as_ptr() as *mut T;
            Ok(ptr.as_mut().unwrap())
        },
    }
}

fn downcast_err<'a, T: PineStaticType + 'a>(item: &(dyn PineType<'a> + 'a)) -> RuntimeErr {
    RuntimeErr::NotCompatible(format!(
        "downcast from {:?} to {:?} is not allowed",
        item.get_type(),
        T::static_type()
    ))
}

pub fn downcast<'a, T: PineStaticType + 'a>(
    item: Box<dyn PineType<'a> + 'a>,
) -> Result<Box<T>, RuntimeErr> {
    if T::static_type() == item.get_type() {
        unsafe {
            let raw: *mut dyn PineType<'a> = Box::into_raw(item);
            Ok(Box::from_raw(raw as *mut T))
        }
    } else {
        Err(downcast_err::<T>(&*item))
    }
}

pub fn downcast_rc<'a, T: PineStaticType + 'a>(
    item: Rc<RefCell<dyn PineType<'a> + 'a>>,
) -> Result<Rc<RefCell<T>>, RuntimeErr> {
    if T::static_type() == item.borrow().get_type() {
        unsafe {
            let raw: *const RefCell<dyn PineType<'a> + 'a> = Rc::into_raw(item);
            Ok(Rc::from_raw(raw as *const RefCell<T>))
        }
    } else {
        Err(downcast_err::<T>(&*item.borrow()))
    }
}

pub fn downcast_ref<'a, 'b, T: PineStaticType + 'a>(
    item: &'b (dyn PineType<'a> + 'a),
) -> Result<&'b T, RuntimeErr> {
    if T::static_type() == item.get_type() {
        unsafe {
            let raw: *const dyn PineType<'a> = item;
            let t = raw as *const T;
            Ok(t.as_ref().unwrap())
        }
    } else {
        Err(downcast_err::<T>(item))
    }
}

pub fn downcast_mut<'a, 'b, T: PineStaticType + 'a>(
    item: &'b mut (dyn PineType<'a> + 'a),
) -> Result<&'b mut T, RuntimeErr> {
    if T::static_type() == item.get_type() {
        unsafe {
            let raw: *mut dyn PineType<'a> = item;
            let t = raw as *mut T;
            Ok(t.as_mut().unwrap())
        }
    } else {
        Err(downcast_err::<T>(item))
    }
}
