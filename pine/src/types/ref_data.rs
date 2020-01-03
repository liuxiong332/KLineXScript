use super::pine_ref::PineRef;
use super::traits::{Category, ComplexType, PineType, SimpleType};
use std::cell::RefCell;
use std::fmt::Debug;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

#[derive(PartialEq, Debug)]
pub enum RefData<T: PartialEq + Debug> {
    Box(Box<T>),
    Rc(Rc<RefCell<T>>),
}

impl<T: PartialEq + Debug + SimpleType> RefData<T> {
    pub fn new_box(item: T) -> RefData<T> {
        RefData::Box(Box::new(item))
    }

    pub fn into_box(self) -> Box<T> {
        match self {
            RefData::Box(item) => item,
            _ => unreachable!(),
        }
    }
}

impl<T: PartialEq + Debug + ComplexType> RefData<T> {
    pub fn new_rc(item: T) -> RefData<T> {
        RefData::Rc(Rc::new(RefCell::new(item)))
    }

    pub fn into_rc(self) -> Rc<RefCell<T>> {
        match self {
            RefData::Rc(item) => item,
            _ => unreachable!(),
        }
    }
}

impl<'a, T: PartialEq + Debug + PineType<'a> + 'a> RefData<T> {
    pub fn new(item: T) -> RefData<T> {
        match item.category() {
            Category::Simple => RefData::Box(Box::new(item)),
            Category::Complex => RefData::Rc(Rc::new(RefCell::new(item))),
        }
    }
}

impl<T: PartialEq + Debug + Clone> RefData<T> {
    // Clone this RefData, MUST invoke with associated method as RefData::clone
    pub fn clone(orig: &RefData<T>) -> RefData<T> {
        match *orig {
            RefData::Box(ref item) => RefData::Box(Box::new(T::clone(item))),
            RefData::Rc(ref item) => RefData::Rc(Rc::clone(item)),
        }
    }

    // Move the inner T data from self, for Rc will clone the data.
    pub fn into_inner(self) -> T {
        match self {
            RefData::Box(item) => *item,
            RefData::Rc(ref item) => item.borrow().clone(),
        }
    }

    pub fn clone_inner(&self) -> T {
        match self {
            RefData::Box(item) => (&**item).clone(),
            RefData::Rc(item) => item.borrow().clone(),
        }
    }
}

impl<'a, T: PartialEq + Debug + PineType<'a> + 'a> RefData<T> {
    // Transform the RefData to PineRef type.
    pub fn into_pf(self) -> PineRef<'a> {
        match self {
            RefData::Box(item) => PineRef::Box(item),
            RefData::Rc(item) => PineRef::Rc(item),
        }
    }
}

impl<T: PartialEq + Debug> Deref for RefData<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        match self {
            &RefData::Box(ref item) => item.as_ref(),
            &RefData::Rc(ref item) => unsafe { item.as_ptr().as_ref().unwrap() },
        }
    }
}

impl<T: PartialEq + Debug> DerefMut for RefData<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self {
            &mut RefData::Box(ref mut item) => item.as_mut(),
            &mut RefData::Rc(ref item) => unsafe { item.as_ptr().as_mut().unwrap() },
        }
    }
}
