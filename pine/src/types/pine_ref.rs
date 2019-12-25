use super::traits::{Category, ComplexType, DataType, PineType, SecondType, SimpleType};
use std::cell::RefCell;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

// PineRef contain the pointer to pine type object.
#[derive(Debug)]
pub enum PineRef<'a> {
    Box(Box<dyn PineType<'a> + 'a>),
    Rc(Rc<RefCell<dyn PineType<'a> + 'a>>),
}

impl<'a> Clone for PineRef<'a> {
    fn clone(&self) -> Self {
        match *self {
            PineRef::Box(ref item) => item.copy(),
            PineRef::Rc(ref item) => PineRef::Rc(Rc::clone(item)),
        }
    }
}

impl<'a> PartialEq for PineRef<'a> {
    fn eq(&self, other: &PineRef<'a>) -> bool {
        match (self, other) {
            (PineRef::Box(ref item1), PineRef::Box(ref item2)) => PartialEq::eq(item1, item2),
            (PineRef::Rc(ref item1), PineRef::Rc(ref item2)) => {
                PartialEq::eq(&*item1.borrow(), &*item2.borrow())
            }
            _ => false,
        }
    }
}

impl<'a> PineRef<'a> {
    pub fn new<T: PineType<'a> + 'a>(item: T) -> PineRef<'a> {
        match item.category() {
            Category::Simple => PineRef::Box(Box::new(item)),
            Category::Complex => PineRef::Rc(Rc::new(RefCell::new(item))),
        }
    }

    pub fn new_box<T: PineType<'a> + SimpleType + 'a>(item: T) -> PineRef<'a> {
        PineRef::Box(Box::new(item))
    }

    pub fn new_rc<T: PineType<'a> + ComplexType + 'a>(item: T) -> PineRef<'a> {
        PineRef::Rc(Rc::new(RefCell::new(item)))
    }

    pub fn into_box(self) -> Box<dyn PineType<'a> + 'a> {
        match self {
            PineRef::Box(item) => item,
            _ => unreachable!(),
        }
    }

    pub fn into_rc(self) -> Rc<RefCell<dyn PineType<'a> + 'a>> {
        match self {
            PineRef::Rc(item) => item,
            _ => unreachable!(),
        }
    }

    pub fn as_ptr(&self) -> *const (dyn PineType<'a> + 'a) {
        match self {
            &PineRef::Box(ref item) => item.as_ref(),
            &PineRef::Rc(ref item) => unsafe { item.as_ptr().as_ref().unwrap() },
        }
    }

    pub fn copy_inner(&self) -> PineRef<'a> {
        match *self {
            PineRef::Box(ref item) => item.copy(),
            PineRef::Rc(ref item) => item.borrow().copy(),
        }
    }
}

impl<'a> PineType<'a> for PineRef<'a> {
    fn get_type(&self) -> (DataType, SecondType) {
        match *self {
            PineRef::Box(ref item) => item.get_type(),
            PineRef::Rc(ref item) => item.borrow().get_type(),
        }
    }

    fn copy(&self) -> PineRef<'a> {
        match *self {
            PineRef::Box(ref item) => item.copy(),
            PineRef::Rc(ref item) => PineRef::Rc(Rc::clone(item)),
        }
    }
}

impl<'a> Deref for PineRef<'a> {
    type Target = dyn PineType<'a> + 'a;

    fn deref(&self) -> &Self::Target {
        match self {
            &PineRef::Box(ref item) => item.as_ref(),
            &PineRef::Rc(ref item) => unsafe { item.as_ptr().as_ref().unwrap() },
        }
    }
}

impl<'a> DerefMut for PineRef<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self {
            &mut PineRef::Box(ref mut item) => item.as_mut(),
            &mut PineRef::Rc(ref item) => unsafe { item.as_ptr().as_mut().unwrap() },
        }
    }
}
