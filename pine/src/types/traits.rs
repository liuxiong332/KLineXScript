use super::downcast::downcast_ref;
use super::error::RuntimeErr;
use super::pine_ref::PineRef;
use super::ref_data::RefData;
use std::fmt;
use std::hash::{Hash, Hasher};

#[derive(Debug, PartialEq)]
pub enum SecondType {
    Simple,
    Array,
    Series,
}

#[derive(Debug, PartialEq)]
pub enum DataType {
    Float,
    Int,
    Bool,
    Color,
    String,
    Line,
    Label,
    NA,
    PineVar,
    Tuple,
    Callable,
    Function,
    Object,
    Evaluate, // The variable need lazy evaluate.
}

#[derive(Debug, PartialEq)]
pub enum Category {
    Simple,
    Complex,
}

pub trait PineStaticType {
    fn static_type() -> (DataType, SecondType);
}

pub trait PineType<'a> {
    fn get_type(&self) -> (DataType, SecondType);

    fn category(&self) -> Category {
        Category::Simple
    }

    fn copy(&self) -> PineRef<'a>;
}

impl<'a> fmt::Debug for dyn PineType<'a> + 'a {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use super::{Float, Int, Series};

        match self.get_type() {
            (DataType::Int, SecondType::Simple) => downcast_ref::<Int>(self).unwrap().fmt(f),
            (DataType::Float, SecondType::Simple) => downcast_ref::<Float>(self).unwrap().fmt(f),
            (DataType::NA, SecondType::Simple) => write!(f, "NA"),
            (DataType::Int, SecondType::Series) => {
                downcast_ref::<Series<Int>>(self).unwrap().fmt(f)
            }
            (DataType::Float, SecondType::Series) => {
                downcast_ref::<Series<Float>>(self).unwrap().fmt(f)
            }
            (DataType::Int, SecondType::Array) => downcast_ref::<Vec<Int>>(self).unwrap().fmt(f),
            (DataType::Float, SecondType::Array) => {
                downcast_ref::<Vec<Float>>(self).unwrap().fmt(f)
            }
            _ => write!(f, "Unkown type"),
        }
    }
}

impl<'a> PartialEq for dyn PineType<'a> + 'a {
    fn eq(&self, other: &(dyn PineType<'a> + 'a)) -> bool {
        use super::{Bool, Float, Int, Series};

        match self.get_type() {
            (DataType::Int, SecondType::Simple) => downcast_ref::<Int>(self)
                .unwrap()
                .eq(downcast_ref::<Int>(other).unwrap()),
            (DataType::Float, SecondType::Simple) => downcast_ref::<Float>(self)
                .unwrap()
                .eq(downcast_ref::<Float>(other).unwrap()),
            (DataType::Bool, SecondType::Simple) => downcast_ref::<Bool>(self)
                .unwrap()
                .eq(downcast_ref::<Bool>(other).unwrap()),
            (DataType::Int, SecondType::Series) => downcast_ref::<Series<Int>>(self)
                .unwrap()
                .eq(downcast_ref::<Series<Int>>(other).unwrap()),
            (DataType::Float, SecondType::Series) => downcast_ref::<Series<Float>>(self)
                .unwrap()
                .eq(downcast_ref::<Series<Float>>(other).unwrap()),
            (DataType::Bool, SecondType::Series) => downcast_ref::<Series<Bool>>(self)
                .unwrap()
                .eq(downcast_ref::<Series<Bool>>(other).unwrap()),
            _ => false,
        }
    }
}

pub trait PineClass<'a> {
    fn custom_type(&self) -> &str;

    fn get(&self, name: &str) -> Result<PineRef<'a>, RuntimeErr>;

    fn set(&self, _name: &str, _property: PineRef<'a>) -> Result<(), RuntimeErr> {
        Err(RuntimeErr::NotSupportOperator)
    }

    fn copy(&self) -> PineRef<'a>;
}

impl<'a> fmt::Debug for dyn PineClass<'a> + 'a {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Pine Class {}", self.custom_type())
    }
}

impl<'a> PartialEq for dyn PineClass<'a> + 'a {
    fn eq(&self, other: &(dyn PineClass<'a> + 'a)) -> bool {
        if self.custom_type() == other.custom_type() {
            true
        } else {
            false
        }
    }
}

pub trait PineFrom<'a, D: 'a + PartialEq + fmt::Debug> {
    // The user force type cast
    fn explicity_from(t: PineRef<'a>) -> Result<RefData<D>, RuntimeErr> {
        Self::implicity_from(t)
    }

    // Create this type from the source type for auto cast
    fn implicity_from(_t: PineRef<'a>) -> Result<RefData<D>, RuntimeErr> {
        unimplemented!()
    }
}

pub trait Negative<D> {
    fn negative(self) -> D;
}

pub trait Arithmetic {
    fn add(self, other: Self) -> Self;

    fn minus(self, other: Self) -> Self;

    fn mul(self, other: Self) -> Self;

    fn div(self, other: Self) -> Self;

    fn rem(self, other: Self) -> Self;
}

pub trait SimpleType {}

pub trait ComplexType {}
