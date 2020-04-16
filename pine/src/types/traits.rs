use super::downcast::downcast_ref;
use super::error::RuntimeErr;
use super::pine_ref::PineRef;
use super::ref_data::RefData;
use crate::runtime::Ctx;
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
    Callable, // The callable function defined by the library
    CallableFactory,
    Function, // The function defined by the script
    Object,
    CallableObject,
    Evaluate, // The variable need lazy evaluate.
    EvaluateFactory,
    CallableEvaluate,       // variable and object
    CallableObjectEvaluate, // function + object + variable
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
        use super::{Bool, Color, Float, Int, Series, Tuple};

        match self.get_type() {
            (DataType::Int, SecondType::Simple) => downcast_ref::<Int>(self).unwrap().fmt(f),
            (DataType::Float, SecondType::Simple) => downcast_ref::<Float>(self).unwrap().fmt(f),
            (DataType::Bool, SecondType::Simple) => downcast_ref::<Bool>(self).unwrap().fmt(f),
            (DataType::NA, SecondType::Simple) => write!(f, "NA"),
            (DataType::String, SecondType::Simple) => downcast_ref::<String>(self).unwrap().fmt(f),
            (DataType::Color, SecondType::Simple) => downcast_ref::<Color>(self).unwrap().fmt(f),

            (DataType::Int, SecondType::Series) => {
                downcast_ref::<Series<Int>>(self).unwrap().fmt(f)
            }
            (DataType::Float, SecondType::Series) => {
                downcast_ref::<Series<Float>>(self).unwrap().fmt(f)
            }
            (DataType::Bool, SecondType::Series) => {
                downcast_ref::<Series<Bool>>(self).unwrap().fmt(f)
            }
            (DataType::NA, SecondType::Series) => write!(f, "series(NA)"),
            (DataType::String, SecondType::Series) => {
                downcast_ref::<Series<String>>(self).unwrap().fmt(f)
            }
            (DataType::Color, SecondType::Series) => {
                downcast_ref::<Series<Color>>(self).unwrap().fmt(f)
            }

            (DataType::Tuple, SecondType::Simple) => downcast_ref::<Tuple>(self).unwrap().fmt(f),

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
        use super::{Bool, Color, Float, Int, Series, NA};

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
            (DataType::NA, SecondType::Simple) => downcast_ref::<NA>(self)
                .unwrap()
                .eq(downcast_ref::<NA>(other).unwrap()),
            (DataType::String, SecondType::Simple) => downcast_ref::<String>(self)
                .unwrap()
                .eq(downcast_ref::<String>(other).unwrap()),
            (DataType::Color, SecondType::Simple) => downcast_ref::<Color>(self)
                .unwrap()
                .eq(downcast_ref::<Color>(other).unwrap()),

            (DataType::Int, SecondType::Series) => downcast_ref::<Series<Int>>(self)
                .unwrap()
                .eq(downcast_ref::<Series<Int>>(other).unwrap()),
            (DataType::Float, SecondType::Series) => downcast_ref::<Series<Float>>(self)
                .unwrap()
                .eq(downcast_ref::<Series<Float>>(other).unwrap()),
            (DataType::Bool, SecondType::Series) => downcast_ref::<Series<Bool>>(self)
                .unwrap()
                .eq(downcast_ref::<Series<Bool>>(other).unwrap()),
            (DataType::NA, SecondType::Series) => downcast_ref::<Series<NA>>(self)
                .unwrap()
                .eq(downcast_ref::<Series<NA>>(other).unwrap()),
            (DataType::String, SecondType::Series) => downcast_ref::<Series<String>>(self)
                .unwrap()
                .eq(downcast_ref::<Series<String>>(other).unwrap()),
            (DataType::Color, SecondType::Series) => downcast_ref::<Series<Color>>(self)
                .unwrap()
                .eq(downcast_ref::<Series<Color>>(other).unwrap()),
            _ => false,
        }
    }
}

pub trait PineClass<'a> {
    fn custom_type(&self) -> &str;

    fn get(&self, context: &mut dyn Ctx<'a>, name: &str) -> Result<PineRef<'a>, RuntimeErr>;

    fn set(&self, _name: &str, _property: PineRef<'a>) -> Result<(), RuntimeErr> {
        Err(RuntimeErr::NotSupportOperator)
    }

    fn copy(&self) -> Box<dyn PineClass<'a> + 'a>;
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
