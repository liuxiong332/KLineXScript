use super::error::RuntimeErr;

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
}

pub trait PineStaticType {
    fn static_type() -> (DataType, SecondType);
}

pub trait PineType<'a> {
    // convert this type to another type by (data_type, second_type)
    // fn into(self: Box<Self>, pri_type: &DataType) -> Result<Box<dyn PineType<'a>>, RuntimeErr> {
    //     Err(RuntimeErr::NotCompatible)
    // }
    fn get_type(&self) -> (DataType, SecondType);

    fn copy(&self) -> Box<dyn PineType<'a> + 'a>;
}

pub trait PineClass<'a> {
    fn custom_type(&self) -> &str;

    fn get(&self, name: &str) -> Result<Box<dyn PineType<'a> + 'a>, RuntimeErr>;

    fn set(&self, _name: &str, _property: Box<dyn PineType<'a> + 'a>) -> Result<(), RuntimeErr> {
        Err(RuntimeErr::NotSupportOperator)
    }

    fn copy(&self) -> Box<dyn PineType<'a> + 'a>;
}

pub trait PineFrom<'a, D: 'a> {
    // The user force type cast
    fn explicity_from(t: Box<dyn PineType<'a> + 'a>) -> Result<Box<D>, RuntimeErr> {
        Self::implicity_from(t)
    }

    // Create this type from the source type for auto cast
    fn implicity_from(_t: Box<dyn PineType<'a> + 'a>) -> Result<Box<D>, RuntimeErr> {
        Err(RuntimeErr::NotCompatible)
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
