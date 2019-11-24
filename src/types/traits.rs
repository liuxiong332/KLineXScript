use super::error::SeriesErr;

#[derive(Debug, PartialEq)]
pub enum SecondType {
    Simple,
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
    Object,
}

#[derive(Debug, PartialEq)]
pub enum ConvertErr {
    NotCompatible,
    NotValidParam,
    NotSupportOperator,
}

pub trait PineStaticType {
    fn static_type() -> (DataType, SecondType);
}

pub trait PineType<'a> {
    // convert this type to another type by (data_type, second_type)
    // fn into(self: Box<Self>, pri_type: &DataType) -> Result<Box<dyn PineType<'a>>, ConvertErr> {
    //     Err(ConvertErr::NotCompatible)
    // }
    fn get_type(&self) -> (DataType, SecondType);
}

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

pub trait PineClass<'a> {
    fn custom_type(&self) -> &str;

    fn get(&self, name: &str) -> Result<Box<dyn PineType<'a> + 'a>, ConvertErr>;

    fn set(&self, name: &str, property: Box<dyn PineType<'a> + 'a>) -> Result<(), ConvertErr>;
}

pub trait PineFrom<'a> {
    // The user force type cast
    fn explicity_from(
        t: Box<dyn PineType<'a> + 'a>,
    ) -> Result<Box<dyn PineType<'a> + 'a>, ConvertErr> {
        Self::implicity_from(t)
    }

    // Create this type from the source type for auto cast
    fn implicity_from(
        t: Box<dyn PineType<'a> + 'a>,
    ) -> Result<Box<dyn PineType<'a> + 'a>, ConvertErr> {
        Err(ConvertErr::NotCompatible)
    }
}
