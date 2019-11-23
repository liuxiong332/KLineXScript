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
}

#[derive(Debug, PartialEq)]
pub enum ConvertErr {
    NotCompatible,
    NotValidParam,
    NotSupportOperator,
}

pub trait PineStaticType {
    fn get_type() -> (DataType, SecondType);
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
    if T::get_type() == item.get_type() {
        unsafe {
            let raw: *mut dyn PineType<'a> = Box::into_raw(item);
            Ok(Box::from_raw(raw as *mut T))
        }
    } else {
        Err(ConvertErr::NotCompatible)
    }
}

pub trait PineClass {
    fn get<'a, D: PineType<'a>, E>(&self, name: &str) -> Result<D, E>;
}

pub trait PineFrom<'a> {
    // The user force type cast
    fn from(t: Box<dyn PineType<'a> + 'a>) -> Result<Box<dyn PineType<'a> + 'a>, ConvertErr> {
        Self::auto_from(t)
    }

    // Create this type from the source type for auto cast
    fn auto_from(t: Box<dyn PineType<'a> + 'a>) -> Result<Box<dyn PineType<'a> + 'a>, ConvertErr>;
}
