pub use crate::ast::stat_expr_types::DataType;

pub enum SecondType {
    Simple,
    Series,
}

pub enum ConvertErr {
    NotCompatible,
    NotValidParam,
}

pub trait PineType<'a> {
    // convert this type to another type by (data_type, second_type)
    fn into(self: Box<Self>, pri_type: &DataType) -> Result<Box<dyn PineType<'a>>, ConvertErr> {
        Err(ConvertErr::NotCompatible)
    }

    fn get_type(&self) -> (DataType, SecondType) {
        (DataType::Int, SecondType::Simple)
    }
}

pub trait PineClass {
    fn get<'a, D: PineType<'a>, E>(&self, name: &str) -> Result<D, E>;
}

pub trait PineFrom<'a> {
    // The user force type cast
    fn from(t: Box<dyn PineType<'a> + 'a>) -> Result<Box<dyn PineType<'a> + 'a>, ConvertErr> {
        PineFrom::auto_from(t)
    }

    // Create this type from the source type for auto cast
    fn auto_from(t: Box<dyn PineType<'a> + 'a>) -> Result<Box<dyn PineType<'a> + 'a>, ConvertErr>;
}
