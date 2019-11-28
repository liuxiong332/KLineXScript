use super::traits::{
    downcast, ConvertErr, DataType, PineFrom, PineStaticType, PineType, SecondType,
};

impl<'a, D: PineStaticType> PineStaticType for Vec<D> {
    fn static_type() -> (DataType, SecondType) {
        (<D as PineStaticType>::static_type().0, SecondType::Array)
    }
}

impl<'a, D> PineType<'a> for Vec<D> {
    fn get_type(&self) -> (DataType, SecondType) {
        (<D as PineStaticType>::static_type().0, SecondType::Array)
    }

    fn copy(&self) -> Box<dyn PineType<'a> + 'a> {
        let series = self.clone();
        Box::new(series)
    }
}
