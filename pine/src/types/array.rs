use super::downcast::downcast_pf;
use super::error::RuntimeErr;
use super::pine_ref::PineRef;
use super::ref_data::RefData;
use super::traits::{
    Category, ComplexType, DataType, PineFrom, PineStaticType, PineType, SecondType,
};
use std::fmt::Debug;

impl<'a, D: PineStaticType> PineStaticType for Vec<D> {
    fn static_type() -> (DataType, SecondType) {
        (<D as PineStaticType>::static_type().0, SecondType::Array)
    }
}

impl<'a, D: PineStaticType + Clone + 'a> PineType<'a> for Vec<D> {
    fn get_type(&self) -> (DataType, SecondType) {
        (<D as PineStaticType>::static_type().0, SecondType::Array)
    }

    fn category(&self) -> Category {
        Category::Complex
    }

    fn copy(&self) -> PineRef<'a> {
        let series: Vec<D> = self.to_vec();
        PineRef::new(series)
    }
}

impl<D> ComplexType for Vec<D> {}

impl<'a, D> PineFrom<'a, Vec<D>> for Vec<D>
where
    D: Debug + PartialEq + Clone + PineStaticType + PineFrom<'a, D> + 'a,
{
    // The user force type cast
    fn explicity_from(t: PineRef<'a>) -> Result<RefData<Vec<D>>, RuntimeErr> {
        Self::implicity_from(t)
    }

    // Create this type from the source type for auto cast
    fn implicity_from(t: PineRef<'a>) -> Result<RefData<Vec<D>>, RuntimeErr> {
        match t.get_type() {
            (_, SecondType::Simple) | (_, SecondType::Series) => {
                let val = D::implicity_from(t)?.into_inner();
                Ok(RefData::new_rc(vec![val]))
            }
            (_, SecondType::Array) => downcast_pf::<Vec<D>>(t),
            _ => Err(RuntimeErr::UnknownRuntimeErr),
        }
    }
}
