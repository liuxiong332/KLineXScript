use super::{
    Bool, DataType, Float, Int, PineFrom, PineRef, PineType, RefData, RuntimeErr, SecondType,
};

pub fn merge_array<'a>(
    cur_array: Option<PineRef<'a>>,
    v: PineRef<'a>,
) -> Result<PineRef<'a>, RuntimeErr> {
    match cur_array {
        None => match v.get_type() {
            (DataType::Int, _) => Ok(PineRef::new(vec![Int::implicity_from(v)?.into_inner()])),
            (DataType::Float, _) => Ok(PineRef::new(vec![Float::implicity_from(v)?.into_inner()])),
            (DataType::Bool, _) => Ok(PineRef::new(vec![Bool::implicity_from(v)?.into_inner()])),
            _t => Err(RuntimeErr::NotCompatible(format!(
                "The array type only support int, float, bool, bug get {:?}",
                _t
            ))),
        },
        Some(val) => match val.get_type() {
            (DataType::Int, SecondType::Array) => {
                let mut vec: RefData<Vec<Int>> = Vec::implicity_from(val)?;
                vec.push(Int::implicity_from(v)?.into_inner());
                Ok(vec.into_pf())
            }
            (DataType::Float, SecondType::Array) => {
                let mut vec: RefData<Vec<Float>> = Vec::implicity_from(val)?;
                vec.push(Float::implicity_from(v)?.into_inner());
                Ok(vec.into_pf())
            }
            (DataType::Bool, SecondType::Array) => {
                let mut vec: RefData<Vec<Bool>> = Vec::implicity_from(val)?;
                vec.push(Bool::implicity_from(v)?.into_inner());
                Ok(vec.into_pf())
            }
            _t => Err(RuntimeErr::NotCompatible(format!(
                "The array type only support int, float, bool, bug get {:?}",
                _t
            ))),
        },
    }
}
