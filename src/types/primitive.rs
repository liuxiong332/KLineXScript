use super::traits::{
    downcast, ConvertErr, DataType, PineFrom, PineStaticType, PineType, SecondType,
};
use std::marker::PhantomData;

// pine int type
pub type Int = Option<i32>;

impl PineStaticType for Int {
    fn static_type() -> (DataType, SecondType) {
        (DataType::Int, SecondType::Simple)
    }
}

impl<'a> PineType<'a> for Option<i32> {
    fn get_type(&self) -> (DataType, SecondType) {
        <Self as PineStaticType>::static_type()
    }
}

impl<'a> PineFrom<'a> for Int {
    // NA -> Int Float -> Int
    fn explicity_from(
        t: Box<dyn PineType<'a> + 'a>,
    ) -> Result<Box<dyn PineType<'a> + 'a>, ConvertErr> {
        match t.get_type() {
            (DataType::Int, SecondType::Simple) => Ok(t),
            (DataType::NA, SecondType::Simple) => {
                let i: Int = None;
                Ok(Box::new(i))
            }
            (DataType::Float, SecondType::Simple) => {
                let f: Box<Float> = downcast::<Float>(t).unwrap();
                let i: Int = match *f {
                    Some(f) => Some(f as i32),
                    None => None,
                };
                Ok(Box::new(i))
            }
            _ => Err(ConvertErr::NotCompatible),
        }
    }

    // NA -> Int
    fn implicity_from(
        t: Box<dyn PineType<'a> + 'a>,
    ) -> Result<Box<dyn PineType<'a> + 'a>, ConvertErr> {
        match t.get_type() {
            (DataType::Int, SecondType::Simple) => Ok(t),
            (DataType::NA, SecondType::Simple) => {
                let i: Int = None;
                Ok(Box::new(i))
            }
            _ => Err(ConvertErr::NotCompatible),
        }
    }
}

// pine float type
pub type Float = Option<f64>;
impl PineStaticType for Float {
    fn static_type() -> (DataType, SecondType) {
        (DataType::Float, SecondType::Simple)
    }
}
impl<'a> PineType<'a> for Float {
    fn get_type(&self) -> (DataType, SecondType) {
        <Self as PineStaticType>::static_type()
    }
}

fn int2float<'a>(t: Box<dyn PineType<'a> + 'a>) -> Box<dyn PineType<'a> + 'a> {
    let i: Box<Int> = downcast::<Int>(t).unwrap();
    let f: Float = match *i {
        Some(i) => Some(i as f64),
        None => None,
    };
    Box::new(f)
}

impl<'a> PineFrom<'a> for Float {
    fn explicity_from(
        t: Box<dyn PineType<'a> + 'a>,
    ) -> Result<Box<dyn PineType<'a> + 'a>, ConvertErr> {
        Self::implicity_from(t)
    }

    fn implicity_from(
        t: Box<dyn PineType<'a> + 'a>,
    ) -> Result<Box<dyn PineType<'a> + 'a>, ConvertErr> {
        match t.get_type() {
            (DataType::Float, SecondType::Simple) => Ok(t),
            (DataType::NA, SecondType::Simple) => {
                let i: Float = None;
                Ok(Box::new(i))
            }
            (DataType::Int, SecondType::Simple) => Ok(int2float(t)),
            _ => Err(ConvertErr::NotCompatible),
        }
    }
}

// pine bool type
pub type Bool = bool;
impl PineStaticType for Bool {
    fn static_type() -> (DataType, SecondType) {
        (DataType::Bool, SecondType::Simple)
    }
}
impl<'a> PineType<'a> for Bool {
    fn get_type(&self) -> (DataType, SecondType) {
        <Self as PineStaticType>::static_type()
    }
}

impl<'a> PineFrom<'a> for Bool {
    fn explicity_from(
        t: Box<dyn PineType<'a> + 'a>,
    ) -> Result<Box<dyn PineType<'a> + 'a>, ConvertErr> {
        Self::implicity_from(t)
    }

    fn implicity_from(
        t: Box<dyn PineType<'a> + 'a>,
    ) -> Result<Box<dyn PineType<'a> + 'a>, ConvertErr> {
        match t.get_type() {
            (DataType::Bool, SecondType::Simple) => Ok(t),
            (DataType::NA, SecondType::Simple) => {
                let i: Bool = false;
                Ok(Box::new(i))
            }
            (DataType::Float, SecondType::Simple) => {
                let f: Box<Float> = downcast::<Float>(t).unwrap();
                let b: Bool = match *f {
                    Some(_) => true,
                    None => false,
                };
                Ok(Box::new(b))
            }

            (DataType::Int, SecondType::Simple) => {
                let f: Box<Int> = downcast::<Int>(t).unwrap();
                let b: Bool = match *f {
                    Some(_) => true,
                    None => false,
                };
                Ok(Box::new(b))
            }
            _ => Err(ConvertErr::NotCompatible),
        }
    }
}

// pine color type
pub struct Color<'a>(pub &'a str);
impl<'a> PineStaticType for Color<'a> {
    fn static_type() -> (DataType, SecondType) {
        (DataType::Color, SecondType::Simple)
    }
}
impl<'a> PineType<'a> for Color<'a> {
    fn get_type(&self) -> (DataType, SecondType) {
        <Self as PineStaticType>::static_type()
    }
}

impl PineStaticType for String {
    fn static_type() -> (DataType, SecondType) {
        (DataType::String, SecondType::Simple)
    }
}
// pine string type
impl<'a> PineType<'a> for String {
    fn get_type(&self) -> (DataType, SecondType) {
        <Self as PineStaticType>::static_type()
    }
}

// pine na type
pub struct NA;
impl PineStaticType for NA {
    fn static_type() -> (DataType, SecondType) {
        (DataType::NA, SecondType::Simple)
    }
}
impl<'a> PineType<'a> for NA {
    fn get_type(&self) -> (DataType, SecondType) {
        <Self as PineStaticType>::static_type()
    }
}

// pine type that represent variable name
pub struct PineVar<'a>(pub &'a str);
impl<'a> PineStaticType for PineVar<'a> {
    fn static_type() -> (DataType, SecondType) {
        (DataType::String, SecondType::Simple)
    }
}
impl<'a> PineType<'a> for PineVar<'a> {
    fn get_type(&self) -> (DataType, SecondType) {
        <Self as PineStaticType>::static_type()
    }
}

// pine tuple type
pub struct Tuple<'a>(pub Vec<Box<dyn PineType<'a> + 'a>>);
impl<'a> PineStaticType for Tuple<'a> {
    fn static_type() -> (DataType, SecondType) {
        (DataType::String, SecondType::Simple)
    }
}
impl<'a> PineType<'a> for Tuple<'a> {
    fn get_type(&self) -> (DataType, SecondType) {
        <Self as PineStaticType>::static_type()
    }
}

// pine callable type
pub struct Callable<'a, D> {
    func: D,
    param_names: Vec<&'static str>,
    phantom: PhantomData<&'a D>,
}
impl<'a, D> PineStaticType for Callable<'a, D> {
    fn static_type() -> (DataType, SecondType) {
        (DataType::String, SecondType::Simple)
    }
}
impl<'a, D> PineType<'a> for Callable<'a, D> {
    fn get_type(&self) -> (DataType, SecondType) {
        <Self as PineStaticType>::static_type()
    }
}

impl<'a, D> Callable<'a, D>
where
    D: Fn(Vec<Box<dyn PineType<'a> + 'a>>) -> Box<dyn PineType<'a> + 'a>,
{
    fn new(func: D, param_names: Vec<&'static str>) -> Callable<'a, D> {
        Callable {
            func,
            param_names,
            phantom: PhantomData,
        }
    }

    fn call(
        &self,
        mut pos_args: Vec<Box<dyn PineType<'a> + 'a>>,
        dict_args: Vec<(Box<PineVar<'a>>, Box<dyn PineType<'a> + 'a>)>,
    ) -> Result<Box<dyn PineType<'a> + 'a>, ConvertErr> {
        if pos_args.len() > self.param_names.len() {
            return Err(ConvertErr::NotValidParam);
        }
        pos_args.resize_with(self.param_names.len(), || Box::new(NA));

        for (varname, val) in dict_args.into_iter() {
            match self.param_names.iter().position(|&v| (*varname).0 == v) {
                None => return Err(ConvertErr::NotValidParam),
                Some(index) => pos_args[index] = val,
            }
        }
        Ok((self.func)(pos_args))
    }
}

#[cfg(test)]
mod tests {
    use super::super::primitive::Int;
    use super::*;

    #[test]
    fn int_test() {
        assert_eq!(
            <Int as PineStaticType>::static_type(),
            (DataType::Int, SecondType::Simple)
        );
        assert_eq!(
            <Int as PineType>::get_type(&Int::default()),
            (DataType::Int, SecondType::Simple)
        );

        assert!(Int::implicity_from(Box::new(NA)).is_ok());
        assert!(Int::explicity_from(Box::new(Some(3i32))).is_ok());
        assert!(Int::explicity_from(Box::new(Some(3f64))).is_ok());
        assert_eq!(
            downcast::<Int>(Int::explicity_from(Box::new(Some(3f64))).unwrap()),
            Ok(Box::new(Some(3i32)))
        );
    }

    #[test]
    fn float_test() {
        assert_eq!(
            <Float as PineStaticType>::static_type(),
            (DataType::Float, SecondType::Simple)
        );
        assert_eq!(
            <Float as PineType>::get_type(&Default::default()),
            (DataType::Float, SecondType::Simple)
        );

        assert!(Float::implicity_from(Box::new(NA)).is_ok());
        assert!(Float::implicity_from(Box::new(Some(3f64))).is_ok());
        assert!(Float::implicity_from(Box::new(Some(3i32))).is_ok());
        assert_eq!(
            downcast::<Float>(Float::implicity_from(Box::new(Some(3i32))).unwrap()),
            Ok(Box::new(Some(3f64)))
        );
    }

    fn from_bool<'a, D: PineType<'a> + 'a>(val: D) -> Result<Box<Bool>, ConvertErr> {
        downcast::<Bool>(Bool::implicity_from(Box::new(val)).unwrap())
    }
    #[test]
    fn bool_test() {
        assert_eq!(
            <Bool as PineStaticType>::static_type(),
            (DataType::Bool, SecondType::Simple)
        );
        assert_eq!(
            <Bool as PineType>::get_type(&Default::default()),
            (DataType::Bool, SecondType::Simple)
        );
        assert_eq!(from_bool(true), Ok(Box::new(true)));
        assert_eq!(from_bool(false), Ok(Box::new(false)));

        assert_eq!(from_bool(NA), Ok(Box::new(false)));

        assert_eq!(from_bool(Some(3i32)), Ok(Box::new(true)));
        assert_eq!(from_bool(None as Int), Ok(Box::new(false)));

        assert_eq!(from_bool(Some(3f64)), Ok(Box::new(true)));
        assert_eq!(from_bool(None as Float), Ok(Box::new(false)));
    }
}
