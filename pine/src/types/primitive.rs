use super::downcast::downcast_pf;
use super::error::RuntimeErr;
use super::pine_ref::PineRef;
use super::ref_data::RefData;
use super::series::Series;
use super::traits::{
    Arithmetic, Category, Comparator, ComplexType, DataType, Negative, PineClass, PineFrom,
    PineStaticType, PineType, SecondType, SimpleType,
};

// pine int type
pub type Int = Option<i64>;

impl Negative<Int> for Int {
    fn negative(self) -> Int {
        match self {
            Some(i) => Some(-i),
            None => None,
        }
    }
}

impl Arithmetic for Int {
    fn add(self, other: Self) -> Self {
        match (self, other) {
            (Some(i1), Some(i2)) => Some(i1 + i2),
            _ => None,
        }
    }

    fn minus(self, other: Self) -> Self {
        match (self, other) {
            (Some(i1), Some(i2)) => Some(i1 - i2),
            _ => None,
        }
    }

    fn mul(self, other: Self) -> Self {
        match (self, other) {
            (Some(i1), Some(i2)) => Some(i1 * i2),
            _ => None,
        }
    }

    fn div(self, other: Self) -> Self {
        match (self, other) {
            (Some(i1), Some(i2)) => Some(i1 / i2),
            _ => None,
        }
    }

    fn rem(self, other: Self) -> Self {
        match (self, other) {
            (Some(i1), Some(i2)) => Some(i1 % i2),
            _ => None,
        }
    }
}

impl PineStaticType for Int {
    fn static_type() -> (DataType, SecondType) {
        (DataType::Int, SecondType::Simple)
    }
}

impl<'a> PineType<'a> for Int {
    fn get_type(&self) -> (DataType, SecondType) {
        <Self as PineStaticType>::static_type()
    }

    fn copy(&self) -> PineRef<'a> {
        PineRef::Box(Box::new(self.clone()))
    }
}

impl<'a> PineFrom<'a, Int> for Int {
    // NA -> Int Float -> Int
    fn explicity_from(t: PineRef<'a>) -> Result<RefData<Int>, RuntimeErr> {
        match t.get_type() {
            (DataType::Int, SecondType::Simple) => Ok(downcast_pf::<Int>(t).unwrap()),
            (DataType::Int, SecondType::Series) => {
                let s = downcast_pf::<Series<Int>>(t).unwrap();
                Ok(RefData::new_box(s.get_current()))
            }
            (DataType::NA, SecondType::Simple) => {
                let i: Int = None;
                Ok(RefData::new_box(i))
            }
            (DataType::Float, SecondType::Simple) => {
                let f: RefData<Float> = downcast_pf::<Float>(t).unwrap();
                let i: Int = match *f {
                    Some(f) => Some(f as i64),
                    None => None,
                };
                Ok(RefData::new_box(i))
            }
            (DataType::Float, SecondType::Series) => {
                let s = downcast_pf::<Series<Float>>(t).unwrap();
                let i: Int = match s.get_current() {
                    Some(f) => Some(f as i64),
                    None => None,
                };
                Ok(RefData::new_box(i))
            }
            _ => Err(RuntimeErr::UnknownRuntimeErr),
        }
    }

    // NA -> Int
    fn implicity_from(t: PineRef<'a>) -> Result<RefData<Int>, RuntimeErr> {
        match t.get_type() {
            (DataType::Int, SecondType::Simple) => Ok(downcast_pf::<Int>(t).unwrap()),
            (DataType::Int, SecondType::Series) => {
                let s = downcast_pf::<Series<Int>>(t).unwrap();
                Ok(RefData::new_box(s.get_current()))
            }
            (DataType::NA, _) => {
                let i: Int = None;
                Ok(RefData::new_box(i))
            }
            _ => Err(RuntimeErr::UnknownRuntimeErr),
        }
    }
}

impl<'a> SimpleType for Int {}

// pine float type
pub type Float = Option<f64>;

impl Negative<Float> for Float {
    fn negative(self) -> Float {
        match self {
            Some(i) => Some(-i),
            None => None,
        }
    }
}

impl Arithmetic for Float {
    fn add(self, other: Self) -> Self {
        match (self, other) {
            (Some(i1), Some(i2)) => Some(i1 + i2),
            _ => None,
        }
    }

    fn minus(self, other: Self) -> Self {
        match (self, other) {
            (Some(i1), Some(i2)) => Some(i1 - i2),
            _ => None,
        }
    }

    fn mul(self, other: Self) -> Self {
        match (self, other) {
            (Some(i1), Some(i2)) => Some(i1 * i2),
            _ => None,
        }
    }

    fn div(self, other: Self) -> Self {
        match (self, other) {
            (Some(i1), Some(i2)) => {
                let v = i1 / i2;
                if v.is_nan() {
                    None
                } else {
                    Some(v)
                }
            }
            _ => None,
        }
    }

    fn rem(self, other: Self) -> Self {
        match (self, other) {
            (Some(i1), Some(i2)) => Some(i1 % i2),
            _ => None,
        }
    }
}

impl Comparator for Float {
    fn gt(self, other: Self) -> bool {
        match (self, other) {
            (Some(v1), Some(v2)) => v1 > v2,
            _ => false,
        }
    }

    fn ge(self, other: Self) -> bool {
        match (self, other) {
            (Some(v1), Some(v2)) => v1 >= v2,
            _ => false,
        }
    }

    fn lt(self, other: Self) -> bool {
        match (self, other) {
            (Some(v1), Some(v2)) => v1 < v2,
            _ => false,
        }
    }

    fn le(self, other: Self) -> bool {
        match (self, other) {
            (Some(v1), Some(v2)) => v1 <= v2,
            _ => false,
        }
    }
}

impl PineStaticType for Float {
    fn static_type() -> (DataType, SecondType) {
        (DataType::Float, SecondType::Simple)
    }
}

impl<'a> PineType<'a> for Float {
    fn get_type(&self) -> (DataType, SecondType) {
        <Self as PineStaticType>::static_type()
    }

    fn copy(&self) -> PineRef<'a> {
        PineRef::Box(Box::new(self.clone()))
    }
}

pub fn int2float<'a>(i: Int) -> Float {
    match i {
        Some(i) => Some(i as f64),
        None => None,
    }
}

pub fn float2int<'a>(i: Float) -> Int {
    match i {
        Some(i) => Some(i as i64),
        None => None,
    }
}

impl<'a> PineFrom<'a, Float> for Float {
    fn explicity_from(t: PineRef<'a>) -> Result<RefData<Float>, RuntimeErr> {
        Self::implicity_from(t)
    }

    // NA => Float Int => Float
    fn implicity_from(t: PineRef<'a>) -> Result<RefData<Float>, RuntimeErr> {
        match t.get_type() {
            (DataType::Float, SecondType::Simple) => Ok(downcast_pf::<Float>(t).unwrap()),
            (DataType::Float, SecondType::Series) => {
                let s = downcast_pf::<Series<Float>>(t).unwrap();
                Ok(RefData::new_box(s.get_current()))
            }
            (DataType::NA, _) => {
                let i: Float = None;
                Ok(RefData::new_box(i))
            }
            (DataType::Int, SecondType::Simple) => {
                let i = downcast_pf::<Int>(t).unwrap();
                Ok(RefData::new_box(int2float(*i)))
            }
            (DataType::Int, SecondType::Series) => {
                let s = downcast_pf::<Series<Int>>(t).unwrap();
                Ok(RefData::new_box(int2float(s.get_current())))
            }
            _ => Err(RuntimeErr::UnknownRuntimeErr),
        }
    }
}

impl<'a> SimpleType for Float {}

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

    fn copy(&self) -> PineRef<'a> {
        PineRef::Box(Box::new(self.clone()))
    }
}

impl<'a> PineFrom<'a, Bool> for Bool {
    fn explicity_from(t: PineRef<'a>) -> Result<RefData<Bool>, RuntimeErr> {
        Self::implicity_from(t)
    }

    // NA => Bool Float => Bool Int => Bool
    fn implicity_from(t: PineRef<'a>) -> Result<RefData<Bool>, RuntimeErr> {
        match t.get_type() {
            (DataType::Bool, SecondType::Simple) => Ok(downcast_pf::<Bool>(t).unwrap()),
            (DataType::Bool, SecondType::Series) => {
                let s = downcast_pf::<Series<Bool>>(t).unwrap();
                Ok(RefData::new_box(s.get_current()))
            }
            (DataType::NA, _) => {
                let i: Bool = false;
                Ok(RefData::new_box(i))
            }
            (DataType::Float, SecondType::Simple) => {
                let f: RefData<Float> = downcast_pf::<Float>(t).unwrap();
                let b: Bool = match *f {
                    Some(_) => true,
                    None => false,
                };
                Ok(RefData::new_box(b))
            }
            (DataType::Float, SecondType::Series) => {
                let f: RefData<Series<Float>> = downcast_pf::<Series<Float>>(t).unwrap();
                let b: Bool = match f.get_current() {
                    Some(_) => true,
                    None => false,
                };
                Ok(RefData::new_box(b))
            }
            (DataType::Int, SecondType::Simple) => {
                let f: RefData<Int> = downcast_pf::<Int>(t).unwrap();
                let b: Bool = match *f {
                    Some(_) => true,
                    None => false,
                };
                Ok(RefData::new_box(b))
            }
            (DataType::Int, SecondType::Series) => {
                let f: RefData<Series<Int>> = downcast_pf::<Series<Int>>(t).unwrap();
                let b: Bool = match f.get_current() {
                    Some(_) => true,
                    None => false,
                };
                Ok(RefData::new_box(b))
            }
            _ => Err(RuntimeErr::UnknownRuntimeErr),
        }
    }
}

impl SimpleType for Bool {}

// pine color type
#[derive(Debug, PartialEq, Clone, Default)]
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

    fn copy(&self) -> PineRef<'a> {
        PineRef::Box(Box::new(self.clone()))
    }
}

impl<'a> PineFrom<'a, Color<'a>> for Color<'a> {
    fn implicity_from(t: PineRef<'a>) -> Result<RefData<Color>, RuntimeErr> {
        match t.get_type() {
            (DataType::Color, SecondType::Simple) => Ok(downcast_pf::<Color>(t).unwrap()),
            (DataType::Color, SecondType::Series) => {
                let f: RefData<Series<Color>> = downcast_pf::<Series<Color>>(t).unwrap();
                Ok(RefData::new_box(f.get_current()))
            }
            (DataType::NA, _) => Ok(RefData::new_box(Color(""))),
            _ => Err(RuntimeErr::UnknownRuntimeErr),
        }
    }
}

impl<'a> SimpleType for Color<'a> {}

// pine na type
#[derive(Debug, PartialEq, Clone, Default)]
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

    fn copy(&self) -> PineRef<'a> {
        PineRef::Box(Box::new(self.clone()))
    }
}

impl<'a> PineFrom<'a, NA> for NA {
    fn implicity_from(t: PineRef<'a>) -> Result<RefData<NA>, RuntimeErr> {
        match t.get_type() {
            (DataType::NA, _) => Ok(RefData::new_box(NA)),
            _ => Err(RuntimeErr::UnknownRuntimeErr),
        }
    }
}
impl SimpleType for NA {}

// pine type that represent variable name
#[derive(Debug, PartialEq, Clone)]
pub struct PineVar<'a>(pub &'a str);
impl<'a> PineStaticType for PineVar<'a> {
    fn static_type() -> (DataType, SecondType) {
        (DataType::PineVar, SecondType::Simple)
    }
}
impl<'a> PineType<'a> for PineVar<'a> {
    fn get_type(&self) -> (DataType, SecondType) {
        <Self as PineStaticType>::static_type()
    }

    fn copy(&self) -> PineRef<'a> {
        PineRef::Box(Box::new(self.clone()))
    }
}
impl<'a> PineFrom<'a, PineVar<'a>> for PineVar<'a> {}

impl<'a> SimpleType for PineVar<'a> {}

// pine tuple type
#[derive(PartialEq, Debug)]
pub struct Tuple<'a>(pub Vec<PineRef<'a>>);

impl<'a> Clone for Tuple<'a> {
    fn clone(&self) -> Self {
        Tuple(self.0.iter().map(|v| v.copy()).collect())
    }
}

impl<'a> PineStaticType for Tuple<'a> {
    fn static_type() -> (DataType, SecondType) {
        (DataType::Tuple, SecondType::Simple)
    }
}
impl<'a> PineType<'a> for Tuple<'a> {
    fn get_type(&self) -> (DataType, SecondType) {
        <Self as PineStaticType>::static_type()
    }

    fn copy(&self) -> PineRef<'a> {
        let new_vec = self.0.iter().map(|it| it.copy()).collect();
        PineRef::Box(Box::new(Tuple(new_vec)))
    }
}
impl<'a> PineFrom<'a, Tuple<'a>> for Tuple<'a> {
    fn implicity_from(t: PineRef<'a>) -> Result<RefData<Tuple<'a>>, RuntimeErr> {
        match t.get_type() {
            (DataType::Tuple, _) => Ok(downcast_pf::<Tuple>(t).unwrap()),
            _ => Err(RuntimeErr::UnknownRuntimeErr),
        }
    }
}

impl<'a> SimpleType for Tuple<'a> {}

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

    fn category(&self) -> Category {
        Category::Complex
    }

    fn copy(&self) -> PineRef<'a> {
        PineRef::new_rc(self.clone())
    }
}

impl<'a> PineFrom<'a, String> for String {
    fn implicity_from(_t: PineRef<'a>) -> Result<RefData<String>, RuntimeErr> {
        match _t.get_type() {
            (DataType::String, SecondType::Simple) => Ok(downcast_pf::<String>(_t)?),
            (DataType::String, SecondType::Series) => {
                let f: RefData<Series<String>> = downcast_pf::<Series<String>>(_t).unwrap();
                Ok(RefData::new_rc(f.get_current()))
            }
            (DataType::NA, _) => Ok(RefData::new_rc(String::from(""))),
            _ => Err(RuntimeErr::NotSupportOperator),
        }
    }
}

impl ComplexType for String {}

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

        assert!(Int::implicity_from(PineRef::new_box(NA)).is_ok());
        assert!(Int::implicity_from(PineRef::new(Series::from(NA))).is_ok());
        assert_eq!(
            Int::implicity_from(PineRef::new_box(Some(1))),
            Ok(RefData::new(Some(1)))
        );
        assert_eq!(
            Int::implicity_from(PineRef::new(Series::from(Some(1)))),
            Ok(RefData::new(Some(1)))
        );

        assert!(Int::explicity_from(PineRef::new_box(NA)).is_ok());
        assert_eq!(
            Int::explicity_from(PineRef::new_box(Some(1))),
            Ok(RefData::new(Some(1)))
        );
        assert_eq!(
            Int::explicity_from(PineRef::new(Series::from(Some(1)))),
            Ok(RefData::new(Some(1)))
        );
        assert_eq!(
            Int::explicity_from(PineRef::new_box(Some(1f64))),
            Ok(RefData::new(Some(1)))
        );
        assert_eq!(
            Int::explicity_from(PineRef::new(Series::from(Some(1f64)))),
            Ok(RefData::new(Some(1)))
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

        assert_eq!(
            Float::implicity_from(PineRef::new_box(NA)),
            Ok(RefData::new(None))
        );
        assert_eq!(
            Float::implicity_from(PineRef::new(Series::from(NA))),
            Ok(RefData::new(None))
        );
        assert_eq!(
            Float::implicity_from(PineRef::new_box(Some(3f64))),
            Ok(RefData::new(Some(3f64)))
        );
        assert_eq!(
            Float::implicity_from(PineRef::new(Series::from(Some(3f64)))),
            Ok(RefData::new(Some(3f64)))
        );
        assert_eq!(
            Float::implicity_from(PineRef::new_box(Some(3i64))),
            Ok(RefData::new(Some(3f64)))
        );
        assert_eq!(
            Float::implicity_from(PineRef::new(Series::from(Some(3i64)))),
            Ok(RefData::new_box(Some(3f64)))
        );

        assert_eq!(
            Float::explicity_from(PineRef::new_box(NA)),
            Ok(RefData::new(None))
        );
        assert_eq!(
            Float::explicity_from(PineRef::new(Series::from(NA))),
            Ok(RefData::new(None))
        );
        assert_eq!(
            Float::explicity_from(PineRef::new_box(Some(3f64))),
            Ok(RefData::new(Some(3f64)))
        );
        assert_eq!(
            Float::explicity_from(PineRef::new(Series::from(Some(3f64)))),
            Ok(RefData::new(Some(3f64)))
        );
        assert_eq!(
            Float::explicity_from(PineRef::new_box(Some(3i64))),
            Ok(RefData::new(Some(3f64)))
        );
        assert_eq!(
            Float::explicity_from(PineRef::new(Series::from(Some(3i64)))),
            Ok(RefData::new_box(Some(3f64)))
        );
    }

    fn from_bool<'a, D>(val: D) -> Result<RefData<Bool>, RuntimeErr>
    where
        D: PineType<'a> + 'a,
    {
        Bool::implicity_from(PineRef::new(val))
    }

    fn ex_from_bool<'a, D>(val: D) -> Result<RefData<Bool>, RuntimeErr>
    where
        D: PineType<'a> + 'a,
    {
        Bool::explicity_from(PineRef::new(val))
    }

    #[test]
    fn bool_test() {
        assert_eq!(
            <Bool as PineStaticType>::static_type(),
            (DataType::Bool, SecondType::Simple)
        );
        assert_eq!(
            <Bool as PineType>::get_type(&Bool::default()),
            (DataType::Bool, SecondType::Simple)
        );

        assert_eq!(from_bool(true), Ok(RefData::new_box(true)));
        assert_eq!(from_bool(false), Ok(RefData::new_box(false)));
        assert_eq!(from_bool(Series::from(true)), Ok(RefData::new_box(true)));

        assert_eq!(from_bool(NA), Ok(RefData::new_box(false)));
        assert_eq!(from_bool(Series::from(NA)), Ok(RefData::new_box(false)));

        assert_eq!(from_bool(Some(3i64)), Ok(RefData::new_box(true)));
        assert_eq!(from_bool(None as Int), Ok(RefData::new_box(false)));
        assert_eq!(
            from_bool(Series::from(Some(3i64))),
            Ok(RefData::new_box(true))
        );

        assert_eq!(from_bool(Some(3f64)), Ok(RefData::new_box(true)));
        assert_eq!(from_bool(None as Float), Ok(RefData::new_box(false)));
        assert_eq!(
            from_bool(Series::from(Some(3f64))),
            Ok(RefData::new_box(true))
        );

        assert_eq!(ex_from_bool(true), Ok(RefData::new_box(true)));
        assert_eq!(ex_from_bool(false), Ok(RefData::new_box(false)));
        assert_eq!(ex_from_bool(Series::from(true)), Ok(RefData::new_box(true)));

        assert_eq!(ex_from_bool(NA), Ok(RefData::new_box(false)));
        assert_eq!(ex_from_bool(Series::from(NA)), Ok(RefData::new_box(false)));

        assert_eq!(ex_from_bool(Some(3i64)), Ok(RefData::new_box(true)));
        assert_eq!(ex_from_bool(None as Int), Ok(RefData::new_box(false)));
        assert_eq!(
            ex_from_bool(Series::from(Some(3i64))),
            Ok(RefData::new_box(true))
        );

        assert_eq!(ex_from_bool(Some(3f64)), Ok(RefData::new_box(true)));
        assert_eq!(ex_from_bool(None as Float), Ok(RefData::new_box(false)));
        assert_eq!(
            ex_from_bool(Series::from(Some(3f64))),
            Ok(RefData::new_box(true))
        );
    }

    #[test]
    fn color_test() {
        assert_eq!(Color::static_type(), (DataType::Color, SecondType::Simple));
        assert_eq!(
            Color::get_type(&Color("")),
            (DataType::Color, SecondType::Simple)
        );
    }
}
