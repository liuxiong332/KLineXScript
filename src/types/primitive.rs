use super::downcast::downcast_pf;
use super::error::RuntimeErr;
use super::pine_ref::PineRef;
use super::ref_data::RefData;
use super::series::Series;
use super::traits::{
    Arithmetic, Category, ComplexType, DataType, Negative, PineClass, PineFrom, PineStaticType,
    PineType, SecondType, SimpleType,
};

// pine int type
pub type Int = Option<i32>;

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
            (DataType::NA, SecondType::Simple) => {
                let i: Int = None;
                Ok(RefData::new_box(i))
            }
            (DataType::Float, SecondType::Simple) => {
                let f: RefData<Float> = downcast_pf::<Float>(t).unwrap();
                let i: Int = match *f {
                    Some(f) => Some(f as i32),
                    None => None,
                };
                Ok(RefData::new_box(i))
            }
            _ => Err(RuntimeErr::NotCompatible),
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
            (DataType::NA, SecondType::Simple) => {
                let i: Int = None;
                Ok(RefData::new_box(i))
            }
            _ => Err(RuntimeErr::NotCompatible),
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

fn int2float<'a>(i: Int) -> Float {
    match i {
        Some(i) => Some(i as f64),
        None => None,
    }
}

impl<'a> PineFrom<'a, Float> for Float {
    fn explicity_from(t: PineRef<'a>) -> Result<RefData<Float>, RuntimeErr> {
        Self::implicity_from(t)
    }

    fn implicity_from(t: PineRef<'a>) -> Result<RefData<Float>, RuntimeErr> {
        match t.get_type() {
            (DataType::Float, SecondType::Simple) => Ok(downcast_pf::<Float>(t).unwrap()),
            (DataType::Float, SecondType::Series) => {
                let s = downcast_pf::<Series<Float>>(t).unwrap();
                Ok(RefData::new_box(s.get_current()))
            }
            (DataType::NA, SecondType::Simple) => {
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
            _ => Err(RuntimeErr::NotCompatible),
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

    fn implicity_from(t: PineRef<'a>) -> Result<RefData<Bool>, RuntimeErr> {
        match t.get_type() {
            (DataType::Bool, SecondType::Simple) => Ok(downcast_pf::<Bool>(t).unwrap()),
            (DataType::Bool, SecondType::Series) => {
                let s = downcast_pf::<Series<Bool>>(t).unwrap();
                Ok(RefData::new_box(s.get_current()))
            }
            (DataType::NA, SecondType::Simple) => {
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

            (DataType::Int, SecondType::Simple) => {
                let f: RefData<Int> = downcast_pf::<Int>(t).unwrap();
                let b: Bool = match *f {
                    Some(_) => true,
                    None => false,
                };
                Ok(RefData::new_box(b))
            }
            _ => Err(RuntimeErr::NotCompatible),
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

impl<'a> PineFrom<'a, Color<'a>> for Color<'a> {}

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

impl<'a> PineFrom<'a, NA> for NA {}
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
impl<'a> PineFrom<'a, Tuple<'a>> for Tuple<'a> {}

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
        PineRef::Box(Box::new(self.clone()))
    }
}

impl<'a> PineFrom<'a, String> for String {
    fn implicity_from(_t: PineRef<'a>) -> Result<RefData<String>, RuntimeErr> {
        match _t.get_type() {
            (DataType::String, SecondType::Simple) => Ok(downcast_pf::<String>(_t)?),
            _ => Err(RuntimeErr::NotSupportOperator),
        }
    }
}

impl ComplexType for String {}

#[derive(Debug)]
pub struct Object<'a> {
    obj: Box<dyn PineClass<'a> + 'a>,
}

impl<'a> PartialEq for Object<'a> {
    fn eq(&self, other: &Object<'a>) -> bool {
        PartialEq::eq(&*self.obj, &*other.obj)
    }
}

impl<'a> PineStaticType for Object<'a> {
    fn static_type() -> (DataType, SecondType) {
        (DataType::Object, SecondType::Simple)
    }
}
impl<'a> PineType<'a> for Object<'a> {
    fn get_type(&self) -> (DataType, SecondType) {
        <Self as PineStaticType>::static_type()
    }

    fn category(&self) -> Category {
        Category::Complex
    }

    fn copy(&self) -> PineRef<'a> {
        self.obj.copy()
    }
}
impl<'a> PineFrom<'a, Object<'a>> for Object<'a> {}

impl<'a> ComplexType for Object<'a> {}

impl<'a> Object<'a> {
    pub fn new(obj: Box<dyn PineClass<'a> + 'a>) -> Object<'a> {
        Object { obj }
    }

    pub fn get(&self, name: &str) -> Result<PineRef<'a>, RuntimeErr> {
        self.obj.get(name)
    }

    pub fn set(&self, name: &str, property: PineRef<'a>) -> Result<(), RuntimeErr> {
        self.obj.set(name, property)
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

        assert!(Int::implicity_from(PineRef::new_box(NA)).is_ok());
        assert!(Int::explicity_from(PineRef::new_box(Some(3i32))).is_ok());
        assert!(Int::explicity_from(PineRef::new_box(Some(3f64))).is_ok());
        assert_eq!(
            Int::explicity_from(PineRef::new_box(Some(3f64))),
            Ok(RefData::new_box(Some(3i32)))
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

        assert!(Float::implicity_from(PineRef::new_box(NA)).is_ok());
        assert!(Float::implicity_from(PineRef::new_box(Some(3f64))).is_ok());
        assert!(Float::implicity_from(PineRef::new_box(Some(3i32))).is_ok());
        assert_eq!(
            Float::implicity_from(PineRef::new_box(Some(3i32))),
            Ok(RefData::new_box(Some(3f64)))
        );
    }

    fn from_bool<'a, D>(val: D) -> Result<RefData<Bool>, RuntimeErr>
    where
        D: PineType<'a> + SimpleType + 'a,
    {
        Bool::implicity_from(PineRef::new_box(val))
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

        assert_eq!(from_bool(NA), Ok(RefData::new_box(false)));

        assert_eq!(from_bool(Some(3i32)), Ok(RefData::new_box(true)));
        assert_eq!(from_bool(None as Int), Ok(RefData::new_box(false)));

        assert_eq!(from_bool(Some(3f64)), Ok(RefData::new_box(true)));
        assert_eq!(from_bool(None as Float), Ok(RefData::new_box(false)));
    }

    #[test]
    fn color_test() {
        assert_eq!(Color::static_type(), (DataType::Color, SecondType::Simple));
        assert_eq!(
            Color::get_type(&Color("")),
            (DataType::Color, SecondType::Simple)
        );
    }

    struct A;
    impl<'a> PineClass<'a> for A {
        fn custom_type(&self) -> &str {
            "Custom A"
        }

        fn get(&self, name: &str) -> Result<PineRef<'a>, RuntimeErr> {
            match name {
                "int1" => Ok(PineRef::new_box(Some(1i32))),
                "int2" => Ok(PineRef::new_box(Some(2i32))),
                "float1" => Ok(PineRef::new_box(Some(1f64))),
                "float2" => Ok(PineRef::new_box(Some(2f64))),
                _ => Err(RuntimeErr::NotSupportOperator),
            }
        }

        fn set(&self, _n: &str, _p: PineRef<'a>) -> Result<(), RuntimeErr> {
            Err(RuntimeErr::NotSupportOperator)
        }

        fn copy(&self) -> PineRef<'a> {
            PineRef::new_rc(Object::new(Box::new(A)))
        }
    }

    #[test]
    fn object_test() {
        let obj = Object::new(Box::new(A));
        assert_eq!(obj.get_type(), (DataType::Object, SecondType::Simple));
        assert_eq!(
            downcast_pf::<Int>(obj.get("int1").unwrap()).unwrap(),
            RefData::new_box(Some(1))
        );
    }
}
