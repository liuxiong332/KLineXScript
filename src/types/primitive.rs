use super::downcast::downcast;
use super::traits::{
    Arithmetic, ConvertErr, DataType, Negative, PineClass, PineFrom, PineStaticType, PineType,
    SecondType,
};
use std::collections::HashMap;

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

    fn copy(&self) -> Box<dyn PineType<'a> + 'a> {
        Box::new(self.clone())
    }
}

impl<'a> PineFrom<'a, Int> for Int {
    // NA -> Int Float -> Int
    fn explicity_from(t: Box<dyn PineType<'a> + 'a>) -> Result<Box<Int>, ConvertErr> {
        match t.get_type() {
            (DataType::Int, SecondType::Simple) => Ok(downcast::<Int>(t).unwrap()),
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
    fn implicity_from(t: Box<dyn PineType<'a> + 'a>) -> Result<Box<Int>, ConvertErr> {
        match t.get_type() {
            (DataType::Int, SecondType::Simple) => Ok(downcast::<Int>(t).unwrap()),
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

    fn copy(&self) -> Box<dyn PineType<'a> + 'a> {
        Box::new(self.clone())
    }
}

fn int2float<'a>(t: Box<dyn PineType<'a> + 'a>) -> Box<Float> {
    let i: Box<Int> = downcast::<Int>(t).unwrap();
    let f: Float = match *i {
        Some(i) => Some(i as f64),
        None => None,
    };
    Box::new(f)
}

impl<'a> PineFrom<'a, Float> for Float {
    fn explicity_from(t: Box<dyn PineType<'a> + 'a>) -> Result<Box<Float>, ConvertErr> {
        Self::implicity_from(t)
    }

    fn implicity_from(t: Box<dyn PineType<'a> + 'a>) -> Result<Box<Float>, ConvertErr> {
        match t.get_type() {
            (DataType::Float, SecondType::Simple) => Ok(downcast::<Float>(t).unwrap()),
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

    fn copy(&self) -> Box<dyn PineType<'a> + 'a> {
        Box::new(self.clone())
    }
}

impl<'a> PineFrom<'a, Bool> for Bool {
    fn explicity_from(t: Box<dyn PineType<'a> + 'a>) -> Result<Box<Bool>, ConvertErr> {
        Self::implicity_from(t)
    }

    fn implicity_from(t: Box<dyn PineType<'a> + 'a>) -> Result<Box<Bool>, ConvertErr> {
        match t.get_type() {
            (DataType::Bool, SecondType::Simple) => Ok(downcast::<Bool>(t).unwrap()),
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

    fn copy(&self) -> Box<dyn PineType<'a> + 'a> {
        Box::new(self.clone())
    }
}

impl<'a> PineFrom<'a, Color<'a>> for Color<'a> {}

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

    fn copy(&self) -> Box<dyn PineType<'a> + 'a> {
        Box::new(self.clone())
    }
}

impl<'a> PineFrom<'a, String> for String {
    fn implicity_from(_t: Box<dyn PineType<'a> + 'a>) -> Result<Box<String>, ConvertErr> {
        match _t.get_type() {
            (DataType::String, SecondType::Simple) => Ok(downcast::<String>(_t)?),
            _ => Err(ConvertErr::NotSupportOperator),
        }
    }
}

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

    fn copy(&self) -> Box<dyn PineType<'a> + 'a> {
        Box::new(self.clone())
    }
}

impl<'a> PineFrom<'a, NA> for NA {}

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

    fn copy(&self) -> Box<dyn PineType<'a> + 'a> {
        Box::new(self.clone())
    }
}
impl<'a> PineFrom<'a, PineVar<'a>> for PineVar<'a> {}

// pine tuple type
pub struct Tuple<'a>(pub Vec<Box<dyn PineType<'a> + 'a>>);

impl<'a> PineStaticType for Tuple<'a> {
    fn static_type() -> (DataType, SecondType) {
        (DataType::Tuple, SecondType::Simple)
    }
}
impl<'a> PineType<'a> for Tuple<'a> {
    fn get_type(&self) -> (DataType, SecondType) {
        <Self as PineStaticType>::static_type()
    }

    fn copy(&self) -> Box<dyn PineType<'a> + 'a> {
        let new_vec = self.0.iter().map(|it| it.copy()).collect();
        Box::new(Tuple(new_vec))
    }
}
impl<'a> PineFrom<'a, Tuple<'a>> for Tuple<'a> {}

// pine callable type
#[derive(Debug, PartialEq, Clone)]
pub struct Callable<'a> {
    func: fn(
        HashMap<&'a str, Box<dyn PineType<'a> + 'a>>,
    ) -> Result<Box<dyn PineType<'a> + 'a>, ConvertErr>,
    param_names: Vec<&'static str>,
}
impl<'a> PineStaticType for Callable<'a> {
    fn static_type() -> (DataType, SecondType) {
        (DataType::Callable, SecondType::Simple)
    }
}
impl<'a> PineType<'a> for Callable<'a> {
    fn get_type(&self) -> (DataType, SecondType) {
        <Self as PineStaticType>::static_type()
    }

    fn copy(&self) -> Box<dyn PineType<'a> + 'a> {
        Box::new(self.clone())
    }
}
impl<'a> PineFrom<'a, Callable<'a>> for Callable<'a> {}

impl<'a> Callable<'a> {
    pub fn new(
        func: fn(
            HashMap<&'a str, Box<dyn PineType<'a> + 'a>>,
        ) -> Result<Box<dyn PineType<'a> + 'a>, ConvertErr>,
        param_names: Vec<&'static str>,
    ) -> Callable<'a> {
        Callable { func, param_names }
    }

    pub fn call(
        &self,
        pos_args: Vec<Box<dyn PineType<'a> + 'a>>,
        dict_args: Vec<(&'a str, Box<dyn PineType<'a> + 'a>)>,
    ) -> Result<Box<dyn PineType<'a> + 'a>, ConvertErr> {
        if pos_args.len() > self.param_names.len() {
            return Err(ConvertErr::NotValidParam);
        }

        let mut all_args: HashMap<&'a str, Box<dyn PineType<'a> + 'a>> = HashMap::new();
        for (i, val) in pos_args.into_iter().enumerate() {
            let name = self.param_names[i];
            all_args.insert(name, val);
        }
        for (name, val) in dict_args.into_iter() {
            match self.param_names.iter().any(|&v| name == v) {
                false => return Err(ConvertErr::NotValidParam),
                true => {
                    all_args.insert(name, val);
                }
            }
        }
        (self.func)(all_args)
    }
}

pub struct Object<'a> {
    obj: Box<dyn PineClass<'a> + 'a>,
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

    fn copy(&self) -> Box<dyn PineType<'a> + 'a> {
        self.obj.copy()
    }
}
impl<'a> PineFrom<'a, Object<'a>> for Object<'a> {}

impl<'a> Object<'a> {
    pub fn new(obj: Box<dyn PineClass<'a> + 'a>) -> Object<'a> {
        Object { obj }
    }

    pub fn get(&self, name: &str) -> Result<Box<dyn PineType<'a> + 'a>, ConvertErr> {
        self.obj.get(name)
    }

    pub fn set(&self, name: &str, property: Box<dyn PineType<'a> + 'a>) -> Result<(), ConvertErr> {
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
            <Bool as PineType>::get_type(&Bool::default()),
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

    #[test]
    fn color_test() {
        assert_eq!(Color::static_type(), (DataType::Color, SecondType::Simple));
        assert_eq!(
            Color::get_type(&Color("")),
            (DataType::Color, SecondType::Simple)
        );
    }

    fn test_func<'a>(
        mut args: HashMap<&'a str, Box<dyn PineType<'a> + 'a>>,
    ) -> Result<Box<dyn PineType<'a> + 'a>, ConvertErr> {
        let (arg1, arg2) = (args.remove("arg1").unwrap(), args.remove("arg2").unwrap());
        let s: Int =
            Some(downcast::<Int>(arg1).unwrap().unwrap() + downcast::<Int>(arg2).unwrap().unwrap());
        Ok(Box::new(s) as Box<dyn PineType>)
    }

    #[test]
    fn callable_test() {
        let callable = Callable::new(test_func, vec!["arg1", "arg2"]);
        let call_res = callable.call(
            vec![
                Box::new(Some(1)) as Box<dyn PineType>,
                Box::new(Some(2)) as Box<dyn PineType>,
            ],
            vec![],
        );
        assert_eq!(
            downcast::<Int>(call_res.unwrap()).unwrap(),
            Box::new(Some(3))
        );
    }

    struct A;
    impl<'a> PineClass<'a> for A {
        fn custom_type(&self) -> &str {
            "Custom A"
        }

        fn get(&self, name: &str) -> Result<Box<dyn PineType<'a> + 'a>, ConvertErr> {
            match name {
                "int1" => Ok(Box::new(Some(1i32))),
                "int2" => Ok(Box::new(Some(2i32))),
                "float1" => Ok(Box::new(Some(1f64))),
                "float2" => Ok(Box::new(Some(2f64))),
                _ => Err(ConvertErr::NotSupportOperator),
            }
        }

        fn set(&self, _n: &str, _p: Box<dyn PineType<'a> + 'a>) -> Result<(), ConvertErr> {
            Err(ConvertErr::NotSupportOperator)
        }

        fn copy(&self) -> Box<dyn PineType<'a> + 'a> {
            Box::new(Object::new(Box::new(A)))
        }
    }

    #[test]
    fn object_test() {
        let obj = Object::new(Box::new(A));
        assert_eq!(obj.get_type(), (DataType::Object, SecondType::Simple));
        assert_eq!(
            downcast::<Int>(obj.get("int1").unwrap()).unwrap(),
            Box::new(Some(1))
        );
    }
}
