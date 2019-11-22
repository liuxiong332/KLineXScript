use super::traits::{ConvertErr, PineFrom, PineType};
use std::any::Any;
use std::marker::PhantomData;

// pine int type
pub type Int = Option<i32>;
impl<'a> PineType<'a> for Option<i32> {}

impl<'a> PineFrom<'a> for Option<i32> {
    fn from(t: Box<dyn PineType<'a> + 'a>) -> Result<Box<dyn PineType<'a> + 'a>, ConvertErr> {}

    fn auto_from(t: Box<dyn PineType<'a> + 'a>) -> Result<Box<dyn PineType<'a> + 'a>, ConvertErr> {
        let any_t: Box<dyn Any + 'a> = Box::from(t);
        if let Ok(int) = any_t.downcast::<Int>() {
            Ok(t)
        } else if let Ok(na) = any_t.downcast::<NA>() {
            Ok(Box::new(None))
        } else {
            Err(ConvertErr::NotCompatible)
        }
    }
}

// pine float type
pub type Float = f64;
impl<'a> PineType<'a> for Float {}

// pine bool type
pub type Bool = bool;
impl<'a> PineType<'a> for Bool {}

// pine color type
pub struct Color<'a>(pub &'a str);
impl<'a> PineType<'a> for Color<'a> {}

// pine string type
impl<'a> PineType<'a> for String {}

// pine na type
pub struct NA;
impl<'a> PineType<'a> for NA {}

// pine type that represent variable name
pub struct PineVar<'a>(pub &'a str);
impl<'a> PineType<'a> for PineVar<'a> {}

// pine tuple type
pub struct Tuple<'a>(pub Vec<Box<dyn PineType<'a> + 'a>>);
impl<'a> PineType<'a> for Tuple<'a> {}

// pine callable type
pub struct Callable<'a, D> {
    func: D,
    param_names: Vec<&'static str>,
    phantom: PhantomData<&'a D>,
}
impl<'a, D> PineType<'a> for Callable<'a, D> {}

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
        pos_args: Vec<Box<dyn PineType<'a> + 'a>>,
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
