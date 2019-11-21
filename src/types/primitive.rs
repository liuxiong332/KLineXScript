use super::traits::PineType;

pub type Int = i32;

impl<'a> PineType<'a> for Int {}

pub type Float = f64;

impl<'a> PineType<'a> for Float {}

pub type Bool = bool;

impl<'a> PineType<'a> for Bool {}

pub struct Color<'a>(pub &'a str);

impl<'a> PineType<'a> for Color<'a> {}

impl<'a> PineType<'a> for String {}

pub struct NA;
impl<'a> PineType<'a> for NA {}

pub struct PineVar<'a>(pub &'a str);
impl<'a> PineType<'a> for PineVar<'a> {}

pub struct Tuple<'a>(pub Vec<Box<dyn PineType<'a> + 'a>>);
impl<'a> PineType<'a> for Tuple<'a> {}
