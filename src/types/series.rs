use super::primitive::Int;
use super::traits::{
    downcast, ConvertErr, DataType, PineFrom, PineStaticType, PineType, SecondType,
};
use std::convert::From;
use std::marker::PhantomData;
use std::mem;

#[derive(Debug, PartialEq, Clone)]
pub struct Series<'a, D: PineStaticType + PineType<'a> + 'a> {
    current: Option<D>,
    history: Vec<Option<D>>,
    phantom: PhantomData<&'a D>,
}

#[derive(Debug, PartialEq)]
pub enum SeriesErr {
    Negative,
    OutBound,
}

impl<'a, D: PineStaticType + PineType<'a> + 'a> From<D> for Series<'a, D> {
    fn from(input: D) -> Self {
        Series {
            current: Some(input),
            history: vec![],
            phantom: PhantomData,
        }
    }
}

impl<'a, D: PineStaticType + PineType<'a> + 'a> Series<'a, D> {
    pub fn new() -> Series<'a, D> {
        Series {
            current: None,
            history: vec![],
            phantom: PhantomData,
        }
    }

    pub fn index(&self, i: usize) -> Result<&Option<D>, SeriesErr> {
        let len = self.history.len();
        match i {
            // m if m < 0 => Err(SeriesErr::Negative),
            0 => Ok(&self.current),
            m if m >= 1 && m <= len => Ok(&self.history[(len - i) as usize]),
            _ => Err(SeriesErr::OutBound),
        }
    }

    pub fn update(&mut self, current: Option<D>) {
        self.current = current;
    }

    pub fn commit(&mut self) {
        self.history.push(mem::replace(&mut self.current, None));
    }

    pub fn roll_back(&mut self) {
        self.history.pop().unwrap();
    }
}

impl<'a, D: PineStaticType + PineType<'a>> PineType<'a> for Series<'a, D> {
    fn get_type(&self) -> (DataType, SecondType) {
        (<D as PineStaticType>::get_type().0, SecondType::Series)
    }
}

impl<'a, D: PineStaticType + PineType<'a> + 'a> PineFrom<'a> for Series<'a, D> {
    fn from(t: Box<dyn PineType<'a> + 'a>) -> Result<Box<dyn PineType<'a> + 'a>, ConvertErr> {
        Self::auto_from(t)
    }

    fn auto_from(t: Box<dyn PineType<'a> + 'a>) -> Result<Box<dyn PineType<'a> + 'a>, ConvertErr> {
        let data_type = <D as PineStaticType>::get_type().0;
        match t.get_type() {
            (d, SecondType::Series) if data_type == d => Ok(t),
            (d, SecondType::Simple) if data_type == d => Ok(Box::new(Series {
                current: Some(*downcast::<D>(t).unwrap()),
                history: vec![],
                phantom: PhantomData,
            })),
            _ => Err(ConvertErr::NotCompatible),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn series_test() {
        let int: Int = Some(1);
        let mut series = <Series<Int> as From<Int>>::from(int);
        assert_eq!(series.index(0), Ok(&Some(Some(1))));
        series.update(Some(Some(2)));
        assert_eq!(series.index(0), Ok(&Some(Some(2))));

        series.commit();
        assert_eq!(series.history, vec![Some(Some(2))]);
        assert_eq!(series.current, None);

        series.roll_back();
        assert_eq!(series.history, vec![]);
    }
}
