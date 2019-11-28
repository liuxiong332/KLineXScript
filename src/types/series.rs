use super::downcast::downcast;
use super::traits::{ConvertErr, DataType, PineFrom, PineStaticType, PineType, SecondType};
use std::convert::From;
use std::marker::PhantomData;
use std::mem;

#[derive(Debug, PartialEq, Clone)]
pub struct Series<'a, D: Default + PineStaticType + PineType<'a> + Clone + 'a> {
    current: D,
    history: Vec<D>,
    phantom: PhantomData<&'a D>,
    na_val: D,
}

impl<'a, D: Default + PineStaticType + PineType<'a> + Clone + 'a> From<D> for Series<'a, D> {
    fn from(input: D) -> Self {
        Series {
            current: input,
            history: vec![],
            phantom: PhantomData,
            na_val: D::default(),
        }
    }
}

impl<'a, D: Default + PineStaticType + PineType<'a> + Clone + 'a> Series<'a, D> {
    pub fn new() -> Series<'a, D> {
        Series {
            current: D::default(),
            history: vec![],
            phantom: PhantomData,
            na_val: D::default(),
        }
    }

    pub fn from_vec(history: Vec<D>) -> Series<'a, D> {
        Series {
            current: D::default(),
            history,
            phantom: PhantomData,
            na_val: D::default(),
        }
    }

    pub fn index(&self, i: usize) -> Result<&D, ConvertErr> {
        let len = self.history.len();
        match i {
            // m if m < 0 => Err(SeriesErr::Negative),
            0 => Ok(&self.current),
            m if m >= 1 && m <= len => Ok(&self.history[(len - i) as usize]),
            _ => Ok(&self.na_val),
        }
    }

    pub fn update(&mut self, current: D) {
        self.current = current;
    }

    pub fn commit(&mut self) {
        self.history
            .push(mem::replace(&mut self.current, D::default()));
    }

    pub fn roll_back(&mut self) {
        self.history.pop().unwrap();
    }
}

impl<'a, D: Default + PineStaticType + PineType<'a> + Clone + 'a> PineStaticType for Series<'a, D> {
    fn static_type() -> (DataType, SecondType) {
        (<D as PineStaticType>::static_type().0, SecondType::Series)
    }
}

impl<'a, D: Default + PineStaticType + PineType<'a> + Clone + 'a> PineType<'a> for Series<'a, D> {
    fn get_type(&self) -> (DataType, SecondType) {
        (<D as PineStaticType>::static_type().0, SecondType::Series)
    }

    fn copy(&self) -> Box<dyn PineType<'a> + 'a> {
        let series = self.clone();
        Box::new(series)
    }
}

impl<'a, D: Default + PineStaticType + PineType<'a> + Clone + 'a> PineFrom<'a, Series<'a, D>>
    for Series<'a, D>
{
    fn explicity_from(t: Box<dyn PineType<'a> + 'a>) -> Result<Box<Series<'a, D>>, ConvertErr> {
        Self::implicity_from(t)
    }

    fn implicity_from(t: Box<dyn PineType<'a> + 'a>) -> Result<Box<Series<'a, D>>, ConvertErr> {
        let data_type = <D as PineStaticType>::static_type().0;
        match t.get_type() {
            (d, SecondType::Series) if data_type == d => Ok(downcast::<Series<D>>(t).unwrap()),
            (d, SecondType::Simple) if data_type == d => Ok(Box::new(Series {
                current: *downcast::<D>(t).unwrap(),
                history: vec![],
                phantom: PhantomData,
                na_val: D::default(),
            })),
            _ => Err(ConvertErr::NotCompatible),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::primitive::Int;
    use super::*;

    #[test]
    fn series_test() {
        let int: Int = Some(1);
        let mut series = <Series<Int> as From<Int>>::from(int);
        assert_eq!(series.index(0), Ok(&Some(1)));
        series.update(Some(2));
        assert_eq!(series.index(0), Ok(&Some(2)));

        series.commit();
        assert_eq!(series.history, vec![Some(2)]);
        assert_eq!(series.current, None);

        series.roll_back();
        assert_eq!(series.history, vec![]);
    }
}
