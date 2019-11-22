use super::traits::PineType;
use std::convert::From;
use std::marker::PhantomData;
use std::mem;

#[derive(Debug, PartialEq, Clone)]
pub struct Series<'a, D: PineType<'a>> {
    current: Option<D>,
    history: Vec<Option<D>>,
    phantom: PhantomData<&'a D>,
}

#[derive(Debug, PartialEq)]
pub enum SeriesErr {
    Negative,
    OutBound,
}

impl<'a, D: PineType<'a>> Series<'a, D> {
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

impl<'a, D: PineType<'a>> PineType<'a> for Series<'a, D> {}

impl<'a, D: PineType<'a>> From<D> for Series<'a, D> {
    fn from(input: D) -> Self {
        Series {
            current: Some(input),
            history: vec![],
            phantom: PhantomData,
        }
    }
}

impl<'a, D: PineType<'a>> From<Option<D>> for Series<'a, D> {
    fn from(input: Option<D>) -> Self {
        Series {
            current: input,
            history: vec![],
            phantom: PhantomData,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn series_test() {
        let series = Series::from(1);
        assert_eq!(series.index(0), Ok(&Some(1)));

        let mut series = Series::from(Some(1));
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
