use super::downcast::downcast_pf;
use super::error::RuntimeErr;
use super::pine_ref::PineRef;
use super::primitive::{Bool, Color, Float, Int, NA};
use super::ref_data::RefData;
use super::traits::{
    Arithmetic, Category, ComplexType, DataType, Negative, PineFrom, PineStaticType, PineType,
    SecondType,
};
use std::cmp::{Ordering, PartialEq, PartialOrd};
use std::convert::{From, Into};
use std::fmt::Debug;
use std::marker::PhantomData;
use std::mem;

#[derive(Debug)]
pub struct Series<'a, D: Clone + Debug + 'a> {
    current: D,
    history: Vec<D>,
    phantom: PhantomData<&'a D>,
}

impl<'a, D: Default + Clone + Debug + 'a> Clone for Series<'a, D> {
    fn clone(&self) -> Self {
        Series {
            current: self.current.clone(),
            history: vec![],
            phantom: PhantomData,
        }
    }
}

impl<'a, D: Default + Clone + Debug + 'a> From<D> for Series<'a, D> {
    fn from(input: D) -> Self {
        Series {
            current: input,
            history: vec![],
            phantom: PhantomData,
        }
    }
}

impl<'a, D: Clone + Debug + 'a> Into<Vec<D>> for Series<'a, D> {
    fn into(self) -> Vec<D> {
        self.history
    }
}

impl<'a, D: Default + PineType<'a> + Clone + Debug + 'a> Series<'a, D> {
    pub fn new() -> Series<'a, D> {
        Series {
            current: D::default(),
            history: vec![],
            phantom: PhantomData,
        }
    }

    pub fn from_vec(history: Vec<D>) -> Series<'a, D> {
        Series {
            current: D::default(),
            history,
            phantom: PhantomData,
        }
    }

    pub fn from_cur_history(current: D, history: Vec<D>) -> Series<'a, D> {
        Series {
            current,
            history,
            phantom: PhantomData,
        }
    }

    pub fn index(&self, i: usize) -> Result<Series<'a, D>, RuntimeErr> {
        let len = self.history.len();
        let val = match i {
            // m if m < 0 => Err(SeriesErr::Negative),
            0 => self.current.clone(),
            m if m >= 1 && m <= len => self.history[(len - i) as usize].clone(),
            _ => D::default(),
        };
        Ok(Series::from(val))
    }

    pub fn index_value(&self, i: usize) -> Result<D, RuntimeErr> {
        let len = self.history.len();
        let val = match i {
            // m if m < 0 => Err(SeriesErr::Negative),
            0 => self.current.clone(),
            m if m >= 1 && len >= 1 && m <= len => self.history[(len - i) as usize].clone(),
            _ => D::default(),
        };
        Ok(val)
    }

    pub fn at(&self, i: usize) -> D {
        let len = self.history.len();
        match i {
            // m if m < 0 => Err(SeriesErr::Negative),
            0 => self.current.clone(),
            m if m >= 1 && len >= 1 && m <= len => self.history[(len - i) as usize].clone(),
            _ => D::default(),
        }
    }

    pub fn update(&mut self, current: D) {
        self.current = current;
    }

    pub fn commit(&mut self) {
        self.history
            .push(mem::replace(&mut self.current, D::default()));
    }

    pub fn update_commit(&mut self, current: D) {
        self.update(current);
        self.commit();
    }

    pub fn roll_back(&mut self) {
        if !self.history.is_empty() {
            self.history.pop().unwrap();
        }
    }

    pub fn get_current(&self) -> D {
        self.current.clone()
    }

    pub fn get_history(&self) -> &Vec<D> {
        &self.history
    }

    pub fn move_history(&mut self) -> Vec<D> {
        mem::replace(&mut self.history, vec![])
    }
}

impl<'a, D: PineStaticType + Clone + Debug + 'a> PineStaticType for Series<'a, D> {
    fn static_type() -> (DataType, SecondType) {
        (<D as PineStaticType>::static_type().0, SecondType::Series)
    }
}

impl<'a, D: PineStaticType + PineType<'a> + Default + Clone + Debug + 'a> PineType<'a>
    for Series<'a, D>
{
    fn get_type(&self) -> (DataType, SecondType) {
        (<D as PineStaticType>::static_type().0, SecondType::Series)
    }

    fn category(&self) -> Category {
        Category::Complex
    }

    fn copy(&self) -> PineRef<'a> {
        PineRef::new_rc(self.clone())
    }
}

impl<'a, D> PineFrom<'a, Series<'a, D>> for Series<'a, D>
where
    D: Default + PineStaticType + PartialEq + Clone + Debug + PineType<'a> + PineFrom<'a, D> + 'a,
{
    fn explicity_from(t: PineRef<'a>) -> Result<RefData<Series<'a, D>>, RuntimeErr> {
        let data_type = <D as PineStaticType>::static_type().0;
        match t.get_type() {
            (d, SecondType::Series) if data_type == d => Ok(downcast_pf::<Series<D>>(t).unwrap()),
            (d, SecondType::Simple) if data_type == d => Ok(RefData::new_rc(Series {
                current: downcast_pf::<D>(t).unwrap().into_inner(),
                history: vec![],
                phantom: PhantomData,
            })),
            (DataType::Int, SecondType::Series) => {
                let series: RefData<Series<Int>> = Series::explicity_from(t)?;
                let val: RefData<D> = D::explicity_from(PineRef::new(series.get_current()))?;
                Ok(RefData::new_rc(Series::from(val.into_inner())))
            }
            (DataType::Int, SecondType::Simple) => Ok(RefData::new_rc(Series::from(
                D::explicity_from(t)?.into_inner(),
            ))),
            (DataType::Float, SecondType::Series) => {
                let series: RefData<Series<Float>> = Series::explicity_from(t)?;
                let val: RefData<D> = D::explicity_from(PineRef::new(series.get_current()))?;
                Ok(RefData::new_rc(Series::from(val.into_inner())))
            }
            (DataType::Float, SecondType::Simple) => Ok(RefData::new_rc(Series::from(
                D::explicity_from(t)?.into_inner(),
            ))),
            (DataType::Bool, SecondType::Series) => {
                let series: RefData<Series<Bool>> = Series::explicity_from(t)?;
                let val: RefData<D> = D::explicity_from(PineRef::new(series.get_current()))?;
                Ok(RefData::new_rc(Series::from(val.into_inner())))
            }
            (DataType::Bool, SecondType::Simple) => Ok(RefData::new_rc(Series::from(
                D::explicity_from(t)?.into_inner(),
            ))),
            (DataType::NA, _) => Ok(RefData::new_rc(Series::from(
                D::explicity_from(PineRef::new_box(NA))?.into_inner(),
            ))),
            (DataType::Color, SecondType::Series) => {
                let series: RefData<Series<Color>> = Series::explicity_from(t)?;
                let val: RefData<D> = D::explicity_from(PineRef::new(series.get_current()))?;
                Ok(RefData::new_rc(Series::from(val.into_inner())))
            }
            (DataType::Color, SecondType::Simple) => Ok(RefData::new_rc(Series::from(
                D::explicity_from(t)?.into_inner(),
            ))),
            (DataType::String, SecondType::Series) => {
                let series: RefData<Series<String>> = Series::explicity_from(t)?;
                let val: RefData<D> = D::explicity_from(PineRef::new(series.get_current()))?;
                Ok(RefData::new_rc(Series::from(val.into_inner())))
            }
            (DataType::String, SecondType::Simple) => Ok(RefData::new_rc(Series::from(
                D::explicity_from(t)?.into_inner(),
            ))),
            _ => Err(RuntimeErr::UnknownRuntimeErr),
        }
    }

    fn implicity_from(t: PineRef<'a>) -> Result<RefData<Series<'a, D>>, RuntimeErr> {
        let data_type = <D as PineStaticType>::static_type().0;
        match t.get_type() {
            (d, SecondType::Series) if data_type == d => Ok(downcast_pf::<Series<D>>(t).unwrap()),
            (d, SecondType::Simple) if data_type == d => Ok(RefData::new_rc(Series {
                current: downcast_pf::<D>(t).unwrap().into_inner(),
                history: vec![],
                phantom: PhantomData,
            })),
            (DataType::Int, SecondType::Series) => {
                let series: RefData<Series<Int>> = Series::implicity_from(t)?;
                let val: RefData<D> = D::implicity_from(PineRef::new(series.get_current()))?;
                Ok(RefData::new_rc(Series::from(val.into_inner())))
            }
            (DataType::Int, SecondType::Simple) => Ok(RefData::new_rc(Series::from(
                D::implicity_from(t)?.into_inner(),
            ))),
            (DataType::Float, SecondType::Series) => {
                let series: RefData<Series<Float>> = Series::implicity_from(t)?;
                let val: RefData<D> = D::implicity_from(PineRef::new(series.get_current()))?;
                Ok(RefData::new_rc(Series::from(val.into_inner())))
            }
            (DataType::Float, SecondType::Simple) => Ok(RefData::new_rc(Series::from(
                D::implicity_from(t)?.into_inner(),
            ))),
            (DataType::Bool, SecondType::Series) => {
                let series: RefData<Series<Bool>> = Series::implicity_from(t)?;
                let val: RefData<D> = D::implicity_from(PineRef::new(series.get_current()))?;
                Ok(RefData::new_rc(Series::from(val.into_inner())))
            }
            (DataType::Bool, SecondType::Simple) => Ok(RefData::new_rc(Series::from(
                D::implicity_from(t)?.into_inner(),
            ))),
            (DataType::NA, _) => Ok(RefData::new_rc(Series::from(
                D::implicity_from(PineRef::new_box(NA))?.into_inner(),
            ))),
            (DataType::Color, SecondType::Series) => {
                let series: RefData<Series<Color>> = Series::implicity_from(t)?;
                let val: RefData<D> = D::implicity_from(PineRef::new(series.get_current()))?;
                Ok(RefData::new_rc(Series::from(val.into_inner())))
            }
            (DataType::Color, SecondType::Simple) => Ok(RefData::new_rc(Series::from(
                D::implicity_from(t)?.into_inner(),
            ))),
            (DataType::String, SecondType::Series) => {
                let series: RefData<Series<String>> = Series::implicity_from(t)?;
                let val: RefData<D> = D::implicity_from(PineRef::new(series.get_current()))?;
                Ok(RefData::new_rc(Series::from(val.into_inner())))
            }
            (DataType::String, SecondType::Simple) => Ok(RefData::new_rc(Series::from(
                D::implicity_from(t)?.into_inner(),
            ))),
            _ => Err(RuntimeErr::UnknownRuntimeErr),
        }
    }
}

impl<'a, D: Clone + Negative<D> + Debug + 'a> Negative<Series<'a, D>> for Series<'a, D> {
    fn negative(mut self) -> Series<'a, D> {
        self.current = self.current.negative();
        self
    }
}

impl<'a, D: Clone + Arithmetic + Debug + 'a> Arithmetic for Series<'a, D> {
    fn add(mut self, other: Self) -> Self {
        self.current = self.current.add(other.current);
        self
    }

    fn minus(mut self, other: Self) -> Self {
        self.current = self.current.minus(other.current);
        self
    }

    fn mul(mut self, other: Self) -> Self {
        self.current = self.current.mul(other.current);
        self
    }

    fn div(mut self, other: Self) -> Self {
        self.current = self.current.div(other.current);
        self
    }

    fn rem(mut self, other: Self) -> Self {
        self.current = self.current.rem(other.current);
        self
    }
}

impl<'a, D: PartialOrd + Clone + Debug + 'a> PartialOrd for Series<'a, D> {
    fn partial_cmp(&self, other: &Series<'a, D>) -> Option<Ordering> {
        self.current.partial_cmp(&other.current)
    }
}

impl<'a, D: PartialEq + Clone + Debug + 'a> PartialEq for Series<'a, D> {
    fn eq(&self, other: &Self) -> bool {
        self.current.eq(&other.current) && self.history.eq(&other.history)
    }
}

impl<'a, D: Clone + Debug + 'a> ComplexType for Series<'a, D> {}

#[cfg(test)]
mod tests {
    use super::super::primitive::Int;
    use super::*;

    #[test]
    fn series_test() {
        let int: Int = Some(1);
        let mut series: Series<Int> = Series::from(int);
        assert_eq!(series.index(0), Ok(Series::from(Some(1))));
        series.update(Some(2));
        assert_eq!(series.index(0), Ok(Series::from(Some(2))));

        series.commit();
        assert_eq!(series.history, vec![Some(2)]);
        assert_eq!(series.current, None);

        series.roll_back();
        assert_eq!(series.history, vec![]);
    }

    #[test]
    fn int_series_test() {
        let int: Int = Some(1);
        // implicity converter
        // Int => Series<Int>
        let series: RefData<Series<Int>> = Series::implicity_from(PineRef::new(int)).unwrap();
        assert_eq!(series.get_current(), int);

        // Series<Int> => Series<Int>
        let series2: RefData<Series<Int>> =
            Series::implicity_from(PineRef::new(Series::from(Some(1)))).unwrap();
        assert_eq!(series2.get_current(), int);

        // NA => Series<Int>
        let series3: RefData<Series<Int>> = Series::implicity_from(PineRef::new(NA)).unwrap();
        assert_eq!(series3.get_current(), None);

        // explicity converter
        // Int => Series<Int>
        let series4: RefData<Series<Int>> = Series::explicity_from(PineRef::new(int)).unwrap();
        assert_eq!(series4.get_current(), int);

        // Series<Int> => Series<Int>
        let series5: RefData<Series<Int>> =
            Series::explicity_from(PineRef::new(Series::from(Some(1)))).unwrap();
        assert_eq!(series5.get_current(), int);

        // NA => Series<Int>
        let series6: RefData<Series<Int>> = Series::explicity_from(PineRef::new(NA)).unwrap();
        assert_eq!(series6.get_current(), None);

        // Float => Series<Int>
        let series7: RefData<Series<Int>> =
            Series::explicity_from(PineRef::new(Some(1f64))).unwrap();
        assert_eq!(series7.get_current(), Some(1));

        // Float => Series<Int>
        let series8: RefData<Series<Int>> = Series::explicity_from(series7.into_pf()).unwrap();
        assert_eq!(series8.get_current(), Some(1));
    }

    #[test]
    fn float_series_test() {
        // implicity converter
        {
            // Float => Series<Float>
            let series: RefData<Series<Float>> =
                Series::implicity_from(PineRef::new(Some(1f64))).unwrap();
            assert_eq!(series.get_current(), Some(1f64));

            // Series<Float> => Series<Float>
            let series2: RefData<Series<Float>> =
                Series::implicity_from(PineRef::new(Series::from(Some(1f64)))).unwrap();
            assert_eq!(series2.get_current(), Some(1f64));

            // Int => Series<Float>
            let series3: RefData<Series<Float>> =
                Series::implicity_from(PineRef::new(Some(1))).unwrap();
            assert_eq!(series3.get_current(), Some(1f64));

            // Series<Int> => Series<Float>
            let series4: RefData<Series<Float>> =
                Series::implicity_from(PineRef::new(Series::from(Some(1)))).unwrap();
            assert_eq!(series4.get_current(), Some(1f64));

            // NA => Series<Float>
            let series5: RefData<Series<Float>> = Series::implicity_from(PineRef::new(NA)).unwrap();
            assert_eq!(series5.get_current(), None);
        }
        // explicity converter
        {
            // Float => Series<Float>
            let series: RefData<Series<Float>> =
                Series::explicity_from(PineRef::new(Some(1f64))).unwrap();
            assert_eq!(series.get_current(), Some(1f64));

            // Series<Float> => Series<Float>
            let series2: RefData<Series<Float>> =
                Series::explicity_from(PineRef::new(Series::from(Some(1f64)))).unwrap();
            assert_eq!(series2.get_current(), Some(1f64));

            // Int => Series<Float>
            let series3: RefData<Series<Float>> =
                Series::explicity_from(PineRef::new(Some(1))).unwrap();
            assert_eq!(series3.get_current(), Some(1f64));

            // Series<Int> => Series<Float>
            let series4: RefData<Series<Float>> =
                Series::explicity_from(PineRef::new(Series::from(Some(1)))).unwrap();
            assert_eq!(series4.get_current(), Some(1f64));

            // NA => Series<Float>
            let series5: RefData<Series<Float>> = Series::explicity_from(PineRef::new(NA)).unwrap();
            assert_eq!(series5.get_current(), None);
        }
    }

    #[test]
    fn bool_series_test() {
        // implicity converter
        {
            // Bool => Series<Bool>
            let series: RefData<Series<Bool>> = Series::implicity_from(PineRef::new(true)).unwrap();
            assert_eq!(series.get_current(), true);

            // Series<Bool> => Series<Bool>
            let series: RefData<Series<Bool>> =
                Series::implicity_from(PineRef::new(Series::from(true))).unwrap();
            assert_eq!(series.get_current(), true);

            // Float => Series<Bool>
            let series: RefData<Series<Bool>> =
                Series::implicity_from(PineRef::new(Some(1f64))).unwrap();
            assert_eq!(series.get_current(), true);

            // Series<Float> => Series<Bool>
            let series2: RefData<Series<Bool>> =
                Series::implicity_from(PineRef::new(Series::from(Some(1f64)))).unwrap();
            assert_eq!(series2.get_current(), true);

            // Int => Series<Bool>
            let series3: RefData<Series<Bool>> =
                Series::implicity_from(PineRef::new(Some(1))).unwrap();
            assert_eq!(series3.get_current(), true);

            // Series<Int> => Series<Bool>
            let series4: RefData<Series<Bool>> =
                Series::implicity_from(PineRef::new(Series::from(Some(1)))).unwrap();
            assert_eq!(series4.get_current(), true);

            // NA => Series<Bool>
            let series5: RefData<Series<Bool>> = Series::implicity_from(PineRef::new(NA)).unwrap();
            assert_eq!(series5.get_current(), false);
        }
        // explicity converter
        {
            // Bool => Series<Bool>
            let series: RefData<Series<Bool>> = Series::explicity_from(PineRef::new(true)).unwrap();
            assert_eq!(series.get_current(), true);

            // Series<Bool> => Series<Bool>
            let series: RefData<Series<Bool>> =
                Series::explicity_from(PineRef::new(Series::from(true))).unwrap();
            assert_eq!(series.get_current(), true);

            // Float => Series<Bool>
            let series: RefData<Series<Bool>> =
                Series::explicity_from(PineRef::new(Some(1f64))).unwrap();
            assert_eq!(series.get_current(), true);

            // Series<Float> => Series<Bool>
            let series2: RefData<Series<Bool>> =
                Series::explicity_from(PineRef::new(Series::from(Some(1f64)))).unwrap();
            assert_eq!(series2.get_current(), true);

            // Int => Series<Bool>
            let series3: RefData<Series<Bool>> =
                Series::explicity_from(PineRef::new(Some(1))).unwrap();
            assert_eq!(series3.get_current(), true);

            // Series<Int> => Series<Bool>
            let series4: RefData<Series<Bool>> =
                Series::explicity_from(PineRef::new(Series::from(Some(1)))).unwrap();
            assert_eq!(series4.get_current(), true);

            // NA => Series<Bool>
            let series5: RefData<Series<Bool>> = Series::explicity_from(PineRef::new(NA)).unwrap();
            assert_eq!(series5.get_current(), false);
        }
    }
}
