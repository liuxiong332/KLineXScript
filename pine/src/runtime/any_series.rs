use crate::types::{
    DataType, Float, Int, PineFrom, PineRef, PineType, RefData, RuntimeErr, Series,
};
use std::fmt;
use std::mem;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum AnySeriesType {
    Int,
    Float,
}

pub struct AnySeries {
    data_ptr: *mut (),
    len: usize,
    cap: usize,
    series_type: AnySeriesType,
}

impl AnySeries {
    pub fn from_int_vec(mut v: Vec<Int>) -> AnySeries {
        v.shrink_to_fit();
        let mut v = mem::ManuallyDrop::new(v);
        AnySeries {
            len: v.len(),
            cap: v.capacity(),
            series_type: AnySeriesType::Int,
            data_ptr: v.as_mut_ptr() as *mut (),
        }
    }

    pub fn from_float_vec(mut v: Vec<Float>) -> AnySeries {
        v.shrink_to_fit();
        let mut v = mem::ManuallyDrop::new(v);
        // mem::forget(v);
        AnySeries {
            len: v.len(),
            cap: v.capacity(),
            series_type: AnySeriesType::Float,
            data_ptr: v.as_mut_ptr() as *mut (),
        }
    }

    pub fn into_vec<T>(self) -> Vec<T> {
        unsafe {
            let pv = mem::transmute::<*mut (), *mut T>(self.data_ptr);
            let res: Vec<T> = Vec::from_raw_parts(pv, self.len, self.cap);
            mem::forget(self);
            res
        }
    }

    pub fn as_vec<T>(&self) -> mem::ManuallyDrop<Vec<T>> {
        unsafe {
            let pv = mem::transmute::<*mut (), *mut T>(self.data_ptr);
            let res: Vec<T> = Vec::from_raw_parts(pv, self.len, self.cap);
            mem::ManuallyDrop::new(res)
        }
    }

    pub fn index<T>(&self, i: isize) -> T {
        unsafe {
            let pv = mem::transmute::<*mut (), *mut T>(self.data_ptr);
            pv.offset(i).read()
        }
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn get_type(&self) -> AnySeriesType {
        self.series_type
    }
}

impl Drop for AnySeries {
    fn drop(&mut self) {
        match self.series_type {
            AnySeriesType::Int => unsafe {
                let pv = mem::transmute::<*mut (), *mut Int>(self.data_ptr);
                Vec::from_raw_parts(pv, self.len, self.cap);
            },
            AnySeriesType::Float => unsafe {
                let pv = mem::transmute::<*mut (), *mut Float>(self.data_ptr);
                Vec::from_raw_parts(pv, self.len, self.cap);
            },
        }
    }
}

impl fmt::Debug for AnySeries {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.series_type {
            AnySeriesType::Int => self.as_vec::<Int>().fmt(f),
            AnySeriesType::Float => self.as_vec::<Float>().fmt(f),
        }
    }
}

impl Clone for AnySeries {
    fn clone(&self) -> Self {
        match self.series_type {
            AnySeriesType::Int => {
                AnySeries::from_int_vec(mem::ManuallyDrop::into_inner(self.as_vec::<Int>().clone()))
            }
            AnySeriesType::Float => AnySeries::from_float_vec(mem::ManuallyDrop::into_inner(
                self.as_vec::<Float>().clone(),
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_test() {
        let mut v = vec![Some(1f64), Some(2f64)];
        v.shrink_to_fit();
        let mut v = mem::ManuallyDrop::new(v);
        let len = v.len();
        let cap = v.capacity();
        let data_ptr = v.as_mut_ptr() as *mut () as *mut Float;
        unsafe {
            let pv = data_ptr as *mut Float;
            // let pv = mem::transmute::<*mut (), *mut Float>(series.data_ptr);
            let res: Vec<Float> = Vec::from_raw_parts(pv, len, cap);
            assert_eq!(res, vec![Some(1f64), Some(2f64)]);
        }
    }

    #[test]
    fn any_series_test() {
        let series = AnySeries::from_float_vec(vec![Some(1f64), Some(2f64)]);

        assert_eq!(series.into_vec::<Float>(), vec![Some(1f64), Some(2f64)]);

        let series = AnySeries::from_float_vec(vec![Some(1f64), Some(2f64)]);
        assert_eq!(
            series.as_vec::<Float>(),
            mem::ManuallyDrop::new(vec![Some(1f64), Some(2f64)])
        );
        assert_eq!(series.index::<Float>(0), Some(1f64));
        assert_eq!(series.index::<Float>(1), Some(2f64));

        assert_eq!(
            series.clone().into_vec::<Float>(),
            vec![Some(1f64), Some(2f64)]
        );
    }
}
