use crate::types::{Float, RefData, Series};

pub fn series_index(series: &Option<RefData<Series<Float>>>, index: usize) -> Float {
    match series {
        None => None,
        Some(s) => s.index_value(index).unwrap(),
    }
}

pub fn float_max2(f1: Float, f2: Float) -> Float {
    vec![f1.unwrap_or(0f64), f2.unwrap_or(0f64)]
        .into_iter()
        .max_by(|a, b| a.partial_cmp(b).unwrap())
}

pub fn float_max(f1: Float, f2: Float, f3: Float) -> Float {
    vec![f1.unwrap_or(0f64), f2.unwrap_or(0f64), f3.unwrap_or(0f64)]
        .into_iter()
        .max_by(|a, b| a.partial_cmp(b).unwrap())
}

pub fn float_abs(val: Float) -> Float {
    match val {
        Some(f1) => Some(f1.abs()),
        None => None,
    }
}
