use crate::types::{Float, RefData, Series};

#[inline]
pub fn series_index(series: &Option<RefData<Series<Float>>>, index: usize) -> Float {
    match series {
        None => None,
        Some(s) => s.index_value(index).unwrap(),
    }
}

#[inline]
pub fn series_index2(series: &Series<Float>, index: usize) -> Float {
    series.index_value(index).unwrap()
}

#[inline]
pub fn series_mul(s1: Float, s2: Float, res: &mut Series<Float>) -> Float {
    use crate::types::traits::Arithmetic;
    let val = s1.mul(s2);
    res.update(val);
    val
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

pub fn float_add(val1: Float, val2: Float) -> Float {
    match (val1, val2) {
        (Some(v1), Some(v2)) => Some(v1 + v2),
        (None, Some(v2)) => Some(v2),
        (Some(v1), None) => Some(v1),
        _ => None,
    }
}
