use crate::types::{Bool, Color, Float, Int, PineFrom, PineRef, RefData, Series};

pub fn pine_ref_to_bool<'a>(val: Option<PineRef<'a>>) -> Option<bool> {
    if val.is_none() {
        return None;
    }
    match Bool::implicity_from(val.unwrap()) {
        Ok(res) => Some(res.into_inner()),
        Err(_) => None,
    }
}

pub fn pine_ref_to_string<'a>(val: Option<PineRef<'a>>) -> Option<String> {
    if val.is_none() {
        return None;
    }
    match String::implicity_from(val.unwrap()) {
        Ok(res) => Some(res.into_inner()),
        Err(_) => None,
    }
}

pub fn pine_ref_to_color<'a>(val: Option<PineRef<'a>>) -> Option<String> {
    if val.is_none() {
        return None;
    }
    match Color::implicity_from(val.unwrap()) {
        Ok(res) => Some(String::from(res.into_inner().0)),
        Err(_) => None,
    }
}

pub fn pine_ref_to_color2<'a>(val: Option<PineRef<'a>>) -> Option<Color> {
    if val.is_none() {
        return None;
    }
    match Color::implicity_from(val.unwrap()) {
        Ok(res) => Some(res.into_inner()),
        Err(_) => None,
    }
}

pub fn pine_ref_to_i64<'a>(val: Option<PineRef<'a>>) -> Option<i64> {
    if val.is_none() {
        return None;
    }
    match Int::implicity_from(val.unwrap()) {
        Ok(res) => res.into_inner(),
        Err(_) => None,
    }
}

pub fn pine_ref_to_f64<'a>(val: Option<PineRef<'a>>) -> Option<f64> {
    if val.is_none() {
        return None;
    }
    match Float::implicity_from(val.unwrap()) {
        Ok(res) => res.into_inner(),
        Err(_) => None,
    }
}

pub fn pine_ref_to_f64_series<'a>(
    val: Option<PineRef<'a>>,
) -> Option<RefData<Series<Option<f64>>>> {
    if val.is_none() {
        return None;
    }
    match Series::implicity_from(val.unwrap()) {
        Ok(res) => Some(res),
        Err(_) => None,
    }
}

pub fn pine_ref_to_i64_series<'a>(
    val: Option<PineRef<'a>>,
) -> Option<RefData<Series<Option<i64>>>> {
    if val.is_none() {
        return None;
    }
    match Series::implicity_from(val.unwrap()) {
        Ok(res) => Some(res),
        Err(_) => None,
    }
}
