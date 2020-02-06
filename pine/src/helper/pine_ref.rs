use crate::types::{Bool, Float, Int, PineFrom, PineRef};

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

pub fn pine_ref_to_i32<'a>(val: Option<PineRef<'a>>) -> Option<i32> {
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
