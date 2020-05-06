use crate::helper::err_msgs::*;
use crate::helper::str_replace;
use crate::types::RuntimeErr;

#[inline]
pub fn require_param<'a, T>(name: &'static str, val: Option<T>) -> Result<T, RuntimeErr> {
    if let Some(v) = val {
        Ok(v)
    } else {
        Err(RuntimeErr::MissingParameters(str_replace(
            REQUIRED_PARAMETERS,
            vec![String::from(name)],
        )))
    }
}

#[inline]
pub fn check_ge1_i64<'a>(name: &'static str, v: i64) -> Result<i64, RuntimeErr> {
    if v < 1 {
        return Err(RuntimeErr::InvalidParameters(str_replace(
            GE_1,
            vec![String::from(name)],
        )));
    }
    Ok(v)
}

#[inline]
pub fn check_ge1_f64<'a>(name: &'static str, v: f64) -> Result<f64, RuntimeErr> {
    if v < 1f64 {
        return Err(RuntimeErr::InvalidParameters(str_replace(
            GE_1,
            vec![String::from(name)],
        )));
    }
    Ok(v)
}

#[inline]
pub fn ge1_param_i64<'a>(name: &'static str, val: Option<i64>) -> Result<i64, RuntimeErr> {
    let v = require_param(name, val)?;
    check_ge1_i64(name, v)?;
    Ok(v)
}

#[inline]
pub fn ge1_param_f64<'a>(name: &'static str, val: Option<f64>) -> Result<f64, RuntimeErr> {
    let v = require_param(name, val)?;
    check_ge1_f64(name, v)?;
    Ok(v)
}
