use crate::helper::err_msgs::*;
use crate::helper::str_replace;
use crate::types::RuntimeErr;

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
