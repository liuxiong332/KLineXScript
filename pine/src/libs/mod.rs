// pub mod plot;
// pub mod print;
use crate::types::PineRef;
use std::collections::HashMap;

pub fn declare_vars<'a>() -> HashMap<&'static str, PineRef<'a>> {
    let mut map = HashMap::new();
    // map.insert(print::VAR_NAME, print::declare_var());
    // map.insert(plot::VAR_NAME, plot::declare_var());
    map
}
