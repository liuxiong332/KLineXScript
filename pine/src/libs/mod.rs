pub mod accdist;
pub mod barstate;
pub mod color;
pub mod input;
pub mod plot;
pub mod plotarrow;
pub mod plotbar;
pub mod plotcandle;
pub mod plotchar;
pub mod plotshape;
pub mod print;
pub mod security;
pub mod study;
pub mod syminfo;
pub mod time;
pub mod timenow;
pub mod timestamp;

use crate::ast::syntax_type::SyntaxType;
use crate::types::PineRef;
use std::collections::HashSet;

pub struct VarResult<'a> {
    pub value: PineRef<'a>,
    pub syntax_type: SyntaxType<'a>,
    pub name: &'static str,
}

impl<'a> VarResult<'a> {
    pub fn new(
        value: PineRef<'a>,
        syntax_type: SyntaxType<'a>,
        name: &'static str,
    ) -> VarResult<'a> {
        VarResult {
            value,
            syntax_type,
            name,
        }
    }
}

fn check_names<'a>(vars: &Vec<VarResult<'a>>) -> bool {
    let mut set = HashSet::new();
    for v in vars {
        if set.contains(v.name) {
            return false;
        }
        set.insert(v.name);
    }
    return true;
}

pub fn declare_vars<'a>() -> Vec<VarResult<'a>> {
    let list = vec![
        plot::declare_var(),
        print::declare_var(),
        input::declare_var(),
        plotarrow::declare_var(),
        plotbar::declare_var(),
        plotcandle::declare_var(),
        plotchar::declare_var(),
        plotshape::declare_var(),
        color::declare_var(),
        study::declare_var(),
        syminfo::declare_var(),
        barstate::declare_var(),
        accdist::declare_var(),
        time::declare_var(),
        timenow::declare_var(),
        timestamp::declare_var(),
        security::declare_var(),
    ];
    debug_assert!(check_names(&list));
    // map.insert(print::VAR_NAME, print::declare_var());
    // map.insert(plot::VAR_NAME, plot::declare_var());
    list
}
