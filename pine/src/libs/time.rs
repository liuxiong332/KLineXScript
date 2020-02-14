use super::VarResult;
use crate::ast::stat_expr_types::VarIndex;
use crate::ast::syntax_type::{FunctionType, FunctionTypes, SyntaxType};
use crate::runtime::{downcast_ctx, Ctx, PineRuntimeError};
use crate::types::{Callable, CallableEvaluate, EvaluateVal, Float, PineRef, RuntimeErr, Series};
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
struct TimeVal {
    time_index: Option<VarIndex>,
}

impl TimeVal {
    pub fn new() -> TimeVal {
        TimeVal { time_index: None }
    }
}

impl<'a> EvaluateVal<'a> for TimeVal {
    fn custom_name(&self) -> &str {
        "time"
    }

    fn run(&mut self, ctx: &mut dyn Ctx<'a>) -> Result<PineRef<'a>, PineRuntimeError> {
        if self.time_index.is_none() {
            let i = downcast_ctx(ctx).get_varname_index("_time").unwrap();
            self.time_index = Some(VarIndex::new(*i, 0));
        }
        match ctx.get_var(self.time_index.unwrap()) {
            Some(val) => Ok(val.clone()),
            _ => Ok(PineRef::new_rc(Series::from(Float::from(None)))),
        }
    }

    fn copy(&self) -> Box<dyn EvaluateVal<'a>> {
        Box::new(self.clone())
    }
}

fn time<'a>(
    _context: &mut dyn Ctx<'a>,
    mut param: Vec<Option<PineRef<'a>>>,
    _func_type: FunctionType<'a>,
) -> Result<PineRef<'a>, RuntimeErr> {
    Ok(PineRef::new_box(Some(1)))
}

pub const VAR_NAME: &'static str = "time";

pub fn declare_var<'a>() -> VarResult<'a> {
    let value = PineRef::new(CallableEvaluate::new(Box::new(TimeVal::new()), || {
        Callable::new(Some(time), None)
    }));

    // plot(series, title, color, linewidth, style, trackprice, transp, histbase, offset, join, editable, show_last) → plot

    let func_type = FunctionTypes(vec![FunctionType::new((
        vec![
            ("resolution", SyntaxType::string()),
            ("session", SyntaxType::string()),
        ],
        SyntaxType::Void,
    ))]);
    let syntax_type = SyntaxType::ValFunction(Box::new(SyntaxType::int()), Rc::new(func_type));
    VarResult::new(value, syntax_type, VAR_NAME)
}
