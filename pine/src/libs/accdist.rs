use crate::types::{
    Bool, Callable, CallableFactory, DataType, Evaluate, EvaluateVal, Float, Int, Object,
    ParamCollectCall, PineClass, PineFrom, PineRef, PineType, RefData, RuntimeErr, SecondType,
    Series, NA,
};

fn timestamp<'a>(
    _context: &mut dyn Ctx<'a>,
    mut param: Vec<Option<PineRef<'a>>>,
    _func_type: FunctionType<'a>,
) -> Result<PineRef<'a>, RuntimeErr> {
    Ok(PineRef::new(NA))
}

#[derive(Debug, Clone, PartialEq)]
struct AccDistVal {
    close_index: VarIndex,
    low_index: VarIndex,
    high_index: VarIndex,
    volume_index: VarIndex,
    prev_cmfv: Float,
    is_init: bool,
}

impl MyVal {
    pub fn new() -> AccDistVal {
        AccDistVal {
            close_index: VarIndex::new(0, 0),
            low_index: VarIndex::new(0, 0),
            high_index: VarIndex::new(0, 0),
            volume_index: VarIndex::new(0, 0),
            prev_cmfv: None,
            is_init: false,
        }
    }
}

// ref to https://www.investopedia.com/terms/a/accumulationdistribution.asp
// CMFV=Current money flow volume = ((Pc - Pl) - (Ph - Pc)) / (Ph - Pl) * V
impl<'a> EvaluateVal<'a> for MyVal {
    fn custom_name(&self) -> &str {
        "accdist"
    }

    fn run(&mut self, ctx: &mut dyn Ctx<'a>) -> Result<PineRef<'a>, PineRuntimeError> {
        if self.is_init {
            self.close_index = VarIndex::new(*ctx.get_varname_index("close").unwrap(), 0);
            self.low_index = VarIndex::new(*ctx.get_varname_index("low").unwrap(), 0);
            self.high_index = VarIndex::new(*ctx.get_varname_index("high").unwrap(), 0);
            self.volume_index = VarIndex::new(*ctx.get_varname_index("volume").unwrap(), 0);
            self.is_init = true;
        }
        match (
            ctx.get_var(self.close_index),
            ctx.get_var(self.low_index),
            ctx.get_var(self.high_index),
            ctx.get_var(self.volume_index),
        ) {
            (Some(close_val), Some(low_val), Some(high_val), Some(volume_val)) => {
                let close = downcast_pf_ref::<Series<Float>>(close_val).unwrap();
                let low = downcast_pf_ref::<Series<Float>>(low_val).unwrap();
                let high = downcast_pf_ref::<Series<Float>>(high_val).unwrap();
                let volume = downcast_pf_ref::<Series<Int>>(volume_val).unwrap();
                Ok(PineRef::new_rc(Series::from(
                    close.get_current().add(open.get_current()),
                )))
            }
            _ => Err(PineRuntimeError::new_no_range(RuntimeErr::VarNotFound)),
        }
    }

    fn copy(&self) -> Box<dyn EvaluateVal<'a>> {
        Box::new(self.clone())
    }
}
pub const VAR_NAME: &'static str = "accdist";

pub fn declare_var<'a>() -> VarResult<'a> {
    let value = PineRef::new(Evaluate::new(Box::new()));

    // plot(series, title, color, linewidth, style, trackprice, transp, histbase, offset, join, editable, show_last) → plot

    let syntax_type = SyntaxType::float_series();
    VarResult::new(value, syntax_type, VAR_NAME)
}
