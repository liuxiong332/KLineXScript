use super::VarResult;
use crate::ast::syntax_type::{FunctionType, FunctionTypes, SimpleSyntaxType, SyntaxType};
use crate::helper::err_msgs::*;
use crate::helper::str_replace;
use crate::helper::{
    move_element, pine_ref_to_bool, pine_ref_to_color, pine_ref_to_f64, pine_ref_to_i64,
    pine_ref_to_string,
};
use crate::runtime::context::{downcast_ctx, Ctx};
use crate::runtime::output::{OutputData, OutputInfo, PlotInfo, StrOptionsData};
use crate::types::{
    Bool, Callable, CallableObject, Category, Color, ComplexType, DataType, Float, Int,
    ParamCollectCall, PineClass, PineFrom, PineRef, PineType, RefData, RuntimeErr, SecondType,
    Series, SeriesCall, NA,
};
use std::borrow::Borrow;
use std::borrow::BorrowMut;
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
struct PerLine {
    x1: Int,
    x2: Int,
}

impl PerLine {
    pub fn new() -> PerLine {
        PerLine { x1: None, x2: None }
    }
}

#[derive(Debug, Clone, PartialEq)]
struct LineInfo {
    pub lines: Vec<Option<PerLine>>,
}

impl LineInfo {
    fn new_line(&mut self) {
        self.lines.push(Some(PerLine::new()))
    }
}

impl<'a> PineType<'a> for LineInfo {
    fn get_type(&self) -> (DataType, SecondType) {
        (DataType::Line, SecondType::Simple)
    }

    fn category(&self) -> Category {
        Category::Complex
    }

    fn copy(&self) -> PineRef<'a> {
        PineRef::new_rc(self.clone())
    }
}

impl ComplexType for LineInfo {}

#[derive(Debug)]
struct PlotVal {
    line_info: RefData<LineInfo>,
}

impl Clone for PlotVal {
    fn clone(&self) -> Self {
        self.borrow().clone()
    }
}

impl PlotVal {
    fn new() -> PlotVal {
        PlotVal {
            line_info: RefData::new_rc(LineInfo { lines: vec![] }),
        }
    }
}

impl<'a> SeriesCall<'a> for PlotVal {
    fn step(
        &mut self,
        context: &mut dyn Ctx<'a>,
        mut p: Vec<Option<PineRef<'a>>>,
        _func_type: FunctionType<'a>,
    ) -> Result<PineRef<'a>, RuntimeErr> {
        self.line_info.borrow_mut().new_line();
        Ok(RefData::clone(&self.line_info).into_pf())
    }

    fn copy(&self) -> Box<dyn SeriesCall<'a> + 'a> {
        Box::new(self.clone())
    }
}

struct PlotProps;

impl<'a> PineClass<'a> for PlotProps {
    fn custom_type(&self) -> &str {
        "line"
    }

    fn get(&self, _ctx: &mut dyn Ctx<'a>, name: &str) -> Result<PineRef<'a>, RuntimeErr> {
        match name {
            _ => Err(RuntimeErr::NotImplement(str_replace(
                NO_FIELD_IN_OBJECT,
                vec![String::from(name), String::from("plot")],
            ))),
        }
    }

    fn copy(&self) -> Box<dyn PineClass<'a> + 'a> {
        Box::new(PlotProps)
    }
}

pub const VAR_NAME: &'static str = "line";

pub fn declare_var<'a>() -> VarResult<'a> {
    let value = PineRef::new(CallableObject::new(Box::new(PlotProps), || {
        Callable::new(None, Some(Box::new(PlotVal::new())))
    }));

    // plot(series, title, color, linewidth, style, trackprice, transp, histbase, offset, join, editable, show_last) → plot

    let func_type = FunctionTypes(vec![FunctionType::new((
        vec![("x", SyntaxType::Simple(SimpleSyntaxType::Na))],
        SyntaxType::ObjectClass("line"),
    ))]);
    let mut obj_type = BTreeMap::new();
    obj_type.insert(
        "delete",
        SyntaxType::Function(Rc::new(FunctionTypes(vec![FunctionType::new((
            vec![("id", SyntaxType::ObjectClass("line"))],
            SyntaxType::Void,
        ))]))),
    );
    let syntax_type = SyntaxType::ObjectFunction(Rc::new(obj_type), Rc::new(func_type));
    VarResult::new(value, syntax_type, VAR_NAME)
}
