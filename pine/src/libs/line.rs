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
    downcast_pf, Bool, Callable, CallableFactory, CallableObject, Category, Color, ComplexType,
    DataType, Float, Int, ParamCollectCall, PineClass, PineFrom, PineRef, PineStaticType, PineType,
    RefData, RuntimeErr, SecondType, Series, SeriesCall, SimpleType, NA,
};
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::fmt;
use std::mem;
use std::ops::Deref;
use std::rc::Rc;

pub type PerLineItem = Rc<RefCell<Option<PerLine>>>;

fn is_line_na<'a>(val: Option<PineRef<'a>>) -> bool {
    if val.is_none() {
        return true;
    }
    match PerLineItem::implicity_from(val.unwrap()) {
        Ok(res) => {
            let item: &PerLineItem = res.deref();
            item.borrow().is_none()
        }
        Err(_) => true,
    }
}

fn pine_ref_to_line<'a>(val: Option<PineRef<'a>>) -> PerLineItem {
    if val.is_none() {
        return Rc::new(RefCell::new(None));
    }
    match PerLineItem::implicity_from(val.unwrap()) {
        Ok(res) => res.into_inner(),
        Err(_) => Rc::new(RefCell::new(None)),
    }
}

// The line definition that represent every line object.
#[derive(Debug, Clone, PartialEq)]
pub struct PerLine {
    x1: Float,
    y1: Float,
    x2: Float,
    y2: Float,
    xloc: i32,
    extend: i32,
    color: Option<String>,
    style: i32,
    width: Option<i32>,
}

impl PerLine {
    pub fn new() -> PerLine {
        PerLine {
            x1: None,
            x2: None,
            y1: None,
            y2: None,
            xloc: 0,
            extend: 0,
            color: None,
            style: 0,
            width: None,
        }
    }

    pub fn new_xy(x1: Float, y1: Float, x2: Float, y2: Float) -> PerLine {
        PerLine {
            x1,
            y1,
            x2,
            y2,
            xloc: 0,
            extend: 0,
            color: None,
            style: 0,
            width: None,
        }
    }
}

impl PineStaticType for PerLineItem {
    fn static_type() -> (DataType, SecondType) {
        (DataType::Line, SecondType::Simple)
    }
}

impl<'a> PineFrom<'a, PerLineItem> for PerLineItem {
    fn implicity_from(t: PineRef<'a>) -> Result<RefData<PerLineItem>, RuntimeErr> {
        match t.get_type() {
            (DataType::Line, SecondType::Simple) => Ok(downcast_pf::<PerLineItem>(t).unwrap()),
            (DataType::Line, SecondType::Series) => {
                let f: RefData<Series<PerLineItem>> =
                    downcast_pf::<Series<PerLineItem>>(t).unwrap();
                Ok(RefData::new(f.get_current()))
            }
            (DataType::NA, _) => Ok(RefData::new(Rc::new(RefCell::new(None)))),
            _ => Err(RuntimeErr::UnknownRuntimeErr),
        }
    }
}

impl<'a> PineType<'a> for PerLineItem {
    fn get_type(&self) -> (DataType, SecondType) {
        (DataType::Line, SecondType::Simple)
    }
    fn copy(&self) -> PineRef<'a> {
        PineRef::new(self.clone())
    }
}

impl<'a> SimpleType for PerLineItem {}

// The line invocation that create new LineInfo object
#[derive(Debug)]
struct LineFromNaVal<'a> {
    lines: RefData<Series<'a, PerLineItem>>,
}

impl<'a> Clone for LineFromNaVal<'a> {
    fn clone(&self) -> Self {
        LineFromNaVal {
            lines: RefData::clone(&self.lines),
        }
    }
}

impl<'a> LineFromNaVal<'a> {
    fn new() -> LineFromNaVal<'a> {
        LineFromNaVal {
            lines: RefData::new(Series::from(Rc::new(RefCell::new(None)))),
        }
    }
}

impl<'a> SeriesCall<'a> for LineFromNaVal<'a> {
    fn step(
        &mut self,
        _context: &mut dyn Ctx<'a>,
        mut p: Vec<Option<PineRef<'a>>>,
        _func_type: FunctionType<'a>,
    ) -> Result<PineRef<'a>, RuntimeErr> {
        if p.len() == 1 {
            let v = mem::replace(&mut p[0], None);
            match is_line_na(v.clone()) {
                true => Ok(RefData::clone(&self.lines).into_pf()),
                false => Ok(Series::<'a, PerLineItem>::implicity_from(v.unwrap())
                    .unwrap()
                    .into_pf()),
            }
        } else {
            move_tuplet!((x1, y1, x2, y2, xloc, extend, color, style, width) = p);
            let line = PerLine {
                x1: pine_ref_to_f64(x1),
                y1: pine_ref_to_f64(y1),
                x2: pine_ref_to_f64(x2),
                y2: pine_ref_to_f64(y2),
                xloc: match pine_ref_to_string(xloc) {
                    None => 0,
                    Some(s) => match &s[..] {
                        "bar_index" => 0,
                        "bar_time" => 1,
                        _ => {
                            return Err(RuntimeErr::InvalidParameters(str_replace(
                                INVALID_VALS,
                                vec![String::from("xloc")],
                            )));
                        }
                    },
                },
                extend: 0,
                color: None,
                style: 0,
                width: None,
            };
            self.lines.update(Rc::new(RefCell::new(Some(line))));
            Ok(RefData::clone(&self.lines).into_pf())
        }
    }

    fn copy(&self) -> Box<dyn SeriesCall<'a> + 'a> {
        Box::new(self.clone())
    }
}

fn delete_func<'a>(
    _context: &mut dyn Ctx<'a>,
    mut param: Vec<Option<PineRef<'a>>>,
    _func_type: FunctionType<'a>,
) -> Result<PineRef<'a>, RuntimeErr> {
    let id = mem::replace(&mut param[0], None);
    let line = pine_ref_to_line(id);
    line.replace(None);
    Ok(PineRef::new(NA))
}

struct PlotProps;

impl<'a> PineClass<'a> for PlotProps {
    fn custom_type(&self) -> &str {
        "line"
    }

    fn get(&self, _ctx: &mut dyn Ctx<'a>, name: &str) -> Result<PineRef<'a>, RuntimeErr> {
        match name {
            "new" => Ok(PineRef::new(CallableFactory::new(|| {
                Callable::new(None, Some(Box::new(LineFromNaVal::new())))
            }))),
            "delete" => Ok(PineRef::new(Callable::new(Some(delete_func), None))),
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
        Callable::new(None, Some(Box::new(LineFromNaVal::new())))
    }));

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
    obj_type.insert(
        "new",
        SyntaxType::Function(Rc::new(FunctionTypes(vec![FunctionType::new((
            vec![
                ("x1", SyntaxType::float_series()),
                ("y1", SyntaxType::float_series()),
                ("x2", SyntaxType::float_series()),
                ("y2", SyntaxType::float_series()),
                ("xloc", SyntaxType::string_series()),
                ("extend", SyntaxType::string_series()),
                ("color", SyntaxType::color_series()),
                ("style", SyntaxType::string_series()),
                ("width", SyntaxType::float_series()),
            ],
            SyntaxType::ObjectClass("line"),
        ))]))),
    );
    let syntax_type = SyntaxType::ObjectFunction(Rc::new(obj_type), Rc::new(func_type));
    VarResult::new(value, syntax_type, VAR_NAME)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::{AnySeries, NoneCallback};
    use crate::{LibInfo, PineParser, PineRunner};

    #[test]
    fn line_capture_test() {
        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
        );
        let src = "line(na)\nx = line(na)\nx2 = if true\n    line(na)\nelse\n    line(na)";
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner.runl(&vec![], 2, None).unwrap();
        assert_eq!(runner.get_context().get_shapes().len(), 3);
    }

    #[test]
    fn line_new_capture_test() {
        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
        );
        let src = "line.new(1, 2, 3 ,4)\nx = line.new(1, 2, 3, 4)\n";
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner.runl(&vec![], 2, None).unwrap();
        assert_eq!(runner.get_context().get_shapes().len(), 2);
    }

    #[test]
    fn line_test() {
        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
        );
        let src = "x = line(na)\nline y = line(na)\nline x2 = line.new(1, 2, 3, 4)";
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner.runl(&vec![], 2, None).unwrap();
    }

    #[test]
    fn line_create_test() {
        use crate::ast::stat_expr_types::VarIndex;

        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
        );
        let src = "x = line(na)\nx := line.new(1, 2, 3, 4)";
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner.runl(&vec![], 2, None).unwrap();

        assert_eq!(runner.get_context().get_shapes().len(), 1);

        let result = runner.get_context().move_var(VarIndex::new(0, 0)).unwrap();
        assert_eq!(
            Series::implicity_from(result).unwrap(),
            RefData::new(Series::from_vec(vec![
                Rc::new(RefCell::new(Some(PerLine::new_xy(
                    Some(1f64),
                    Some(2f64),
                    Some(3f64),
                    Some(4f64)
                )))),
                Rc::new(RefCell::new(Some(PerLine::new_xy(
                    Some(1f64),
                    Some(2f64),
                    Some(3f64),
                    Some(4f64)
                ))))
            ]))
        );
    }

    #[test]
    fn line_delete_test() {
        use crate::ast::stat_expr_types::VarIndex;

        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
        );
        let src = "x = line(na)\nx := line.new(1, 2, 3, 4)\nline.delete(x[1])";
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner.runl(&vec![], 2, None).unwrap();

        assert_eq!(runner.get_context().get_shapes().len(), 1);

        let result = runner.get_context().move_var(VarIndex::new(0, 0)).unwrap();
        assert_eq!(
            Series::implicity_from(result).unwrap(),
            RefData::new(Series::from_vec(vec![
                Rc::new(RefCell::new(None)),
                Rc::new(RefCell::new(Some(PerLine::new_xy(
                    Some(1f64),
                    Some(2f64),
                    Some(3f64),
                    Some(4f64)
                ))))
            ]))
        );
    }
}
