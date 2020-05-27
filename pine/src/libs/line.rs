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
use std::cell::{Ref, RefCell};
use std::collections::BTreeMap;
use std::fmt;
use std::mem;
use std::ops::Deref;
use std::rc::Rc;

pub type PerLineItem = Rc<RefCell<Option<PerLine>>>;

#[derive(Clone, Copy, Debug)]
pub enum ExtendEnum {
    None = 0,
    Right = 1,
    Left = 2,
    Both = 3,
}

impl ExtendEnum {
    fn from_str(s: &str) -> Result<ExtendEnum, RuntimeErr> {
        match s {
            "none" => Ok(ExtendEnum::None),
            "right" => Ok(ExtendEnum::Right),
            "left" => Ok(ExtendEnum::Left),
            "both" => Ok(ExtendEnum::Both),
            _ => Err(RuntimeErr::InvalidParameters(str_replace(
                INVALID_VALS,
                vec![String::from("extend")],
            ))),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum StyleEnum {
    Solid = 0,
    Dotted = 1,
    Dashed = 2,
    ArrowLeft = 3,
    ArrowRight = 4,
    ArrowBoth = 5,
}

impl StyleEnum {
    fn from_str(s: &str) -> Result<StyleEnum, RuntimeErr> {
        match s {
            "solid" => Ok(StyleEnum::Solid),
            "dotted" => Ok(StyleEnum::Dotted),
            "dashed" => Ok(StyleEnum::Dashed),
            "arrow_left" => Ok(StyleEnum::ArrowLeft),
            "arrow_right" => Ok(StyleEnum::ArrowRight),
            "arrow_both" => Ok(StyleEnum::ArrowBoth),
            _ => Err(RuntimeErr::InvalidParameters(str_replace(
                INVALID_VALS,
                vec![String::from("style")],
            ))),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum XlocEnum {
    BarIndex = 0,
    BarTime = 1,
}

impl XlocEnum {
    fn from_str(s: &str) -> Result<XlocEnum, RuntimeErr> {
        match s {
            "bar_index" => Ok(XlocEnum::BarIndex),
            "bar_time" => Ok(XlocEnum::BarTime),
            _ => Err(RuntimeErr::InvalidParameters(str_replace(
                INVALID_VALS,
                vec![String::from("xloc")],
            ))),
        }
    }
}

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
    x1: Int,
    y1: Float,
    x2: Int,
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

    pub fn new_xy(x1: Int, y1: Float, x2: Int, y2: Float) -> PerLine {
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
                x1: pine_ref_to_i64(x1),
                y1: pine_ref_to_f64(y1),
                x2: pine_ref_to_i64(x2),
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

fn get_val_func<'a, T: Default + Clone + fmt::Debug + PineType<'a> + PineStaticType + 'a>(
    _context: &mut dyn Ctx<'a>,
    mut param: Vec<Option<PineRef<'a>>>,
    func: impl Fn(&PerLine) -> T,
) -> Result<PineRef<'a>, RuntimeErr> {
    let id = mem::replace(&mut param[0], None);
    let line = pine_ref_to_line(id);
    match unsafe { line.as_ptr().as_ref().unwrap() } {
        None => Ok(PineRef::new(NA)),
        Some(v) => Ok(PineRef::new(Series::from(func(v)))),
    }
}

fn get_x1_func<'a>(
    _context: &mut dyn Ctx<'a>,
    param: Vec<Option<PineRef<'a>>>,
    _func_type: FunctionType<'a>,
) -> Result<PineRef<'a>, RuntimeErr> {
    get_val_func(_context, param, |v| v.x1)
}

fn get_x2_func<'a>(
    _context: &mut dyn Ctx<'a>,
    param: Vec<Option<PineRef<'a>>>,
    _func_type: FunctionType<'a>,
) -> Result<PineRef<'a>, RuntimeErr> {
    get_val_func(_context, param, |v| v.x2)
}

fn get_y1_func<'a>(
    _context: &mut dyn Ctx<'a>,
    param: Vec<Option<PineRef<'a>>>,
    _func_type: FunctionType<'a>,
) -> Result<PineRef<'a>, RuntimeErr> {
    get_val_func(_context, param, |v| v.y1)
}

fn get_y2_func<'a>(
    _context: &mut dyn Ctx<'a>,
    param: Vec<Option<PineRef<'a>>>,
    _func_type: FunctionType<'a>,
) -> Result<PineRef<'a>, RuntimeErr> {
    get_val_func(_context, param, |v| v.y2)
}

fn set_val_func<'a>(
    _context: &mut dyn Ctx<'a>,
    mut param: Vec<Option<PineRef<'a>>>,
    mut func: impl FnMut(&mut PerLine, Option<PineRef<'a>>) -> Result<(), RuntimeErr>,
) -> Result<PineRef<'a>, RuntimeErr> {
    let id = mem::replace(&mut param[0], None);
    let val = mem::replace(&mut param[1], None);
    let line = pine_ref_to_line(id);

    if line.borrow_mut().is_none() {
        *line.borrow_mut() = Some(PerLine::new());
    }
    func(line.borrow_mut().as_mut().unwrap(), val);
    Ok(PineRef::new(NA))
}

fn set_x1_func<'a>(
    _c: &mut dyn Ctx<'a>,
    p: Vec<Option<PineRef<'a>>>,
    _f: FunctionType<'a>,
) -> Result<PineRef<'a>, RuntimeErr> {
    set_val_func(_c, p, |l, v| {
        l.x1 = pine_ref_to_i64(v);
        Ok(())
    })
}

fn set_x2_func<'a>(
    _context: &mut dyn Ctx<'a>,
    p: Vec<Option<PineRef<'a>>>,
    _f: FunctionType<'a>,
) -> Result<PineRef<'a>, RuntimeErr> {
    set_val_func(_context, p, |l, v| {
        l.x2 = pine_ref_to_i64(v);
        Ok(())
    })
}

fn set_y1_func<'a>(
    _context: &mut dyn Ctx<'a>,
    param: Vec<Option<PineRef<'a>>>,
    _func_type: FunctionType<'a>,
) -> Result<PineRef<'a>, RuntimeErr> {
    set_val_func(_context, param, |l, v| {
        l.y1 = pine_ref_to_f64(v);
        Ok(())
    })
}

fn set_y2_func<'a>(
    _context: &mut dyn Ctx<'a>,
    param: Vec<Option<PineRef<'a>>>,
    _func_type: FunctionType<'a>,
) -> Result<PineRef<'a>, RuntimeErr> {
    set_val_func(_context, param, |l, v| {
        l.y2 = pine_ref_to_f64(v);
        Ok(())
    })
}

fn set_color_func<'a>(
    _context: &mut dyn Ctx<'a>,
    param: Vec<Option<PineRef<'a>>>,
    _func_type: FunctionType<'a>,
) -> Result<PineRef<'a>, RuntimeErr> {
    set_val_func(_context, param, |l, v| {
        l.color = pine_ref_to_color(v);
        Ok(())
    })
}

fn set_extend_func<'a>(
    _context: &mut dyn Ctx<'a>,
    param: Vec<Option<PineRef<'a>>>,
    _func_type: FunctionType<'a>,
) -> Result<PineRef<'a>, RuntimeErr> {
    set_val_func(_context, param, |l, v| {
        l.extend = match pine_ref_to_color(v) {
            None => 0,
            Some(v) => ExtendEnum::from_str(&v[..])? as i32,
        };
        Ok(())
    })
}

fn set_style_func<'a>(
    _context: &mut dyn Ctx<'a>,
    param: Vec<Option<PineRef<'a>>>,
    _func_type: FunctionType<'a>,
) -> Result<PineRef<'a>, RuntimeErr> {
    set_val_func(_context, param, |l, v| {
        l.style = match pine_ref_to_string(v) {
            None => 0,
            Some(v) => StyleEnum::from_str(&v[..])? as i32,
        };
        Ok(())
    })
}

fn set_width_func<'a>(
    _context: &mut dyn Ctx<'a>,
    param: Vec<Option<PineRef<'a>>>,
    _func_type: FunctionType<'a>,
) -> Result<PineRef<'a>, RuntimeErr> {
    set_val_func(_context, param, |l, v| {
        l.width = match pine_ref_to_i64(v) {
            None => None,
            Some(v) => Some(v as i32),
        };
        Ok(())
    })
}

fn set_xloc_func<'a>(
    _context: &mut dyn Ctx<'a>,
    mut param: Vec<Option<PineRef<'a>>>,
    _func_type: FunctionType<'a>,
) -> Result<PineRef<'a>, RuntimeErr> {
    move_tuplet!((id, x1, x2, xloc) = param);
    let line = pine_ref_to_line(id);

    if line.borrow_mut().is_none() {
        *line.borrow_mut() = Some(PerLine::new());
    }
    let mut li = line.borrow_mut();
    li.as_mut().unwrap().x1 = pine_ref_to_i64(x1);
    li.as_mut().unwrap().x2 = pine_ref_to_i64(x2);
    li.as_mut().unwrap().xloc = match pine_ref_to_string(xloc) {
        None => XlocEnum::BarIndex as i32,
        Some(s) => XlocEnum::from_str(&s[..])? as i32,
    };
    Ok(PineRef::new(NA))
}

fn set_xy1_func<'a>(
    _context: &mut dyn Ctx<'a>,
    mut param: Vec<Option<PineRef<'a>>>,
    _func_type: FunctionType<'a>,
) -> Result<PineRef<'a>, RuntimeErr> {
    move_tuplet!((id, x, y) = param);
    let line = pine_ref_to_line(id);
    if line.borrow_mut().is_none() {
        *line.borrow_mut() = Some(PerLine::new());
    }
    let mut li = line.borrow_mut();
    li.as_mut().unwrap().x1 = pine_ref_to_i64(x);
    li.as_mut().unwrap().y1 = pine_ref_to_f64(y);
    Ok(PineRef::new(NA))
}

fn set_xy2_func<'a>(
    _context: &mut dyn Ctx<'a>,
    mut param: Vec<Option<PineRef<'a>>>,
    _func_type: FunctionType<'a>,
) -> Result<PineRef<'a>, RuntimeErr> {
    move_tuplet!((id, x, y) = param);
    let line = pine_ref_to_line(id);
    if line.borrow_mut().is_none() {
        *line.borrow_mut() = Some(PerLine::new());
    }
    let mut li = line.borrow_mut();
    li.as_mut().unwrap().x2 = pine_ref_to_i64(x);
    li.as_mut().unwrap().y2 = pine_ref_to_f64(y);
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
            "get_x1" => Ok(PineRef::new(Callable::new(Some(get_x1_func), None))),
            "get_x2" => Ok(PineRef::new(Callable::new(Some(get_x2_func), None))),
            "get_y1" => Ok(PineRef::new(Callable::new(Some(get_y1_func), None))),
            "get_y2" => Ok(PineRef::new(Callable::new(Some(get_y2_func), None))),
            "set_x1" => Ok(PineRef::new(Callable::new(Some(set_x1_func), None))),
            "set_x2" => Ok(PineRef::new(Callable::new(Some(set_x2_func), None))),
            "set_y1" => Ok(PineRef::new(Callable::new(Some(set_y1_func), None))),
            "set_y2" => Ok(PineRef::new(Callable::new(Some(set_y2_func), None))),
            "set_color" => Ok(PineRef::new(Callable::new(Some(set_color_func), None))),
            "set_extend" => Ok(PineRef::new(Callable::new(Some(set_extend_func), None))),
            "set_style" => Ok(PineRef::new(Callable::new(Some(set_style_func), None))),
            "set_width" => Ok(PineRef::new(Callable::new(Some(set_width_func), None))),
            "set_x1" => Ok(PineRef::new(Callable::new(Some(set_x1_func), None))),
            "set_x2" => Ok(PineRef::new(Callable::new(Some(set_x2_func), None))),
            "set_y1" => Ok(PineRef::new(Callable::new(Some(set_y1_func), None))),
            "set_y2" => Ok(PineRef::new(Callable::new(Some(set_y2_func), None))),
            "set_xloc" => Ok(PineRef::new(Callable::new(Some(set_xloc_func), None))),
            "set_xy1" => Ok(PineRef::new(Callable::new(Some(set_xy1_func), None))),
            "set_xy2" => Ok(PineRef::new(Callable::new(Some(set_xy2_func), None))),
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
                ("x1", SyntaxType::int_series()),
                ("y1", SyntaxType::float_series()),
                ("x2", SyntaxType::int_series()),
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
    obj_type.insert(
        "get_x1",
        SyntaxType::Function(Rc::new(FunctionTypes(vec![FunctionType::new((
            vec![("id", SyntaxType::ObjectClass("line"))],
            SyntaxType::int_series(),
        ))]))),
    );
    obj_type.insert(
        "get_x2",
        SyntaxType::Function(Rc::new(FunctionTypes(vec![FunctionType::new((
            vec![("id", SyntaxType::ObjectClass("line"))],
            SyntaxType::int_series(),
        ))]))),
    );
    obj_type.insert(
        "get_y1",
        SyntaxType::Function(Rc::new(FunctionTypes(vec![FunctionType::new((
            vec![("id", SyntaxType::ObjectClass("line"))],
            SyntaxType::float_series(),
        ))]))),
    );
    obj_type.insert(
        "get_y2",
        SyntaxType::Function(Rc::new(FunctionTypes(vec![FunctionType::new((
            vec![("id", SyntaxType::ObjectClass("line"))],
            SyntaxType::float_series(),
        ))]))),
    );
    obj_type.insert(
        "set_color",
        SyntaxType::Function(Rc::new(FunctionTypes(vec![FunctionType::new((
            vec![
                ("id", SyntaxType::ObjectClass("line")),
                ("color", SyntaxType::color_series()),
            ],
            SyntaxType::Void,
        ))]))),
    );
    obj_type.insert(
        "set_extend",
        SyntaxType::Function(Rc::new(FunctionTypes(vec![FunctionType::new((
            vec![
                ("id", SyntaxType::ObjectClass("line")),
                ("extend", SyntaxType::string_series()),
            ],
            SyntaxType::Void,
        ))]))),
    );
    obj_type.insert(
        "set_style",
        SyntaxType::Function(Rc::new(FunctionTypes(vec![FunctionType::new((
            vec![
                ("id", SyntaxType::ObjectClass("line")),
                ("extend", SyntaxType::string_series()),
            ],
            SyntaxType::Void,
        ))]))),
    );
    obj_type.insert(
        "set_width",
        SyntaxType::Function(Rc::new(FunctionTypes(vec![FunctionType::new((
            vec![
                ("id", SyntaxType::ObjectClass("line")),
                ("extend", SyntaxType::int_series()),
            ],
            SyntaxType::Void,
        ))]))),
    );
    obj_type.insert(
        "set_x1",
        SyntaxType::Function(Rc::new(FunctionTypes(vec![FunctionType::new((
            vec![
                ("id", SyntaxType::ObjectClass("line")),
                ("x", SyntaxType::int_series()),
            ],
            SyntaxType::Void,
        ))]))),
    );
    obj_type.insert(
        "set_x2",
        SyntaxType::Function(Rc::new(FunctionTypes(vec![FunctionType::new((
            vec![
                ("id", SyntaxType::ObjectClass("line")),
                ("x", SyntaxType::int_series()),
            ],
            SyntaxType::Void,
        ))]))),
    );
    obj_type.insert(
        "set_y1",
        SyntaxType::Function(Rc::new(FunctionTypes(vec![FunctionType::new((
            vec![
                ("id", SyntaxType::ObjectClass("line")),
                ("y", SyntaxType::float_series()),
            ],
            SyntaxType::Void,
        ))]))),
    );
    obj_type.insert(
        "set_y2",
        SyntaxType::Function(Rc::new(FunctionTypes(vec![FunctionType::new((
            vec![
                ("id", SyntaxType::ObjectClass("line")),
                ("y", SyntaxType::float_series()),
            ],
            SyntaxType::Void,
        ))]))),
    );
    obj_type.insert(
        "set_xloc",
        SyntaxType::Function(Rc::new(FunctionTypes(vec![FunctionType::new((
            vec![
                ("id", SyntaxType::ObjectClass("line")),
                ("x1", SyntaxType::int_series()),
                ("x2", SyntaxType::int_series()),
                ("xloc", SyntaxType::string_series()),
            ],
            SyntaxType::Void,
        ))]))),
    );
    obj_type.insert(
        "set_xy1",
        SyntaxType::Function(Rc::new(FunctionTypes(vec![FunctionType::new((
            vec![
                ("id", SyntaxType::ObjectClass("line")),
                ("x", SyntaxType::int_series()),
                ("y", SyntaxType::float_series()),
            ],
            SyntaxType::Void,
        ))]))),
    );
    obj_type.insert(
        "set_xy2",
        SyntaxType::Function(Rc::new(FunctionTypes(vec![FunctionType::new((
            vec![
                ("id", SyntaxType::ObjectClass("line")),
                ("x", SyntaxType::int_series()),
                ("y", SyntaxType::float_series()),
            ],
            SyntaxType::Void,
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
                    Some(1i64),
                    Some(2f64),
                    Some(3i64),
                    Some(4f64)
                )))),
                Rc::new(RefCell::new(Some(PerLine::new_xy(
                    Some(1i64),
                    Some(2f64),
                    Some(3i64),
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
                    Some(1i64),
                    Some(2f64),
                    Some(3i64),
                    Some(4f64)
                ))))
            ]))
        );
    }

    #[test]
    fn line_get_test() {
        use crate::ast::stat_expr_types::VarIndex;

        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
        );
        let src = r"
        x = line.new(1, 2, 3, 4)
        x1 = line.get_x1(x)
        x2 = line.get_x2(x)
        y1 = line.get_y1(x)
        y2 = line.get_y2(x)
        ";
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner.runl(&vec![], 2, None).unwrap();

        assert_eq!(runner.get_context().get_shapes().len(), 1);

        assert_eq!(
            runner.get_context().move_var(VarIndex::new(1, 0)),
            Some(PineRef::new(Series::from_vec(vec![Some(1i64), Some(1i64)])))
        );
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(2, 0)),
            Some(PineRef::new(Series::from_vec(vec![Some(3i64), Some(3i64)])))
        );
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(3, 0)),
            Some(PineRef::new(Series::from_vec(vec![Some(2f64), Some(2f64)])))
        );
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(4, 0)),
            Some(PineRef::new(Series::from_vec(vec![Some(4f64), Some(4f64)])))
        );
    }

    #[test]
    fn line_set_test() {
        use crate::ast::stat_expr_types::VarIndex;

        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
        );
        let src = r#"
        x = line.new(1, 2, 3, 4)
        line.set_x1(x, int(close))
        line.set_x2(x, int(close) + 1)
        line.set_y1(x, close + 10)
        line.set_y2(x, close + 100)
        line.set_color(x, #ffffff)
        line.set_extend(x, "none")
        line.set_style(x, "solid")
        line.set_width(x, 2)
        "#;
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner
            .run(
                &vec![(
                    "close",
                    AnySeries::from_float_vec(vec![Some(1f64), Some(2f64)]),
                )],
                None,
            )
            .unwrap();
        assert_eq!(runner.get_context().get_shapes().len(), 1);

        let result = runner.get_context().move_var(VarIndex::new(0, 0)).unwrap();

        let mut line1 = PerLine::new();
        line1.x1 = Some(1i64);
        line1.y1 = Some(11f64);
        line1.x2 = Some(2i64);
        line1.y2 = Some(101f64);
        line1.xloc = 0;
        line1.extend = 0;
        line1.color = Some(String::from("#ffffff"));
        line1.style = 0;
        line1.width = Some(2);

        let mut line2 = PerLine::new();
        line2.x1 = Some(2i64);
        line2.y1 = Some(12f64);
        line2.x2 = Some(3i64);
        line2.y2 = Some(102f64);
        line2.xloc = 0;
        line2.extend = 0;
        line2.color = Some(String::from("#ffffff"));
        line2.style = 0;
        line2.width = Some(2);

        assert_eq!(
            Series::implicity_from(result).unwrap(),
            RefData::new(Series::from_vec(vec![
                Rc::new(RefCell::new(Some(line1))),
                Rc::new(RefCell::new(Some(line2)))
            ]))
        );
    }

    #[test]
    fn line_set2_test() {
        use crate::ast::stat_expr_types::VarIndex;

        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
        );
        let src = r#"
        x = line.new(1, 2, 3, 4)
        line.set_xloc(x, 1, 2, "bar_time")
        "#;
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner.runl(&vec![], 1, None).unwrap();
        assert_eq!(runner.get_context().get_shapes().len(), 1);

        let result = runner.get_context().move_var(VarIndex::new(0, 0)).unwrap();
        let mut line1 = PerLine::new();
        line1.x1 = Some(1i64);
        line1.x2 = Some(2i64);
        line1.y1 = Some(2f64);
        line1.y2 = Some(4f64);
        line1.xloc = 1;
        assert_eq!(
            Series::implicity_from(result).unwrap(),
            RefData::new(Series::from_vec(vec![Rc::new(RefCell::new(Some(line1))),]))
        );
    }

    #[test]
    fn line_set3_test() {
        use crate::ast::stat_expr_types::VarIndex;

        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
        );
        let src = r#"
        x = line.new(1, 2, 3, 4)
        line.set_xy1(x, 10, 20)
        line.set_xy2(x, 11, 21)
        "#;
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner.runl(&vec![], 1, None).unwrap();
        assert_eq!(runner.get_context().get_shapes().len(), 1);

        let result = runner.get_context().move_var(VarIndex::new(0, 0)).unwrap();
        let mut line1 = PerLine::new();
        line1.x1 = Some(10i64);
        line1.x2 = Some(11i64);
        line1.y1 = Some(20f64);
        line1.y2 = Some(21f64);
        assert_eq!(
            Series::implicity_from(result).unwrap(),
            RefData::new(Series::from_vec(vec![Rc::new(RefCell::new(Some(line1))),]))
        );
    }
}
