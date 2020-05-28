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

pub type PerLabelItem = Rc<RefCell<Option<PerLabel>>>;

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

    fn from_pf<'a>(s: Option<PineRef<'a>>) -> Result<StyleEnum, RuntimeErr> {
        match pine_ref_to_string(s) {
            None => Ok(StyleEnum::Solid),
            Some(s) => StyleEnum::from_str(&s[..]),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum LocEnum {
    BarIndex = 0,
    BarTime = 1,
}

impl LocEnum {
    fn from_str(s: &str, name: &str) -> Result<LocEnum, RuntimeErr> {
        match s {
            "bar_index" => Ok(LocEnum::BarIndex),
            "bar_time" => Ok(LocEnum::BarTime),
            _ => Err(RuntimeErr::InvalidParameters(str_replace(
                INVALID_VALS,
                vec![String::from(name)],
            ))),
        }
    }

    fn from_pf<'a>(s: Option<PineRef<'a>>, name: &str) -> Result<LocEnum, RuntimeErr> {
        match pine_ref_to_string(s) {
            None => Ok(LocEnum::BarIndex),
            Some(s) => LocEnum::from_str(&s[..], name),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum SizeEnum {
    Auto = 0,
    Huge = 1,
    Large = 2,
    Normal = 3,
    Small = 4,
    Tiny = 5,
}

impl SizeEnum {
    fn from_str(s: &str) -> Result<SizeEnum, RuntimeErr> {
        match s {
            "auto" => Ok(SizeEnum::Auto),
            "huge" => Ok(SizeEnum::Huge),
            "large" => Ok(SizeEnum::Large),
            "normal" => Ok(SizeEnum::Normal),
            "small" => Ok(SizeEnum::Small),
            "tiny" => Ok(SizeEnum::Tiny),
            _ => Err(RuntimeErr::InvalidParameters(str_replace(
                INVALID_VALS,
                vec![String::from("size")],
            ))),
        }
    }

    fn from_pf<'a>(s: Option<PineRef<'a>>) -> Result<SizeEnum, RuntimeErr> {
        match pine_ref_to_string(s) {
            None => Ok(SizeEnum::Auto),
            Some(s) => SizeEnum::from_str(&s[..]),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum TextAlignEnum {
    Left = 0,
    Center = 1,
    Right = 2,
}

impl TextAlignEnum {
    fn from_str(s: &str) -> Result<TextAlignEnum, RuntimeErr> {
        match s {
            "left" => Ok(TextAlignEnum::Left),
            "center" => Ok(TextAlignEnum::Center),
            "right" => Ok(TextAlignEnum::Right),
            _ => Err(RuntimeErr::InvalidParameters(str_replace(
                INVALID_VALS,
                vec![String::from("textalign")],
            ))),
        }
    }

    fn from_pf<'a>(s: Option<PineRef<'a>>) -> Result<TextAlignEnum, RuntimeErr> {
        match pine_ref_to_string(s) {
            None => Ok(TextAlignEnum::Left),
            Some(s) => TextAlignEnum::from_str(&s[..]),
        }
    }
}

fn is_label_na<'a>(val: Option<PineRef<'a>>) -> bool {
    if val.is_none() {
        return true;
    }
    match PerLabelItem::implicity_from(val.unwrap()) {
        Ok(res) => {
            let item: &PerLabelItem = res.deref();
            item.borrow().is_none()
        }
        Err(_) => true,
    }
}

fn pine_ref_to_label<'a>(val: Option<PineRef<'a>>) -> PerLabelItem {
    if val.is_none() {
        return Rc::new(RefCell::new(None));
    }
    match PerLabelItem::implicity_from(val.unwrap()) {
        Ok(res) => res.into_inner(),
        Err(_) => Rc::new(RefCell::new(None)),
    }
}

// The label definition that represent every label object.
#[derive(Debug, Clone, PartialEq)]
pub struct PerLabel {
    // x, y, text, xloc, yloc, color, style, textcolor, size, textalign
    x: Int,
    y: Float,
    text: Option<String>,
    xloc: i32,
    yloc: i32,

    color: Option<String>,
    style: i32,
    textcolor: Option<String>,
    size: i32,
    textalign: i32,
}

impl PerLabel {
    pub fn new() -> PerLabel {
        PerLabel {
            x: None,
            y: None,
            text: None,
            xloc: 0,
            yloc: 0,
            color: None,
            style: 0,
            textcolor: None,
            size: 0,
            textalign: 0,
        }
    }
}

impl PineStaticType for PerLabelItem {
    fn static_type() -> (DataType, SecondType) {
        (DataType::Label, SecondType::Simple)
    }
}

impl<'a> PineFrom<'a, PerLabelItem> for PerLabelItem {
    fn implicity_from(t: PineRef<'a>) -> Result<RefData<PerLabelItem>, RuntimeErr> {
        match t.get_type() {
            (DataType::Label, SecondType::Simple) => Ok(downcast_pf::<PerLabelItem>(t).unwrap()),
            (DataType::Label, SecondType::Series) => {
                let f: RefData<Series<PerLabelItem>> =
                    downcast_pf::<Series<PerLabelItem>>(t).unwrap();
                Ok(RefData::new(f.get_current()))
            }
            (DataType::NA, _) => Ok(RefData::new(Rc::new(RefCell::new(None)))),
            _ => Err(RuntimeErr::UnknownRuntimeErr),
        }
    }
}

impl<'a> PineType<'a> for PerLabelItem {
    fn get_type(&self) -> (DataType, SecondType) {
        (DataType::Line, SecondType::Simple)
    }
    fn copy(&self) -> PineRef<'a> {
        PineRef::new(self.clone())
    }
}

impl<'a> SimpleType for PerLabelItem {}

// The label invocation that create new LineInfo object
#[derive(Debug)]
struct LineFromNaVal<'a> {
    labels: RefData<Series<'a, PerLabelItem>>,
}

impl<'a> Clone for LineFromNaVal<'a> {
    fn clone(&self) -> Self {
        LineFromNaVal {
            labels: RefData::clone(&self.labels),
        }
    }
}

impl<'a> LineFromNaVal<'a> {
    fn new() -> LineFromNaVal<'a> {
        LineFromNaVal {
            labels: RefData::new(Series::from(Rc::new(RefCell::new(None)))),
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
            match is_label_na(v.clone()) {
                true => Ok(RefData::clone(&self.labels).into_pf()),
                false => Ok(Series::<'a, PerLabelItem>::implicity_from(v.unwrap())
                    .unwrap()
                    .into_pf()),
            }
        } else {
            move_tuplet!((x, y, text, xloc, yloc, color, style, textcolor, size, textalign) = p);
            let label = PerLabel {
                x: pine_ref_to_i64(x),
                y: pine_ref_to_f64(y),
                text: pine_ref_to_string(text),
                xloc: LocEnum::from_pf(xloc, "xloc")? as i32,
                yloc: LocEnum::from_pf(yloc, "yloc")? as i32,
                color: pine_ref_to_color(color),
                style: StyleEnum::from_pf(style)? as i32,
                textcolor: pine_ref_to_color(textcolor),
                size: SizeEnum::from_pf(size)? as i32,
                textalign: TextAlignEnum::from_pf(textalign)? as i32,
            };
            self.labels.update(Rc::new(RefCell::new(Some(label))));
            Ok(RefData::clone(&self.labels).into_pf())
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
    let label = pine_ref_to_label(id);
    label.replace(None);
    Ok(PineRef::new(NA))
}

fn get_val_func<'a, T: Default + Clone + fmt::Debug + PineType<'a> + PineStaticType + 'a>(
    _context: &mut dyn Ctx<'a>,
    mut param: Vec<Option<PineRef<'a>>>,
    func: impl Fn(&PerLabel) -> T,
) -> Result<PineRef<'a>, RuntimeErr> {
    let id = mem::replace(&mut param[0], None);
    let label = pine_ref_to_label(id);
    match unsafe { label.as_ptr().as_ref().unwrap() } {
        None => Ok(PineRef::new(NA)),
        Some(v) => Ok(PineRef::new(Series::from(func(v)))),
    }
}

fn get_x_func<'a>(
    _context: &mut dyn Ctx<'a>,
    param: Vec<Option<PineRef<'a>>>,
    _func_type: FunctionType<'a>,
) -> Result<PineRef<'a>, RuntimeErr> {
    get_val_func(_context, param, |v| v.x)
}

fn get_y_func<'a>(
    _context: &mut dyn Ctx<'a>,
    param: Vec<Option<PineRef<'a>>>,
    _func_type: FunctionType<'a>,
) -> Result<PineRef<'a>, RuntimeErr> {
    get_val_func(_context, param, |v| v.y)
}
fn set_val_func<'a>(
    _context: &mut dyn Ctx<'a>,
    mut param: Vec<Option<PineRef<'a>>>,
    mut func: impl FnMut(&mut PerLabel, Option<PineRef<'a>>) -> Result<(), RuntimeErr>,
) -> Result<PineRef<'a>, RuntimeErr> {
    let id = mem::replace(&mut param[0], None);
    let val = mem::replace(&mut param[1], None);
    let label = pine_ref_to_label(id);

    if label.borrow_mut().is_none() {
        *label.borrow_mut() = Some(PerLabel::new());
    }
    func(label.borrow_mut().as_mut().unwrap(), val);
    Ok(PineRef::new(NA))
}

fn set_x_func<'a>(
    _c: &mut dyn Ctx<'a>,
    p: Vec<Option<PineRef<'a>>>,
    _f: FunctionType<'a>,
) -> Result<PineRef<'a>, RuntimeErr> {
    set_val_func(_c, p, |l, v| {
        l.x = pine_ref_to_i64(v);
        Ok(())
    })
}
fn set_y_func<'a>(
    _context: &mut dyn Ctx<'a>,
    param: Vec<Option<PineRef<'a>>>,
    _func_type: FunctionType<'a>,
) -> Result<PineRef<'a>, RuntimeErr> {
    set_val_func(_context, param, |l, v| {
        l.y = pine_ref_to_f64(v);
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
fn set_style_func<'a>(
    _context: &mut dyn Ctx<'a>,
    param: Vec<Option<PineRef<'a>>>,
    _func_type: FunctionType<'a>,
) -> Result<PineRef<'a>, RuntimeErr> {
    set_val_func(_context, param, |l, v| {
        l.style = StyleEnum::from_pf(v)? as i32;
        Ok(())
    })
}
// fn set_xloc_func<'a>(
//     _context: &mut dyn Ctx<'a>,
//     mut param: Vec<Option<PineRef<'a>>>,
//     _func_type: FunctionType<'a>,
// ) -> Result<PineRef<'a>, RuntimeErr> {
//     move_tuplet!((id, x1, x2, xloc) = param);
//     let label = pine_ref_to_label(id);

//     if label.borrow_mut().is_none() {
//         *label.borrow_mut() = Some(PerLabel::new());
//     }
//     let mut li = label.borrow_mut();
//     li.as_mut().unwrap().x1 = pine_ref_to_i64(x1);
//     li.as_mut().unwrap().x2 = pine_ref_to_i64(x2);
//     li.as_mut().unwrap().xloc = match pine_ref_to_string(xloc) {
//         None => LocEnum::BarIndex as i32,
//         Some(s) => LocEnum::from_str(&s[..])? as i32,
//     };
//     Ok(PineRef::new(NA))
// }

fn set_xy_func<'a>(
    _context: &mut dyn Ctx<'a>,
    mut param: Vec<Option<PineRef<'a>>>,
    _func_type: FunctionType<'a>,
) -> Result<PineRef<'a>, RuntimeErr> {
    move_tuplet!((id, x, y) = param);
    let label = pine_ref_to_label(id);
    if label.borrow_mut().is_none() {
        *label.borrow_mut() = Some(PerLabel::new());
    }
    let mut li = label.borrow_mut();
    li.as_mut().unwrap().x = pine_ref_to_i64(x);
    li.as_mut().unwrap().y = pine_ref_to_f64(y);
    Ok(PineRef::new(NA))
}

struct PlotProps;

impl<'a> PineClass<'a> for PlotProps {
    fn custom_type(&self) -> &str {
        "label"
    }

    fn get(&self, _ctx: &mut dyn Ctx<'a>, name: &str) -> Result<PineRef<'a>, RuntimeErr> {
        match name {
            "new" => Ok(PineRef::new(CallableFactory::new(|| {
                Callable::new(None, Some(Box::new(LineFromNaVal::new())))
            }))),
            "delete" => Ok(PineRef::new(Callable::new(Some(delete_func), None))),
            // "get_x1" => Ok(PineRef::new(Callable::new(Some(get_x1_func), None))),
            // "get_x2" => Ok(PineRef::new(Callable::new(Some(get_x2_func), None))),
            // "get_y1" => Ok(PineRef::new(Callable::new(Some(get_y1_func), None))),
            // "get_y2" => Ok(PineRef::new(Callable::new(Some(get_y2_func), None))),
            // "set_x1" => Ok(PineRef::new(Callable::new(Some(set_x1_func), None))),
            // "set_x2" => Ok(PineRef::new(Callable::new(Some(set_x2_func), None))),
            // "set_y1" => Ok(PineRef::new(Callable::new(Some(set_y1_func), None))),
            // "set_y2" => Ok(PineRef::new(Callable::new(Some(set_y2_func), None))),
            // "set_color" => Ok(PineRef::new(Callable::new(Some(set_color_func), None))),
            // "set_extend" => Ok(PineRef::new(Callable::new(Some(set_extend_func), None))),
            // "set_style" => Ok(PineRef::new(Callable::new(Some(set_style_func), None))),
            // "set_width" => Ok(PineRef::new(Callable::new(Some(set_width_func), None))),
            // "set_x1" => Ok(PineRef::new(Callable::new(Some(set_x1_func), None))),
            // "set_x2" => Ok(PineRef::new(Callable::new(Some(set_x2_func), None))),
            // "set_y1" => Ok(PineRef::new(Callable::new(Some(set_y1_func), None))),
            // "set_y2" => Ok(PineRef::new(Callable::new(Some(set_y2_func), None))),
            // "set_xloc" => Ok(PineRef::new(Callable::new(Some(set_xloc_func), None))),
            // "set_xy1" => Ok(PineRef::new(Callable::new(Some(set_xy1_func), None))),
            // "set_xy2" => Ok(PineRef::new(Callable::new(Some(set_xy2_func), None))),
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

pub const VAR_NAME: &'static str = "label";

pub fn declare_var<'a>() -> VarResult<'a> {
    let value = PineRef::new(CallableObject::new(Box::new(PlotProps), || {
        Callable::new(None, Some(Box::new(LineFromNaVal::new())))
    }));

    let func_type = FunctionTypes(vec![FunctionType::new((
        vec![("x", SyntaxType::Simple(SimpleSyntaxType::Na))],
        SyntaxType::ObjectClass("label"),
    ))]);
    let mut obj_type = BTreeMap::new();
    obj_type.insert(
        "delete",
        SyntaxType::Function(Rc::new(FunctionTypes(vec![FunctionType::new((
            vec![("id", SyntaxType::ObjectClass("label"))],
            SyntaxType::Void,
        ))]))),
    );
    obj_type.insert(
        "new",
        // x, y, text, xloc, yloc, color, style, textcolor, size, textalign
        SyntaxType::Function(Rc::new(FunctionTypes(vec![FunctionType::new((
            vec![
                ("x", SyntaxType::int_series()),
                ("y", SyntaxType::float_series()),
                ("text", SyntaxType::string_series()),
                ("xloc", SyntaxType::string_series()),
                ("yloc", SyntaxType::string_series()),
                ("color", SyntaxType::color_series()),
                ("style", SyntaxType::string_series()),
                ("textcolor", SyntaxType::color_series()),
                ("size", SyntaxType::string_series()),
                ("textalign", SyntaxType::string_series()),
            ],
            SyntaxType::ObjectClass("label"),
        ))]))),
    );
    obj_type.insert(
        "get_x1",
        SyntaxType::Function(Rc::new(FunctionTypes(vec![FunctionType::new((
            vec![("id", SyntaxType::ObjectClass("label"))],
            SyntaxType::int_series(),
        ))]))),
    );
    obj_type.insert(
        "get_x2",
        SyntaxType::Function(Rc::new(FunctionTypes(vec![FunctionType::new((
            vec![("id", SyntaxType::ObjectClass("label"))],
            SyntaxType::int_series(),
        ))]))),
    );
    obj_type.insert(
        "get_y1",
        SyntaxType::Function(Rc::new(FunctionTypes(vec![FunctionType::new((
            vec![("id", SyntaxType::ObjectClass("label"))],
            SyntaxType::float_series(),
        ))]))),
    );
    obj_type.insert(
        "get_y2",
        SyntaxType::Function(Rc::new(FunctionTypes(vec![FunctionType::new((
            vec![("id", SyntaxType::ObjectClass("label"))],
            SyntaxType::float_series(),
        ))]))),
    );
    obj_type.insert(
        "set_color",
        SyntaxType::Function(Rc::new(FunctionTypes(vec![FunctionType::new((
            vec![
                ("id", SyntaxType::ObjectClass("label")),
                ("color", SyntaxType::color_series()),
            ],
            SyntaxType::Void,
        ))]))),
    );
    obj_type.insert(
        "set_extend",
        SyntaxType::Function(Rc::new(FunctionTypes(vec![FunctionType::new((
            vec![
                ("id", SyntaxType::ObjectClass("label")),
                ("extend", SyntaxType::string_series()),
            ],
            SyntaxType::Void,
        ))]))),
    );
    obj_type.insert(
        "set_style",
        SyntaxType::Function(Rc::new(FunctionTypes(vec![FunctionType::new((
            vec![
                ("id", SyntaxType::ObjectClass("label")),
                ("extend", SyntaxType::string_series()),
            ],
            SyntaxType::Void,
        ))]))),
    );
    obj_type.insert(
        "set_width",
        SyntaxType::Function(Rc::new(FunctionTypes(vec![FunctionType::new((
            vec![
                ("id", SyntaxType::ObjectClass("label")),
                ("extend", SyntaxType::int_series()),
            ],
            SyntaxType::Void,
        ))]))),
    );
    obj_type.insert(
        "set_x1",
        SyntaxType::Function(Rc::new(FunctionTypes(vec![FunctionType::new((
            vec![
                ("id", SyntaxType::ObjectClass("label")),
                ("x", SyntaxType::int_series()),
            ],
            SyntaxType::Void,
        ))]))),
    );
    obj_type.insert(
        "set_x2",
        SyntaxType::Function(Rc::new(FunctionTypes(vec![FunctionType::new((
            vec![
                ("id", SyntaxType::ObjectClass("label")),
                ("x", SyntaxType::int_series()),
            ],
            SyntaxType::Void,
        ))]))),
    );
    obj_type.insert(
        "set_y1",
        SyntaxType::Function(Rc::new(FunctionTypes(vec![FunctionType::new((
            vec![
                ("id", SyntaxType::ObjectClass("label")),
                ("y", SyntaxType::float_series()),
            ],
            SyntaxType::Void,
        ))]))),
    );
    obj_type.insert(
        "set_y2",
        SyntaxType::Function(Rc::new(FunctionTypes(vec![FunctionType::new((
            vec![
                ("id", SyntaxType::ObjectClass("label")),
                ("y", SyntaxType::float_series()),
            ],
            SyntaxType::Void,
        ))]))),
    );
    obj_type.insert(
        "set_xloc",
        SyntaxType::Function(Rc::new(FunctionTypes(vec![FunctionType::new((
            vec![
                ("id", SyntaxType::ObjectClass("label")),
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
                ("id", SyntaxType::ObjectClass("label")),
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
                ("id", SyntaxType::ObjectClass("label")),
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
    fn label_capture_test() {
        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
        );
        let src = "label(na)\nx = label(na)\nx2 = if true\n    label(na)\nelse\n    label(na)";
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner.runl(&vec![], 2, None).unwrap();
        assert_eq!(runner.get_context().get_shapes().len(), 3);
    }

    #[test]
    fn label_new_capture_test() {
        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
        );
        let src = "label.new(1, 2)\nx = label.new(1, 2)\n";
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner.runl(&vec![], 2, None).unwrap();
        assert_eq!(runner.get_context().get_shapes().len(), 2);
    }

    #[test]
    fn label_test() {
        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
        );
        let src = "x = label(na)\nlabel y = label(na)\nlabel x2 = label.new(1, 2)";
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
        let src = "x = label(na)\nx := label.new(1, 2)";
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner.runl(&vec![], 2, None).unwrap();

        assert_eq!(runner.get_context().get_shapes().len(), 1);

        let result = runner.get_context().move_var(VarIndex::new(0, 0)).unwrap();

        let mut label = PerLabel::new();
        label.x = Some(1i64);
        label.y = Some(2f64);
        assert_eq!(
            Series::implicity_from(result).unwrap(),
            RefData::new(Series::from_vec(vec![
                Rc::new(RefCell::new(Some(label.clone()))),
                Rc::new(RefCell::new(Some(label.clone()))),
            ]))
        );
    }
}
