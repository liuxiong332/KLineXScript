use super::xloc::*;
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

const LABEL_STYLE_NONE: &'static str = "none";
const LABEL_STYLE_XCROSS: &'static str = "xcross";
const LABEL_STYLE_CROSS: &'static str = "cross";
const LABEL_STYLE_TRIANGLEUP: &'static str = "triangleup";
const LABEL_STYLE_TRIANGLEDOWN: &'static str = "triangledown";
const LABEL_STYLE_FLAG: &'static str = "flag";
const LABEL_STYLE_CIRCLE: &'static str = "circle";
const LABEL_STYLE_ARROWUP: &'static str = "arrowup";
const LABEL_STYLE_ARROWDOWN: &'static str = "arrowdown";
const LABEL_STYLE_LABELUP: &'static str = "labelup";
const LABEL_STYLE_LABELDOWN: &'static str = "labeldown";
const LABEL_STYLE_LABELLEFT: &'static str = "labelleft";
const LABEL_STYLE_LABELRIGHT: &'static str = "labelright";
const LABEL_STYLE_LABELCENTER: &'static str = "labelcenter";
const LABEL_STYLE_SQUARE: &'static str = "square";
const LABEL_STYLE_DIAMOND: &'static str = "diamond";

#[derive(Clone, Copy, Debug)]
pub enum StyleEnum {
    None = 0,
    Xcross = 1,
    Cross = 2,
    Triangleup = 3,
    Triangledown = 4,
    Flag = 5,
    Circle = 6,
    Arrowup = 7,
    Arrowdown = 8,
    Labelup = 9,
    Labeldown = 10,
    Labelleft = 11,
    Labelright = 12,
    Labelcenter = 13,
    Square = 14,
    Diamond = 15,
}

impl StyleEnum {
    fn from_str(s: &str) -> Result<StyleEnum, RuntimeErr> {
        match s {
            LABEL_STYLE_NONE => Ok(StyleEnum::None),
            LABEL_STYLE_XCROSS => Ok(StyleEnum::Xcross),
            LABEL_STYLE_CROSS => Ok(StyleEnum::Cross),
            LABEL_STYLE_TRIANGLEUP => Ok(StyleEnum::Triangleup),
            LABEL_STYLE_TRIANGLEDOWN => Ok(StyleEnum::Triangledown),
            LABEL_STYLE_FLAG => Ok(StyleEnum::Flag),
            LABEL_STYLE_CIRCLE => Ok(StyleEnum::Circle),
            LABEL_STYLE_ARROWUP => Ok(StyleEnum::Arrowup),
            LABEL_STYLE_ARROWDOWN => Ok(StyleEnum::Arrowdown),
            LABEL_STYLE_LABELUP => Ok(StyleEnum::Labelup),
            LABEL_STYLE_LABELDOWN => Ok(StyleEnum::Labeldown),
            LABEL_STYLE_LABELLEFT => Ok(StyleEnum::Labelleft),
            LABEL_STYLE_LABELRIGHT => Ok(StyleEnum::Labelright),
            LABEL_STYLE_LABELCENTER => Ok(StyleEnum::Labelcenter),
            LABEL_STYLE_SQUARE => Ok(StyleEnum::Square),
            LABEL_STYLE_DIAMOND => Ok(StyleEnum::Diamond),
            _ => Err(RuntimeErr::InvalidParameters(str_replace(
                INVALID_VALS,
                vec![String::from("style")],
            ))),
        }
    }

    fn from_pf<'a>(s: Option<PineRef<'a>>) -> Result<StyleEnum, RuntimeErr> {
        match pine_ref_to_string(s) {
            None => Ok(StyleEnum::None),
            Some(s) => StyleEnum::from_str(&s[..]),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum XLocEnum {
    BarIndex = 0,
    BarTime = 1,
}

impl XLocEnum {
    fn from_str(s: &str) -> Result<XLocEnum, RuntimeErr> {
        match s {
            XLOC_BAR_INDEX => Ok(XLocEnum::BarIndex),
            XLOC_BAR_TIME => Ok(XLocEnum::BarTime),
            _ => Err(RuntimeErr::InvalidParameters(str_replace(
                INVALID_VALS,
                vec![String::from("xloc")],
            ))),
        }
    }

    fn from_pf<'a>(s: Option<PineRef<'a>>) -> Result<XLocEnum, RuntimeErr> {
        match pine_ref_to_string(s) {
            None => Ok(XLocEnum::BarIndex),
            Some(s) => XLocEnum::from_str(&s[..]),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum YLocEnum {
    Price = 0,
    Abovebar = 1,
    Belowbar = 2,
}

impl YLocEnum {
    fn from_str(s: &str) -> Result<YLocEnum, RuntimeErr> {
        match s {
            "price" => Ok(YLocEnum::Price),
            "abovebar" => Ok(YLocEnum::Abovebar),
            "belowbar" => Ok(YLocEnum::Belowbar),
            _ => Err(RuntimeErr::InvalidParameters(str_replace(
                INVALID_VALS,
                vec![String::from("yloc")],
            ))),
        }
    }

    fn from_pf<'a>(s: Option<PineRef<'a>>) -> Result<YLocEnum, RuntimeErr> {
        match pine_ref_to_string(s) {
            None => Ok(YLocEnum::Price),
            Some(s) => YLocEnum::from_str(&s[..]),
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
                xloc: XLocEnum::from_pf(xloc)? as i32,
                yloc: YLocEnum::from_pf(yloc)? as i32,
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

fn get_text_func<'a>(
    _context: &mut dyn Ctx<'a>,
    param: Vec<Option<PineRef<'a>>>,
    _func_type: FunctionType<'a>,
) -> Result<PineRef<'a>, RuntimeErr> {
    get_val_func(_context, param, |v| {
        v.text.clone().unwrap_or(String::from(""))
    })
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

fn set_size_func<'a>(
    _context: &mut dyn Ctx<'a>,
    param: Vec<Option<PineRef<'a>>>,
    _func_type: FunctionType<'a>,
) -> Result<PineRef<'a>, RuntimeErr> {
    set_val_func(_context, param, |l, v| {
        l.size = SizeEnum::from_pf(v)? as i32;
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

fn set_textalign_func<'a>(
    _context: &mut dyn Ctx<'a>,
    param: Vec<Option<PineRef<'a>>>,
    _func_type: FunctionType<'a>,
) -> Result<PineRef<'a>, RuntimeErr> {
    set_val_func(_context, param, |l, v| {
        l.textalign = TextAlignEnum::from_pf(v)? as i32;
        Ok(())
    })
}

fn set_text_func<'a>(
    _context: &mut dyn Ctx<'a>,
    param: Vec<Option<PineRef<'a>>>,
    _func_type: FunctionType<'a>,
) -> Result<PineRef<'a>, RuntimeErr> {
    set_val_func(_context, param, |l, v| {
        l.text = pine_ref_to_string(v);
        Ok(())
    })
}

fn set_textcolor_func<'a>(
    _context: &mut dyn Ctx<'a>,
    param: Vec<Option<PineRef<'a>>>,
    _func_type: FunctionType<'a>,
) -> Result<PineRef<'a>, RuntimeErr> {
    set_val_func(_context, param, |l, v| {
        l.textcolor = pine_ref_to_color(v);
        Ok(())
    })
}

fn set_xloc_func<'a>(
    _context: &mut dyn Ctx<'a>,
    mut param: Vec<Option<PineRef<'a>>>,
    _func_type: FunctionType<'a>,
) -> Result<PineRef<'a>, RuntimeErr> {
    move_tuplet!((id, x, xloc) = param);
    let label = pine_ref_to_label(id);

    if label.borrow_mut().is_none() {
        *label.borrow_mut() = Some(PerLabel::new());
    }
    let mut li = label.borrow_mut();
    li.as_mut().unwrap().x = pine_ref_to_i64(x);
    li.as_mut().unwrap().xloc = XLocEnum::from_pf(xloc)? as i32;
    Ok(PineRef::new(NA))
}

fn set_yloc_func<'a>(
    _context: &mut dyn Ctx<'a>,
    param: Vec<Option<PineRef<'a>>>,
    _func_type: FunctionType<'a>,
) -> Result<PineRef<'a>, RuntimeErr> {
    set_val_func(_context, param, |l, v| {
        l.yloc = YLocEnum::from_pf(v)? as i32;
        Ok(())
    })
}

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
            "get_x" => Ok(PineRef::new(Callable::new(Some(get_x_func), None))),
            "get_y" => Ok(PineRef::new(Callable::new(Some(get_y_func), None))),
            "get_text" => Ok(PineRef::new(Callable::new(Some(get_text_func), None))),
            "set_x" => Ok(PineRef::new(Callable::new(Some(set_x_func), None))),
            "set_y" => Ok(PineRef::new(Callable::new(Some(set_y_func), None))),
            "set_xy" => Ok(PineRef::new(Callable::new(Some(set_xy_func), None))),
            "set_color" => Ok(PineRef::new(Callable::new(Some(set_color_func), None))),
            "set_size" => Ok(PineRef::new(Callable::new(Some(set_size_func), None))),
            "set_style" => Ok(PineRef::new(Callable::new(Some(set_style_func), None))),
            "set_text" => Ok(PineRef::new(Callable::new(Some(set_text_func), None))),
            "set_textalign" => Ok(PineRef::new(Callable::new(Some(set_textalign_func), None))),
            "set_textcolor" => Ok(PineRef::new(Callable::new(Some(set_textcolor_func), None))),
            "set_xloc" => Ok(PineRef::new(Callable::new(Some(set_xloc_func), None))),
            "set_yloc" => Ok(PineRef::new(Callable::new(Some(set_yloc_func), None))),

            "style_none" => Ok(PineRef::new(String::from(LABEL_STYLE_NONE))),
            "style_arrowdown" => Ok(PineRef::new(String::from(LABEL_STYLE_ARROWDOWN))),
            "style_arrowup" => Ok(PineRef::new(String::from(LABEL_STYLE_ARROWUP))),
            "style_circle" => Ok(PineRef::new(String::from(LABEL_STYLE_CIRCLE))),
            "style_cross" => Ok(PineRef::new(String::from(LABEL_STYLE_CROSS))),
            "style_diamond" => Ok(PineRef::new(String::from(LABEL_STYLE_DIAMOND))),
            "style_flag" => Ok(PineRef::new(String::from(LABEL_STYLE_FLAG))),
            "style_label_center" => Ok(PineRef::new(String::from(LABEL_STYLE_LABELCENTER))),
            "style_label_down" => Ok(PineRef::new(String::from(LABEL_STYLE_LABELDOWN))),
            "style_label_left" => Ok(PineRef::new(String::from(LABEL_STYLE_LABELLEFT))),
            "style_label_right" => Ok(PineRef::new(String::from(LABEL_STYLE_LABELRIGHT))),
            "style_label_up" => Ok(PineRef::new(String::from(LABEL_STYLE_LABELUP))),
            "style_square" => Ok(PineRef::new(String::from(LABEL_STYLE_SQUARE))),
            "style_triangledown" => Ok(PineRef::new(String::from(LABEL_STYLE_TRIANGLEDOWN))),
            "style_triangleup" => Ok(PineRef::new(String::from(LABEL_STYLE_TRIANGLEUP))),
            "style_xcross" => Ok(PineRef::new(String::from(LABEL_STYLE_XCROSS))),
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
        "get_x",
        SyntaxType::Function(Rc::new(FunctionTypes(vec![FunctionType::new((
            vec![("id", SyntaxType::ObjectClass("label"))],
            SyntaxType::int_series(),
        ))]))),
    );
    obj_type.insert(
        "get_y",
        SyntaxType::Function(Rc::new(FunctionTypes(vec![FunctionType::new((
            vec![("id", SyntaxType::ObjectClass("label"))],
            SyntaxType::float_series(),
        ))]))),
    );
    obj_type.insert(
        "get_text",
        SyntaxType::Function(Rc::new(FunctionTypes(vec![FunctionType::new((
            vec![("id", SyntaxType::ObjectClass("label"))],
            SyntaxType::string_series(),
        ))]))),
    );

    obj_type.insert(
        "set_x",
        SyntaxType::Function(Rc::new(FunctionTypes(vec![FunctionType::new((
            vec![
                ("id", SyntaxType::ObjectClass("label")),
                ("x", SyntaxType::int_series()),
            ],
            SyntaxType::Void,
        ))]))),
    );
    obj_type.insert(
        "set_y",
        SyntaxType::Function(Rc::new(FunctionTypes(vec![FunctionType::new((
            vec![
                ("id", SyntaxType::ObjectClass("label")),
                ("x", SyntaxType::float_series()),
            ],
            SyntaxType::Void,
        ))]))),
    );
    obj_type.insert(
        "set_xy",
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
        "set_size",
        SyntaxType::Function(Rc::new(FunctionTypes(vec![FunctionType::new((
            vec![
                ("id", SyntaxType::ObjectClass("label")),
                ("size", SyntaxType::string_series()),
            ],
            SyntaxType::Void,
        ))]))),
    );
    obj_type.insert(
        "set_style",
        SyntaxType::Function(Rc::new(FunctionTypes(vec![FunctionType::new((
            vec![
                ("id", SyntaxType::ObjectClass("label")),
                ("style", SyntaxType::string_series()),
            ],
            SyntaxType::Void,
        ))]))),
    );
    obj_type.insert(
        "set_text",
        SyntaxType::Function(Rc::new(FunctionTypes(vec![FunctionType::new((
            vec![
                ("id", SyntaxType::ObjectClass("label")),
                ("text", SyntaxType::string_series()),
            ],
            SyntaxType::Void,
        ))]))),
    );
    obj_type.insert(
        "set_textalign",
        SyntaxType::Function(Rc::new(FunctionTypes(vec![FunctionType::new((
            vec![
                ("id", SyntaxType::ObjectClass("label")),
                ("textalign", SyntaxType::string_series()),
            ],
            SyntaxType::Void,
        ))]))),
    );
    obj_type.insert(
        "set_textcolor",
        SyntaxType::Function(Rc::new(FunctionTypes(vec![FunctionType::new((
            vec![
                ("id", SyntaxType::ObjectClass("label")),
                ("textcolor", SyntaxType::color_series()),
            ],
            SyntaxType::Void,
        ))]))),
    );
    obj_type.insert(
        "set_xloc",
        SyntaxType::Function(Rc::new(FunctionTypes(vec![FunctionType::new((
            vec![
                ("id", SyntaxType::ObjectClass("label")),
                ("x", SyntaxType::int_series()),
                ("xloc", SyntaxType::string_series()),
            ],
            SyntaxType::Void,
        ))]))),
    );
    obj_type.insert(
        "set_yloc",
        SyntaxType::Function(Rc::new(FunctionTypes(vec![FunctionType::new((
            vec![
                ("id", SyntaxType::ObjectClass("label")),
                ("yloc", SyntaxType::string_series()),
            ],
            SyntaxType::Void,
        ))]))),
    );

    obj_type.insert("style_none", SyntaxType::string());
    obj_type.insert("style_arrowdown", SyntaxType::string());
    obj_type.insert("style_arrowup", SyntaxType::string());
    obj_type.insert("style_circle", SyntaxType::string());
    obj_type.insert("style_cross", SyntaxType::string());
    obj_type.insert("style_diamond", SyntaxType::string());
    obj_type.insert("style_flag", SyntaxType::string());
    obj_type.insert("style_label_center", SyntaxType::string());
    obj_type.insert("style_label_down", SyntaxType::string());
    obj_type.insert("style_label_left", SyntaxType::string());
    obj_type.insert("style_label_right", SyntaxType::string());
    obj_type.insert("style_label_up", SyntaxType::string());
    obj_type.insert("style_square", SyntaxType::string());
    obj_type.insert("style_triangledown", SyntaxType::string());
    obj_type.insert("style_triangleup", SyntaxType::string());
    obj_type.insert("style_xcross", SyntaxType::string());
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
    fn label_create_test() {
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

    #[test]
    fn label_delete_test() {
        use crate::ast::stat_expr_types::VarIndex;

        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
        );
        let src = "x = label(na)\nx := label.new(1, 2)\nlabel.delete(x[1])";
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
                Rc::new(RefCell::new(None)),
                Rc::new(RefCell::new(Some(label)))
            ]))
        );
    }

    #[test]
    fn label_get_test() {
        use crate::ast::stat_expr_types::VarIndex;

        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
        );
        let src = r#"
        x = label.new(1, 2, "hello")
        x1 = label.get_x(x)
        x2 = label.get_y(x)
        x3 = label.get_text(x)
        "#;
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
            Some(PineRef::new(Series::from_vec(vec![Some(2f64), Some(2f64)])))
        );
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(3, 0)),
            Some(PineRef::new(Series::from_vec(vec![
                String::from("hello"),
                String::from("hello"),
            ])))
        );
    }

    #[test]
    fn label_set_test() {
        use crate::ast::stat_expr_types::VarIndex;

        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
        );
        let src = r#"
        x = label.new(1, 2)
        label.set_x(x, int(close))
        label.set_y(x, close + 10)
        label.set_color(x, #ffffff)
        label.set_size(x, "auto")
        label.set_style(x, "none")
        label.set_text(x, "hello")
        label.set_textalign(x, "left")
        label.set_textcolor(x, #111111)
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

        let mut label1 = PerLabel::new();
        label1.x = Some(1i64);
        label1.y = Some(11f64);
        label1.text = Some(String::from("hello"));
        label1.color = Some(String::from("#ffffff"));
        label1.size = 0;
        label1.style = 0;
        label1.textalign = 0;
        label1.textcolor = Some(String::from("#111111"));

        let mut label2 = label1.clone();
        label2.x = Some(2i64);
        label2.y = Some(12f64);

        assert_eq!(
            Series::implicity_from(result).unwrap(),
            RefData::new(Series::from_vec(vec![
                Rc::new(RefCell::new(Some(label1))),
                Rc::new(RefCell::new(Some(label2)))
            ]))
        );
    }

    #[test]
    fn label_set2_test() {
        use crate::ast::stat_expr_types::VarIndex;

        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
        );
        let src = r#"
        x = label.new(1, 2)
        label.set_xy(x, 3, 4)
        label.set_xloc(x, 8, "bar_time")
        label.set_yloc(x, "abovebar")
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

        let mut label1 = PerLabel::new();
        label1.x = Some(8i64);
        label1.y = Some(4f64);
        label1.xloc = XLocEnum::BarTime as i32;
        label1.yloc = YLocEnum::Abovebar as i32;

        assert_eq!(
            Series::implicity_from(result).unwrap(),
            RefData::new(Series::from_vec(vec![
                Rc::new(RefCell::new(Some(label1.clone()))),
                Rc::new(RefCell::new(Some(label1.clone())))
            ]))
        );
    }

    #[test]
    fn label_style_test() {
        use crate::ast::stat_expr_types::VarIndex;
        use crate::runtime::VarOperate;
        use crate::types::{downcast_pf, Tuple};

        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![("close", SyntaxType::float_series())],
        );
        let src = r"m = [
            label.style_none, label.style_arrowdown, label.style_arrowup, label.style_circle,
            label.style_cross, label.style_diamond, label.style_flag, label.style_label_center,
            label.style_label_down, label.style_label_left, label.style_label_right, label.style_label_up,
            label.style_square, label.style_triangledown, label.style_triangleup, label.style_xcross
        ]";

        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner
            .run(
                &vec![("close", AnySeries::from_float_vec(vec![Some(1f64)]))],
                None,
            )
            .unwrap();
        let tuple_res =
            downcast_pf::<Tuple>(runner.get_context().move_var(VarIndex::new(0, 0)).unwrap());
        let tuple_vec = tuple_res.unwrap().into_inner().0;
        assert_eq!(
            tuple_vec,
            vec![
                "none",
                "arrowdown",
                "arrowup",
                "circle",
                "cross",
                "diamond",
                "flag",
                "labelcenter",
                "labeldown",
                "labelleft",
                "labelright",
                "labelup",
                "square",
                "triangledown",
                "triangleup",
                "xcross"
            ]
            .iter()
            .map(|&s| PineRef::new_rc(String::from(s)))
            .collect::<Vec<_>>()
        );
    }
}
