#[derive(Debug, PartialEq, Clone)]
pub struct BoolInputInfo {
    defval: Option<bool>,
    title: Option<String>,
    input_type: String,
    confirm: Option<bool>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IntInputInfo {
    defval: Option<i32>,
    title: Option<String>,
    input_type: String,
    minval: Option<i32>,
    maxval: Option<i32>,
    confirm: Option<bool>,
    step: Option<i32>,
    options: Option<Vec<i32>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FloatInputInfo {
    defval: Option<f64>,
    title: Option<String>,
    input_type: String,
    minval: Option<f64>,
    maxval: Option<f64>,
    confirm: Option<bool>,
    step: Option<f64>,
    options: Option<Vec<f64>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StringInputInfo {
    defval: Option<String>,
    title: Option<String>,
    input_type: String,
    confirm: Option<bool>,
    options: Option<Vec<String>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum InputInfo {
    Bool(BoolInputInfo),
    Int(IntInputInfo),
    Float(FloatInputInfo),
    String(StringInputInfo),
}

#[derive(Debug, PartialEq, Clone)]
pub struct PlotInfo {
    title: Option<String>,
    color: Option<String>,
    linewidth: Option<i32>,
    style: Option<i32>,
    transp: Option<i32>,
    trackprice: Option<bool>,
    histbase: Option<f64>,
    offset: Option<i32>,
    join: Option<bool>,
    editable: Option<bool>,
    show_last: Option<i32>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum OutputInfo {
    Plot(PlotInfo),
}

#[derive(Debug, PartialEq, Clone)]
pub struct IOInfo {
    inputs: Vec<InputInfo>,
    outputs: Vec<OutputInfo>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct OutputData {
    from: Option<i32>,
    to: Option<i32>,
    series: Vec<Option<f64>>,
}
