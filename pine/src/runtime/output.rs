#[derive(Debug, PartialEq, Clone)]
pub struct BoolInputInfo {
    pub defval: Option<bool>,
    pub title: Option<String>,
    pub input_type: String,
    pub confirm: Option<bool>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IntInputInfo {
    pub defval: Option<i32>,
    pub title: Option<String>,
    pub input_type: String,
    pub minval: Option<i32>,
    pub maxval: Option<i32>,
    pub confirm: Option<bool>,
    pub step: Option<i32>,
    pub options: Option<Vec<i32>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FloatInputInfo {
    pub defval: Option<f64>,
    pub title: Option<String>,
    pub input_type: String,
    pub minval: Option<f64>,
    pub maxval: Option<f64>,
    pub confirm: Option<bool>,
    pub step: Option<f64>,
    pub options: Option<Vec<f64>>,
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
    pub title: Option<String>,
    pub color: Option<String>,
    pub linewidth: Option<i32>,
    pub style: Option<i32>,
    pub transp: Option<i32>,
    pub trackprice: Option<bool>,
    pub histbase: Option<f64>,
    pub offset: Option<i32>,
    pub join: Option<bool>,
    pub editable: Option<bool>,
    pub show_last: Option<i32>,
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

impl IOInfo {
    pub fn new() -> IOInfo {
        IOInfo {
            inputs: vec![],
            outputs: vec![],
        }
    }

    pub fn new_with_io(inputs: Vec<InputInfo>, outputs: Vec<OutputInfo>) -> IOInfo {
        IOInfo { inputs, outputs }
    }

    pub fn push_input(&mut self, input: InputInfo) {
        self.inputs.push(input);
    }

    pub fn push_output(&mut self, output: OutputInfo) {
        self.outputs.push(output);
    }

    pub fn get_inputs(&self) -> &Vec<InputInfo> {
        &self.inputs
    }

    pub fn get_outputs(&self) -> &Vec<OutputInfo> {
        &self.outputs
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct OutputData {
    from: Option<i32>,
    to: Option<i32>,
    series: Vec<Option<f64>>,
}

impl OutputData {
    pub fn new(from: Option<i32>, to: Option<i32>, series: Vec<Option<f64>>) -> OutputData {
        OutputData { from, to, series }
    }
}
