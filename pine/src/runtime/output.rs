#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct BoolInputInfo {
    pub defval: Option<bool>,
    pub title: Option<String>,
    pub input_type: String,
    pub confirm: Option<bool>,
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
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

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
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

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct StringInputInfo {
    defval: Option<String>,
    title: Option<String>,
    input_type: String,
    confirm: Option<bool>,
    options: Option<Vec<String>>,
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum InputInfo {
    Bool(BoolInputInfo),
    Int(IntInputInfo),
    Float(FloatInputInfo),
    String(StringInputInfo),
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
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

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum OutputInfo {
    Plot(PlotInfo),
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct IOInfo {
    inputs: Vec<InputInfo>,
    input_srcs: Vec<String>,
    outputs: Vec<OutputInfo>,
}

impl IOInfo {
    fn gen_srcs() -> Vec<String> {
        vec![
            String::from("close"),
            String::from("open"),
            String::from("high"),
            String::from("low"),
        ]
    }

    pub fn new() -> IOInfo {
        IOInfo {
            inputs: vec![],
            input_srcs: IOInfo::gen_srcs(),
            outputs: vec![],
        }
    }

    pub fn new_with_io(
        inputs: Vec<InputInfo>,
        outputs: Vec<OutputInfo>,
        input_srcs: Vec<String>,
    ) -> IOInfo {
        IOInfo {
            inputs,
            outputs,
            input_srcs,
        }
    }

    pub fn push_input(&mut self, input: InputInfo) {
        self.inputs.push(input);
    }

    pub fn push_output(&mut self, output: OutputInfo) {
        self.outputs.push(output);
    }

    pub fn set_input_srcs(&mut self, input_srcs: Vec<String>) {
        self.input_srcs = input_srcs;
    }

    pub fn get_inputs(&self) -> &Vec<InputInfo> {
        &self.inputs
    }

    pub fn get_outputs(&self) -> &Vec<OutputInfo> {
        &self.outputs
    }
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct OutputData {
    pub from: Option<i32>,
    pub to: Option<i32>,
    pub series: Vec<Option<f64>>,
}

impl OutputData {
    pub fn new(from: Option<i32>, to: Option<i32>, series: Vec<Option<f64>>) -> OutputData {
        OutputData { from, to, series }
    }
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
#[serde(tag = "type", content = "content")]
pub enum InputVal {
    Int(i32),
    Float(f64),
    Bool(bool),
    String(String),
}
