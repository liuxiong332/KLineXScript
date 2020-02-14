#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct StudyScript {
    pub title: String,
    pub shorttitle: Option<String>,
    pub overlay: Option<bool>,
    pub format: Option<String>,
    pub precision: Option<i64>,
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum ScriptPurpose {
    Study(StudyScript),
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct BoolInputInfo {
    pub defval: Option<bool>,
    pub title: Option<String>,
    pub input_type: String,
    pub confirm: Option<bool>,
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct IntInputInfo {
    pub defval: Option<i64>,
    pub title: Option<String>,
    pub input_type: String,
    pub minval: Option<i64>,
    pub maxval: Option<i64>,
    pub confirm: Option<bool>,
    pub step: Option<i64>,
    pub options: Option<Vec<i64>>,
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
    pub defval: Option<String>,
    pub title: Option<String>,
    pub input_type: String,
    pub confirm: Option<bool>,
    pub options: Option<Vec<String>>,
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct SourceInputInfo {
    pub defval: Option<String>,
    pub title: Option<String>,
    pub input_type: String,
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum InputInfo {
    Bool(BoolInputInfo),
    Int(IntInputInfo),
    Float(FloatInputInfo),
    String(StringInputInfo),
    Source(SourceInputInfo),
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct PlotInfo {
    pub title: Option<String>,
    pub color: Option<String>,
    pub linewidth: Option<i64>,
    pub style: Option<String>,
    pub transp: Option<i64>,
    pub trackprice: Option<bool>,
    pub histbase: Option<f64>,
    pub offset: Option<i64>,
    pub join: Option<bool>,
    pub editable: Option<bool>,
    pub show_last: Option<i64>,
    pub display: Option<i64>,
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct PlotArrowInfo {
    pub title: Option<String>,
    pub colorup: Option<String>,
    pub colordown: Option<String>,
    pub transp: Option<i64>,
    pub offset: Option<i64>,
    pub minheight: Option<i64>,
    pub maxheight: Option<i64>,
    pub editable: Option<bool>,
    pub show_last: Option<i64>,
    pub display: Option<i64>,
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct PlotBarInfo {
    pub title: Option<String>,
    pub color: Option<String>,
    pub editable: Option<bool>,
    pub show_last: Option<i64>,
    pub display: Option<i64>,
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct PlotCandleInfo {
    pub title: Option<String>,
    pub color: Option<String>,
    pub wickcolor: Option<String>,
    pub bordercolor: Option<String>,
    pub editable: Option<bool>,
    pub show_last: Option<i64>,
    pub display: Option<i64>,
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct PlotCharInfo {
    pub title: Option<String>,
    pub char: Option<String>,
    pub location: Option<String>,
    pub color: Option<String>,
    pub transp: Option<i64>,
    pub offset: Option<i64>,
    pub text: Option<String>,
    pub textcolor: Option<String>,
    pub editable: Option<bool>,
    pub size: Option<String>,
    pub show_last: Option<i64>,
    pub display: Option<i64>,
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct PlotShapeInfo {
    pub title: Option<String>,
    pub style: Option<String>,
    pub location: Option<String>,
    pub color: Option<String>,
    pub transp: Option<i64>,
    pub offset: Option<i64>,
    pub text: Option<String>,
    pub textcolor: Option<String>,
    pub editable: Option<bool>,
    pub size: Option<String>,
    pub show_last: Option<i64>,
    pub display: Option<i64>,
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum OutputInfo {
    Plot(PlotInfo),
    PlotArrow(PlotArrowInfo),
    PlotBar(PlotBarInfo),
    PlotCandle(PlotCandleInfo),
    PlotChar(PlotCharInfo),
    PlotShape(PlotShapeInfo),
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct IOInfo {
    script_type: Option<ScriptPurpose>,
    inputs: Vec<InputInfo>,
    input_srcs: Vec<String>,
    outputs: Vec<OutputInfo>,
    want_syminfo: bool,
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
            script_type: None,
            inputs: vec![],
            input_srcs: IOInfo::gen_srcs(),
            outputs: vec![],
            want_syminfo: false,
        }
    }

    pub fn new_with_io(
        inputs: Vec<InputInfo>,
        outputs: Vec<OutputInfo>,
        input_srcs: Vec<String>,
    ) -> IOInfo {
        IOInfo {
            script_type: None,
            inputs,
            outputs,
            input_srcs,
            want_syminfo: false,
        }
    }

    pub fn push_input(&mut self, input: InputInfo) {
        self.inputs.push(input);
    }

    pub fn push_output(&mut self, output: OutputInfo) {
        self.outputs.push(output);
    }

    pub fn set_script_type(&mut self, script_type: ScriptPurpose) {
        self.script_type = Some(script_type);
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

    pub fn get_script_type(&self) -> &Option<ScriptPurpose> {
        &self.script_type
    }
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct OutputData {
    // pub from: Option<i32>,
    // pub to: Option<i32>,
    pub series: Vec<Vec<Option<f64>>>,
}

impl OutputData {
    pub fn new(series: Vec<Vec<Option<f64>>>) -> OutputData {
        OutputData { series }
    }
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct OutputDataCollect {
    pub from: i32,
    pub to: i32,
    pub data_list: Vec<Option<OutputData>>,
}

impl OutputDataCollect {
    pub fn new(from: i32, to: i32, data_list: Vec<Option<OutputData>>) -> OutputDataCollect {
        OutputDataCollect {
            from,
            to,
            data_list,
        }
    }

    pub fn new_with_one(from: i32, to: i32, data: Vec<Option<f64>>) -> OutputDataCollect {
        OutputDataCollect {
            from,
            to,
            data_list: vec![Some(OutputData::new(vec![data]))],
        }
    }
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
#[serde(tag = "type", content = "content")]
pub enum InputVal {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct SymbolInfo {
    pub symbol_type: String,
    pub timezone: String,
    pub ticker: String,
    pub session: String, // regular or extended
    pub trade_start: String,
    pub trade_end: String,
    pub root: Option<String>, // Root for derivatives like futures contract.
    pub currency: String,     // "USD", "EUR", etc.
    pub description: String,
    pub mintick: f64, // Min tick value for current symbol
}
