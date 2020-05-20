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
    pub opacity: Option<i64>,
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
    pub opacity: Option<i64>,
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
    pub opacity: Option<i64>,
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
    pub opacity: Option<i64>,
    pub offset: Option<i64>,
    pub text: Option<String>,
    pub textcolor: Option<String>,
    pub editable: Option<bool>,
    pub size: Option<String>,
    pub show_last: Option<i64>,
    pub display: Option<i64>,
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct FillInfo {
    pub fill_type: String,
    pub start: i64,
    pub end: i64,
    pub title: Option<String>,
    pub color: Option<String>,
    pub opacity: Option<i64>,
    pub editable: Option<bool>,
    pub show_last: Option<i64>,
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct HLineInfo {
    pub price: Option<f64>,
    pub title: Option<String>,
    pub color: Option<String>,
    pub linestyle: Option<String>,
    pub linewidth: Option<i64>,
    pub editable: Option<bool>,
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
    Fill(FillInfo),
    HLine(HLineInfo),
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct InputSrc {
    pub ticker: Option<String>, // The ticker name e.g. NASDAQ:FB
    pub srcs: Vec<String>,
}

impl InputSrc {
    pub fn new(ticker: Option<String>, srcs: Vec<String>) -> InputSrc {
        InputSrc { ticker, srcs }
    }
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct IOInfo {
    script_type: Option<ScriptPurpose>,
    inputs: Vec<InputInfo>,
    input_srcs: Vec<InputSrc>,
    outputs: Vec<OutputInfo>,
    want_syminfo: bool,
}

impl IOInfo {
    fn gen_srcs() -> Vec<InputSrc> {
        vec![InputSrc {
            ticker: None,
            srcs: vec![],
        }]
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
        input_srcs: Vec<InputSrc>,
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

    pub fn push_output_retindex(&mut self, output: OutputInfo) -> i32 {
        self.outputs.push(output);
        self.outputs.len() as i32 - 1
    }

    pub fn set_script_type(&mut self, script_type: ScriptPurpose) {
        self.script_type = Some(script_type);
    }

    pub fn add_input_src(&mut self, mut input_src: InputSrc) {
        // If the input source contain `_time`, we should transfer to `time`
        let pos = input_src.srcs.iter().position(|s| s.as_str() == "_time");
        if pos.is_some() {
            input_src.srcs.remove(pos.unwrap());
            if !input_src.srcs.contains(&String::from("time")) {
                input_src.srcs.push(String::from("time"));
            }
        }

        let exist_input = self
            .input_srcs
            .iter_mut()
            .find(|x| x.ticker == input_src.ticker);
        match exist_input {
            None => {
                self.input_srcs.push(input_src);
            }
            Some(v) => {
                let mut m: Vec<_> = input_src
                    .srcs
                    .into_iter()
                    .filter(|x| !v.srcs.iter().any(|s| s == x))
                    .collect();
                v.srcs.append(&mut m);
            }
        }
    }

    pub fn get_inputs(&self) -> &Vec<InputInfo> {
        &self.inputs
    }

    pub fn get_outputs(&self) -> &Vec<OutputInfo> {
        &self.outputs
    }

    pub fn get_input_srcs(&self) -> &Vec<InputSrc> {
        &self.input_srcs
    }

    pub fn get_script_type(&self) -> &Option<ScriptPurpose> {
        &self.script_type
    }
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct StrOptionsData {
    pub options: Vec<String>,
    pub values: Vec<Option<i32>>,
}

impl StrOptionsData {
    pub fn new() -> StrOptionsData {
        StrOptionsData {
            options: vec![],
            values: vec![],
        }
    }
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct OutputData {
    // pub from: Option<i32>,
    // pub to: Option<i32>,
    pub series: Vec<Vec<Option<f64>>>,
    pub colors: Vec<StrOptionsData>,
}

impl OutputData {
    pub fn new(series: Vec<Vec<Option<f64>>>) -> OutputData {
        OutputData {
            series,
            colors: vec![],
        }
    }

    pub fn new_with_sc(series: Vec<Vec<Option<f64>>>, colors: Vec<StrOptionsData>) -> OutputData {
        OutputData {
            series,
            colors: colors,
        }
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
    Source(String),
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn input_src_test() {
        let mut io_info = IOInfo::new();
        io_info.input_srcs = vec![];
        io_info.add_input_src(InputSrc::new(
            None,
            vec![String::from("close"), String::from("open")],
        ));
        io_info.add_input_src(InputSrc::new(
            None,
            vec![String::from("close"), String::from("low")],
        ));
        io_info.add_input_src(InputSrc::new(
            None,
            vec![String::from("close"), String::from("high")],
        ));
        io_info.add_input_src(InputSrc::new(
            Some(String::from("AAPL")),
            vec![String::from("close"), String::from("high")],
        ));

        io_info.add_input_src(InputSrc::new(
            Some(String::from("AAPL")),
            vec![String::from("_time"), String::from("time")],
        ));

        assert_eq!(
            io_info.get_input_srcs(),
            &vec![
                InputSrc::new(
                    None,
                    vec![
                        String::from("close"),
                        String::from("open"),
                        String::from("low"),
                        String::from("high")
                    ]
                ),
                InputSrc::new(
                    Some(String::from("AAPL")),
                    vec![
                        String::from("close"),
                        String::from("high"),
                        String::from("time")
                    ]
                )
            ]
        );
    }
}
