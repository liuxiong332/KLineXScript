use crate::{DocBase, VarType};

const DESCRIPTION: &'static str = r#"
Measure of difference between the series and it's sma
"#;

const ARGUMENTS: &'static str = r#"
**source (series)** Series of values to process.
**length (int)** Number of bars (length).
"#;

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "dev",
        signatures: vec![],
        description: DESCRIPTION,
        example: "",
        returns: "Deviation of x for y bars back.",
        arguments: ARGUMENTS,
        remarks: "",
        links: "[variance](#fun-variance) [stdev](#fun-stdev)",
    };
    vec![fn_doc]
}
