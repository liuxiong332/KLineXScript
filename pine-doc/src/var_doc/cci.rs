use crate::{DocBase, VarType};

const DESCRIPTION: &'static str = r#"
The CCI (commodity channel index) is calculated as the difference between the typical price of a commodity and its simple moving average, divided by the mean absolute deviation of the typical price. The index is scaled by an inverse factor of 0.015 to provide more readable numbers
"#;

const PINE_FN_ARGUMENTS: &'static str = "
**source (series(float))** Series of values to process.
**length (int)** Number of bars (length).
";

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "cci",
        signatures: vec![],
        description: DESCRIPTION,
        example: "",
        returns: "Commodity channel index of x for y bars back.",
        arguments: PINE_FN_ARGUMENTS,
        remarks: "",
        links: "",
    };
    vec![fn_doc]
}
