use crate::{DocBase, VarType};

const DESCRIPTION: &'static str = r#"
Correlation coefficient. Describes the degree to which two series tend to deviate from their sma values.
"#;

const PINE_FN_ARGUMENTS: &'static str = "
**source_a (series(float))** Source series.
**source_b (series(float))** Target series.
**length (int)** Length (number of bars back).
";

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "correlation",
        signatures: vec![],
        description: DESCRIPTION,
        example: "",
        returns: "Correlation coefficient.",
        arguments: PINE_FN_ARGUMENTS,
        remarks: "",
        links: "",
    };
    vec![fn_doc]
}
