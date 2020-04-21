use crate::{DocBase, VarType};

const DESCRIPTION: &'static str = r#"
The cog (center of gravity) is an indicator based on statistics and the Fibonacci golden ratio.
"#;

const PINE_FN_ARGUMENTS: &'static str = "
**source (series)** Series of values to process.
**length (int)** Number of bars (length).
";

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "cog",
        signatures: vec![],
        description: DESCRIPTION,
        example: "",
        returns: "Center of Gravity.",
        arguments: PINE_FN_ARGUMENTS,
        remarks: "",
        links: "[stoch](#fun-stoch)",
    };
    vec![fn_doc]
}
