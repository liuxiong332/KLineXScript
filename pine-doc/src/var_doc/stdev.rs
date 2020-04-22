use crate::{DocBase, VarType};

const ARGUMENT: &'static str = r#"
**source (series(float))** Series of values to process.
**length (integer)** Number of bars (length).
"#;

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "stdev",
        signatures: vec![],
        description: "",
        example: "",
        returns: "Standard deviation.",
        arguments: ARGUMENT,
        remarks: "This is a biased estimation of standard deviation.",
        links: "[dev](#fun-dev)",
    };
    vec![fn_doc]
}
