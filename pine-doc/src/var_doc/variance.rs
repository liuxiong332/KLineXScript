use crate::{DocBase, VarType};

const DESCRIPTION: &'static str = r#"
Variance is the expectation of the squared deviation of a series from its mean ([sma](#fun-sma)), and it informally measures how far a set of numbers are spread out from their mean.
"#;

const ARGUMENTS: &'static str = r#"
**source (series(float))** Series of values to process.
**length (int)** Number of bars (length).
"#;

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "variance",
        signatures: vec![],
        description: DESCRIPTION,
        example: "",
        returns: "Variance of x for y bars back.",
        arguments: ARGUMENTS,
        remarks: "This is a biased estimation of sample variance.",
        links: "[dev](#fun-dev) [stdev](#fun-stdev)",
    };
    vec![fn_doc]
}
