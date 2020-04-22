use crate::{DocBase, VarType};

const DESCRIPTION: &'static str = r#"
Square root of any x >= 0 is the unique y >= 0 such that y^2 = x
"#;

const ARGUMENT: &'static str = r#"
**source (series(float))** Series of values to process.
**length (int)** Number of bars (length).
"#;

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "sqrt",
        signatures: vec![],
        description: DESCRIPTION,
        example: "",
        returns: "The square root of x.",
        arguments: ARGUMENT,
        remarks: "",
        links: "[pow](#fun-pow)",
    };
    vec![fn_doc]
}
