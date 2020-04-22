use crate::{DocBase, VarType};

const DESCRIPTION: &'static str = r#"
The round function returns the value of the argument rounded to the nearest integer, with ties rounding up.
"#;

const ARGUMENTS: &'static str = r#"
source (series(float)) Series of values to process.
length (int) Offset from the current bar to the previous bar.
"#;

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "round",
        signatures: vec![],
        description: DESCRIPTION,
        example: "",
        returns: "The value of the argument rounded to the nearest integer.",
        arguments: ARGUMENTS,
        remarks: "",
        links: "[ceil](#fun-ceil) [floor](#fun-floor)",
    };
    vec![fn_doc]
}
