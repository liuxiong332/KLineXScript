use crate::{DocBase, VarType};

const DESCRIPTION: &'static str = r#"
Difference between current value and previous, `x - x[y]`.
"#;

const PINE_FN_ARGUMENTS: &'static str = "
**source (series(float))**
**length (int)** Offset from the current bar to the previous bar. Optional, if not given, length = 1 is used.
";

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "change",
        signatures: vec![],
        description: DESCRIPTION,
        example: "",
        returns: "Differences series.",
        arguments: PINE_FN_ARGUMENTS,
        remarks: "",
        links: "",
    };
    vec![fn_doc]
}
