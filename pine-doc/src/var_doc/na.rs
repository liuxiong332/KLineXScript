use crate::{DocBase, VarType};

const DESCRIPTION: &'static str = r#"
Test value if it's a NaN.
"#;

pub fn gen_doc() -> Vec<DocBase> {
    vec![DocBase {
        var_type: VarType::Function,
        name: "na",
        signatures: vec![],
        description: DESCRIPTION,
        example: "",
        returns: "true if x is not a valid number (x is NaN), otherwise false.",
        arguments: "",
        remarks: "",
        links: "[nz](#fun-nz) [fixnan](#fun-fixnan)",
    }]
}
