use crate::{DocBase, VarType};

const REMARKS: &'static str = r#"
Two args version: x is a series and y is a length.
One arg version: x is a length. Algorithm uses low as a source series.
"#;

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "lowestbars",
        signatures: vec![],
        description: "Lowest value offset for a given number of bars back.",
        example: "",
        returns: "Offset to the lowest bar.",
        arguments: "",
        remarks: REMARKS,
        links: "[lowest](#fun-lowest) [highest](#fun-highest)",
    };
    vec![fn_doc]
}
