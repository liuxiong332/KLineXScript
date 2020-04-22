use crate::{DocBase, VarType};

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "highest",
        signatures: vec![],
        description: "Highest value for a given number of bars back.",
        example: "",
        returns: "Highest value.",
        arguments: "",
        remarks: "Two args version: x is a series and y is a length.
        One arg version: x is a length. Algorithm uses high as a source series.",
        links: "[lowest](#fun-lowest)",
    };
    vec![fn_doc]
}
