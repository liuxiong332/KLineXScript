use crate::{DocBase, VarType};

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "lowest",
        signatures: vec![],
        description: "Lowest value for a given number of bars back.",
        example: "",
        returns: "Lowest value.",
        arguments: "",
        remarks: "Two args version: x is a series and y is a length.
        One arg version: x is a length. Algorithm uses low as a source series.",
        links: "[highest](#fun-highest)",
    };
    vec![fn_doc]
}
