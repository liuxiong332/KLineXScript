use crate::{DocBase, VarType};

pub fn gen_doc() -> Vec<DocBase> {
    vec![DocBase {
        var_type: VarType::Variable,
        name: "ohlc4",
        signatures: vec![],
        description: "Is a shortcut for `(open + high + low + close)/4`.",
        example: "",
        returns: "",
        arguments: "",
        remarks: "",
        links: "",
    }]
}
