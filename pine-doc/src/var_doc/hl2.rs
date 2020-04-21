use crate::{DocBase, VarType};

pub fn gen_doc() -> Vec<DocBase> {
    vec![DocBase {
        var_type: VarType::Variable,
        name: "hl2",
        signatures: vec![],
        description: "Is a shortcut for `(high + low)/2`.",
        example: "",
        returns: "",
        arguments: "",
        remarks: "",
        links: "",
    }]
}
