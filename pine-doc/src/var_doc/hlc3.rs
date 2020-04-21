use crate::{DocBase, VarType};

pub fn gen_doc() -> Vec<DocBase> {
    vec![DocBase {
        var_type: VarType::Variable,
        name: "hlc3",
        signatures: vec![],
        description: "Is a shortcut for `(high + low + close)/3`.",
        example: "",
        returns: "",
        arguments: "",
        remarks: "",
        links: "",
    }]
}
