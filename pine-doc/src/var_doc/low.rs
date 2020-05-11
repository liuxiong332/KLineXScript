use crate::{DocBase, VarType};

pub fn gen_doc() -> Vec<DocBase> {
    vec![DocBase {
        var_type: VarType::Variable,
        name: "low",
        signatures: vec![],
        description: "Current low price.",
        example: "",
        returns: "",
        arguments: "",
        remarks: "Previous values may be accessed with square brackets operator [], e.g. `low[1]`, `low[2]`.",
        links: "",
    }]
}
