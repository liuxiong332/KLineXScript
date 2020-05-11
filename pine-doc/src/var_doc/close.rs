use crate::{DocBase, VarType};

pub fn gen_doc() -> Vec<DocBase> {
    vec![DocBase {
        var_type: VarType::Variable,
        name: "close",
        signatures: vec![],
        description: "Current close price.",
        example: "",
        returns: "",
        arguments: "",
        remarks: "Previous values may be accessed with square brackets operator [], e.g. `close[1]`, `close[2]`.",
        links: "",
    }]
}
