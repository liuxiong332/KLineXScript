use crate::{DocBase, VarType};

pub fn gen_doc() -> Vec<DocBase> {
    vec![DocBase {
        var_type: VarType::Variable,
        name: "high",
        signatures: vec![],
        description: "Current high price.",
        example: "",
        returns: "",
        arguments: "",
        remarks: "Previous values may be accessed with square brackets operator [], e.g. `high[1]`, `high[2]`.",
        links: "",
    }]
}
