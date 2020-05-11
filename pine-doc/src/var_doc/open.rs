use crate::{DocBase, VarType};

pub fn gen_doc() -> Vec<DocBase> {
    vec![DocBase {
        var_type: VarType::Variable,
        name: "open",
        signatures: vec![],
        description: "Current open price.",
        example: "",
        returns: "",
        arguments: "",
        remarks: "Previous values may be accessed with square brackets operator [], e.g. `open[1]`, `open[2]`.",
        links: "",
    }]
}
