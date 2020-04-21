use crate::{DocBase, VarType};

pub fn gen_doc() -> Vec<DocBase> {
    vec![
        DocBase {
            var_type: VarType::Variable,
            name: "timenow",
            signatures: vec![],
            description: "Current time in UNIX format. It is the number of milliseconds that have elapsed since 00:00:00 UTC, 1 January 1970.",
            example: "",
            returns: "",
            arguments: "",
            remarks: "",
            links: "",
        },
    ]
}
