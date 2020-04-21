use crate::{DocBase, VarType};

pub fn gen_doc() -> Vec<DocBase> {
    vec![
        DocBase {
            var_type: VarType::Variable,
            name: "hour",
            signatures: vec![],
            description: "Current bar hour in exchange timezone.",
            example: "",
            returns: "",
            arguments: "",
            remarks: "",
            links: "",
        },
        DocBase {
            var_type: VarType::Function,
            name: "hour",
            signatures: vec![],
            description: "",
            example: "",
            returns: "Hour (in exchange timezone) for provided UNIX time.",
            arguments: "**time (series(int))** UNIX time in milliseconds.",
            remarks: "UNIX time is the number of milliseconds that have elapsed since 00:00:00 UTC, 1 January 1970.",
            links: "",
        },
    ]
}
