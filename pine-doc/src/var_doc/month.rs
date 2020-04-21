use crate::{DocBase, VarType};

pub fn gen_doc() -> Vec<DocBase> {
    vec![
        DocBase {
            var_type: VarType::Variable,
            name: "month",
            signatures: vec![],
            description: "Current bar month in exchange timezone.",
            example: "",
            returns: "",
            arguments: "",
            remarks: "",
            links: "",
        },
        DocBase {
            var_type: VarType::Function,
            name: "month",
            signatures: vec![],
            description: "",
            example: "",
            returns: "Month (in exchange timezone) for provided UNIX time.",
            arguments: "**time (series(int))** UNIX time in milliseconds.",
            remarks: "UNIX time is the number of milliseconds that have elapsed since 00:00:00 UTC, 1 January 1970.",
            links: "",
        },
    ]
}
