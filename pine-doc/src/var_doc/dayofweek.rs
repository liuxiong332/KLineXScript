use crate::{DocBase, VarType};

const VAR_REMARKS: &'static str = r#"
You can use [dayofweek.sunday](#var-dayofweek-sunday), [dayofweek.monday](#var-dayofweek-monday), [dayofweek.tuesday](#var-dayofweek-tuesday), [dayofweek.wednesday](#var-dayofweek-wednesday), [dayofweek.thursday](#var-dayofweek-thursday), [dayofweek.friday](#var-dayofweek-friday) and [dayofweek.saturday](#var-dayofweek-saturday) variables for comparisons.
"#;

pub fn gen_doc() -> Vec<DocBase> {
    vec![
        DocBase {
            var_type: VarType::Variable,
            name: "dayofweek",
            signatures: vec![],
            description: "Day of week for current bar time in exchange timezone.",
            example: "",
            returns: "",
            arguments: "",
            remarks: VAR_REMARKS,
            links: "",
        },
        DocBase {
            var_type: VarType::Function,
            name: "dayofweek",
            signatures: vec![],
            description: "",
            example: "",
            returns: "Day of week (in exchange timezone) for provided UNIX time.",
            arguments: "**time (series(int))** UNIX time in milliseconds.",
            remarks: "UNIX time is the number of milliseconds that have elapsed since 00:00:00 UTC, 1 January 1970.",
            links: "",
        },
    ]
}
