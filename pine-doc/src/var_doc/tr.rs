use crate::{DocBase, VarType};

const TR_ARGU: &'static str = r#"
**handle_na (bool)** How NaN values are handled. if true, and previous day's close is NaN then tr would be calculated as current day high-low. Otherwise (if false) tr would return NaN in such cases. Also note, that atr uses tr(true).
"#;

pub fn gen_doc() -> Vec<DocBase> {
    vec![
        DocBase {
            var_type: VarType::Variable,
            name: "tr",
            signatures: vec![],
            description: "True range. Same as `tr(false)`. It is `max(high - low, abs(high - close[1]), abs(low - close[1]))`",
            example: "",
            returns: "",
            arguments: "",
            remarks: "",
            links: "[tr](#fun-tr) [atr](#var-atr)",
        },
        DocBase {
            var_type: VarType::Function,
            name: "tr",
            signatures: vec![],
            description: "",
            example: "",
            returns: "True range. It is `max(high - low, abs(high - close[1]), abs(low - close[1]))`",
            arguments: TR_ARGU,
            remarks: "tr(false) is exactly the same as [tr](#var-tr).",
            links: "[tr](#var-tr) [atr](#var-atr)",
        },
    ]
}
