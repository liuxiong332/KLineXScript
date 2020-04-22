use crate::{DocBase, VarType};

const DESCRIPTION: &'static str = r#"
Stochastic. It is calculated by a formula: `100 * (close - lowest(low, length)) / (highest(high, length) - lowest(low, length))`
"#;

const ARGUMENT: &'static str = r#"
**source (series)** Source series.
**high (series)** Series of high.
**low (series)** Series of low.
**length (integer)** Length (number of bars back).
"#;

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "stoch",
        signatures: vec![],
        description: DESCRIPTION,
        example: "",
        returns: "Stochastic.",
        arguments: ARGUMENT,
        remarks: "",
        links: "",
    };
    vec![fn_doc]
}
