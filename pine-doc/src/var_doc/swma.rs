use crate::{DocBase, VarType};

const DESCRIPTION: &'static str = r#"
Symmetrically weighted moving average with fixed length: 4. Weights: `[1/6, 2/6, 2/6, 1/6]`.
"#;

const EXAMPLE: &'static str = r#"
```pine
plot(swma(close))

// same on pine, but less efficient
pine_swma(x) =>
    x[3] * 1 / 6 + x[2] * 2 / 6 + x[1] * 2 / 6 + x[0] * 1 / 6
plot(pine_swma(close))
```
"#;

const ARGUMENT: &'static str = r#"
**x (series)** Source series.
"#;

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "swma",
        signatures: vec![],
        description: DESCRIPTION,
        example: EXAMPLE,
        returns: "Symmetrically weighted moving average.",
        arguments: ARGUMENT,
        remarks: "",
        links: "[sma](#fun-sma) [ema](#fun-ema) [rma](#fun-rma) [wma](#fun-wma) [vwma](#fun-vwma)",
    };
    vec![fn_doc]
}
