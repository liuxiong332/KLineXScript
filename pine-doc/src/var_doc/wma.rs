use crate::{DocBase, VarType};

const DESCRIPTION: &'static str = r#"
The wma function returns weighted moving average of x for y bars back. In wma weighting factors decrease in arithmetical progression.
"#;

const EXAMPLE: &'static str = r#"
```pine
plot(wma(close, 15))

// same on pine, but much less efficient
pine_wma(x, y) =>
    norm = 0.0
    sum = 0.0
    for i = 0 to y - 1
        weight = (y - i) * y
        norm := norm + weight
        sum := sum + x[i] * weight
    sum / norm
plot(pine_wma(close, 15))
```
"#;

const ARGUMENTS: &'static str = r#"
**source (series(float))** Series of values to process.
**length (int)** Number of bars (length).
"#;

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "wma",
        signatures: vec![],
        description: DESCRIPTION,
        example: EXAMPLE,
        returns: "Weighted moving average of x for y bars back.",
        arguments: ARGUMENTS,
        remarks: "",
        links: "[sma](#fun-sma) [ema](#fun-ema) [rma](#fun-rma) [vwma](#fun-vwma) [swma](#fun-swma) [alma](#fun-alma)",
    };
    vec![fn_doc]
}
