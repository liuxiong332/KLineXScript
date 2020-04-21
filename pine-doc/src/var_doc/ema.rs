use crate::{DocBase, VarType};

const DESCRIPTION: &'static str = r#"
The sma function returns the exponentially weighted moving average. In ema weighting factors decrease exponentially. It calculates by sing a formula: `EMA = alpha * x + (1 - alpha) * EMA[1]`, where `alpha = 2 / (y + 1)`
"#;

const EXAMPLE: &'static str = r#"
```pine
plot(ema(close, 15))

// same on pine, but much less efficient
pine_ema(x, y) =>
    alpha = 2.0 / (y + 1)
    sum = 0.0
    sum := alpha * x + (1 - alpha) * nz(sum[1])
plot(pine_ema(close, 15))
```
"#;

const ARGUMENTS: &'static str = r#"
source (series(float)) Series of values to process.
length (int) Number of bars (length).
"#;

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "ema",
        signatures: vec![],
        description: DESCRIPTION,
        example: EXAMPLE,
        returns: "Exponential moving average of x with `alpha = 2 / (y + 1)`",
        arguments: ARGUMENTS,
        remarks: "",
        links:
            "[sma](#fun-sma) [rma](#fun-rma) [wma](#fun-wma) [vwma](#fun-vwma) [swma](#fun-swma)",
    };
    vec![fn_doc]
}
