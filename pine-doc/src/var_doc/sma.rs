use crate::{DocBase, VarType};

const DESCRIPTION: &'static str = r#"
The sma function returns the moving average, that is the sum of last y values of x, divided by y.
"#;

const EXAMPLE: &'static str = r#"
```pine
plot(sma(close, 15))

// same on pine, but much less efficient
pine_sma(x, y) =>
    sum = 0.0
    for i = 0 to y - 1
        sum := sum + x[i] / y
    sum
plot(pine_sma(close, 15))
```
"#;

const ARGUMENT: &'static str = r#"
**source (series(float))** Series of values to process.
**length (int)** Number of bars (length).
"#;

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "sma",
        signatures: vec![],
        description: DESCRIPTION,
        example: EXAMPLE,
        returns: "Simple moving average of x for y bars back.",
        arguments: ARGUMENT,
        remarks: "",
        links: "[ema](#fun-ema) [rma](#fun-rma) [wma](#fun-wma) [vwma](#fun-vwma) [swma](#fun-swma) [alma](#fun-alma)",
    };
    vec![fn_doc]
}
