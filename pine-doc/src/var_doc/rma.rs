use crate::{DocBase, VarType};

const DESCRIPTION: &'static str = r#"
Moving average used in RSI. It is the exponentially weighted moving average with `alpha = 1 / length`.
"#;

const EXAMPLE: &'static str = r#"
```pine
plot(rma(close, 15))

// same on pine, but much less efficient
pine_rma(x, y) =>
	alpha = y
    sum = 0.0
    sum := (x + (alpha - 1) * nz(sum[1])) / alpha
plot(pine_rma(close, 15))
```
"#;

const ARGUMENTS: &'static str = r#"
source (series(float)) Series of values to process.
length (int) Number of bars (length).
"#;

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "rma",
        signatures: vec![],
        description: DESCRIPTION,
        example: EXAMPLE,
        returns: "Exponential moving average of x with `alpha = 1 / y`.",
        arguments: ARGUMENTS,
        remarks: "",
        links: "[sma](#fun-sma)",
    };
    vec![fn_doc]
}
