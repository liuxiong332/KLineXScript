use crate::{DocBase, VarType};

const DESCRIPTION: &'static str = r#"
The vwma function returns volume-weighted moving average of x for y bars back. It is the same as: `sma(x * volume, y) / sma(volume, y)`
"#;

const EXAMPLE: &'static str = r#"
```pine
plot(vwma(close, 15))

// same on pine, but less efficient
pine_vwma(x, y) =>
    sma(x * volume, y) / sma(volume, y)
plot(pine_vwma(close, 15))
```
"#;

const ARGUMENTS: &'static str = r#"
source (series(float)) Series of values to process.
length (int) Number of bars (length).
"#;

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "vwma",
        signatures: vec![],
        description: DESCRIPTION,
        example: EXAMPLE,
        returns: "Volume-weighted moving average of x for y bars back.",
        arguments: ARGUMENTS,
        remarks: "",
        links: "[sma](#fun-sma)",
    };
    vec![fn_doc]
}
