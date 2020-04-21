use crate::{DocBase, VarType};

const DESCRIPTION: &'static str = r#"
Bollinger Bands Width. The Bollinger Band Width is the difference between the upper and the lower Bollinger Bands divided by the middle band.
"#;

const EXAMPLE: &'static str = r#"
```pine
study('My Script')

plot(bbw(close, 5, 4), color=color.yellow)

// the same on pine
f_bbw(src, length, mult) =>
    float basis = sma(src, length)
    float dev = mult * stdev(src, length)
    ((basis + dev) - (basis - dev)) / basis

plot(f_bbw(close, 5, 4))
```
"#;

const PINE_FN_ARGUMENTS: &'static str = "
**series (series(float))** Series of values to process.
**length (int)** Number of bars (length).
**mult (float)** Standard deviation factor
";

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "bbw",
        signatures: vec![],
        description: DESCRIPTION,
        example: EXAMPLE,
        returns: "Bollinger Bands Width.",
        arguments: PINE_FN_ARGUMENTS,
        remarks: "",
        links: "[bb](#fun-bb) [stdev](#fun-stdev) [kc](#fun-kc)",
    };
    vec![fn_doc]
}
