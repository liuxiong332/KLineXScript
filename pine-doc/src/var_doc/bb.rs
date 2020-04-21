use crate::{DocBase, VarType};

const DESCRIPTION: &'static str = r#"
Bollinger Bands. A Bollinger Band is a technical analysis tool defined by a set of lines plotted two standard deviations (positively and negatively) away from a simple moving average (SMA) of the security's price, but can be adjusted to user preferences.
"#;

const EXAMPLE: &'static str = r#"
```pine
study('My Script')

[middle, upper, lower] = bb(close, 5, 4)
plot(middle, color=color.yellow)
plot(upper, color=color.yellow)
plot(lower, color=color.yellow)

// the same on pine
f_bb(src, length, mult) =>
    float basis = sma(src, length)
    float dev = mult * stdev(src, length)
    [basis, basis + dev, basis - dev]

[pineMiddle, pineUpper, pineLower] = f_bb(close, 5, 4)

plot(pineMiddle)
plot(pineUpper)
plot(pineLower)
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
        name: "bb",
        signatures: vec![],
        description: DESCRIPTION,
        example: EXAMPLE,
        returns: "Bollinger Bands.",
        arguments: PINE_FN_ARGUMENTS,
        remarks: "",
        links: "[sma](#fun-sma) [stdev](#fun-stdev) [kc](#fun-kc)",
    };
    vec![fn_doc]
}
