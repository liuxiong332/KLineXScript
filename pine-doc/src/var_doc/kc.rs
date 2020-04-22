use crate::{DocBase, VarType};

const DESCRIPTION: &'static str = r#"
Keltner Channels. Keltner channel is a technical analysis indicator showing a central moving average line plus channel lines at a distance above and below.
"#;

const EXAMPLES: &'static str = r#"
```pine
study('My Script')

[middle, upper, lower] = kc(close, 5, 4)
plot(middle, color=color.yellow)
plot(upper, color=color.yellow)
plot(lower, color=color.yellow)


// the same on pine
f_kc(src, length, mult, useTrueRange) =>
    float basis = ema(src, length)
    float range = (useTrueRange) ? tr : (high - low)
    float rangeEma = ema(range, length)
    [basis, basis + rangeEma * mult, basis - rangeEma * mult]
    
[pineMiddle, pineUpper, pineLower] = f_kc(close, 5, 4, true)

plot(pineMiddle)
plot(pineUpper)
plot(pineLower)
```
"#;

const ARGUMENTS: &'static str = r#"
series (series(float)) Series of values to process.
length (int) Number of bars (length).
mult (float) Standard deviation factor
useTrueRange (bool) An optional parameter. Specifies if True Range is used; default is true. If the value is false, the range will be calculated with the expression `(high - low)`
"#;

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "kc",
        signatures: vec![],
        description: DESCRIPTION,
        example: EXAMPLES,
        returns: "Keltner Channels.",
        arguments: ARGUMENTS,
        remarks: "",
        links: "[ema](#fun-ema) [bb](#fun-bb)",
    };
    vec![fn_doc]
}
