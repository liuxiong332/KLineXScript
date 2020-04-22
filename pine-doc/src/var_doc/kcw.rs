use crate::{DocBase, VarType};

const DESCRIPTION: &'static str = r#"
Keltner Channels Width. The Keltner Channels Width is the difference between the upper and the lower Keltner Channels divided by the middle channel.
"#;

const EXAMPLES: &'static str = r#"
```pine
study('My Script')

plot(kcw(close, 5, 4), color=color.yellow)

// the same on pine
f_kcw(src, length, mult, useTrueRange) =>
    float basis = ema(src, length)
    float range = (useTrueRange) ? tr : (high - low)
    float rangeEma = ema(range, length)
    
    ((basis + rangeEma * mult) - (basis - rangeEma * mult)) / basis

plot(f_kcw(close, 5, 4, true))
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
        name: "kcw",
        signatures: vec![],
        description: DESCRIPTION,
        example: EXAMPLES,
        returns: "Keltner Channels Width.",
        arguments: ARGUMENTS,
        remarks: "",
        links: "[kc](#fun-kc) [ema](#fun-ema) [bb](#fun-bb)",
    };
    vec![fn_doc]
}
