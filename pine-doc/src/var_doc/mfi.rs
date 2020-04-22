use crate::{DocBase, VarType};

const DESCRIPTION: &'static str = r#"
Money Flow Index. The Money Flow Index (MFI) is a technical oscillator that uses price and volume for identifying overbought or oversold conditions in an asset.
"#;

const EXAMPLES: &'static str = r#"
```pine
study('My Script')

plot(mfi(close, 5), color=color.yellow)

// the same on pine
f_mfi(src, length) =>
    float upper = sum(volume * (change(src) <= 0.0 ? 0.0 : src), length)
    float lower = sum(volume * (change(src) >= 0.0 ? 0.0 : src), length)
    
    if na(lower)
        float res = na
        return = res
    else
        return = rsi(upper, lower)

plot(f_mfi(close, 5))
```
"#;

const ARGUMENTS: &'static str = r#"
**series (series(float))** Series of values to process.
**length (int)** Number of bars (length).
"#;

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "mfi",
        signatures: vec![],
        description: DESCRIPTION,
        example: EXAMPLES,
        returns: "Money Flow Index.",
        arguments: ARGUMENTS,
        remarks: "",
        links: "[rsi](#fun-rsi) [sum](#fun-sum)",
    };
    vec![fn_doc]
}
