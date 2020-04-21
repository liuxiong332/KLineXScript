use crate::{DocBase, VarType};

const EXAMPLES: &'static str = r#"
```pine
plot(alma(close, 9, 0.85, 6))

// same on pine, but much less efficient
pine_alma(series, windowsize, offset, sigma) =>
    m = floor(offset * (windowsize - 1))
    s = windowsize / sigma
    norm = 0.0
    sum = 0.0
    for i = 0 to windowsize - 1
        weight = exp(-1 * pow(i - m, 2) / (2 * pow(s, 2)))
        norm := norm + weight
        sum := sum + series[windowsize - i - 1] * weight
    sum / norm
plot(pine_alma(close, 9, 0.85, 6))
```
"#;

const ARGUMENTS: &'static str = r#"
**series (series)** Series of values to process.
**length (int)** Number of bars (length).
**offset (float)** Controls tradeoff between smoothness (closer to 1) and responsiveness (closer to 0).
**sigma (float)** Changes the smoothness of ALMA. The larger sigma the smoother ALMA.
"#;

const LINKS: &'static str = r#"
[sma](#fun-sma) [ema](#fun-ema) [rma](#fun-rma) [wma](#fun-wma) [vwma](#fun-vwma) [swma](#fun-swma)
"#;

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "alma",
        signatures: vec![],
        description: "Arnaud Legoux Moving Average. It uses Gaussian distribution as weights for moving average.",
        example: EXAMPLES,
        returns: "Arnaud Legoux Moving Average.",
        arguments: ARGUMENTS,
        remarks: "",
        links: LINKS,
    };
    vec![fn_doc]
}
