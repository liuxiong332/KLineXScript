use crate::{DocBase, VarType};

const DESCRIPTION: &'static str = r#"
MACD (moving average convergence/divergence). It is supposed to reveal changes in the strength, direction, momentum, and duration of a trend in a stock's price.
"#;

const EXAMPLES: &'static str = r#"
```pine
// Example 1
study('MACD')
[macdLine, signalLine, histLine] = macd(close, 12, 26, 9)
plot(macdLine, color=color.blue)
plot(signalLine, color=color.orange)
plot(histLine, color=color.red, style=plot.style_histogram)

// Example 2
// If you need only one value, use placeholders '_' like this:
study('MACD')
[_, signalLine, _] = macd(close, 12, 26, 9)
plot(signalLine, color=color.orange)
```
"#;

const ARGUMENTS: &'static str = r#"
source (series) Series of values to process.
fastlen (int) Fast Length parameter.
slowlen (int) Slow Length parameter.
siglen (int) Signal Length parameter.
"#;

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "macd",
        signatures: vec![],
        description: DESCRIPTION,
        example: EXAMPLES,
        returns: "Tuple of three MACD series: MACD line, signal line and histogram line.",
        arguments: ARGUMENTS,
        remarks: "",
        links: "[sma](#fun-sma) [ema](#fun-ema)",
    };
    vec![fn_doc]
}
