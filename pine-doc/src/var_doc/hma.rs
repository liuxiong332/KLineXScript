use crate::{DocBase, VarType};

const DESCRIPTION: &'static str = r#"
The hma function returns the Hull Moving Average.
"#;

const EXAMPLES: &'static str = r#"
```pine
study("Hull Moving Average")
src = input(defval=close, type=input.source, title="Source")
length = input(defval=9, type=input.integer, title="Length")
hmaBuildIn = hma(src, length)
plot(hmaBuildIn, title="Hull MA", color=#674EA7)
```
"#;

const ARGUMENTS: &'static str = r#"
source (series(float)) Series of values to process.
length (int) Number of bars.
"#;

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "hma",
        signatures: vec![],
        description: DESCRIPTION,
        example: EXAMPLES,
        returns: "Hull moving average of 'source' for 'length' bars back.",
        arguments: ARGUMENTS,
        remarks: "",
        links: "[ema](#fun-ema) [rma](#fun-rma) [wma](#fun-wma) [vwma](#fun-vwma) [sma](#fun-sma)",
    };
    vec![fn_doc]
}
