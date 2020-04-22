use crate::{DocBase, VarType};

const DESCRIPTION: &'static str = r#"
Replaces NaN values with zeros (or given value) in a series.
"#;

const EXAMPLE: &'static str = r#"
```pine
nz(sma(close, 100))
```
"#;

const ARGUMENT: &'static str = r#"
**x (series(float))** Series of values to process.
**y (float)** Value that will be inserted instead of all NaN values in x series.
"#;

const RETURN: &'static str = r#"
Two args version: returns x if it's a valid (not NaN) number, otherwise y
One arg version: returns x if it's a valid (not NaN) number, otherwise 0
"#;

pub fn gen_doc() -> Vec<DocBase> {
    vec![DocBase {
        var_type: VarType::Function,
        name: "nz",
        signatures: vec![],
        description: DESCRIPTION,
        example: EXAMPLE,
        returns: RETURN,
        arguments: ARGUMENT,
        remarks: "",
        links: "[na](#fun-na) [fixnan](#fun-fixnan)",
    }]
}
