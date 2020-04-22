use crate::{DocBase, VarType};

const DESCRIPTION: &'static str = r#"
Relative strength index. It is calculated based on rma's of upward and downward change of x.
"#;

const REMARKS: &'static str = r#"
If x is a series and y is integer then x is a source series and y is a length.
If x is a series and y is a series then x and y are considered to be 2 calculated MAs for upward and downward changes.
"#;

const EXAMPLE: &'static str = r#"
```pine
plot(rsi(close, 7))

// same on pine, but less efficient
pine_rsi(x, y) => 
    u = max(x - x[1], 0) // upward change
    d = max(x[1] - x, 0) // downward change
    rs = rma(u, y) / rma(d, y)
    res = 100 - 100 / (1 + rs)
    res

plot(pine_rsi(close, 7))
```
"#;

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "rsi",
        signatures: vec![],
        description: DESCRIPTION,
        example: EXAMPLE,
        returns: "Relative strength index.",
        arguments: "",
        remarks: REMARKS,
        links: "[rma](#fun-rma)",
    };
    vec![fn_doc]
}
