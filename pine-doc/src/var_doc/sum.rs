use crate::{DocBase, VarType};

const DESCRIPTION: &'static str = r#"
The sum function returns the sliding sum of last y values of x.
"#;

const EXAMPLE: &'static str = r#"
```pine
study(title='MyScriptStudy')
study(title="MyScriptStudy", shorttitle="MSS", precision=6, overlay=true)
```
"#;

const ARGUMENT: &'static str = r#"
**source (series(float))** Series of values to process.
**length (int)** Number of bars (length).
"#;

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "sum",
        signatures: vec![],
        description: DESCRIPTION,
        example: "",
        returns: "The sum function returns the sliding sum of last y values of x.",
        arguments: ARGUMENT,
        remarks: "",
        links: "[cum](#fun-cum)",
    };
    vec![fn_doc]
}
