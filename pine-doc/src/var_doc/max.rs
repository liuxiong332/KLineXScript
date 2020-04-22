use crate::{DocBase, VarType};

const DESCRIPTION: &'static str = r#"
Returns the greatest of multiple values
"#;

const EXAMPLES: &'static str = r#"
```pine
max(close, open)
max(close, max(open, 42))
```
"#;

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "max",
        signatures: vec![],
        description: DESCRIPTION,
        example: EXAMPLES,
        returns: "The greatest of multiple given values.",
        arguments: "",
        remarks: "",
        links: "[min](#fun-min)",
    };
    vec![fn_doc]
}
