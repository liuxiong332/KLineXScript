use crate::{DocBase, VarType};

const DESCRIPTION: &'static str = r#"
Returns the smallest of multiple values
"#;

const EXAMPLES: &'static str = r#"
```pine
min(close, open)
min(close, min(open, 42))
```
"#;

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "min",
        signatures: vec![],
        description: DESCRIPTION,
        example: EXAMPLES,
        returns: "The smallest of multiple given values.",
        arguments: "",
        remarks: "",
        links: "[max](#fun-max)",
    };
    vec![fn_doc]
}
