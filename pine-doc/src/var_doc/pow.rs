use crate::{DocBase, VarType};

const DESCRIPTION: &'static str = r#"
Mathematical power function.
"#;

const EXAMPLES: &'static str = r#"
```pine
pow(close, 2)
```
"#;

const ARGUMENTS: &'static str = r#"
**base** Specify the base to use.
**exponent (float)** Specifies the exponent.
"#;

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "pow",
        signatures: vec![],
        description: DESCRIPTION,
        example: EXAMPLES,
        returns: "x raised to the power of y. If x is a series, it is calculated elementwise.",
        arguments: ARGUMENTS,
        remarks: "",
        links: "[sqrt](#fun-sqrt) [exp](#fun-exp)",
    };
    vec![fn_doc]
}
