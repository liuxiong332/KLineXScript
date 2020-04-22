use crate::{DocBase, VarType};

const DESCRIPTION: &'static str = r#"
True strength index. It uses moving averages of the underlying momentum of a financial instrument.
"#;

const ARGUMENTS: &'static str = r#"
**source (series(float))** Source series.
**short_length (int)** Short length.
**long_length (int)** Long length.
"#;

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "tsi",
        signatures: vec![],
        description: DESCRIPTION,
        example: "",
        returns: "True strength index. A value in range `[-1, 1]`",
        arguments: ARGUMENTS,
        remarks: "",
        links: "",
    };
    vec![fn_doc]
}
