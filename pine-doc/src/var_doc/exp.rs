use crate::{DocBase, VarType};

const DESCRIPTION: &'static str = r#"
The exp function of x is e^x, where x is the argument and e is Euler's number.
"#;

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "exp",
        signatures: vec![],
        description: DESCRIPTION,
        example: "",
        returns: "A number representing e^x.",
        arguments: "",
        remarks: "",
        links: "[pow](#fun-pow)",
    };
    vec![fn_doc]
}
