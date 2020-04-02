use crate::{DocBase, VarType};
pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "abs",
        signatures: vec![],
        description: "Absolute value of x is x if x >= 0, or -x otherwise.",
        example: "",
        returns: "The absolute value of x",
        arguments: "",
        remarks: "",
        links: "",
    };
    vec![fn_doc]
}
