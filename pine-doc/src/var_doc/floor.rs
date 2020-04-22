use crate::{DocBase, VarType};

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "floor",
        signatures: vec![],
        description: "",
        example: "",
        returns: "The largest integer less than or equal to the given number.",
        arguments: "",
        remarks: "",
        links: "[ceil](#fun-ceil) [round](#fun-round)",
    };
    vec![fn_doc]
}
