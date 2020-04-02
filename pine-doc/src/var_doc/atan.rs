use crate::{DocBase, VarType};

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "atan",
        signatures: vec![],
        description: "The atan function returns the arctangent (in radians) of number such that tan(atan(y)) = y for any y.",
        example: "",
        returns: "The arc tangent of a value; the returned angle is in the range [-Pi/2, Pi/2].",
        arguments: "",
        remarks: "",
        links: "",
    };
    vec![fn_doc]
}
