use crate::{DocBase, VarType};
pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "acos",
        signatures: vec![],
        description: "The acos function returns the arccosine (in radians) of number such that cos(acos(y)) = y for y in range [-1, 1].",
        example: "",
        returns: "The arc cosine of a value; the returned angle is in the range [0, Pi], or na if y is outside of range [-1, 1].",
        arguments: "",
        remarks: "",
        links: "",
    };
    vec![fn_doc]
}
