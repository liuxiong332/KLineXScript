use crate::{DocBase, VarType};

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "asin",
        signatures: vec![],
        description: "The asin function returns the arcsine (in radians) of number such that sin(asin(y)) = y for y in range [-1, 1].",
        example: "",
        returns: "The arcsine of a value; the returned angle is in the range [-Pi/2, Pi/2], or na if y is outside of range [-1, 1].",
        arguments: "",
        remarks: "",
        links: "",
    };
    vec![fn_doc]
}
