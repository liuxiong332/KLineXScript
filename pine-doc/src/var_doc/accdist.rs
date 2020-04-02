use crate::{DocBase, VarType};
pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Variable,
        name: "accdist",
        signatures: vec![],
        description: "Accumulation/distribution index.",
        example: "",
        returns: "",
        arguments: "",
        remarks: "",
        links: "",
    };
    vec![fn_doc]
}
