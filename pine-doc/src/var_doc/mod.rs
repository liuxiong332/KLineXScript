mod plot;
use super::DocBase;

pub fn declare_vars() -> Vec<DocBase> {
    vec![plot::gen_doc()].into_iter().flatten().collect()
}
