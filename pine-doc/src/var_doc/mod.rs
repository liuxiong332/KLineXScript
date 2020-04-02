mod abs;
mod accdist;
mod acos;
mod alma;
mod asin;
mod input;
mod plot;
use super::DocBase;

pub fn declare_vars() -> Vec<DocBase> {
    vec![
        plot::gen_doc(),
        input::gen_doc(),
        accdist::gen_doc(),
        abs::gen_doc(),
        acos::gen_doc(),
        alma::gen_doc(),
        asin::gen_doc(),
    ]
    .into_iter()
    .flatten()
    .collect()
}
