mod abs;
mod accdist;
mod acos;
mod alma;
mod asin;
mod atan;
mod atr;
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
        atan::gen_doc(),
        atr::gen_doc(),
    ]
    .into_iter()
    .flatten()
    .collect()
}
