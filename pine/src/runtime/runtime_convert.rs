use crate::ast::syntax_type::{SimpleSyntaxType, SyntaxType};
use crate::types::{Bool, Color, Float, Int, PineFrom, PineRef, RefData, Series, Tuple, NA};
use std::mem;

pub fn convert<'a>(val: PineRef<'a>, dest_type: &SyntaxType<'a>) -> PineRef<'a> {
    match dest_type {
        SyntaxType::Series(SimpleSyntaxType::Bool) => {
            let s: RefData<Series<Bool>> = Series::implicity_from(val).unwrap();
            s.into_pf()
        }
        SyntaxType::Series(SimpleSyntaxType::Na) => {
            let s: RefData<Series<NA>> = Series::implicity_from(val).unwrap();
            s.into_pf()
        }
        SyntaxType::Series(SimpleSyntaxType::Int) => {
            let s: RefData<Series<Int>> = Series::implicity_from(val).unwrap();
            s.into_pf()
        }
        SyntaxType::Series(SimpleSyntaxType::Float) => {
            let s: RefData<Series<Float>> = Series::implicity_from(val).unwrap();
            s.into_pf()
        }
        SyntaxType::Series(SimpleSyntaxType::Color) => {
            let s: RefData<Series<Color>> = Series::implicity_from(val).unwrap();
            s.into_pf()
        }
        SyntaxType::Series(SimpleSyntaxType::String) => {
            let s: RefData<Series<String>> = Series::implicity_from(val).unwrap();
            s.into_pf()
        }
        SyntaxType::Simple(SimpleSyntaxType::Bool) => Bool::implicity_from(val).unwrap().into_pf(),
        SyntaxType::Simple(SimpleSyntaxType::Na) => NA::implicity_from(val).unwrap().into_pf(),
        SyntaxType::Simple(SimpleSyntaxType::Int) => Int::implicity_from(val).unwrap().into_pf(),
        SyntaxType::Simple(SimpleSyntaxType::Float) => {
            Float::implicity_from(val).unwrap().into_pf()
        }
        SyntaxType::Simple(SimpleSyntaxType::Color) => {
            Color::implicity_from(val).unwrap().into_pf()
        }
        SyntaxType::Simple(SimpleSyntaxType::String) => {
            String::implicity_from(val).unwrap().into_pf()
        }
        SyntaxType::Void => val,
        SyntaxType::Tuple(tuple) => {
            let mut tuple_val = Tuple::implicity_from(val).unwrap();
            let res: Vec<_> = tuple
                .iter()
                .zip(mem::replace(&mut tuple_val.0, vec![]).into_iter())
                .map(|d| convert(d.1, d.0))
                .collect();
            PineRef::new_box(Tuple(res))
        }
        _ => val, // _ => val,
    }
}
