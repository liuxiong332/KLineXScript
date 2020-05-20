use super::{SimpleSyntaxType, SyntaxType};
use crate::ast::stat_expr_types::DataType;

pub fn explicity_type_cast<'a>(
    cur_type: &SyntaxType<'a>,
    dest_type: &DataType<'a>,
) -> (bool, SyntaxType<'a>) {
    let cur_type = cur_type.get_v_for_vf();
    let (mut is_cast_err, mut result) = implicity_type_cast(cur_type, dest_type);
    match dest_type {
        DataType::Int => match cur_type {
            SyntaxType::Series(SimpleSyntaxType::Float) => {
                is_cast_err = false;
                result = SyntaxType::Series(SimpleSyntaxType::Int);
            }

            SyntaxType::Simple(SimpleSyntaxType::Float) => {
                is_cast_err = false;
                result = SyntaxType::Simple(SimpleSyntaxType::Int);
            }
            _ => (),
        },
        _ => (),
    };
    (is_cast_err, result)
}

pub fn implicity_type_cast<'a>(
    cur_type: &SyntaxType<'a>,
    dest_type: &DataType<'a>,
) -> (bool, SyntaxType<'a>) {
    let mut is_cast_err = false;
    let cur_type = cur_type.get_v_for_vf();
    let result = match dest_type {
        DataType::Bool => match cur_type {
            SyntaxType::Series(SimpleSyntaxType::Int)
            | SyntaxType::Series(SimpleSyntaxType::Float)
            | SyntaxType::Series(SimpleSyntaxType::Bool)
            | SyntaxType::Series(SimpleSyntaxType::Na) => {
                SyntaxType::Series(SimpleSyntaxType::Bool)
            }
            SyntaxType::Series(_) => {
                is_cast_err = true;
                SyntaxType::Series(SimpleSyntaxType::Bool)
            }
            SyntaxType::Simple(SimpleSyntaxType::Int)
            | SyntaxType::Simple(SimpleSyntaxType::Float)
            | SyntaxType::Simple(SimpleSyntaxType::Bool)
            | SyntaxType::Simple(SimpleSyntaxType::Na) => {
                SyntaxType::Simple(SimpleSyntaxType::Bool)
            }
            _ => {
                is_cast_err = true;
                SyntaxType::Simple(SimpleSyntaxType::Bool)
            }
        },
        DataType::Int => match cur_type {
            SyntaxType::Series(SimpleSyntaxType::Int)
            | SyntaxType::Series(SimpleSyntaxType::Na) => SyntaxType::Series(SimpleSyntaxType::Int),
            SyntaxType::Series(_) => {
                is_cast_err = true;
                SyntaxType::Series(SimpleSyntaxType::Int)
            }
            SyntaxType::Simple(SimpleSyntaxType::Int)
            | SyntaxType::Simple(SimpleSyntaxType::Na) => SyntaxType::Simple(SimpleSyntaxType::Int),
            _ => {
                is_cast_err = true;
                SyntaxType::Simple(SimpleSyntaxType::Int)
            }
        },
        DataType::Float => match cur_type {
            SyntaxType::Series(SimpleSyntaxType::Int)
            | SyntaxType::Series(SimpleSyntaxType::Float)
            | SyntaxType::Series(SimpleSyntaxType::Na) => {
                SyntaxType::Series(SimpleSyntaxType::Float)
            }
            SyntaxType::Series(_) => {
                is_cast_err = true;
                SyntaxType::Series(SimpleSyntaxType::Float)
            }

            SyntaxType::Simple(SimpleSyntaxType::Int)
            | SyntaxType::Simple(SimpleSyntaxType::Float)
            | SyntaxType::Simple(SimpleSyntaxType::Na) => {
                SyntaxType::Simple(SimpleSyntaxType::Float)
            }
            _ => {
                is_cast_err = true;
                SyntaxType::Simple(SimpleSyntaxType::Float)
            }
        },
        DataType::Color => match cur_type {
            SyntaxType::Series(SimpleSyntaxType::Color) => {
                SyntaxType::Series(SimpleSyntaxType::Color)
            }
            SyntaxType::Series(_) => {
                is_cast_err = true;
                SyntaxType::Series(SimpleSyntaxType::Color)
            }
            SyntaxType::Simple(SimpleSyntaxType::Color) => {
                SyntaxType::Simple(SimpleSyntaxType::Color)
            }
            _ => {
                is_cast_err = true;
                SyntaxType::Simple(SimpleSyntaxType::Color)
            }
        },
        DataType::String => match cur_type {
            SyntaxType::Series(SimpleSyntaxType::String) => {
                SyntaxType::Series(SimpleSyntaxType::String)
            }
            SyntaxType::Series(_) => {
                is_cast_err = true;
                SyntaxType::Series(SimpleSyntaxType::String)
            }
            SyntaxType::Simple(SimpleSyntaxType::String) => {
                SyntaxType::Simple(SimpleSyntaxType::String)
            }
            _ => {
                is_cast_err = true;
                SyntaxType::Simple(SimpleSyntaxType::String)
            }
        },
        DataType::Custom(t) => match cur_type {
            SyntaxType::Series(SimpleSyntaxType::Na) => SyntaxType::ObjectClass(*t),
            SyntaxType::Simple(SimpleSyntaxType::Na) => SyntaxType::ObjectClass(*t),
            SyntaxType::ObjectClass(n) if n == t => SyntaxType::ObjectClass(n),
            _ => {
                is_cast_err = true;
                SyntaxType::ObjectClass(*t)
            }
        },
    };
    (is_cast_err, result)
}
