use super::{SimpleSyntaxType, SyntaxType};
use crate::ast::stat_expr_types::DataType;

pub fn parse_type_cast<'a>(
    cur_type: &SyntaxType<'a>,
    dest_type: &DataType,
) -> (bool, SyntaxType<'a>) {
    let mut is_cast_err = false;
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
            | SyntaxType::Series(SimpleSyntaxType::Float)
            | SyntaxType::Series(SimpleSyntaxType::Na) => SyntaxType::Series(SimpleSyntaxType::Int),
            SyntaxType::Series(_) => {
                is_cast_err = true;
                SyntaxType::Series(SimpleSyntaxType::Int)
            }
            SyntaxType::Simple(SimpleSyntaxType::Int)
            | SyntaxType::Simple(SimpleSyntaxType::Float)
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
    };
    (is_cast_err, result)
}
