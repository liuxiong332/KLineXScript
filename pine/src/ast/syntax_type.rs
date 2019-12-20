use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionType<'a> {
    pub types: Vec<(Vec<(&'a str, SyntaxType<'a>)>, SyntaxType<'a>)>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum SimpleSyntaxType {
    Int,
    Float,
    Bool,
    Na,
    String,
    Color,
}

#[derive(Debug, PartialEq, Clone)]
pub enum SyntaxType<'a> {
    Void,
    Simple(SimpleSyntaxType),
    Series(SimpleSyntaxType),
    Tuple(Rc<Vec<SyntaxType<'a>>>),
    Object(Rc<HashMap<&'a str, SyntaxType<'a>>>),
    Function(Rc<FunctionType<'a>>),
    UserFunction(Rc<Vec<&'a str>>),
    Any,
}

impl<'a> SyntaxType<'a> {
    pub fn is_na(&self) -> bool {
        match self {
            SyntaxType::Simple(SimpleSyntaxType::Na) | SyntaxType::Series(SimpleSyntaxType::Na) => {
                true
            }
            _ => false,
        }
    }

    pub fn is_void(&self) -> bool {
        self == &SyntaxType::Void
    }

    pub fn is_int(&self) -> bool {
        match self {
            SyntaxType::Simple(SimpleSyntaxType::Int)
            | SyntaxType::Series(SimpleSyntaxType::Int) => true,
            _ => false,
        }
    }

    pub fn is_num(&self) -> bool {
        match self {
            SyntaxType::Simple(SimpleSyntaxType::Int)
            | SyntaxType::Series(SimpleSyntaxType::Int)
            | SyntaxType::Simple(SimpleSyntaxType::Float)
            | SyntaxType::Series(SimpleSyntaxType::Float) => true,
            _ => false,
        }
    }

    pub fn is_string(&self) -> bool {
        match self {
            SyntaxType::Simple(SimpleSyntaxType::String)
            | SyntaxType::Series(SimpleSyntaxType::String) => true,
            _ => false,
        }
    }
}
