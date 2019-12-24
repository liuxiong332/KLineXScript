use std::collections::BTreeMap;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct FunctionType<'a>(pub (Vec<(&'a str, SyntaxType<'a>)>, SyntaxType<'a>));

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct FunctionTypes<'a>(pub Vec<FunctionType<'a>>);

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum SimpleSyntaxType {
    Int,
    Float,
    Bool,
    Na,
    String,
    Color,
}

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum SyntaxType<'a> {
    Void,
    Simple(SimpleSyntaxType),
    Series(SimpleSyntaxType),
    Tuple(Rc<Vec<SyntaxType<'a>>>),
    Object(Rc<BTreeMap<&'a str, SyntaxType<'a>>>),
    Function(Rc<FunctionTypes<'a>>),
    UserFunction(Rc<(Vec<&'a str>, SyntaxType<'a>)>),
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
