use super::stat_expr_types::DataType;
use std::collections::BTreeMap;
use std::convert::From;
use std::rc::Rc;
use std::string::ToString;

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

impl From<DataType> for SimpleSyntaxType {
    fn from(data_type: DataType) -> Self {
        match data_type {
            DataType::Bool => SimpleSyntaxType::Bool,
            DataType::Int => SimpleSyntaxType::Int,
            DataType::Float => SimpleSyntaxType::Float,
            DataType::String => SimpleSyntaxType::String,
            DataType::Color => SimpleSyntaxType::Color,
        }
    }
}

impl<'a> From<SyntaxType<'a>> for SimpleSyntaxType {
    fn from(syntax_type: SyntaxType<'a>) -> Self {
        match syntax_type {
            SyntaxType::Simple(simple_type) => simple_type,
            SyntaxType::Series(simple_type) => simple_type,
            _ => unreachable!(),
        }
    }
}

impl ToString for SimpleSyntaxType {
    fn to_string(&self) -> String {
        match self {
            SimpleSyntaxType::Int => String::from("int"),
            SimpleSyntaxType::Float => String::from("float"),
            SimpleSyntaxType::Bool => String::from("bool"),
            SimpleSyntaxType::Na => String::from("na"),
            SimpleSyntaxType::String => String::from("string"),
            SimpleSyntaxType::Color => String::from("color"),
        }
    }
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
