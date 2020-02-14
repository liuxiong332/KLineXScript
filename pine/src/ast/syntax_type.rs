use super::stat_expr_types::DataType;
use std::collections::BTreeMap;
use std::convert::From;
use std::rc::Rc;
use std::string::ToString;

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct FunctionType<'a> {
    pub signature: (Vec<(&'a str, SyntaxType<'a>)>, SyntaxType<'a>),
}

impl<'a> FunctionType<'a> {
    pub fn new(signature: (Vec<(&'a str, SyntaxType<'a>)>, SyntaxType<'a>)) -> FunctionType<'a> {
        FunctionType { signature }
    }

    pub fn arg_names(&self) -> Vec<&'a str> {
        (self.signature).0.iter().map(|s| s.0).collect()
    }
}

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
    List(SimpleSyntaxType), // tuple list like [1, 2, 3]
    Tuple(Rc<Vec<SyntaxType<'a>>>),
    Object(Rc<BTreeMap<&'a str, SyntaxType<'a>>>),
    Function(Rc<FunctionTypes<'a>>),
    ObjectFunction(Rc<BTreeMap<&'a str, SyntaxType<'a>>>, Rc<FunctionTypes<'a>>),
    ValFunction(Box<SyntaxType<'a>>, Rc<FunctionTypes<'a>>),
    UserFunction(Rc<(Vec<&'a str>, SyntaxType<'a>)>),
    Any,
}

impl<'a> SyntaxType<'a> {
    // Get the value type from ValFunction type.
    pub fn get_v_for_vf(&self) -> &Self {
        match self {
            SyntaxType::ValFunction(t, _) => &*t,
            t => t,
        }
    }

    pub fn into_v_for_vf(self) -> Self {
        match self {
            SyntaxType::ValFunction(t, _) => *t,
            t => t,
        }
    }

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

    pub fn int() -> SyntaxType<'a> {
        SyntaxType::Simple(SimpleSyntaxType::Int)
    }

    pub fn bool() -> SyntaxType<'a> {
        SyntaxType::Simple(SimpleSyntaxType::Bool)
    }

    pub fn float() -> SyntaxType<'a> {
        SyntaxType::Simple(SimpleSyntaxType::Float)
    }

    pub fn string() -> SyntaxType<'a> {
        SyntaxType::Simple(SimpleSyntaxType::String)
    }

    pub fn color() -> SyntaxType<'a> {
        SyntaxType::Simple(SimpleSyntaxType::Color)
    }

    pub fn int_series() -> SyntaxType<'a> {
        SyntaxType::Series(SimpleSyntaxType::Int)
    }

    pub fn float_series() -> SyntaxType<'a> {
        SyntaxType::Series(SimpleSyntaxType::Float)
    }
}
