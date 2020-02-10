use super::stat_expr_types::DataType;
use std::collections::BTreeMap;
use std::convert::From;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::mem;
use std::rc::Rc;
use std::string::ToString;

#[derive(Clone)]
pub struct FunctionType<'a> {
    pub signature: (Vec<(&'a str, SyntaxType<'a>)>, SyntaxType<'a>),
    pub match_func: Option<*const ()>,
}

impl<'a> fmt::Debug for FunctionType<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.signature.fmt(f)
    }
}

impl<'a> PartialEq for FunctionType<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.signature == other.signature
    }
}

impl<'a> Eq for FunctionType<'a> {}

impl<'a> Hash for FunctionType<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.signature.hash(state);
    }
}

impl<'a> FunctionType<'a> {
    pub fn new(signature: (Vec<(&'a str, SyntaxType<'a>)>, SyntaxType<'a>)) -> FunctionType<'a> {
        FunctionType {
            signature,
            match_func: None,
        }
    }

    pub fn set_match_func(&mut self, func: fn(&Vec<SyntaxType<'a>>) -> bool) {
        self.match_func = Some(func as *const ());
    }

    pub fn has_match_func(&self) -> bool {
        self.match_func.is_some()
    }

    pub fn check_match(&self, params: &Vec<SyntaxType<'a>>) -> bool {
        if let Some(match_func) = self.match_func {
            let func = unsafe {
                mem::transmute::<*const (), fn(&Vec<SyntaxType<'a>>) -> bool>(match_func)
            };
            func(params)
        } else {
            true
        }
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
}
