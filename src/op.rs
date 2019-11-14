#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum BinaryOp {
    Plus,
    Minus,
    Mul,
    Div,
    Mod,
    Lt,
    Leq,
    Gt,
    Geq,
    Eq,
    Neq,
    BoolAnd,
    BoolOr,
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum UnaryOp {
    Reference,
    Minus,
    BoolNot,
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum ThirdOp {
    Conditional,
}
