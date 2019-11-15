use crate::name::VarName;
use crate::op::{BinaryOp, UnaryOp};

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionCall<'a> {
    pub method: Option<VarName<'a>>,
    pub pos_args: Vec<Exp<'a>>,
    pub dict_args: Vec<(&'a str, Exp<'a>)>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ReferenceCall<'a> {
    pub method: VarName<'a>,
    pub arg: Exp<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Exp<'a> {
    Na,
    Bool(bool),
    Int(i32),
    Float(f64),
    Str(String),
    Color(String),
    Tuple(Vec<Box<Exp<'a>>>),
    VarName(VarName<'a>),
    FuncCall(Box<FunctionCall<'a>>),
    ReferenceCall(Box<ReferenceCall<'a>>),
    Ite(Box<IfThenElse<'a>>),
    ForRange(Box<ForRange<'a>>),
    UnaryOp(UnaryOp, Box<Exp<'a>>),
    BinaryOp(BinaryOp, Box<Exp<'a>>, Box<Exp<'a>>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum OpOrExp2<'a> {
    Op(UnOrBinOp),
    Exp2(Exp2<'a>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum UnOrBinOp {
    UnaryOp(UnaryOp),
    BinaryOp(BinaryOp),
}

#[derive(Clone, Debug, PartialEq)]
pub struct FlatExp<'a>(pub Vec<OpOrExp2<'a>>);

#[derive(Clone, Debug, PartialEq)]
pub enum Exp2<'a> {
    Na,
    Bool(bool),
    Int(i32),
    Float(f64),
    Str(&'a str),
    Color(&'a str),
    VarName(VarName<'a>),
    RetTuple(Box<Vec<VarName<'a>>>),
    Tuple(Box<Vec<Exp<'a>>>),
    FuncCall(Box<FunctionCall<'a>>),
    ReferenceCall(Box<ReferenceCall<'a>>),
    Ite(Box<IfThenElse<'a>>),
    ForRange(Box<ForRange<'a>>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Assignment<'a> {
    pub vars: Vec<VarName<'a>>,
    pub vals: Vec<Exp<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Block<'a> {
    pub stmts: Vec<Statement<'a>>,
    pub ret_stmt: Option<Exp<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct IfThenElse<'a> {
    pub cond: Exp<'a>,
    pub then_blk: Block<'a>,
    pub elseifs: Vec<(Exp<'a>, Block<'a>)>,
    pub else_blk: Option<Block<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ForRange<'a> {
    pub var: VarName<'a>,
    pub start: Exp<'a>,
    pub end: Exp<'a>,
    pub step: Option<Exp<'a>>,
    pub do_blk: Block<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Statement<'a> {
    Break,
    Continue,
    Assignment(VarName<'a>, Box<Exp<'a>>),
    Ite(Box<IfThenElse<'a>>),
    ForRange(Box<ForRange<'a>>),
    FuncCall(Box<FunctionCall<'a>>),
}
