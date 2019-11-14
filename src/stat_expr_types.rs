use crate::name::VarName;
use crate::op::{BinaryOp, ThirdOp, UnaryOp};

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionCall<'a> {
    pub method: Option<VarName<'a>>,
    pub pos_args: Vec<Exp<'a>>,
    pub dict_args: Vec<(&'a str, Exp<'a>)>,
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
    FuncCall(FunctionCall<'a>),
    UnaryOp(UnaryOp, Box<Exp<'a>>),
    BinaryOp(BinaryOp, Box<Exp<'a>>, Box<Exp<'a>>),
    ThirdOp(ThirdOp, Box<Exp<'a>>, Box<Exp<'a>>, Box<Exp<'a>>),
    Ite(IfThenElse<'a>),
    ForRange(ForRange<'a>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Assignment<'a> {
    pub vars: Vec<VarName()>,
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
    FuncCall(FunctionCall<'a>),
    Ite(IfThenElse<'a>),
    ForRange(ForRange<'a>),
    FuncDef(FunctionDef<'a>),
}
