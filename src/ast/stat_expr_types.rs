use super::name::VarName;
use super::num::Numeral;
use super::op::{BinaryOp, UnaryOp};

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionCall<'a> {
    pub method: Exp<'a>,
    pub pos_args: Vec<Exp<'a>>,
    pub dict_args: Vec<(VarName<'a>, Exp<'a>)>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct RefCall<'a> {
    pub name: Exp<'a>,
    pub arg: Exp<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Condition<'a> {
    pub cond: Exp<'a>,
    pub exp1: Exp<'a>,
    pub exp2: Exp<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Exp<'a> {
    Na,
    Bool(bool),
    Num(Numeral),
    Str(String),
    Color(&'a str),
    VarName(VarName<'a>),
    // RetTuple(Box<Vec<VarName<'a>>>),
    Tuple(Box<Vec<Exp<'a>>>),
    TypeCast(Box<TypeCast<'a>>),
    FuncCall(Box<FunctionCall<'a>>),
    RefCall(Box<RefCall<'a>>),
    PrefixExp(Box<PrefixExp<'a>>),
    Condition(Box<Condition<'a>>),
    Ite(Box<IfThenElse<'a>>),
    ForRange(Box<ForRange<'a>>),
    UnaryExp(UnaryOp, Box<Exp<'a>>),
    BinaryExp(BinaryOp, Box<Exp<'a>>, Box<Exp<'a>>),
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
    Num(Numeral),
    Str(String),
    Color(&'a str),
    VarName(VarName<'a>),
    // RetTuple(Box<Vec<VarName<'a>>>),
    Tuple(Box<Vec<Exp<'a>>>),
    TypeCast(Box<TypeCast<'a>>),
    FuncCall(Box<FunctionCall<'a>>),
    RefCall(Box<RefCall<'a>>),
    PrefixExp(Box<PrefixExp<'a>>),
    Exp(Exp<'a>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeCast<'a> {
    pub data_type: DataType,
    pub exp: Exp<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PrefixExp<'a> {
    pub var_chain: Vec<VarName<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum DataType {
    Float,
    Int,
    Bool,
    Color,
    String,
    Line,
    Label,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Assignment<'a> {
    pub name: VarName<'a>,
    pub val: Exp<'a>,
    pub var_type: Option<DataType>,
    pub var: bool,
}

impl<'a> Assignment<'a> {
    pub fn new(
        name: VarName<'a>,
        val: Exp<'a>,
        var: bool,
        var_type: Option<DataType>,
    ) -> Assignment<'a> {
        Assignment {
            name,
            val,
            var,
            var_type,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarAssignment<'a> {
    pub name: VarName<'a>,
    pub val: Exp<'a>,
}

impl<'a> VarAssignment<'a> {
    pub fn new(name: VarName<'a>, val: Exp<'a>) -> VarAssignment<'a> {
        VarAssignment { name, val }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Block<'a> {
    pub stmts: Vec<Statement<'a>>,
    pub ret_stmt: Option<Exp<'a>>,
}

impl<'a> Block<'a> {
    pub fn new(stmts: Vec<Statement<'a>>, ret_stmt: Option<Exp<'a>>) -> Block<'a> {
        Block { stmts, ret_stmt }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct IfThenElse<'a> {
    pub cond: Exp<'a>,
    pub then_blk: Block<'a>,
    // pub elseifs: Vec<(Exp<'a>, Block<'a>)>,
    pub else_blk: Option<Block<'a>>,
}

impl<'a> IfThenElse<'a> {
    pub fn new(cond: Exp<'a>, then_blk: Block<'a>, else_blk: Option<Block<'a>>) -> Self {
        IfThenElse {
            cond,
            then_blk,
            else_blk,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ForRange<'a> {
    pub var: VarName<'a>,
    pub start: i32,
    pub end: i32,
    pub step: Option<i32>,
    pub do_blk: Block<'a>,
}

impl<'a> ForRange<'a> {
    pub fn new(
        var: VarName<'a>,
        start: i32,
        end: i32,
        step: Option<i32>,
        do_blk: Block<'a>,
    ) -> Self {
        ForRange {
            var,
            start,
            end,
            step,
            do_blk,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionDef<'a> {
    pub name: VarName<'a>,
    pub params: Vec<VarName<'a>>,
    pub body: Block<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Statement<'a> {
    Break,
    Continue,
    None,
    Assignment(Box<Assignment<'a>>),
    VarAssignment(Box<VarAssignment<'a>>),
    Ite(Box<IfThenElse<'a>>),
    ForRange(Box<ForRange<'a>>),
    FuncCall(Box<FunctionCall<'a>>),
    FuncDef(Box<FunctionDef<'a>>),
}
