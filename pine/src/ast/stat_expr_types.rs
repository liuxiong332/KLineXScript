use super::color::ColorNode;
use super::input::{Input, Position};
use super::name::VarName;
use super::num::Numeral;
use super::op::{BinaryOp, UnaryOp};

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionCall<'a> {
    pub method: Exp<'a>,
    pub pos_args: Vec<Exp<'a>>,
    pub dict_args: Vec<(VarName<'a>, Exp<'a>)>,
    pub ctxid: i32,
    pub input: Input<'a>,
}

impl<'a> FunctionCall<'a> {
    #[inline]
    pub fn new(
        method: Exp<'a>,
        pos_args: Vec<Exp<'a>>,
        dict_args: Vec<(VarName<'a>, Exp<'a>)>,
        ctxid: i32,
        input: Input<'a>,
    ) -> Self {
        FunctionCall {
            method,
            pos_args,
            dict_args,
            ctxid,
            input,
        }
    }

    pub fn new_no_ctxid(
        method: Exp<'a>,
        pos_args: Vec<Exp<'a>>,
        dict_args: Vec<(VarName<'a>, Exp<'a>)>,
    ) -> Self {
        FunctionCall {
            method,
            pos_args,
            dict_args,
            ctxid: 0,
            input: Input::new("", Position::new(0, 0), Position::max()),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct RefCall<'a> {
    pub name: Exp<'a>,
    pub arg: Exp<'a>,
    pub input: Input<'a>,
}

impl<'a> RefCall<'a> {
    #[inline]
    pub fn new(name: Exp<'a>, arg: Exp<'a>, input: Input<'a>) -> RefCall<'a> {
        RefCall { name, arg, input }
    }

    pub fn new_no_input(name: Exp<'a>, arg: Exp<'a>) -> RefCall<'a> {
        RefCall {
            name,
            arg,
            input: Input::new("", Position::new(0, 0), Position::max()),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Condition<'a> {
    pub cond: Exp<'a>,
    pub exp1: Exp<'a>,
    pub exp2: Exp<'a>,
    pub input: Input<'a>,
}

impl<'a> Condition<'a> {
    #[inline]
    pub fn new(cond: Exp<'a>, exp1: Exp<'a>, exp2: Exp<'a>, input: Input<'a>) -> Condition<'a> {
        Condition {
            cond,
            exp1,
            exp2,
            input,
        }
    }

    #[inline]
    pub fn new_no_input(cond: Exp<'a>, exp1: Exp<'a>, exp2: Exp<'a>) -> Condition<'a> {
        Condition {
            cond,
            exp1,
            exp2,
            input: Input::new_empty(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Exp<'a> {
    Na,
    Bool(bool),
    Num(Numeral<'a>),
    Str(String),
    Color(ColorNode<'a>),
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
    Num(Numeral<'a>),
    Str(String),
    Color(ColorNode<'a>),
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
    pub input: Input<'a>,
}

impl<'a> TypeCast<'a> {
    pub fn new(data_type: DataType, exp: Exp<'a>, input: Input<'a>) -> TypeCast<'a> {
        TypeCast {
            data_type,
            exp,
            input,
        }
    }

    pub fn new_no_input(data_type: DataType, exp: Exp<'a>) -> TypeCast<'a> {
        TypeCast {
            data_type,
            exp,
            input: Input::new_empty(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct PrefixExp<'a> {
    pub var_chain: Vec<VarName<'a>>,
    pub input: Input<'a>,
}

impl<'a> PrefixExp<'a> {
    pub fn new(var_chain: Vec<VarName<'a>>, input: Input<'a>) -> PrefixExp<'a> {
        PrefixExp { var_chain, input }
    }

    pub fn new_no_input(var_chain: Vec<VarName<'a>>) -> PrefixExp<'a> {
        PrefixExp {
            var_chain,
            input: Input::new_empty(),
        }
    }
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
    pub names: Vec<VarName<'a>>,
    pub val: Exp<'a>,
    pub var_type: Option<DataType>,
    pub var: bool,
    pub input: Input<'a>,
}

impl<'a> Assignment<'a> {
    pub fn new(
        names: Vec<VarName<'a>>,
        val: Exp<'a>,
        var: bool,
        var_type: Option<DataType>,
        input: Input<'a>,
    ) -> Assignment<'a> {
        Assignment {
            names,
            val,
            var,
            var_type,
            input,
        }
    }

    pub fn new_no_input(
        names: Vec<VarName<'a>>,
        val: Exp<'a>,
        var: bool,
        var_type: Option<DataType>,
    ) -> Assignment<'a> {
        Assignment {
            names,
            val,
            var,
            var_type,
            input: Input::new_empty(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarAssignment<'a> {
    pub name: VarName<'a>,
    pub val: Exp<'a>,
    pub input: Input<'a>,
}

impl<'a> VarAssignment<'a> {
    pub fn new(name: VarName<'a>, val: Exp<'a>, input: Input<'a>) -> VarAssignment<'a> {
        VarAssignment { name, val, input }
    }

    pub fn new_no_input(name: VarName<'a>, val: Exp<'a>) -> VarAssignment<'a> {
        VarAssignment {
            name,
            val,
            input: Input::new_empty(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Block<'a> {
    pub stmts: Vec<Statement<'a>>,
    pub ret_stmt: Option<Exp<'a>>,
    pub input: Input<'a>,
}

impl<'a> Block<'a> {
    pub fn new(
        stmts: Vec<Statement<'a>>,
        ret_stmt: Option<Exp<'a>>,
        input: Input<'a>,
    ) -> Block<'a> {
        Block {
            stmts,
            ret_stmt,
            input,
        }
    }

    pub fn new_no_input(stmts: Vec<Statement<'a>>, ret_stmt: Option<Exp<'a>>) -> Block<'a> {
        Block {
            stmts,
            ret_stmt,
            input: Input::new_empty(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct IfThenElse<'a> {
    pub cond: Exp<'a>,
    pub then_blk: Block<'a>,
    pub then_ctxid: i32,
    // pub elseifs: Vec<(Exp<'a>, Block<'a>)>,
    pub else_blk: Option<Block<'a>>,
    pub else_ctxid: i32,
    pub input: Input<'a>,
}

impl<'a> IfThenElse<'a> {
    pub fn new(
        cond: Exp<'a>,
        then_blk: Block<'a>,
        else_blk: Option<Block<'a>>,
        then_ctxid: i32,
        else_ctxid: i32,
        input: Input<'a>,
    ) -> Self {
        IfThenElse {
            cond,
            then_blk,
            then_ctxid,
            else_blk,
            else_ctxid,
            input,
        }
    }

    pub fn new_no_ctxid(cond: Exp<'a>, then_blk: Block<'a>, else_blk: Option<Block<'a>>) -> Self {
        IfThenElse {
            cond,
            then_blk,
            else_blk,
            then_ctxid: 0,
            else_ctxid: 1,
            input: Input::new_empty(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ForRange<'a> {
    pub var: VarName<'a>,
    pub start: Exp<'a>,
    pub end: Exp<'a>,
    pub step: Option<Exp<'a>>,
    pub do_blk: Block<'a>,
    pub ctxid: i32,
    pub input: Input<'a>,
}

impl<'a> ForRange<'a> {
    pub fn new(
        var: VarName<'a>,
        start: Exp<'a>,
        end: Exp<'a>,
        step: Option<Exp<'a>>,
        do_blk: Block<'a>,
        ctxid: i32,
        input: Input<'a>,
    ) -> Self {
        ForRange {
            var,
            start,
            end,
            step,
            do_blk,
            ctxid,
            input,
        }
    }

    pub fn new_no_ctxid(
        var: VarName<'a>,
        start: Exp<'a>,
        end: Exp<'a>,
        step: Option<Exp<'a>>,
        do_blk: Block<'a>,
    ) -> Self {
        ForRange {
            var,
            start,
            end,
            step,
            do_blk,
            ctxid: 0,
            input: Input::new_empty(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionDef<'a> {
    pub name: VarName<'a>,
    pub params: Vec<VarName<'a>>,
    pub body: Block<'a>,
    pub input: Input<'a>,
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
