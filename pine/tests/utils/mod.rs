use pine::ast::input::*;
use pine::ast::name::*;
use pine::ast::num::*;
use pine::ast::op::*;
use pine::ast::stat_expr_types::*;

pub fn gen_unop<'a>(op: UnaryOp, exp: Exp<'a>, range: StrRange) -> Exp<'a> {
    Exp::UnaryExp(Box::new(UnaryExp { op, exp, range }))
}

pub fn gen_binop<'a>(op: BinaryOp, exp1: Exp<'a>, exp2: Exp<'a>, range: StrRange) -> Exp<'a> {
    Exp::BinaryExp(Box::new(BinaryExp::new(op, exp1, exp2, range)))
}

pub fn gen_func_call<'a>(
    method: VarName<'a>,
    pos_args: Vec<Exp<'a>>,
    dict_args: Vec<(VarName<'a>, Exp<'a>)>,
    range: StrRange,
) -> Exp<'a> {
    Exp::FuncCall(Box::new(FunctionCall::new_no_ctxid(
        Exp::VarName(RVVarName::new(method)),
        pos_args,
        dict_args,
        range,
    )))
}

pub fn gen_ref_call<'a>(name: VarName<'a>, exp: Exp<'a>, range: StrRange) -> Exp<'a> {
    Exp::RefCall(Box::new(RefCall::new(
        Exp::VarName(RVVarName::new(name)),
        exp,
        range,
    )))
}

pub fn gen_prefix<'a>(var1: VarName<'a>, var2: VarName<'a>, range: StrRange) -> Exp<'a> {
    Exp::PrefixExp(Box::new(PrefixExp::new(
        Exp::VarName(RVVarName::new(var1)),
        var2,
        range,
    )))
}

pub fn gen_prefix_exp<'a>(var1: Exp<'a>, var2: VarName<'a>, range: StrRange) -> Exp<'a> {
    Exp::PrefixExp(Box::new(PrefixExp::new(var1, var2, range)))
}

pub fn gen_int(num: i64, range: StrRange) -> Exp<'static> {
    Exp::Num(Numeral::Int(IntNode { value: num, range }))
}

pub fn gen_name(name: VarName) -> Exp {
    Exp::VarName(RVVarName::new(name))
}

pub fn gen_condition<'a>(cond: Exp<'a>, exp1: Exp<'a>, exp2: Exp<'a>, range: StrRange) -> Exp<'a> {
    Exp::Condition(Box::new(Condition::new(cond, exp1, exp2, range)))
}

pub fn gen_assign<'a>(name: VarName<'a>, val: Exp<'a>, range: StrRange) -> Statement<'a> {
    Statement::Assignment(Box::new(Assignment::new(
        vec![name],
        val,
        false,
        None,
        range,
    )))
}

pub fn gen_assign_exp<'a>(name: VarName<'a>, val: Exp<'a>, range: StrRange) -> Exp<'a> {
    Exp::Assignment(Box::new(Assignment::new(
        vec![name],
        val,
        false,
        None,
        range,
    )))
}

pub fn gen_func_def<'a>(
    name: VarName<'a>,
    params: Vec<VarName<'a>>,
    body: Block<'a>,
    range: StrRange,
) -> Statement<'a> {
    Statement::FuncDef(Box::new(FunctionDef::new(name, params, body, range)))
}

pub fn gen_func_call_stmt<'a>(
    method: VarName<'a>,
    pos_args: Vec<Exp<'a>>,
    dict_args: Vec<(VarName<'a>, Exp<'a>)>,
    range: StrRange,
) -> Statement<'a> {
    Statement::Exp(Exp::FuncCall(Box::new(FunctionCall::new_no_ctxid(
        Exp::VarName(RVVarName::new(method)),
        pos_args,
        dict_args,
        range,
    ))))
}
