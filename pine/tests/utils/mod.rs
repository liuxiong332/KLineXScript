use pine::ast::name::*;
use pine::ast::num::*;
use pine::ast::op::*;
use pine::ast::stat_expr_types::*;

pub fn gen_unop<'a>(op: UnaryOp, exp: Exp<'a>) -> Exp<'a> {
    Exp::UnaryExp(op, Box::new(exp))
}

pub fn gen_binop<'a>(op: BinaryOp, exp1: Exp<'a>, exp2: Exp<'a>) -> Exp<'a> {
    Exp::BinaryExp(op, Box::new(exp1), Box::new(exp2))
}

pub fn gen_func_call<'a>(
    method: &'a str,
    pos_args: Vec<Exp<'a>>,
    dict_args: Vec<(VarName<'a>, Exp<'a>)>,
) -> Exp<'a> {
    Exp::FuncCall(Box::new(FunctionCall::new_no_ctxid(
        Exp::VarName(VarName(method)),
        pos_args,
        dict_args,
    )))
}

pub fn gen_ref_call<'a>(name: &'a str, exp: Exp<'a>) -> Exp<'a> {
    Exp::RefCall(Box::new(RefCall {
        name: Exp::VarName(VarName(name)),
        arg: exp,
    }))
}

pub fn gen_prefix(vars: Vec<&str>) -> Exp {
    Exp::PrefixExp(Box::new(PrefixExp {
        var_chain: vars.into_iter().map(|s| VarName(s)).collect(),
    }))
}

pub fn gen_int(num: i32) -> Exp<'static> {
    Exp::Num(Numeral::from_i32(num))
}

pub fn gen_name(name: &str) -> Exp {
    Exp::VarName(VarName(name))
}

pub fn gen_condition<'a>(cond: Exp<'a>, exp1: Exp<'a>, exp2: Exp<'a>) -> Exp<'a> {
    Exp::Condition(Box::new(Condition { cond, exp1, exp2 }))
}

pub fn gen_assign<'a>(name: &'a str, val: Exp<'a>) -> Statement<'a> {
    Statement::Assignment(Box::new(Assignment::new(
        vec![VarName(name)],
        val,
        false,
        None,
    )))
}

pub fn gen_func_def<'a>(name: &'a str, params: Vec<&'a str>, body: Block<'a>) -> Statement<'a> {
    Statement::FuncDef(Box::new(FunctionDef {
        name: VarName(name),
        params: params.into_iter().map(|s| VarName(s)).collect(),
        body: body,
    }))
}

pub fn gen_func_call_stmt<'a>(
    method: &'a str,
    pos_args: Vec<Exp<'a>>,
    dict_args: Vec<(VarName<'a>, Exp<'a>)>,
) -> Statement<'a> {
    Statement::FuncCall(Box::new(FunctionCall::new_no_ctxid(
        Exp::VarName(VarName(method)),
        pos_args,
        dict_args,
    )))
}
