extern crate pine;
// use pine::error::PineError;
use pine::name::*;
use pine::num::*;
use pine::op::*;
use pine::stat_expr_types::*;

#[test]
fn expr_test() {
    let add_expr = Block::new(
        vec![Statement::Assignment(Box::new(Assignment::new(
            VarName("a"),
            Exp::BinaryExp(
                BinaryOp::Plus,
                Box::new(Exp::BinaryExp(
                    BinaryOp::Plus,
                    Box::new(Exp::BinaryExp(
                        BinaryOp::Plus,
                        Box::new(Exp::VarName(VarName("open"))),
                        Box::new(Exp::VarName(VarName("high"))),
                    )),
                    Box::new(Exp::VarName(VarName("low"))),
                )),
                Box::new(Exp::VarName(VarName("close"))),
            ),
            false,
            None,
        )))],
        None,
    );
    assert_eq!(
        pine::parse_all("a = open + high + low + close\n"),
        Ok(add_expr.clone())
    );
    assert_eq!(
        pine::parse_all("a = open + \n high + \n low + \n  close\n"),
        Ok(add_expr.clone())
    );
}

const FUNC_CALL_STAT: &str = "plot(correlation(src, ovr, length),
color=color.purple,
style=plot.style_area,
transp=40)
";

#[test]
fn func_call_test() {
    assert_eq!(
        pine::parse_all(FUNC_CALL_STAT),
        Ok(Block::new(
            vec![Statement::FuncCall(Box::new(FunctionCall {
                method: VarName("plot"),
                pos_args: vec![Exp::FuncCall(Box::new(FunctionCall {
                    method: VarName("correlation"),
                    pos_args: vec![
                        Exp::VarName(VarName("src")),
                        Exp::VarName(VarName("ovr")),
                        Exp::VarName(VarName("length"))
                    ],
                    dict_args: vec![],
                }))],
                dict_args: vec![
                    (
                        VarName("color"),
                        Exp::PrefixExp(Box::new(PrefixExp {
                            var_chain: vec![VarName("color"), VarName("purple")]
                        }))
                    ),
                    (
                        VarName("style"),
                        Exp::PrefixExp(Box::new(PrefixExp {
                            var_chain: vec![VarName("plot"), VarName("style_area")]
                        }))
                    ),
                    (VarName("transp"), Exp::Num(Numeral::Int(40)))
                ],
            }))],
            None
        ))
    );
}

const FUNC_DEF: &str = "
updown(s) =>
    isEqual = s == s[1]
";

#[test]
fn func_def_test() {
    assert_eq!(
        pine::parse_all(FUNC_DEF),
        Ok(Block::new(
            vec![
                Statement::None,
                Statement::FuncDef(Box::new(FunctionDef {
                    name: VarName("updown"),
                    params: vec![VarName("s")],
                    body: Block::new(
                        vec![Statement::Assignment(Box::new(Assignment::new(
                            VarName("isEqual"),
                            Exp::BinaryExp(
                                BinaryOp::Eq,
                                Box::new(Exp::VarName(VarName("s")),),
                                Box::new(Exp::RefCall(Box::new(RefCall {
                                    name: VarName("s"),
                                    arg: Exp::Num(Numeral::Int(1))
                                })))
                            ),
                            false,
                            None
                        )))],
                        None
                    )
                }))
            ],
            None
        ))
    );
}

fn gen_unop<'a>(op: UnaryOp, exp: Exp<'a>) -> Exp<'a> {
    Exp::UnaryExp(op, Box::new(exp))
}

fn gen_binop<'a>(op: BinaryOp, exp1: Exp<'a>, exp2: Exp<'a>) -> Exp<'a> {
    Exp::BinaryExp(op, Box::new(exp1), Box::new(exp2))
}

fn gen_func_call<'a>(
    method: &'a str,
    pos_args: Vec<Exp<'a>>,
    dict_args: Vec<(VarName<'a>, Exp<'a>)>,
) -> Exp<'a> {
    Exp::FuncCall(Box::new(FunctionCall {
        method: VarName(method),
        pos_args: pos_args,
        dict_args: dict_args,
    }))
}

fn gen_ref_call<'a>(name: &'a str, exp: Exp<'a>) -> Exp<'a> {
    Exp::RefCall(Box::new(RefCall {
        name: VarName(name),
        arg: exp,
    }))
}

fn gen_int(num: i32) -> Exp<'static> {
    Exp::Num(Numeral::Int(num))
}

fn gen_name(name: &str) -> Exp {
    Exp::VarName(VarName(name))
}

fn gen_condition<'a>(cond: Exp<'a>, exp1: Exp<'a>, exp2: Exp<'a>) -> Exp<'a> {
    Exp::Condition(Box::new(Condition { cond, exp1, exp2 }))
}

fn gen_assign<'a>(name: &'a str, val: Exp<'a>) -> Statement<'a> {
    Statement::Assignment(Box::new(Assignment::new(VarName(name), val, false, None)))
}

fn gen_func_def<'a>(name: &'a str, params: Vec<&'a str>, body: Block<'a>) -> Statement<'a> {
    Statement::FuncDef(Box::new(FunctionDef {
        name: VarName(name),
        params: params.into_iter().map(|s| VarName(s)).collect(),
        body: body,
    }))
}

const FUNC_DEF2: &str = "\
updown(s) =>
    isEqual = s == s[1]
    isGrowing = s > s[1]
    ud = isEqual ?
           0 :
           isGrowing ?
               (nz(ud[1]) <= 0 ?
                     1 :
                   nz(ud[1])+1) :
               (nz(ud[1]) >= 0 ?
                   -1 :
                   nz(ud[1])-1)
";

#[test]
fn cond_expr_test() {
    let gen_nz_call = || gen_func_call("nz", vec![gen_ref_call("ud", gen_int(1))], vec![]);

    assert_eq!(
        pine::parse_all("m = nz(ud[1])\n"),
        Ok(Block::new(vec![gen_assign("m", gen_nz_call())], None))
    );

    assert_eq!(
        pine::parse_all("m = (nz(ud[1]) >= 0)\n"),
        Ok(Block::new(
            vec![gen_assign(
                "m",
                gen_binop(BinaryOp::Geq, gen_nz_call(), gen_int(0))
            )],
            None
        ))
    );

    assert_eq!(
        pine::parse_all("m = (1 >= 0 ? 1 : 2)\n"),
        Ok(Block::new(
            vec![gen_assign(
                "m",
                gen_condition(
                    gen_binop(BinaryOp::Geq, gen_int(1), gen_int(0)),
                    gen_int(1),
                    gen_int(2)
                )
            )],
            None
        ))
    );
    assert_eq!(
        pine::parse_all(FUNC_DEF2),
        Ok(Block::new(
            vec![gen_func_def(
                "updown",
                vec!["s"],
                Block::new(
                    vec![
                        gen_assign(
                            "isEqual",
                            gen_binop(BinaryOp::Eq, gen_name("s"), gen_ref_call("s", gen_int(1)))
                        ),
                        gen_assign(
                            "isGrowing",
                            gen_binop(BinaryOp::Gt, gen_name("s"), gen_ref_call("s", gen_int(1)))
                        ),
                        gen_assign(
                            "ud",
                            gen_condition(
                                gen_name("isEqual"),
                                gen_int(0),
                                gen_condition(
                                    gen_name("isGrowing"),
                                    gen_condition(
                                        gen_binop(BinaryOp::Leq, gen_nz_call(), gen_int(0)),
                                        gen_int(1),
                                        gen_binop(BinaryOp::Plus, gen_nz_call(), gen_int(1))
                                    ),
                                    gen_condition(
                                        gen_binop(BinaryOp::Geq, gen_nz_call(), gen_int(0)),
                                        gen_unop(UnaryOp::Minus, gen_int(1)),
                                        gen_binop(BinaryOp::Minus, gen_nz_call(), gen_int(1))
                                    )
                                )
                            )
                        ),
                    ],
                    None
                )
            )],
            None
        ))
    );
}
