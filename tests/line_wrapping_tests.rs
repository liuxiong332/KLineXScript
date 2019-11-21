extern crate pine;
// use pine::error::PineError;
use pine::ast::name::*;
use pine::ast::num::*;
use pine::ast::op::*;
use pine::ast::stat_expr_types::*;

mod utils;
pub use utils::*;

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
            vec![gen_func_call_stmt(
                "plot",
                vec![gen_func_call(
                    "correlation",
                    vec![gen_name("src"), gen_name("ovr"), gen_name("length")],
                    vec![]
                )],
                vec![
                    (VarName("color"), gen_prefix(vec!["color", "purple"]),),
                    (VarName("style"), gen_prefix(vec!["plot", "style_area"]),),
                    (VarName("transp"), gen_int(40))
                ]
            ),],
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
                                    name: Exp::VarName(VarName("s")),
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

const EXPR_COMMENT: &str = "c = open > close ? color.red :
high > high[1] ? color.lime : // a comment
low < low[1] ? color.blue : color.black
bgcolor(c)
";

#[test]
fn expr_comment_test() {
    assert_eq!(
        pine::parse_all(EXPR_COMMENT),
        Ok(Block::new(
            vec![
                gen_assign(
                    "c",
                    gen_condition(
                        gen_binop(BinaryOp::Gt, gen_name("open"), gen_name("close")),
                        gen_prefix(vec!["color", "red"]),
                        gen_condition(
                            gen_binop(
                                BinaryOp::Gt,
                                gen_name("high"),
                                gen_ref_call("high", gen_int(1))
                            ),
                            gen_prefix(vec!["color", "lime"]),
                            gen_condition(
                                gen_binop(
                                    BinaryOp::Lt,
                                    gen_name("low"),
                                    gen_ref_call("low", gen_int(1))
                                ),
                                gen_prefix(vec!["color", "blue"]),
                                gen_prefix(vec!["color", "black"])
                            )
                        )
                    )
                ),
                gen_func_call_stmt("bgcolor", vec![gen_name("c")], vec![])
            ],
            None
        ))
    );
}
