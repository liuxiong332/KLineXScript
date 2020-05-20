extern crate pine;
// use pine::error::PineError;
use pine::ast::input::*;
use pine::ast::name::*;
use pine::ast::num::*;
use pine::ast::op::*;
use pine::ast::stat_expr_types::*;

mod utils;
pub use utils::*;

#[test]
fn expr_test() {
    let add_expr = |range, open, high, low, close| {
        Block::new(
            vec![Statement::Assignment(Box::new(Assignment::new(
                vec![VarName::new_with_start("a", Position::new(0, 0))],
                Exp::BinaryExp(Box::new(BinaryExp::new(
                    BinaryOp::Plus,
                    Exp::BinaryExp(Box::new(BinaryExp::new(
                        BinaryOp::Plus,
                        Exp::BinaryExp(Box::new(BinaryExp::new(
                            BinaryOp::Plus,
                            Exp::VarName(RVVarName::new(open)),
                            Exp::VarName(RVVarName::new(high)),
                            StrRange::new(open.range.start, high.range.end),
                        ))),
                        Exp::VarName(RVVarName::new(low)),
                        StrRange::new(open.range.start, low.range.end),
                    ))),
                    Exp::VarName(RVVarName::new(close)),
                    StrRange::new(open.range.start, close.range.end),
                ))),
                false,
                None,
                range,
            )))],
            None,
            range,
        )
    };
    assert_eq!(
        pine::parse_ast("a = open + high + low + close\n"),
        Ok(add_expr(
            StrRange::new(Position::new(0, 0), Position::new(0, 29)),
            VarName::new_with_start("open", Position::new(0, 4)),
            VarName::new_with_start("high", Position::new(0, 11)),
            VarName::new_with_start("low", Position::new(0, 18)),
            VarName::new_with_start("close", Position::new(0, 24)),
        ))
    );
    assert_eq!(
        pine::parse_ast("a = open + \n high + \n low + \n  close\n"),
        Ok(add_expr(
            StrRange::new(Position::new(0, 0), Position::new(3, 7)),
            VarName::new_with_start("open", Position::new(0, 4)),
            VarName::new_with_start("high", Position::new(1, 1)),
            VarName::new_with_start("low", Position::new(2, 1)),
            VarName::new_with_start("close", Position::new(3, 2)),
        ))
    );
}

const FUNC_CALL_STAT: &str = "plot(correlation(src, ovr, length),
color=color.purple,
style=plot.style_area,
opacity=40)
";

#[test]
fn func_call_test() {
    assert_eq!(
        pine::parse_ast(FUNC_CALL_STAT),
        Ok(Block::new(
            vec![gen_func_call_stmt(
                VarName::new_with_start("plot", Position::new(0, 0)),
                vec![gen_func_call(
                    VarName::new_with_start("correlation", Position::new(0, 5)),
                    vec![
                        gen_name(VarName::new_with_start("src", Position::new(0, 17))),
                        gen_name(VarName::new_with_start("ovr", Position::new(0, 22))),
                        gen_name(VarName::new_with_start("length", Position::new(0, 27)))
                    ],
                    vec![],
                    StrRange::new(Position::new(0, 5), Position::new(0, 34))
                )],
                vec![
                    (
                        VarName::new_with_start("color", Position::new(1, 0)),
                        gen_prefix(
                            VarName::new_with_start("color", Position::new(1, 6)),
                            VarName::new_with_start("purple", Position::new(1, 12)),
                            StrRange::from_start("color.purple", Position::new(1, 6))
                        ),
                    ),
                    (
                        VarName::new_with_start("style", Position::new(2, 0)),
                        gen_prefix(
                            VarName::new_with_start("plot", Position::new(2, 6)),
                            VarName::new_with_start("style_area", Position::new(2, 11)),
                            StrRange::from_start("plot.style_area", Position::new(2, 6))
                        ),
                    ),
                    (
                        VarName::new_with_start("opacity", Position::new(3, 0)),
                        gen_int(40, StrRange::from_start("40", Position::new(3, 8)))
                    )
                ],
                StrRange::new(Position::new(0, 0), Position::new(3, 11))
            ),],
            None,
            StrRange::new(Position::new(0, 0), Position::new(3, 11))
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
        pine::parse_ast(FUNC_DEF),
        Ok(Block::new(
            vec![
                // Statement::None(StrRange::from_start("\n", Position::new(0, 0))),
                Statement::FuncDef(Box::new(FunctionDef::new(
                    VarName::new_with_start("updown", Position::new(1, 0)),
                    vec![VarName::new_with_start("s", Position::new(1, 7))],
                    Block::new(
                        vec![],
                        Some(Exp::Assignment(Box::new(Assignment::new(
                            vec![VarName::new_with_start("isEqual", Position::new(2, 4))],
                            Exp::BinaryExp(Box::new(BinaryExp::new(
                                BinaryOp::Eq,
                                Exp::VarName(RVVarName::new_with_start("s", Position::new(2, 14))),
                                Exp::RefCall(Box::new(RefCall {
                                    name: Exp::VarName(RVVarName::new_with_start(
                                        "s",
                                        Position::new(2, 19)
                                    )),
                                    arg: Exp::Num(Numeral::Int(IntNode::new(
                                        1,
                                        StrRange::from_start("1", Position::new(2, 21))
                                    ))),
                                    range: StrRange::from_start("s[1]", Position::new(2, 19))
                                })),
                                StrRange::from_start("s == s[1]", Position::new(2, 14))
                            ))),
                            false,
                            None,
                            StrRange::new(Position::new(2, 4), Position::new(2, 23))
                        )))),
                        StrRange::new(Position::new(2, 4), Position::new(2, 23))
                    ),
                    StrRange::new(Position::new(1, 0), Position::new(2, 23))
                )))
            ],
            None,
            StrRange::new(Position::new(1, 0), Position::new(2, 23))
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
    let gen_nz_call = |pos| {
        gen_func_call(
            VarName::new_with_start("nz", pos),
            vec![gen_ref_call(
                VarName::new_with_start("ud", pos.translate(0, 3)),
                gen_int(1, StrRange::from_start("1", pos.translate(0, 6))),
                StrRange::from_start("ud[1]", pos.translate(0, 3)),
            )],
            vec![],
            StrRange::from_start("nz(ud[1])", pos),
        )
    };

    assert_eq!(
        pine::parse_ast("m = nz(ud[1])\n"),
        Ok(Block::new(
            vec![gen_assign(
                VarName::new_with_start("m", Position::new(0, 0)),
                gen_nz_call(Position::new(0, 4)),
                StrRange::from_start("m = nz(ud[1])", Position::new(0, 0))
            )],
            None,
            StrRange::from_start("m = nz(ud[1])", Position::new(0, 0))
        ))
    );

    assert_eq!(
        pine::parse_ast("m = (nz(ud[1]) >= 0)\n"),
        Ok(Block::new(
            vec![gen_assign(
                VarName::new_with_start("m", Position::new(0, 0)),
                gen_binop(
                    BinaryOp::Geq,
                    gen_nz_call(Position::new(0, 5)),
                    gen_int(0, StrRange::from_start("0", Position::new(0, 18))),
                    StrRange::from_start("nz(ud[1]) >= 0", Position::new(0, 5))
                ),
                StrRange::from_start("m = (nz(ud[1]) >= 0", Position::new(0, 0))
            )],
            None,
            StrRange::from_start("m = (nz(ud[1]) >= 0", Position::new(0, 0))
        ))
    );

    assert_eq!(
        pine::parse_ast("m = (1 >= 0 ? 1 : 2)\n"),
        Ok(Block::new(
            vec![gen_assign(
                VarName::new_with_start("m", Position::new(0, 0)),
                gen_condition(
                    gen_binop(
                        BinaryOp::Geq,
                        gen_int(1, StrRange::from_start("1", Position::new(0, 5))),
                        gen_int(0, StrRange::from_start("0", Position::new(0, 10))),
                        StrRange::from_start("1 >= 0", Position::new(0, 5))
                    ),
                    gen_int(1, StrRange::from_start("1", Position::new(0, 14))),
                    gen_int(2, StrRange::from_start("2", Position::new(0, 18))),
                    StrRange::from_start("1 >= 0 ? 1 : 2", Position::new(0, 5))
                ),
                StrRange::from_start("m = (1 >= 0 ? 1 : 2", Position::new(0, 0))
            )],
            None,
            StrRange::from_start("m = (1 >= 0 ? 1 : 2", Position::new(0, 0))
        ))
    );

    assert_eq!(
        pine::parse_ast(FUNC_DEF2),
        Ok(Block::new(
            vec![gen_func_def(
                VarName::new_with_start("updown", Position::new(0, 0)),
                vec![VarName::new_with_start("s", Position::new(0, 7))],
                Block::new(
                    vec![
                        gen_assign(
                            VarName::new_with_start("isEqual", Position::new(1, 4)),
                            gen_binop(
                                BinaryOp::Eq,
                                gen_name(VarName::new_with_start("s", Position::new(1, 14))),
                                gen_ref_call(
                                    VarName::new_with_start("s", Position::new(1, 19)),
                                    gen_int(1, StrRange::from_start("1", Position::new(1, 21))),
                                    StrRange::from_start("s[1]", Position::new(1, 19))
                                ),
                                StrRange::from_start("s == s[1]", Position::new(1, 14))
                            ),
                            StrRange::from_start("isEqual = s == s[1]", Position::new(1, 4))
                        ),
                        gen_assign(
                            VarName::new_with_start("isGrowing", Position::new(2, 4)),
                            gen_binop(
                                BinaryOp::Gt,
                                gen_name(VarName::new_with_start("s", Position::new(2, 16))),
                                gen_ref_call(
                                    VarName::new_with_start("s", Position::new(2, 20)),
                                    gen_int(1, StrRange::from_start("1", Position::new(2, 22))),
                                    StrRange::from_start("s[1]", Position::new(2, 20))
                                ),
                                StrRange::from_start("s > s[1]", Position::new(2, 16))
                            ),
                            StrRange::from_start("isGrowing = s > s[1]", Position::new(2, 4))
                        ),
                    ],
                    Some(gen_assign_exp(
                        VarName::new_with_start("ud", Position::new(3, 4)),
                        gen_condition(
                            gen_name(VarName::new_with_start("isEqual", Position::new(3, 9))),
                            gen_int(0, StrRange::from_start("0", Position::new(4, 11))),
                            gen_condition(
                                gen_name(VarName::new_with_start(
                                    "isGrowing",
                                    Position::new(5, 11)
                                )),
                                gen_condition(
                                    gen_binop(
                                        BinaryOp::Leq,
                                        gen_nz_call(Position::new(6, 16)),
                                        gen_int(0, StrRange::from_start("0", Position::new(6, 29))),
                                        StrRange::from_start(
                                            "nz(ud[1]) <= 0",
                                            Position::new(6, 16)
                                        )
                                    ),
                                    gen_int(1, StrRange::from_start("1", Position::new(7, 21))),
                                    gen_binop(
                                        BinaryOp::Plus,
                                        gen_nz_call(Position::new(8, 19)),
                                        gen_int(1, StrRange::from_start("1", Position::new(8, 29))),
                                        StrRange::from_start("nz(ud[1])+1", Position::new(8, 19))
                                    ),
                                    StrRange::new(Position::new(6, 16), Position::new(8, 30))
                                ),
                                gen_condition(
                                    gen_binop(
                                        BinaryOp::Geq,
                                        gen_nz_call(Position::new(9, 16)),
                                        gen_int(0, StrRange::from_start("0", Position::new(9, 29))),
                                        StrRange::from_start(
                                            "nz(ud[1]) >= 0",
                                            Position::new(9, 16)
                                        )
                                    ),
                                    gen_unop(
                                        UnaryOp::Minus,
                                        gen_int(
                                            1,
                                            StrRange::from_start("1", Position::new(10, 20))
                                        ),
                                        StrRange::from_start("-1", Position::new(10, 19))
                                    ),
                                    gen_binop(
                                        BinaryOp::Minus,
                                        gen_nz_call(Position::new(11, 19)),
                                        gen_int(
                                            1,
                                            StrRange::from_start("1", Position::new(11, 29))
                                        ),
                                        StrRange::from_start("nz(ud[1])-1", Position::new(11, 19))
                                    ),
                                    StrRange::new(Position::new(9, 16), Position::new(11, 30))
                                ),
                                StrRange::new(Position::new(5, 11), Position::new(11, 30))
                            ),
                            StrRange::new(Position::new(3, 9), Position::new(11, 30))
                        ),
                        StrRange::new(Position::new(3, 4), Position::new(11, 30))
                    )),
                    StrRange::new(Position::new(1, 4), Position::new(11, 30))
                ),
                StrRange::new(Position::new(0, 0), Position::new(11, 30))
            )],
            None,
            StrRange::new(Position::new(0, 0), Position::new(11, 30))
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
        pine::parse_ast(EXPR_COMMENT),
        Ok(Block::new(
            vec![
                gen_assign(
                    VarName::new_with_start("c", Position::new(0, 0)),
                    gen_condition(
                        gen_binop(
                            BinaryOp::Gt,
                            gen_name(VarName::new_with_start("open", Position::new(0, 4))),
                            gen_name(VarName::new_with_start("close", Position::new(0, 11))),
                            StrRange::from_start("open > close", Position::new(0, 4))
                        ),
                        gen_prefix(
                            VarName::new_with_start("color", Position::new(0, 19)),
                            VarName::new_with_start("red", Position::new(0, 25)),
                            StrRange::from_start("color.red", Position::new(0, 19))
                        ),
                        gen_condition(
                            gen_binop(
                                BinaryOp::Gt,
                                gen_name(VarName::new_with_start("high", Position::new(1, 0))),
                                gen_ref_call(
                                    VarName::new_with_start("high", Position::new(1, 7)),
                                    gen_int(1, StrRange::from_start("1", Position::new(1, 12))),
                                    StrRange::from_start("high[1]", Position::new(1, 7))
                                ),
                                StrRange::from_start("high > high[1]", Position::new(1, 0))
                            ),
                            gen_prefix(
                                VarName::new_with_start("color", Position::new(1, 17)),
                                VarName::new_with_start("lime", Position::new(1, 23)),
                                StrRange::from_start("color.lime", Position::new(1, 17))
                            ),
                            gen_condition(
                                gen_binop(
                                    BinaryOp::Lt,
                                    gen_name(VarName::new_with_start("low", Position::new(2, 0))),
                                    gen_ref_call(
                                        VarName::new_with_start("low", Position::new(2, 6)),
                                        gen_int(1, StrRange::from_start("1", Position::new(2, 10))),
                                        StrRange::from_start("low[1]", Position::new(2, 6))
                                    ),
                                    StrRange::from_start("low < low[1]", Position::new(2, 0))
                                ),
                                gen_prefix(
                                    VarName::new_with_start("color", Position::new(2, 15)),
                                    VarName::new_with_start("blue", Position::new(2, 21)),
                                    StrRange::from_start("color.blue", Position::new(2, 15))
                                ),
                                gen_prefix(
                                    VarName::new_with_start("color", Position::new(2, 28)),
                                    VarName::new_with_start("black", Position::new(2, 34)),
                                    StrRange::from_start("color.black", Position::new(2, 28))
                                ),
                                StrRange::from_start(
                                    "low < low[1] ? color.blue : color.black",
                                    Position::new(2, 0)
                                )
                            ),
                            StrRange::new(Position::new(1, 0), Position::new(2, 39))
                        ),
                        StrRange::new(Position::new(0, 4), Position::new(2, 39))
                    ),
                    StrRange::new(Position::new(0, 0), Position::new(2, 39))
                ),
                gen_func_call_stmt(
                    VarName::new_with_start("bgcolor", Position::new(3, 0)),
                    vec![gen_name(VarName::new_with_start("c", Position::new(3, 8)))],
                    vec![],
                    StrRange::from_start("bgcolor(c)", Position::new(3, 0))
                )
            ],
            None,
            StrRange::new(Position::new(0, 0), Position::new(3, 10))
        ))
    );
}
