extern crate pine;
// use pine::error::PineError;
use pine::ast::input::*;
use pine::ast::name::*;
use pine::ast::op::*;
use pine::ast::stat_expr_types::*;
use pine::ast::string::*;

mod utils;
pub use utils::*;

#[test]
fn expression_test() {
    assert_eq!(
        pine::parse_ast("m = (high + low + close) / 3\n"),
        Ok(Block::new(
            vec![gen_assign(
                VarName::new_with_start("m", Position::new(0, 0)),
                gen_binop(
                    BinaryOp::Div,
                    gen_binop(
                        BinaryOp::Plus,
                        gen_binop(
                            BinaryOp::Plus,
                            gen_name(VarName::new_with_start("high", Position::new(0, 5)),),
                            gen_name(VarName::new_with_start("low", Position::new(0, 12))),
                            StrRange::from_start("high + low", Position::new(0, 5))
                        ),
                        gen_name(VarName::new_with_start("close", Position::new(0, 18))),
                        StrRange::from_start("high + low + close", Position::new(0, 5))
                    ),
                    gen_int(3, StrRange::from_start("3", Position::new(0, 27))),
                    // StrRange::from_start("(high + low + close) / 3", Position::new(0, 4)) bracket excluded
                    StrRange::new(Position::new(0, 5), Position::new(0, 28))
                ),
                StrRange::from_start("m = (high + low + close) / 3", Position::new(0, 0))
            )],
            None,
            StrRange::from_start("m = (high + low + close) / 3", Position::new(0, 0))
        ))
    );

    let max_range = StrRange::from_start(
        "m = sma(high - low, 10) + sma(close, 20)",
        Position::new(0, 0),
    );
    assert_eq!(
        pine::parse_ast("m = sma(high - low, 10) + sma(close, 20)\n"),
        Ok(Block::new(
            vec![gen_assign(
                VarName::new_with_start("m", Position::new(0, 0)),
                gen_binop(
                    BinaryOp::Plus,
                    gen_func_call(
                        VarName::new_with_start("sma", Position::new(0, 4)),
                        vec![
                            gen_binop(
                                BinaryOp::Minus,
                                gen_name(VarName::new_with_start("high", Position::new(0, 8))),
                                gen_name(VarName::new_with_start("low", Position::new(0, 15))),
                                StrRange::from_start("high - low", Position::new(0, 8))
                            ),
                            gen_int(10, StrRange::from_start("10", Position::new(0, 20)))
                        ],
                        vec![],
                        StrRange::from_start("sma(high - low, 10)", Position::new(0, 4))
                    ),
                    gen_func_call(
                        VarName::new_with_start("sma", Position::new(0, 26)),
                        vec![
                            gen_name(VarName::new_with_start("close", Position::new(0, 30))),
                            gen_int(20, StrRange::from_start("20", Position::new(0, 37)))
                        ],
                        vec![],
                        StrRange::from_start("sma(close, 20)", Position::new(0, 26))
                    ),
                    StrRange::from_start(
                        "sma(high - low, 10) + sma(close, 20)",
                        Position::new(0, 4)
                    )
                ),
                max_range,
            )],
            None,
            max_range
        ))
    );
}

const VAR_ASSIGN: &str = "price = close
if hl2 > price
    price := hl2
plot(price)
";

#[test]
fn var_assign_test() {
    assert_eq!(
        pine::parse_ast(VAR_ASSIGN),
        Ok(Block::new(
            vec![
                gen_assign(
                    VarName::new_with_start("price", Position::new(0, 0)),
                    gen_name(VarName::new_with_start("close", Position::new(0, 8))),
                    StrRange::from_start("price = close", Position::new(0, 0))
                ),
                Statement::Ite(Box::new(IfThenElse::new_no_ctxid(
                    gen_binop(
                        BinaryOp::Gt,
                        gen_name(VarName::new_with_start("hl2", Position::new(1, 3))),
                        gen_name(VarName::new_with_start("price", Position::new(1, 9))),
                        StrRange::from_start("hl2 > price", Position::new(1, 3))
                    ),
                    Block::new(
                        vec![Statement::VarAssignment(Box::new(VarAssignment::new(
                            VarName::new_with_start("price", Position::new(2, 4)),
                            gen_name(VarName::new_with_start("hl2", Position::new(2, 13))),
                            StrRange::from_start("price := hl2", Position::new(2, 4))
                        )))],
                        None,
                        StrRange::from_start("price := hl2", Position::new(2, 4))
                    ),
                    None,
                    StrRange::new(Position::new(1, 0), Position::new(2, 16))
                ))),
                gen_func_call_stmt(
                    VarName::new_with_start("plot", Position::new(3, 0)),
                    vec![gen_name(VarName::new_with_start(
                        "price",
                        Position::new(3, 5)
                    ))],
                    vec![],
                    StrRange::from_start("plot(price)", Position::new(3, 0))
                ),
            ],
            None,
            StrRange::new(Position::new(0, 0), Position::new(3, 11))
        ))
    );
}

const VAR_DECLARATION: &str = "m = true
bool m = true
var m = true
var bool m = true
";

#[test]
fn variable_declare_test() {
    let gen_assign = |src, var, var_type, row, name_col, bool_col| {
        Statement::Assignment(Box::new(Assignment::new(
            vec![VarName::new_with_start("m", Position::new(row, name_col))],
            Exp::Bool(BoolNode::new(
                true,
                StrRange::from_start("true", Position::new(row, bool_col)),
            )),
            var,
            var_type,
            StrRange::from_start(src, Position::new(row, 0)),
        )))
    };

    assert_eq!(
        pine::parse_ast(VAR_DECLARATION),
        Ok(Block::new(
            vec![
                gen_assign("m = true", false, None, 0, 0, 4),
                gen_assign("bool m = true", false, Some(DataType::Bool), 1, 5, 9),
                gen_assign("var m = true", true, None, 2, 4, 8),
                gen_assign("var bool m = true", true, Some(DataType::Bool), 3, 9, 13),
            ],
            None,
            StrRange::new(Position::new(0, 0), Position::new(3, 17))
        ))
    );
}

const IF_STATES: &str = "// This code compiles
x = if close > open
    close
else
    open
";

const IF_NEST_STATS: &str = "
x = if close > open
    b = if close > close[1]
        close
    else
        close[1]
    b
else
    open
";

#[test]
fn if_stats_test() {
    assert_eq!(
        pine::parse_ast(IF_STATES),
        Ok(Block::new(
            vec![
                // Statement::None(StrRange::from_start(
                //     "// This code compiles\n",
                //     Position::new(0, 0)
                // )),
                gen_assign(
                    VarName::new_with_start("x", Position::new(1, 0)),
                    Exp::Ite(Box::new(IfThenElse::new_no_ctxid(
                        gen_binop(
                            BinaryOp::Gt,
                            gen_name(VarName::new_with_start("close", Position::new(1, 7))),
                            gen_name(VarName::new_with_start("open", Position::new(1, 15))),
                            StrRange::from_start("close > open", Position::new(1, 7))
                        ),
                        Block::new(
                            vec![],
                            Some(gen_name(VarName::new_with_start(
                                "close",
                                Position::new(2, 4)
                            ))),
                            StrRange::from_start("close", Position::new(2, 4))
                        ),
                        Some(Block::new(
                            vec![],
                            Some(gen_name(VarName::new_with_start(
                                "open",
                                Position::new(4, 4)
                            ))),
                            StrRange::from_start("open", Position::new(4, 4))
                        )),
                        StrRange::new(Position::new(1, 4), Position::new(4, 8))
                    ))),
                    StrRange::new(Position::new(1, 0), Position::new(4, 8))
                ),
            ],
            None,
            StrRange::new(Position::new(1, 0), Position::new(4, 8))
        ))
    );

    assert_eq!(
        pine::parse_ast(IF_NEST_STATS),
        Ok(Block::new(
            vec![
                // Statement::None(StrRange::from_start("\n", Position::new(0, 0))),
                gen_assign(
                    VarName::new_with_start("x", Position::new(1, 0)),
                    Exp::Ite(Box::new(IfThenElse::new_no_ctxid(
                        gen_binop(
                            BinaryOp::Gt,
                            gen_name(VarName::new_with_start("close", Position::new(1, 7))),
                            gen_name(VarName::new_with_start("open", Position::new(1, 15))),
                            StrRange::from_start("close > open", Position::new(1, 7))
                        ),
                        Block::new(
                            vec![gen_assign(
                                VarName::new_with_start("b", Position::new(2, 4)),
                                Exp::Ite(Box::new(IfThenElse::new_no_ctxid(
                                    gen_binop(
                                        BinaryOp::Gt,
                                        gen_name(VarName::new_with_start(
                                            "close",
                                            Position::new(2, 11)
                                        )),
                                        gen_ref_call(
                                            VarName::new_with_start("close", Position::new(2, 19)),
                                            gen_int(
                                                1,
                                                StrRange::from_start("1", Position::new(2, 25))
                                            ),
                                            StrRange::from_start("close[1]", Position::new(2, 19))
                                        ),
                                        StrRange::from_start(
                                            "close > close[1]",
                                            Position::new(2, 11)
                                        )
                                    ),
                                    Block::new(
                                        vec![],
                                        Some(gen_name(VarName::new_with_start(
                                            "close",
                                            Position::new(3, 8)
                                        ))),
                                        StrRange::from_start("close", Position::new(3, 8))
                                    ),
                                    Some(Block::new(
                                        vec![],
                                        Some(gen_ref_call(
                                            VarName::new_with_start("close", Position::new(5, 8)),
                                            gen_int(
                                                1,
                                                StrRange::from_start("1", Position::new(5, 14))
                                            ),
                                            StrRange::from_start("close[1]", Position::new(5, 8))
                                        )),
                                        StrRange::from_start("close[1]", Position::new(5, 8))
                                    )),
                                    StrRange::new(Position::new(2, 8), Position::new(5, 16))
                                ))),
                                StrRange::new(Position::new(2, 4), Position::new(5, 16))
                            )],
                            Some(gen_name(VarName::new_with_start("b", Position::new(6, 4)))),
                            StrRange::new(Position::new(2, 4), Position::new(6, 5))
                        ),
                        Some(Block::new(
                            vec![],
                            Some(gen_name(VarName::new_with_start(
                                "open",
                                Position::new(8, 4)
                            ))),
                            StrRange::from_start("open", Position::new(8, 4))
                        )),
                        StrRange::new(Position::new(1, 4), Position::new(8, 8))
                    ))),
                    StrRange::new(Position::new(1, 0), Position::new(8, 8))
                )
            ],
            None,
            StrRange::new(Position::new(1, 0), Position::new(8, 8))
        ))
    );
}

const IF_EXPR: &str = "
if (crossover(source, lower))
    strategy.entry(\"BBandLE\", strategy.long, stop=lower,
                   oca_name=\"BollingerBands\",
                   oca_type=strategy.oca.cancel, comment=\"BBandLE\")
else
    strategy.cancel(id=\"BBandLE\")
";

fn gen_dot_func_call<'a>(
    methods: Vec<VarName<'a>>,
    methods_range: StrRange,
    pos_args: Vec<Exp<'a>>,
    dict_args: Vec<(VarName<'a>, Exp<'a>)>,
    range: StrRange,
) -> Statement<'a> {
    Statement::Exp(Exp::FuncCall(Box::new(FunctionCall::new_no_ctxid(
        gen_prefix(methods[0], methods[1], methods_range),
        pos_args,
        dict_args,
        range,
    ))))
}

#[test]
fn if_expr_test() {
    assert_eq!(
        pine::parse_ast(IF_EXPR),
        Ok(Block::new(
            vec![
                // Statement::None(StrRange::from_start("\n", Position::new(0, 0))),
                Statement::Ite(Box::new(IfThenElse::new_no_ctxid(
                    gen_func_call(
                        VarName::new_with_start("crossover", Position::new(1, 4)),
                        vec![
                            gen_name(VarName::new_with_start("source", Position::new(1, 14))),
                            gen_name(VarName::new_with_start("lower", Position::new(1, 22)))
                        ],
                        vec![],
                        StrRange::from_start("crossover(source, lower)", Position::new(1, 4))
                    ),
                    Block::new(
                        vec![gen_dot_func_call(
                            vec![
                                VarName::new_with_start("strategy", Position::new(2, 4)),
                                VarName::new_with_start("entry", Position::new(2, 13))
                            ],
                            StrRange::from_start("strategy.entry", Position::new(2, 4)),
                            vec![
                                Exp::Str(StringNode::new(
                                    String::from("BBandLE"),
                                    StrRange::from_start("BBandLE", Position::new(2, 20))
                                )),
                                gen_prefix(
                                    VarName::new_with_start("strategy", Position::new(2, 30)),
                                    VarName::new_with_start("long", Position::new(2, 39)),
                                    StrRange::from_start("strategy.long", Position::new(2, 30))
                                ),
                            ],
                            vec![
                                (
                                    VarName::new_with_start("stop", Position::new(2, 45)),
                                    gen_name(VarName::new_with_start(
                                        "lower",
                                        Position::new(2, 50)
                                    ))
                                ),
                                (
                                    VarName::new_with_start("oca_name", Position::new(3, 19)),
                                    Exp::Str(StringNode::new(
                                        String::from("BollingerBands"),
                                        StrRange::from_start(
                                            "BollingerBands",
                                            Position::new(3, 29)
                                        )
                                    ))
                                ),
                                (
                                    VarName::new_with_start("oca_type", Position::new(4, 19)),
                                    gen_prefix_exp(
                                        gen_prefix(
                                            VarName::new_with_start(
                                                "strategy",
                                                Position::new(4, 28)
                                            ),
                                            VarName::new_with_start("oca", Position::new(4, 37)),
                                            StrRange::from_start(
                                                "strategy.oca",
                                                Position::new(4, 28)
                                            )
                                        ),
                                        VarName::new_with_start("cancel", Position::new(4, 41)),
                                        StrRange::from_start(
                                            "strategy.oca.cancel",
                                            Position::new(4, 28)
                                        )
                                    )
                                ),
                                (
                                    VarName::new_with_start("comment", Position::new(4, 49)),
                                    Exp::Str(StringNode::new(
                                        String::from("BBandLE"),
                                        StrRange::from_start("BBandLE", Position::new(4, 58))
                                    ))
                                ),
                            ],
                            StrRange::new(Position::new(2, 4), Position::new(4, 67))
                        )],
                        None,
                        StrRange::new(Position::new(2, 4), Position::new(4, 67))
                    ),
                    Some(Block::new(
                        vec![gen_dot_func_call(
                            vec![
                                VarName::new_with_start("strategy", Position::new(6, 4)),
                                VarName::new_with_start("cancel", Position::new(6, 13))
                            ],
                            StrRange::from_start("strategy.cancel", Position::new(6, 4)),
                            vec![],
                            vec![(
                                VarName::new_with_start("id", Position::new(6, 20)),
                                Exp::Str(StringNode::new(
                                    String::from("BBandLE"),
                                    StrRange::from_start("BBandLE", Position::new(6, 24))
                                ))
                            )],
                            StrRange::new(Position::new(6, 4), Position::new(6, 33))
                        )],
                        None,
                        StrRange::new(Position::new(6, 4), Position::new(6, 33))
                    )),
                    StrRange::new(Position::new(1, 0), Position::new(6, 33))
                )))
            ],
            None,
            StrRange::new(Position::new(1, 0), Position::new(6, 33))
        ))
    );
}
