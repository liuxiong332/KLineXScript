extern crate pine;
// use pine::error::PineError;
use pine::ast::name::*;
use pine::ast::op::*;
use pine::ast::stat_expr_types::*;

mod utils;
pub use utils::*;

#[test]
fn expression_test() {
    assert_eq!(
        pine::parse_all("m = (high + low + close) / 3\n"),
        Ok(Block::new(
            vec![gen_assign(
                "m",
                gen_binop(
                    BinaryOp::Div,
                    gen_binop(
                        BinaryOp::Plus,
                        gen_binop(BinaryOp::Plus, gen_name("high"), gen_name("low")),
                        gen_name("close")
                    ),
                    gen_int(3)
                )
            )],
            None
        ))
    );

    assert_eq!(
        pine::parse_all("m = sma(high - low, 10) + sma(close, 20)\n"),
        Ok(Block::new(
            vec![gen_assign(
                "m",
                gen_binop(
                    BinaryOp::Plus,
                    gen_func_call(
                        "sma",
                        vec![
                            gen_binop(BinaryOp::Minus, gen_name("high"), gen_name("low")),
                            gen_int(10)
                        ],
                        vec![]
                    ),
                    gen_func_call("sma", vec![gen_name("close"), gen_int(20)], vec![])
                )
            )],
            None
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
        pine::parse_all(VAR_ASSIGN),
        Ok(Block::new(
            vec![
                gen_assign("price", gen_name("close")),
                Statement::Ite(Box::new(IfThenElse::new(
                    gen_binop(BinaryOp::Gt, gen_name("hl2"), gen_name("price")),
                    Block::new(
                        vec![Statement::VarAssignment(Box::new(VarAssignment::new(
                            VarName("price"),
                            gen_name("hl2")
                        )))],
                        None
                    ),
                    None
                ))),
                gen_func_call_stmt("plot", vec![gen_name("price")], vec![])
            ],
            None
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
    let gen_assign = |var, var_type| {
        Statement::Assignment(Box::new(Assignment::new(
            VarName("m"),
            Exp::Bool(true),
            var,
            var_type,
        )))
    };

    assert_eq!(
        pine::parse_all(VAR_DECLARATION),
        Ok(Block::new(
            vec![
                gen_assign(false, None),
                gen_assign(false, Some(DataType::Bool)),
                gen_assign(true, None),
                gen_assign(true, Some(DataType::Bool)),
            ],
            None
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
        pine::parse_all(IF_STATES),
        Ok(Block::new(
            vec![
                Statement::None,
                gen_assign(
                    "x",
                    Exp::Ite(Box::new(IfThenElse::new(
                        gen_binop(BinaryOp::Gt, gen_name("close"), gen_name("open")),
                        Block::new(vec![], Some(gen_name("close"))),
                        Some(Block::new(vec![], Some(gen_name("open"))))
                    )))
                ),
            ],
            None
        ))
    );

    assert_eq!(
        pine::parse_all(IF_NEST_STATS),
        Ok(Block::new(
            vec![
                Statement::None,
                gen_assign(
                    "x",
                    Exp::Ite(Box::new(IfThenElse::new(
                        gen_binop(BinaryOp::Gt, gen_name("close"), gen_name("open")),
                        Block::new(
                            vec![gen_assign(
                                "b",
                                Exp::Ite(Box::new(IfThenElse::new(
                                    gen_binop(
                                        BinaryOp::Gt,
                                        gen_name("close"),
                                        gen_ref_call("close", gen_int(1))
                                    ),
                                    Block::new(vec![], Some(gen_name("close"))),
                                    Some(Block::new(
                                        vec![],
                                        Some(gen_ref_call("close", gen_int(1)))
                                    ))
                                )))
                            )],
                            Some(gen_name("b"))
                        ),
                        Some(Block::new(vec![], Some(gen_name("open"))))
                    )))
                )
            ],
            None
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
    methods: Vec<&'a str>,
    pos_args: Vec<Exp<'a>>,
    dict_args: Vec<(VarName<'a>, Exp<'a>)>,
) -> Statement<'a> {
    Statement::FuncCall(Box::new(FunctionCall {
        method: gen_prefix(methods),
        pos_args: pos_args,
        dict_args: dict_args,
    }))
}

#[test]
fn if_expr_test() {
    assert_eq!(
        pine::parse_all(IF_EXPR),
        Ok(Block::new(
            vec![
                Statement::None,
                Statement::Ite(Box::new(IfThenElse::new(
                    gen_func_call(
                        "crossover",
                        vec![gen_name("source"), gen_name("lower")],
                        vec![]
                    ),
                    Block::new(
                        vec![gen_dot_func_call(
                            vec!["strategy", "entry"],
                            vec![
                                Exp::Str(String::from("BBandLE")),
                                gen_prefix(vec!["strategy", "long"]),
                            ],
                            vec![
                                (VarName("stop"), gen_name("lower")),
                                (
                                    VarName("oca_name"),
                                    Exp::Str(String::from("BollingerBands"))
                                ),
                                (
                                    VarName("oca_type"),
                                    gen_prefix(vec!["strategy", "oca", "cancel"])
                                ),
                                (VarName("comment"), Exp::Str(String::from("BBandLE"))),
                            ]
                        )],
                        None
                    ),
                    Some(Block::new(
                        vec![gen_dot_func_call(
                            vec!["strategy", "cancel"],
                            vec![],
                            vec![(VarName("id"), Exp::Str(String::from("BBandLE")))]
                        )],
                        None
                    ))
                )))
            ],
            None
        ))
    );
}
