extern crate pine;
// use pine::error::PineError;
use pine::name::*;
use pine::op::*;
use pine::stat_expr_types::*;

mod utils;
use utils::*;

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
