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
