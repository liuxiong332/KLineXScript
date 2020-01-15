extern crate pine;
// use pine::error::PineError;
use pine::ast::input::*;
use pine::ast::name::*;
use pine::ast::stat_expr_types::*;
use pine::ast::string::*;

const TEXT_WITH_COMMENT: &str = "//@version=4
study(\"Test\")
// This line is a comment
a = close // This is also a comment
plot(a)
";

#[test]
fn comment_test() {
    assert_eq!(
        pine::parse_ast(TEXT_WITH_COMMENT),
        Ok(Block::new(
            vec![
                // Statement::None(StrRange::from_start("//@version=4\n", Position::new(0, 0))),
                Statement::Exp(Exp::FuncCall(Box::new(FunctionCall::new_no_ctxid(
                    Exp::VarName(RVVarName::new_with_start("study", Position::new(1, 0))),
                    vec![Exp::Str(StringNode::new(
                        String::from("Test"),
                        StrRange::from_start("Test", Position::new(1, 7))
                    ))],
                    vec![],
                    StrRange::from_start("study(\"Test\")", Position::new(1, 0))
                )))),
                // Statement::None(StrRange::from_start(
                //     "// This line is a comment\n",
                //     Position::new(2, 0)
                // )),
                Statement::Assignment(Box::new(Assignment::new(
                    vec![VarName::new_with_start("a", Position::new(3, 0))],
                    Exp::VarName(RVVarName::new_with_start("close", Position::new(3, 4))),
                    false,
                    None,
                    StrRange::from_start("a = close", Position::new(3, 0))
                ))),
                Statement::Exp(Exp::FuncCall(Box::new(FunctionCall::new_no_ctxid(
                    Exp::VarName(RVVarName::new_with_start("plot", Position::new(4, 0))),
                    vec![Exp::VarName(RVVarName::new_with_start(
                        "a",
                        Position::new(4, 5)
                    ))],
                    vec![],
                    StrRange::from_start("plot(a)", Position::new(4, 0))
                ))))
            ],
            None,
            StrRange::new(Position::new(1, 0), Position::new(4, 7))
        ))
    )
}
