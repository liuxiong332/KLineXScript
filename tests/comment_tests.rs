extern crate pine;
// use pine::error::PineError;
use pine::ast::name::*;
use pine::ast::stat_expr_types::*;

const TEXT_WITH_COMMENT: &str = "//@version=4
study(\"Test\")
// This line is a comment
a = close // This is also a comment
plot(a)
";

#[test]
fn comment_test() {
    assert_eq!(
        pine::parse_all(TEXT_WITH_COMMENT),
        Ok(Block::new(
            vec![
                Statement::None,
                Statement::FuncCall(Box::new(FunctionCall::new_no_ctxid(
                    Exp::VarName(VarName("study")),
                    vec![Exp::Str(String::from("Test"))],
                    vec![]
                ))),
                Statement::None,
                Statement::Assignment(Box::new(Assignment::new(
                    vec![VarName("a")],
                    Exp::VarName(VarName("close")),
                    false,
                    None
                ))),
                Statement::FuncCall(Box::new(FunctionCall::new_no_ctxid(
                    Exp::VarName(VarName("plot")),
                    vec![Exp::VarName(VarName("a"))],
                    vec![]
                )))
            ],
            None
        ))
    )
}
