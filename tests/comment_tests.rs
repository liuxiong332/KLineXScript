extern crate pine;
// use pine::error::PineError;
use pine::name::*;
use pine::stat_expr_types::*;

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
                Statement::Comment,
                Statement::FuncCall(Box::new(FunctionCall {
                    method: VarName("study"),
                    pos_args: vec![Exp::Str(String::from("Test"))],
                    dict_args: vec![]
                })),
                Statement::Comment,
                Statement::Assignment(Box::new(Assignment::new(
                    VarName("a"),
                    Exp::VarName(VarName("close")),
                    false,
                    None
                ))),
                Statement::FuncCall(Box::new(FunctionCall {
                    method: VarName("plot"),
                    pos_args: vec![Exp::VarName(VarName("a"))],
                    dict_args: vec![]
                }))
            ],
            None
        ))
    )
}
