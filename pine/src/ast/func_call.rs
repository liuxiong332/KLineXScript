use nom::{
    bytes::complete::tag,
    combinator::{map, opt},
    sequence::{preceded, tuple},
    Err,
};

use super::error::{PineError, PineErrorKind, PineResult};
use super::input::{Input, StrRange};
use super::name::{varname, varname_ws, VarName};
use super::stat_expr::all_exp;
use super::stat_expr_types::{Exp, FunctionCall, RVVarName};
use super::state::AstState;
use super::utils::eat_sep;

#[derive(Debug, PartialEq)]
struct FuncCallArg<'a> {
    pub name: Option<VarName<'a>>,
    pub arg: Exp<'a>,
    pub range: StrRange,
}

impl<'a> FuncCallArg<'a> {
    pub fn new(name: Option<VarName<'a>>, arg: Exp<'a>, range: StrRange) -> FuncCallArg<'a> {
        FuncCallArg { name, arg, range }
    }
}

fn func_call_arg<'a>(input: Input<'a>, state: &AstState) -> PineResult<'a, FuncCallArg<'a>> {
    if let Ok((input, result)) = map(
        tuple((
            |s| varname_ws(s, state),
            eat_sep(tag("=")),
            |s| all_exp(s, state),
        )),
        |s| FuncCallArg {
            name: Some(s.0),
            range: StrRange::new(s.0.range.start, s.2.range().end),
            arg: s.2,
        },
    )(input)
    {
        Ok((input, result))
    } else {
        let result = map(
            |s| all_exp(s, state),
            |s| FuncCallArg {
                name: None,
                range: s.range(),
                arg: s,
            },
        )(input)?;
        Ok(result)
    }
}

pub fn func_call_args<'a>(
    input: Input<'a>,
    state: &AstState,
) -> PineResult<'a, (Vec<Exp<'a>>, Vec<(VarName<'a>, Exp<'a>)>)> {
    let (input, arg1) = opt(|s| func_call_arg(s, state))(input)?;
    if arg1.is_none() {
        return Ok((input, (vec![], vec![])));
    }
    let arg1 = arg1.unwrap();
    let mut is_dict_args = arg1.name.is_some();
    let mut pos_args: Vec<Exp> = vec![];
    let mut dict_args: Vec<(VarName, Exp)> = vec![];
    if is_dict_args {
        dict_args = vec![(arg1.name.unwrap(), arg1.arg)]
    } else {
        pos_args = vec![arg1.arg];
    };

    let mut cur_input = input;

    while let Ok((next_input, arg)) =
        preceded(eat_sep(tag(",")), |s| func_call_arg(s, state))(cur_input)
    {
        match arg.name {
            Some(name) => {
                is_dict_args = true;
                dict_args.push((name, arg.arg));
            }
            _ => {
                if is_dict_args {
                    return Err(Err::Error(PineError::from_pine_kind(
                        input,
                        PineErrorKind::InvalidFuncCallArgs,
                    )));
                }
                pos_args.push(arg.arg);
            }
        }
        cur_input = next_input;
    }
    Ok((cur_input, (pos_args, dict_args)))
}

pub fn func_call<'a>(input: Input<'a>, state: &AstState) -> PineResult<'a, FunctionCall<'a>> {
    let (input, (method, (_, (pos_args, dict_args), paren_r))) = tuple((
        |s| varname(s, state),
        tuple((
            eat_sep(tag("(")),
            |s| func_call_args(s, state),
            eat_sep(tag(")")),
        )),
    ))(input)?;
    let start = method.range.start;
    Ok((
        input,
        FunctionCall::new_no_ctxid(
            Exp::VarName(RVVarName::new(method)),
            pos_args,
            dict_args,
            StrRange::new(start, paren_r.end),
        ),
    ))
}

pub fn func_call_ws<'a>(input: Input<'a>, state: &AstState) -> PineResult<'a, FunctionCall<'a>> {
    eat_sep(|s| func_call(s, state))(input)
}

#[cfg(test)]
mod tests {
    use super::super::input::Position;
    use super::super::stat_expr_types::*;
    use super::*;
    use crate::ast::string::*;
    use std::convert::TryInto;
    use std::fmt::Debug;

    fn check_res<'a, F, O>(s: &'a str, handler: F, res: O)
    where
        F: Fn(Input<'a>, &AstState) -> PineResult<'a, O>,
        O: Debug + PartialEq,
    {
        let test_input = Input::new_with_str(s);
        let input_len: u32 = test_input.len().try_into().unwrap();
        assert_eq!(
            handler(test_input, &AstState::new()),
            Ok((
                Input::new("", Position::new(0, input_len), Position::max()),
                res
            ))
        );
    }

    #[test]
    fn func_call_test() {
        check_res(
            "a = true",
            func_call_arg,
            FuncCallArg {
                name: Some(VarName::new_with_start("a", Position::new(0, 0))),
                arg: Exp::Bool(BoolNode::new(
                    true,
                    StrRange::from_start("true", Position::new(0, 4)),
                )),
                range: StrRange::new(Position::new(0, 0), Position::new(0, 8)),
            },
        );
        check_res(
            "funa(arg1, arg2, a = true)",
            func_call_ws,
            FunctionCall::new_no_ctxid(
                Exp::VarName(RVVarName::new(VarName::new(
                    "funa",
                    StrRange::from_start("funa", Position::new(0, 0)),
                ))),
                vec![
                    Exp::VarName(RVVarName::new(VarName::new(
                        "arg1",
                        StrRange::from_start("arg1", Position::new(0, 5)),
                    ))),
                    Exp::VarName(RVVarName::new(VarName::new(
                        "arg2",
                        StrRange::from_start("arg2", Position::new(0, 11)),
                    ))),
                ],
                vec![(
                    VarName::new("a", StrRange::from_start("a", Position::new(0, 17))),
                    Exp::Bool(BoolNode::new(
                        true,
                        StrRange::from_start("true", Position::new(0, 21)),
                    )),
                )],
                StrRange::from_start("funa(arg1, arg2, a = true)", Position::new(0, 0)),
            ),
        );

        check_res(
            "funa()",
            func_call_ws,
            FunctionCall::new_no_ctxid(
                Exp::VarName(RVVarName::new(VarName::new(
                    "funa",
                    StrRange::from_start("funa", Position::new(0, 0)),
                ))),
                vec![],
                vec![],
                StrRange::from_start("funa()", Position::new(0, 0)),
            ),
        );

        check_res(
            r#"funa("s1", "s2")"#,
            func_call_ws,
            FunctionCall::new_no_ctxid(
                Exp::VarName(RVVarName::new(VarName::new(
                    "funa",
                    StrRange::from_start("funa", Position::new(0, 0)),
                ))),
                vec![
                    Exp::Str(StringNode::new(
                        String::from("s1"),
                        StrRange::from_start(r#"s1"#, Position::new(0, 6)),
                    )),
                    Exp::Str(StringNode::new(
                        String::from("s2"),
                        StrRange::from_start(r#"s2"#, Position::new(0, 12)),
                    )),
                ],
                vec![],
                StrRange::from_start(r#"funa("s1", "s2")"#, Position::new(0, 0)),
            ),
        );
    }

    #[test]
    fn func_call_recursive_test() {
        check_res(
            "funa(funb())",
            func_call_ws,
            FunctionCall::new_no_ctxid(
                Exp::VarName(RVVarName::new(VarName::new(
                    "funa",
                    StrRange::from_start("funa", Position::new(0, 0)),
                ))),
                vec![Exp::FuncCall(Box::new(FunctionCall::new_no_ctxid(
                    Exp::VarName(RVVarName::new(VarName::new(
                        "funb",
                        StrRange::from_start("funb", Position::new(0, 5)),
                    ))),
                    vec![],
                    vec![],
                    StrRange::from_start("funb()", Position::new(0, 5)),
                )))],
                vec![],
                StrRange::from_start("funa(funb())", Position::new(0, 0)),
            ),
        );
    }
}
