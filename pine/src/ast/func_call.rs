use nom::{
    bytes::complete::tag,
    combinator::{map, opt},
    sequence::{delimited, preceded, tuple},
    Err,
};

use super::error::{PineError, PineErrorKind, PineResult};
use super::input::Input;
use super::name::{varname_ws, VarName};
use super::stat_expr::{callable_expr, exp};
use super::stat_expr_types::{Exp, FunctionCall};
use super::utils::eat_sep;

#[derive(Debug, PartialEq)]
struct FuncCallArg<'a> {
    name: Option<VarName<'a>>,
    arg: Exp<'a>,
}

fn func_call_arg(input: Input) -> PineResult<FuncCallArg> {
    if let Ok((input, result)) = map(tuple((varname_ws, eat_sep(tag("=")), exp)), |s| {
        FuncCallArg {
            name: Some(s.0),
            arg: s.2,
        }
    })(input)
    {
        Ok((input, result))
    } else {
        let result = map(exp, |s| FuncCallArg { name: None, arg: s })(input)?;
        Ok(result)
    }
}

fn func_call_args(input: Input) -> PineResult<(Vec<Exp>, Vec<(VarName, Exp)>)> {
    let (input, arg1) = opt(func_call_arg)(input)?;
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

    while let Ok((next_input, arg)) = preceded(eat_sep(tag(",")), func_call_arg)(cur_input) {
        match arg.name {
            Some(name) => {
                is_dict_args = true;
                dict_args.push((name, arg.arg));
            }
            _ => {
                if is_dict_args {
                    return Err(Err::Error(PineError::from_pine_kind(
                        input,
                        PineErrorKind::InvalidFuncCallArgs(
                            "Position argument must appear before the dict argument",
                        ),
                    )));
                }
                pos_args.push(arg.arg);
            }
        }
        cur_input = next_input;
    }
    Ok((cur_input, (pos_args, dict_args)))
}

pub fn func_call(input: Input) -> PineResult<FunctionCall> {
    let (input, (method, (pos_args, dict_args))) = tuple((
        callable_expr,
        delimited(eat_sep(tag("(")), func_call_args, eat_sep(tag(")"))),
    ))(input)?;
    Ok((
        input,
        FunctionCall::new_no_ctxid(method, pos_args, dict_args),
    ))
}

pub fn func_call_ws(input: Input) -> PineResult<FunctionCall> {
    eat_sep(func_call)(input)
}

#[cfg(test)]
mod tests {
    use super::super::input::Position;
    use super::*;
    use crate::ast::stat_expr_types::PrefixExp;
    use std::convert::TryInto;
    use std::fmt::Debug;

    fn check_res<'a, F, O>(s: &'a str, handler: F, res: O)
    where
        F: Fn(Input<'a>) -> PineResult<O>,
        O: Debug + PartialEq,
    {
        let test_input = Input::new_with_str(s);
        let input_len: u32 = test_input.len().try_into().unwrap();
        assert_eq!(
            handler(test_input),
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
                name: Some(VarName("a")),
                arg: Exp::Bool(true),
            },
        );
        check_res(
            "funa(arg1, arg2, a = true)",
            func_call_ws,
            FunctionCall::new_no_ctxid(
                Exp::VarName(VarName("funa")),
                vec![Exp::VarName(VarName("arg1")), Exp::VarName(VarName("arg2"))],
                vec![(VarName("a"), Exp::Bool(true))],
            ),
        );

        check_res(
            "funa()",
            func_call_ws,
            FunctionCall::new_no_ctxid(Exp::VarName(VarName("funa")), vec![], vec![]),
        );

        check_res(
            "funa.funb()",
            func_call_ws,
            FunctionCall::new_no_ctxid(
                Exp::PrefixExp(Box::new(PrefixExp {
                    var_chain: vec![VarName("funa"), VarName("funb")],
                })),
                vec![],
                vec![],
            ),
        );
    }

    #[test]
    fn func_call_recursive_test() {
        check_res(
            "funa(funb())",
            func_call_ws,
            FunctionCall::new_no_ctxid(
                Exp::VarName(VarName("funa")),
                vec![Exp::FuncCall(Box::new(FunctionCall::new_no_ctxid(
                    Exp::VarName(VarName("funb")),
                    vec![],
                    vec![],
                )))],
                vec![],
            ),
        );
    }
}
