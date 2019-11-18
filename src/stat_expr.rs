use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::{map, opt, peek, value},
    multi::{many0, separated_list},
    sequence::{delimited, preceded, separated_pair, tuple},
    Err,
};

use crate::color::color_lit;
use crate::error::{PineError, PineErrorKind, PineResult};
use crate::name::{varname, VarName};
use crate::op::*;
use crate::stat_expr_types::*;
use crate::string::string_lit;
use crate::trans::flatexp_from_components;
use crate::utils::{eat_sep, multi_opt_separated_pair};

pub fn exp2(input: &str) -> PineResult<Exp2> {
    alt((
        value(Exp2::Na, eat_sep(tag("na"))),
        value(Exp2::Bool(true), eat_sep(tag("true"))),
        value(Exp2::Bool(false), eat_sep(tag("false"))),
        map(string_lit, Exp2::Str),
        map(color_lit, Exp2::Color),
        map(varname, Exp2::VarName),
        map(rettupledef, |varnames| Exp2::RetTuple(Box::new(varnames))),
        map(tupledef, |exps| Exp2::Tuple(Box::new(exps))),
    ))(input)
}

pub fn unopexp2(input: &str) -> PineResult<(Vec<UnaryOp>, Exp2)> {
    tuple((many0(unary_op), exp2))(input)
}

pub fn flatexp(input: &str) -> PineResult<FlatExp> {
    let (input, head) = unopexp2(input)?;
    let (input, binop_chain) = many0(tuple((binary_op, unopexp2)))(input)?;
    Ok((input, flatexp_from_components(head, binop_chain)))
}

pub fn exp(input: &str) -> PineResult<Exp> {
    map(flatexp, Exp::from)(input)
}

// The left return tuple of expression `[a, b] = [1, 2]` that contain variable name between square brackets
fn rettupledef(input: &str) -> PineResult<Vec<VarName>> {
    eat_sep(delimited(
        eat_sep(tag("[")),
        separated_list(eat_sep(tag(",")), varname),
        eat_sep(tag("]")),
    ))(input)
}

// The right tuple of expression `[a, b] = [1, 2]` that contain expressions splited by dot between square brackets
fn tupledef(input: &str) -> PineResult<Vec<Exp>> {
    eat_sep(delimited(
        eat_sep(tag("[")),
        separated_list(eat_sep(tag(",")), exp),
        eat_sep(tag("]")),
    ))(input)
}

fn ref_call(input: &str) -> PineResult<RefCall> {
    let (input, (name, arg)) = eat_sep(tuple((
        varname,
        delimited(eat_sep(tag("[")), exp, eat_sep(tag("]"))),
    )))(input)?;
    Ok((input, RefCall { name, arg }))
}

#[derive(Debug, PartialEq)]
struct FuncCallArg<'a> {
    name: Option<VarName<'a>>,
    arg: Exp<'a>,
}

fn func_call_arg(input: &str) -> PineResult<FuncCallArg> {
    let (next, _) = map(tuple((varname, eat_sep(tag("=")), exp)), |s| FuncCallArg {
        name: Some(s.0),
        arg: s.2,
    })(input)?;
    println!("now input {:?}", next);

    if let Ok((input, result)) = map(tuple((varname, eat_sep(tag("=")), exp)), |s| FuncCallArg {
        name: Some(s.0),
        arg: s.2,
    })(input)
    {
        Ok((input, result))
    } else {
        let result = map(exp, |s| FuncCallArg { name: None, arg: s })(input)?;
        Ok(result)
    }
}

fn func_call_args(input: &str) -> PineResult<(Vec<Exp>, Vec<(VarName, Exp)>)> {
    multi_opt_separated_pair(
        separated_list(eat_sep(tag(",")), exp),
        eat_sep(tag(",")),
        separated_list(
            eat_sep(tag(",")),
            separated_pair(varname, eat_sep(tag("=")), exp),
        ),
    )(input)
}

fn func_call(input: &str) -> PineResult<FunctionCall> {
    let (input, (method, (pos_args, dict_args))) = eat_sep(tuple((
        varname,
        delimited(eat_sep(tag("(")), func_call_args, eat_sep(tag(")"))),
    )))(input)?;
    Ok((
        input,
        FunctionCall {
            method,
            pos_args,
            dict_args,
        },
    ))
}

fn function_exp_def(input: &str) -> PineResult<FunctionDef> {
    let (input, name) = varname(input)?;
    let (input, args) = delimited(
        eat_sep(tag("(")),
        separated_list(eat_sep(tag(",")), varname),
        eat_sep(tag(")")),
    )(input)?;
    let (input, body) = exp(input)?;
    Ok((
        input,
        FunctionDef {
            name,
            params: args,
            body: Block {
                stmts: vec![],
                ret_stmt: Some(body),
            },
        },
    ))
}
// fn functiondef(input: &str) -> PineResult<FunctionCall> {

// }

#[cfg(test)]
mod tests {
    use super::*;
    use crate::num::Numeral;
    #[test]
    fn rettupledef_test() {
        assert_eq!(
            rettupledef(" [hello, good]"),
            Ok(("", vec![VarName("hello"), VarName("good")]))
        );
        assert_eq!(
            rettupledef(" [hello, good,  my]hello"),
            Ok((
                "hello",
                vec![VarName("hello"), VarName("good"), VarName("my")]
            ))
        );

        assert_eq!(
            rettupledef(" [ hello  , good ]"),
            Ok(("", vec![VarName("hello"), VarName("good")]))
        );
    }

    #[test]
    fn tupledef_test() {
        assert_eq!(
            tupledef(" [ hello , true ]"),
            Ok(("", vec![Exp::VarName(VarName("hello")), Exp::Bool(true),]))
        );
    }

    #[test]
    fn func_call_test() {
        assert_eq!(
            func_call_arg("a = true"),
            Ok((
                "",
                FuncCallArg {
                    name: Some(VarName("a")),
                    arg: Exp::Bool(true)
                }
            ))
        );
        assert_eq!(
            func_call("funa(arg1, arg2, a = true)"),
            Ok((
                "",
                FunctionCall {
                    method: VarName("funa"),
                    pos_args: vec![Exp::VarName(VarName("arg1")), Exp::VarName(VarName("arg2"))],
                    dict_args: vec![(VarName("a"), Exp::Bool(true))]
                }
            ))
        );
    }
}
