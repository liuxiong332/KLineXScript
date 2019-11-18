use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::{map, opt, peek, value},
    multi::{count, many0, separated_list},
    sequence::{delimited, preceded, separated_pair, tuple},
    Err,
};

use crate::color::color_lit;
use crate::error::{PineError, PineErrorKind, PineResult};
use crate::func_call::func_call;
use crate::name::{varname, VarName};
use crate::op::*;
use crate::stat_expr_types::*;
use crate::string::string_lit;
use crate::trans::flatexp_from_components;
use crate::utils::{eat_sep, eat_statement, statement_indent};

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
        map(func_call, |exp| Exp2::FuncCall(Box::new(exp))),
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
    let (input, (name, arg)) = tuple((
        varname,
        delimited(eat_sep(tag("[")), exp, eat_sep(tag("]"))),
    ))(input)?;
    Ok((input, RefCall { name, arg }))
}

fn condition(input: &str) -> PineResult<Condition> {
    let (input, (cond, _, exp1, _, exp2)) =
        tuple((exp, eat_sep(tag("?")), exp, eat_sep(tag(":")), exp))(input)?;
    Ok((input, Condition { cond, exp1, exp2 }))
}

// fn if_then_else(input: &str) -> PineResult<IfThenElse> {
//     tuple((
//         tag("if")),
//         exp,
//     ))
// }

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

// fn statement_indent(input: &str, indent_count: usize) -> PineResult<Statement> {
//     let line_start = count(alt((tag("    "), tag("\t"))), indent_count);
//     for line in input.lines().iter() {

//     }
// }

fn statement_with_indent<'a>(indent: usize) -> impl Fn(&'a str) -> PineResult<Statement> {
    move |input: &'a str| {
        alt((
            value(
                Statement::Break,
                eat_statement(statement_indent(indent), tag("break")),
            ),
            value(
                Statement::Continue,
                eat_statement(statement_indent(indent), tag("continue")),
            ),
        ))(input)
    }
}

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
    fn ref_call_test() {
        assert_eq!(
            ref_call("hello[true]"),
            Ok((
                "",
                RefCall {
                    name: VarName("hello"),
                    arg: Exp::Bool(true)
                }
            ))
        );
    }

    #[test]
    fn condition_test() {
        assert_eq!(
            condition("a ? b : c"),
            Ok((
                "",
                Condition {
                    cond: Exp::VarName(VarName("a")),
                    exp1: Exp::VarName(VarName("b")),
                    exp2: Exp::VarName(VarName("c")),
                }
            ))
        );
    }

    #[test]
    fn statement_test() {
        assert_eq!(
            statement_with_indent(1)("    break \n"),
            Ok(("", Statement::Break))
        );
    }
}
