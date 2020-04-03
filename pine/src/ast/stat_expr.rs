use super::color::color_lit;
use super::error::{PineError, PineErrorKind, PineResult};
use super::func_call::{func_call, func_call_args, func_call_ws};
use super::input::{Input, StrRange};
use super::name::{varname, varname_only, varname_ws};
use super::num::num_lit_ws;
use super::op::*;
use super::stat_expr_types::*;
use super::state::{AstState, PineInputError};
use super::string::string_lit_ws;
use super::trans::flatexp_from_components;
use super::utils::{eat_sep, eat_space, eat_statement, statement_end, statement_indent};
use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::{map, opt},
    multi::{many0, separated_list},
    sequence::{delimited, preceded, terminated, tuple},
    Err,
};

// exp2 contain the expressions that can apply the binary operators(+,-,*,/) and unary operators(+,-)
pub fn exp2<'a>(input: Input<'a>, state: &AstState) -> PineResult<'a, Exp2<'a>> {
    alt((
        map(eat_sep(tag("true")), |s| {
            Exp2::Bool(BoolNode::new(true, StrRange::from_input(&s)))
        }),
        map(eat_sep(tag("false")), |s| {
            Exp2::Bool(BoolNode::new(false, StrRange::from_input(&s)))
        }),
        map(num_lit_ws, Exp2::Num),
        map(string_lit_ws, Exp2::Str),
        map(|s| color_lit(s, state), Exp2::Color),
        map(|input| bracket_expr(input, state), Exp2::Exp),
        map(|s| tupledef(s, state), |exps| Exp2::Tuple(Box::new(exps))), // match [a, b + c]
        map(|s| type_cast(s, state), |exp| Exp2::TypeCast(Box::new(exp))), // match float(b)
        // map(
        //     |s| func_call_ws(s, state),
        //     |exp| Exp2::FuncCall(Box::new(exp)),
        // ), // match a(b) (a)(b) a.b.c(m)
        // map(|s| ref_call(s, state), |exp| Exp2::RefCall(Box::new(exp))), // match a[b] (a)[b] a.b.c[m]
        // map(
        //     |s| prefix_exp_ws(s, state),
        //     |exp| Exp2::PrefixExp(Box::new(exp)),
        // ), // match a.b.c
        map(|s| prefix_ref_func_call(s, state), |exp| Exp2::Exp(exp)),
        // map(eat_sep(tag("na")), |s| {
        //     Exp2::Na(NaNode::new(StrRange::from_input(&s)))
        // }), // match na but not na(m) and na.m
        // map(|s| varname_ws(s, state), Exp2::VarName), // match a
    ))(input)
}

pub fn unopexp2<'a>(input: Input<'a>, state: &AstState) -> PineResult<'a, UnOpExp2<'a>> {
    let (input, (ops, exp)) = tuple((many0(unary_op), |s| exp2(s, state)))(input)?;
    let range = if ops.is_empty() {
        exp.range()
    } else {
        StrRange::new(ops[0].range.start, exp.range().end)
    };
    Ok((input, UnOpExp2::new(ops, exp, range)))
}

pub fn flatexp<'a>(input: Input<'a>, state: &AstState) -> PineResult<'a, FlatExp<'a>> {
    let (input, head) = unopexp2(input, state)?;
    let (input, binop_chain) = many0(tuple((binary_op, |s| unopexp2(s, state))))(input)?;
    Ok((input, flatexp_from_components(head, binop_chain)))
}

pub fn exp<'a>(input: Input<'a>, state: &AstState) -> PineResult<'a, Exp<'a>> {
    alt((
        map(|s| condition(s, state), |exp| Exp::Condition(Box::new(exp))), // match a ? b : c
        map(eat_sep(|s| assign_expr(s, state)), |s| {
            Exp::Assignment(Box::new(s))
        }), // a = b
        map(eat_sep(|s| var_assign_expr(s, state)), |s| {
            Exp::VarAssignment(Box::new(s))
        }), // a := b
        map(|s| flatexp(s, state), Exp::from),
    ))(input)
}

// Will parse all the expression. if-then-else and for-range expression will consume the statement end.
pub fn all_exp<'a>(input: Input<'a>, state: &AstState) -> PineResult<'a, Exp<'a>> {
    alt((
        |s| exp(s, state),
        map(eat_sep(|s| if_then_else_exp(s, state)), |s| {
            Exp::Ite(Box::new(s))
        }),
        map(eat_sep(|s| for_range_exp(s, state)), |s| {
            Exp::ForRange(Box::new(s))
        }),
    ))(input)
}

// Parse the expression that end with statement end, so the expression must end with \n
pub fn exp_with_stmt_end<'a>(input: Input<'a>, state: &AstState) -> PineResult<'a, Exp<'a>> {
    alt((
        terminated(|s| exp(s, state), statement_end),
        map(eat_sep(|s| if_then_else_exp(s, state)), |s| {
            Exp::Ite(Box::new(s))
        }),
        map(eat_sep(|s| for_range_exp(s, state)), |s| {
            Exp::ForRange(Box::new(s))
        }),
    ))(input)
}

// The left return tuple of expression `[a, b] = [1, 2]` that contain variable name between square brackets
fn rettupledef<'a>(input: Input<'a>, state: &AstState) -> PineResult<'a, LVTupleNode<'a>> {
    let (input, (paren_l, names, paren_r)) = eat_sep(tuple((
        eat_sep(tag("[")),
        separated_list(eat_sep(tag(",")), |s| varname_ws(s, state)),
        eat_sep(tag("]")),
    )))(input)?;
    if names.is_empty() {
        Err(Err::Error(PineError::from_pine_kind(
            input,
            PineErrorKind::LVTupleNoNames,
        )))
    } else {
        Ok((
            input,
            LVTupleNode::new(names, StrRange::new(paren_l.start, paren_r.end)),
        ))
    }
}

// The right tuple of expression `[a, b] = [1, 2]` that contain expressions splited by dot between square brackets
fn tupledef<'a>(input: Input<'a>, state: &AstState) -> PineResult<'a, TupleNode<'a>> {
    let (input, (paren_l, items, paren_r)) = eat_sep(tuple((
        eat_sep(tag("[")),
        separated_list(eat_sep(tag(",")), |s| all_exp(s, state)),
        eat_sep(tag("]")),
    )))(input)?;
    Ok((
        input,
        TupleNode::new(items, StrRange::new(paren_l.start, paren_r.end)),
    ))
}

fn type_cast<'a>(input: Input<'a>, state: &AstState) -> PineResult<'a, TypeCast<'a>> {
    let (input, (data_type, _, e, end_tag)) = eat_sep(tuple((
        |s| datatype(s, state),
        eat_sep(tag("(")),
        |s| all_exp(s, state),
        eat_sep(tag(")")),
    )))(input)?;
    Ok((
        input,
        TypeCast::new(
            data_type.value,
            e,
            StrRange::new(data_type.range.start, end_tag.end),
        ),
    ))
}

// pub fn callable_expr<'a>(input: Input<'a>, state: &AstState) -> PineResult<'a, Exp<'a>> {
//     alt((
//         delimited(tag("("), |s| all_exp(s, state), eat_sep(tag(")"))),
//         map(
//             |s| prefix_exp(s, state),
//             |exp| Exp::PrefixExp(Box::new(exp)),
//         ), // match a.b.c
//         map(
//             |s| varname(s, state),
//             |name| Exp::VarName(RVVarName::new(name)),
//         ), // match a
//     ))(input)
// }

// pub fn ref_callable_expr<'a>(input: Input<'a>, state: &AstState) -> PineResult<'a, Exp<'a>> {
//     alt((
//         delimited(tag("("), |s| all_exp(s, state), eat_sep(tag(")"))),
//         map(
//             |s| prefix_exp(s, state),
//             |exp| Exp::PrefixExp(Box::new(exp)),
//         ), // match a.b.c
//         map(|s| func_call(s, state), |exp| Exp::FuncCall(Box::new(exp))), // match a(b)
//         map(
//             |s| varname(s, state),
//             |name| Exp::VarName(RVVarName::new(name)),
//         ), // match a
//     ))(input)
// }

// fn ref_call<'a>(input: Input<'a>, state: &AstState) -> PineResult<'a, RefCall<'a>> {
//     let (input, (name, (_, arg, paren_r))) = tuple((
//         eat_sep(|s| ref_callable_expr(s, state)),
//         tuple((eat_sep(tag("[")), |s| all_exp(s, state), eat_sep(tag("]")))),
//     ))(input)?;

//     let range = StrRange::new(name.range().start, paren_r.end);
//     Ok((input, RefCall::new(name, arg, range)))
// }

fn search_prefix_ref_func<'a>(
    input: Input<'a>,
    state: &AstState,
    exp: Exp<'a>,
) -> PineResult<'a, Exp<'a>> {
    match eat_sep(tag("."))(input) {
        Err(_) => (),
        Ok((input, _)) => match eat_sep(|s| varname_only(s))(input) {
            Err(_) => (),
            Ok((input, name)) => {
                if name.is_reserved() {
                    state.catch(PineInputError::new(
                        PineErrorKind::ReservedVarName,
                        name.range,
                    ));
                }
                let range = StrRange::new(exp.range().start, name.range.end);
                return Ok((
                    input,
                    Exp::PrefixExp(Box::new(PrefixExp::new(exp, name, range))),
                ));
            }
        },
    };
    match tuple((eat_sep(tag("[")), |s| all_exp(s, state), eat_sep(tag("]"))))(input) {
        Err(_) => (),
        Ok((input, (_, arg, paren_r))) => {
            let range = StrRange::new(exp.range().start, paren_r.end);
            return Ok((input, Exp::RefCall(Box::new(RefCall::new(exp, arg, range)))));
        }
    };

    match tuple((
        // eat_sep(tag("(")),
        eat_space(tag("(")), // The function call cannot cross multi lines
        |s| func_call_args(s, state),
        eat_sep(tag(")")),
    ))(input)
    {
        Err(_) => (),
        Ok((input, (_, (pos_args, dict_args), paren_r))) => {
            let start = exp.range().start;
            return Ok((
                input,
                Exp::FuncCall(Box::new(FunctionCall::new_no_ctxid(
                    exp,
                    pos_args,
                    dict_args,
                    StrRange::new(start, paren_r.end),
                ))),
            ));
        }
    };
    Err(Err::Error(PineError::from_pine_kind(
        input,
        PineErrorKind::UnknownErr,
    )))
}

fn prefix_ref_func_call<'a>(input: Input<'a>, state: &AstState) -> PineResult<'a, Exp<'a>> {
    let (input, mut var_exp) = eat_sep(alt((
        delimited(tag("("), |s| all_exp(s, state), eat_sep(tag(")"))),
        map(
            |s| varname(s, state),
            |name| Exp::VarName(RVVarName::new(name)),
        ), // match a
    )))(input)?;
    let mut cur_input = input;
    loop {
        match search_prefix_ref_func(cur_input, state, var_exp.clone()) {
            Ok((input, exp)) => {
                cur_input = input;
                var_exp = exp;
            }
            Err(_) => break,
        };
    }
    if let Exp::VarName(RVVarName { name, .. }) = var_exp {
        if name.value == "na" {
            return Ok((cur_input, Exp::Na(NaNode::new(name.range))));
        }
    }
    Ok((cur_input, var_exp))
}

fn bracket_expr<'a>(input: Input<'a>, state: &AstState) -> PineResult<'a, Exp<'a>> {
    delimited(eat_sep(tag("(")), |s| all_exp(s, state), eat_sep(tag(")")))(input)
}

fn condition<'a>(input: Input<'a>, state: &AstState) -> PineResult<'a, Condition<'a>> {
    let (input, (cond, _, exp1, _, exp2)) = tuple((
        map(|s| flatexp(s, state), |s| Exp::from(s)),
        eat_sep(tag("?")),
        |s| all_exp(s, state),
        eat_sep(tag(":")),
        |s| all_exp(s, state),
    ))(input)?;

    let range = StrRange::new(cond.range().start, exp2.range().end);
    Ok((input, Condition::new(cond, exp1, exp2, range)))
}

// fn prefix_exp<'a>(input: Input<'a>, state: &AstState) -> PineResult<'a, PrefixExp<'a>> {
//     let (input, (prefix, _, names)) = tuple((
//         |s| varname(s, state),
//         tag("."),
//         separated_list(tag("."), |s| varname(s, state)),
//     ))(input)?;

//     if names.len() == 0 {
//         Err(Err::Error(PineError::from_pine_kind(
//             input,
//             PineErrorKind::PrefixNoNamesAfterDot,
//         )))
//     } else {
//         let range = StrRange::new(prefix.range.start, names.last().unwrap().range.end);
//         let exps: Vec<_> = [vec![prefix], names]
//             .concat()
//             .into_iter()
//             .map(|v| Exp::VarName(RVVarName::new(v)))
//             .collect();
//         Ok((input, PrefixExp::new(exps, range)))
//     }
// }

// fn prefix_exp_ws<'a>(input: Input<'a>, state: &AstState) -> PineResult<'a, PrefixExp<'a>> {
//     eat_sep(|s| prefix_exp(s, state))(input)
// }

fn if_then_else<'a, F>(
    block_parser: F,
) -> impl Fn(Input<'a>, &AstState) -> PineResult<'a, IfThenElse<'a>>
where
    F: Fn(Input<'a>, &AstState) -> PineResult<'a, Block<'a>>,
{
    move |input: Input<'a>, state: &AstState| {
        let (input, (if_tag, cond, then_block, else_block)) = tuple((
            tag("if"),
            |s| exp_with_stmt_end(s, state),
            |s| block_parser(s, state),
            opt(tuple((
                preceded(statement_indent(state.get_indent()), tag("else")),
                statement_end,
                |s| block_parser(s, state),
            ))),
        ))(input)?;
        if let Some((_, _, else_block)) = else_block {
            let range = StrRange::new(if_tag.start, else_block.range.end);
            Ok((
                input,
                IfThenElse::new_no_ctxid(cond, then_block, Some(else_block), range),
            ))
        } else {
            let range = StrRange::new(if_tag.start, then_block.range.end);
            Ok((
                input,
                IfThenElse::new_no_ctxid(cond, then_block, None, range),
            ))
        }
    }
}

pub fn if_then_else_exp<'a>(input: Input<'a>, state: &AstState) -> PineResult<'a, IfThenElse<'a>> {
    if_then_else(inner_block_for_exp)(input, state)
}

pub fn if_then_else_with_indent<'a>(
    input: Input<'a>,
    state: &AstState,
) -> PineResult<'a, IfThenElse<'a>> {
    preceded(statement_indent(state.get_indent()), |s| {
        if_then_else(inner_block_for_stmt)(s, state)
    })(input)
}

fn for_range<'a, F>(
    block_parser: F,
) -> impl Fn(Input<'a>, &AstState) -> PineResult<'a, ForRange<'a>>
where
    F: Fn(Input<'a>, &AstState) -> PineResult<'a, Block<'a>>,
{
    move |input: Input<'a>, state| {
        let (input, (for_tag, var, _, start, _, end, by, _, do_blk)) = tuple((
            tag("for"),
            |s| varname_ws(s, state),
            eat_sep(tag("=")),
            |s| all_exp(s, state), // int_lit_ws,
            eat_sep(tag("to")),
            |s| all_exp(s, state), // int_lit_ws,
            opt(tuple((eat_sep(tag("by")), |s| all_exp(s, state)))),
            statement_end,
            |s| block_parser(s, state),
        ))(input)?;

        let range = StrRange::new(for_tag.start, do_blk.range.end);
        if let Some((_, step)) = by {
            Ok((
                input,
                ForRange::new_no_ctxid(var, start, end, Some(step), do_blk, range),
            ))
        } else {
            Ok((
                input,
                ForRange::new_no_ctxid(var, start, end, None, do_blk, range),
            ))
        }
    }
}

pub fn for_range_exp<'a>(input: Input<'a>, state: &AstState) -> PineResult<'a, ForRange<'a>> {
    for_range(inner_block_for_exp)(input, state)
}

fn for_range_with_indent<'a>(input: Input<'a>, state: &AstState) -> PineResult<'a, ForRange<'a>> {
    preceded(statement_indent(state.get_indent()), |s| {
        for_range(inner_block_for_stmt)(s, state)
    })(input)
}

pub fn function_def_with_indent<'a>(
    input: Input<'a>,
    state: &AstState,
) -> PineResult<'a, FunctionDef<'a>> {
    let (input, (_, name, _, params, _, _, body)) = tuple((
        statement_indent(state.get_indent()),
        |s| varname(s, state),
        eat_sep(tag("(")),
        separated_list(eat_sep(tag(",")), |s| varname_ws(s, state)),
        eat_sep(tag(")")),
        eat_sep(tag("=>")),
        alt((
            preceded(statement_end, |s| inner_block_for_exp(s, state)),
            map(
                |s| exp_with_stmt_end(s, state),
                |s| {
                    let range = s.range();
                    Block::new(vec![], Some(s), range)
                },
            ),
        )),
    ))(input)?;

    let range = StrRange::new(name.range.start, body.range.end);
    Ok((input, FunctionDef::new(name, params, body, range)))
}

#[derive(Clone, Debug, PartialEq)]
struct DataTypeNode {
    pub value: DataType,
    pub range: StrRange,
}

impl DataTypeNode {
    pub fn new(value: DataType, range: StrRange) -> DataTypeNode {
        DataTypeNode { value, range }
    }
}

fn datatype<'a>(input: Input<'a>, _state: &AstState) -> PineResult<'a, DataTypeNode> {
    let (input, label) = alt((
        tag("float"),
        tag("int"),
        tag("bool"),
        tag("color"),
        tag("string"),
        // tag("line"),
        // tag("label"),
    ))(input)?;
    let data_type = match label.src {
        "float" => DataType::Float,
        "int" => DataType::Int,
        "bool" => DataType::Bool,
        "color" => DataType::Color,
        "string" => DataType::String,
        // "line" => DataType::Line,
        // "label" => DataType::Label,
        _ => unreachable!(),
    };
    Ok((
        input,
        DataTypeNode::new(data_type, StrRange::from_input(&label)),
    ))
}

fn assign_lv_names<'a>(input: Input<'a>, state: &AstState) -> PineResult<'a, LVTupleNode<'a>> {
    alt((
        map(
            |s| varname_ws(s, state),
            |name| LVTupleNode::new(vec![name], name.range),
        ),
        |s| rettupledef(s, state),
    ))(input)
}

pub fn parse_assign<'a, F>(
    input: Input<'a>,
    state: &AstState,
    assign_fn: F,
) -> PineResult<'a, Assignment<'a>>
where
    F: Fn(Input<'a>, &AstState) -> PineResult<'a, Exp<'a>>,
{
    alt((
        map(
            tuple((
                tag("var"),
                eat_sep(|s| datatype(s, state)),
                |s| assign_lv_names(s, state),
                eat_sep(tag("=")),
                |s| assign_fn(s, state),
            )),
            |s| {
                let range = StrRange::new(s.0.start, s.4.range().end);
                Assignment::new(s.2.names, s.4, true, Some(s.1.value), range)
            },
        ),
        map(
            tuple((
                tag("var"),
                |s| assign_lv_names(s, state),
                eat_sep(tag("=")),
                |s| assign_fn(s, state),
            )),
            |s| {
                let range = StrRange::new(s.0.start, s.3.range().end);
                Assignment::new(s.1.names, s.3, true, None, range)
            },
        ),
        map(
            tuple((
                |s| datatype(s, state),
                |s| assign_lv_names(s, state),
                eat_sep(tag("=")),
                |s| assign_fn(s, state),
            )),
            |s| {
                let range = StrRange::new(s.0.range.start, s.3.range().end);
                Assignment::new(s.1.names, s.3, false, Some(s.0.value), range)
            },
        ),
        map(
            tuple((
                |s| assign_lv_names(s, state),
                eat_sep(tag("=")),
                |s| assign_fn(s, state),
            )),
            |s| {
                let range = StrRange::new(s.0.range.start, s.2.range().end);
                Assignment::new(s.0.names, s.2, false, None, range)
            },
        ),
    ))(input)
}

pub fn assign_expr<'a>(input: Input<'a>, state: &AstState) -> PineResult<'a, Assignment<'a>> {
    parse_assign(input, state, all_exp)
}

pub fn assign_stmt<'a>(input: Input<'a>, state: &AstState) -> PineResult<'a, Assignment<'a>> {
    preceded(statement_indent(state.get_indent()), |s| {
        parse_assign(s, state, exp_with_stmt_end)
    })(input)
}

pub fn parse_var_assign<'a, F>(
    input: Input<'a>,
    state: &AstState,
    assign_fn: F,
) -> PineResult<'a, VarAssignment<'a>>
where
    F: Fn(Input<'a>, &AstState) -> PineResult<'a, Exp<'a>>,
{
    map(
        tuple((
            |s| varname(s, state),
            eat_sep(tag(":=")),
            |input| assign_fn(input, state),
        )),
        |s| {
            let range = StrRange::new(s.0.range.start, s.2.range().end);
            VarAssignment::new(s.0, s.2, range)
        },
    )(input)
}

pub fn var_assign_expr<'a>(
    input: Input<'a>,
    state: &AstState,
) -> PineResult<'a, VarAssignment<'a>> {
    parse_var_assign(input, state, all_exp)
}

pub fn var_assign_stmt<'a>(
    input: Input<'a>,
    state: &AstState,
) -> PineResult<'a, VarAssignment<'a>> {
    preceded(statement_indent(state.get_indent()), |s| {
        parse_var_assign(s, state, exp_with_stmt_end)
    })(input)
}

fn block_with_indent<'a>(input: Input<'a>, state: &AstState) -> PineResult<'a, Block<'a>> {
    let gen_indent = statement_indent(state.get_indent());

    let mut stmts: Vec<Statement<'a>> = vec![];
    let mut cur_input = input;
    while cur_input.len() > 0 {
        if let Ok((next_input, stas)) = statement_with_indent(cur_input, state) {
            match stas {
                Statement::None(_) => (),
                _ => stmts.push(stas),
            }
            cur_input = next_input;
        } else {
            break;
        }
    }
    if cur_input.len() > 0 {
        if let Ok((next_input, ret_stmt)) =
            eat_statement(gen_indent, |input| exp_with_stmt_end(input, state))(cur_input)
        {
            let range = if stmts.is_empty() {
                ret_stmt.range()
            } else {
                StrRange::new(stmts[0].range().start, ret_stmt.range().end)
            };
            return Ok((next_input, Block::new(stmts, Some(ret_stmt), range)));
        }
    }
    if stmts.is_empty() {
        state.catch(PineInputError::new(
            PineErrorKind::BlockNoStmts,
            StrRange::new(input.start, input.start),
        ));
        Ok((
            cur_input,
            Block::new(stmts, None, StrRange::new(input.start, input.start)),
        ))
    } else {
        let range = StrRange::new(stmts[0].range().start, stmts.last().unwrap().range().end);
        Ok((cur_input, Block::new(stmts, None, range)))
    }
}

fn transfer_block_ret<'a>(mut blk: Block<'a>) -> Block<'a> {
    if blk.ret_stmt.is_some() {
        return blk;
    }
    match blk.stmts.last() {
        Some(&Statement::Ite(_)) | Some(&Statement::ForRange(_)) | Some(&Statement::Exp(_)) => {
            match blk.stmts.pop().unwrap() {
                Statement::Ite(mut s) => {
                    s.then_blk = transfer_block_ret(s.then_blk);
                    if let Some(else_blk) = s.else_blk {
                        s.else_blk = Some(transfer_block_ret(else_blk));
                    }
                    Block::new(blk.stmts, Some(Exp::Ite(s)), blk.range)
                }
                Statement::ForRange(mut s) => {
                    s.do_blk = transfer_block_ret(s.do_blk);
                    Block::new(blk.stmts, Some(Exp::ForRange(s)), blk.range)
                }
                Statement::Exp(s) => Block::new(blk.stmts, Some(s), blk.range),
                _ => unreachable!(),
            }
        }
        _ => blk,
    }
}

// parse block statements in the scope(e.g. if, for, function) for expression.
fn inner_block_for_exp<'a>(input: Input<'a>, state: &AstState) -> PineResult<'a, Block<'a>> {
    state.enter_scope();
    let (input, blk) = block_with_indent(input, state)?;
    state.exit_scope();
    Ok((input, transfer_block_ret(blk)))
}

// parse block statements in the scope(e.g. if, for, function) for statement.
fn inner_block_for_stmt<'a>(input: Input<'a>, state: &AstState) -> PineResult<'a, Block<'a>> {
    state.enter_scope();
    let (input, blk) = block_with_indent(input, state)?;
    state.exit_scope();
    Ok((input, blk))
}

fn statement_with_indent<'a>(input: Input<'a>, state: &AstState) -> PineResult<'a, Statement<'a>> {
    let gen_indent = || statement_indent(state.get_indent());
    alt((
        map(eat_statement(gen_indent(), tag("break")), |s| {
            Statement::Break(StrRange::from_input(&s))
        }),
        map(eat_statement(gen_indent(), tag("continue")), |s| {
            Statement::Continue(StrRange::from_input(&s))
        }),
        map(
            |input| if_then_else_with_indent(input, state),
            |s| Statement::Ite(Box::new(s)),
        ),
        map(
            |input| for_range_with_indent(input, state),
            |s| Statement::ForRange(Box::new(s)),
        ),
        map(statement_end, |s| Statement::None(StrRange::from_input(&s))),
        map(
            |input| function_def_with_indent(input, state),
            |s| Statement::FuncDef(Box::new(s)),
        ),
        // map(eat_statement(gen_indent(), |s| func_call(s, state)), |s| {
        //     Statement::FuncCall(Box::new(s))
        // }),
        map(
            |input| assign_stmt(input, state),
            |s| Statement::Assignment(Box::new(s)),
        ),
        map(
            |input| var_assign_stmt(input, state),
            |s| Statement::VarAssignment(Box::new(s)),
        ),
        map(
            preceded(gen_indent(), |input| exp_with_stmt_end(input, state)),
            |s| Statement::Exp(s),
        ),
    ))(input)
}

pub fn statement<'a>(input: Input<'a>, state: &AstState) -> PineResult<'a, Statement<'a>> {
    statement_with_indent(input, state)
}

pub fn block<'a>(input: Input<'a>, state: &AstState) -> PineResult<'a, Block<'a>> {
    block_with_indent(input, state)
}

#[cfg(test)]
mod tests {
    use super::super::input::Position;
    use super::super::name::VarName;
    use super::super::num::{IntNode, Numeral};
    use super::*;
    use std::fmt::Debug;

    fn check_res_input<'a, F, O>(s: &'a str, handler: F, res: O, res_input: &'a str)
    where
        F: Fn(Input<'a>, &AstState) -> PineResult<'a, O>,
        O: Debug + PartialEq,
    {
        let test_input = Input::new_with_str(s);
        let input_len = test_input.len() - res_input.len();
        let consume_input = Input::new_with_start(&s[..input_len], Position::new(0, 0));
        assert_eq!(
            handler(test_input, &AstState::new()),
            Ok((
                Input::new(res_input, consume_input.end, Position::max()),
                res
            ))
        );
    }

    fn check_res<'a, F, O>(s: &'a str, handler: F, res: O)
    where
        F: Fn(Input<'a>, &AstState) -> PineResult<'a, O>,
        O: Debug + PartialEq,
    {
        check_res_input(s, handler, res, "")
    }

    #[test]
    fn string_test() {
        use crate::ast::string::*;

        check_res(
            r#""hello""#,
            all_exp,
            Exp::Str(StringNode::new(
                String::from("hello"),
                StrRange::new(Position::new(0, 1), Position::new(0, 6)),
            )),
        );
    }

    #[test]
    fn rettupledef_test() {
        check_res(
            " [hello, good]",
            rettupledef,
            LVTupleNode::new(
                vec![
                    VarName::new_with_start("hello", Position::new(0, 2)),
                    VarName::new_with_start("good", Position::new(0, 9)),
                ],
                StrRange::from_start("[hello, good]", Position::new(0, 1)),
            ),
        );
        check_res_input(
            " [hello, good,  my]hello",
            rettupledef,
            LVTupleNode::new(
                vec![
                    VarName::new_with_start("hello", Position::new(0, 2)),
                    VarName::new_with_start("good", Position::new(0, 9)),
                    VarName::new_with_start("my", Position::new(0, 16)),
                ],
                StrRange::from_start("[hello, good,  my]", Position::new(0, 1)),
            ),
            "hello",
        );

        check_res(
            " [ hello  , good ]",
            rettupledef,
            LVTupleNode::new(
                vec![
                    VarName::new_with_start("hello", Position::new(0, 3)),
                    VarName::new_with_start("good", Position::new(0, 12)),
                ],
                StrRange::from_start("[ hello  , good ]", Position::new(0, 1)),
            ),
        );
    }

    #[test]
    fn tupledef_test() {
        check_res(
            " [ hello , true ]",
            tupledef,
            TupleNode::new(
                vec![
                    Exp::VarName(RVVarName::new_with_start("hello", Position::new(0, 3))),
                    Exp::Bool(BoolNode::new(
                        true,
                        StrRange::from_start("true", Position::new(0, 11)),
                    )),
                ],
                StrRange::from_start("[ hello , true ]", Position::new(0, 1)),
            ),
        );
    }

    #[test]
    fn ref_call_test() {
        check_res(
            "hello[true]",
            prefix_ref_func_call,
            Exp::RefCall(Box::new(RefCall::new(
                Exp::VarName(RVVarName::new_with_start("hello", Position::new(0, 0))),
                Exp::Bool(BoolNode::new(
                    true,
                    StrRange::from_start("true", Position::new(0, 6)),
                )),
                StrRange::from_start("hello[true]", Position::new(0, 0)),
            ))),
        );
    }

    #[test]
    fn condition_test() {
        check_res(
            "a ? b : c",
            condition,
            Condition::new(
                Exp::VarName(RVVarName::new_with_start("a", Position::new(0, 0))),
                Exp::VarName(RVVarName::new_with_start("b", Position::new(0, 4))),
                Exp::VarName(RVVarName::new_with_start("c", Position::new(0, 8))),
                StrRange::from_start("a ? b : c", Position::new(0, 0)),
            ),
        );
    }

    #[test]
    fn condition_statement_test() {
        check_res(
            "m = a ? b : c \n",
            statement_with_indent,
            Statement::Assignment(Box::new(Assignment::new(
                vec![VarName::new_with_start("m", Position::new(0, 0))],
                Exp::Condition(Box::new(Condition::new(
                    Exp::VarName(RVVarName::new_with_start("a", Position::new(0, 4))),
                    Exp::VarName(RVVarName::new_with_start("b", Position::new(0, 8))),
                    Exp::VarName(RVVarName::new_with_start("c", Position::new(0, 12))),
                    StrRange::from_start("a ? b : c", Position::new(0, 4)),
                ))),
                false,
                None,
                StrRange::from_start("m = a ? b : c", Position::new(0, 0)),
            ))),
        );
        check_res(
            "m = na \n",
            statement_with_indent,
            Statement::Assignment(Box::new(Assignment::new(
                vec![VarName::new_with_start("m", Position::new(0, 0))],
                Exp::Na(NaNode::new(StrRange::from_start("na", Position::new(0, 4)))),
                false,
                None,
                StrRange::from_start("m = na", Position::new(0, 0)),
            ))),
        );
    }

    #[test]
    fn statement_test() {
        check_res(
            "    break \n",
            |s, state| {
                state.enter_scope();
                statement_with_indent(s, state)
            },
            Statement::Break(StrRange::from_start("break", Position::new(0, 4))),
        );

        check_res(
            "a = b \n",
            statement_with_indent,
            Statement::Assignment(Box::new(Assignment::new(
                vec![VarName::new_with_start("a", Position::new(0, 0))],
                Exp::VarName(RVVarName::new_with_start("b", Position::new(0, 4))),
                false,
                None,
                StrRange::from_start("a = b", Position::new(0, 0)),
            ))),
        );
        check_res(
            "    a(arg1) \n",
            |s, state| {
                state.enter_scope();
                statement_with_indent(s, state)
            },
            Statement::Exp(Exp::FuncCall(Box::new(FunctionCall::new_no_ctxid(
                Exp::VarName(RVVarName::new_with_start("a", Position::new(0, 4))),
                vec![Exp::VarName(RVVarName::new_with_start(
                    "arg1",
                    Position::new(0, 6),
                ))],
                vec![],
                StrRange::from_start("a(arg1)", Position::new(0, 4)),
            )))),
        );
        check_res(
            "    a(arg1) => b \n",
            |s, state| {
                state.enter_scope();
                statement_with_indent(s, state)
            },
            Statement::FuncDef(Box::new(FunctionDef::new(
                VarName::new_with_start("a", Position::new(0, 4)),
                vec![VarName::new_with_start("arg1", Position::new(0, 6))],
                Block::new(
                    vec![],
                    Some(Exp::VarName(RVVarName::new_with_start(
                        "b",
                        Position::new(0, 15),
                    ))),
                    StrRange::from_start("b", Position::new(0, 15)),
                ),
                StrRange::from_start("a(arg1) => b", Position::new(0, 4)),
            ))),
        );

        check_res(
            "    a(arg1) => \n        b \n",
            |s, state| {
                state.enter_scope();
                statement_with_indent(s, state)
            },
            Statement::FuncDef(Box::new(FunctionDef::new(
                VarName::new_with_start("a", Position::new(0, 4)),
                vec![VarName::new_with_start("arg1", Position::new(0, 6))],
                Block::new(
                    vec![],
                    Some(Exp::VarName(RVVarName::new_with_start(
                        "b",
                        Position::new(1, 8),
                    ))),
                    StrRange::from_start("b", Position::new(1, 8)),
                ),
                StrRange::from_start("a(arg1) => \n        b", Position::new(0, 4)),
            ))),
        );

        check_res_input(
            "    //helo world \na = close",
            |s, state| {
                state.enter_scope();
                statement_with_indent(s, state)
            },
            Statement::None(StrRange::from_start(
                "    //helo world \n",
                Position::new(0, 0),
            )),
            "a = close",
        );
    }

    #[test]
    fn assignment_test() {
        check_res(
            "a = close",
            all_exp,
            Exp::Assignment(Box::new(Assignment::new(
                vec![VarName::new_with_start("a", Position::new(0, 0))],
                Exp::VarName(RVVarName::new_with_start("close", Position::new(0, 4))),
                false,
                None,
                StrRange::from_start("a = close", Position::new(0, 0)),
            ))),
        );
        check_res(
            "a = close // This is also a comment\n",
            statement_with_indent,
            Statement::Assignment(Box::new(Assignment::new(
                vec![VarName::new_with_start("a", Position::new(0, 0))],
                Exp::VarName(RVVarName::new_with_start("close", Position::new(0, 4))),
                false,
                None,
                StrRange::from_start("a = close", Position::new(0, 0)),
            ))),
        );
        check_res(
            "var a = close",
            all_exp,
            Exp::Assignment(Box::new(Assignment::new(
                vec![VarName::new_with_start("a", Position::new(0, 4))],
                Exp::VarName(RVVarName::new_with_start("close", Position::new(0, 8))),
                true,
                None,
                StrRange::from_start("var a = close", Position::new(0, 0)),
            ))),
        );
        check_res(
            "var a = close",
            statement,
            Statement::Assignment(Box::new(Assignment::new(
                vec![VarName::new_with_start("a", Position::new(0, 4))],
                Exp::VarName(RVVarName::new_with_start("close", Position::new(0, 8))),
                true,
                None,
                StrRange::from_start("var a = close", Position::new(0, 0)),
            ))),
        );
        check_res(
            "var float a = close",
            all_exp,
            Exp::Assignment(Box::new(Assignment::new(
                vec![VarName::new_with_start("a", Position::new(0, 10))],
                Exp::VarName(RVVarName::new_with_start("close", Position::new(0, 14))),
                true,
                Some(DataType::Float),
                StrRange::from_start("var float a = close", Position::new(0, 0)),
            ))),
        );
        check_res(
            "a = b = close // This is also a comment\n",
            statement_with_indent,
            Statement::Assignment(Box::new(Assignment::new(
                vec![VarName::new_with_start("a", Position::new(0, 0))],
                Exp::Assignment(Box::new(Assignment::new(
                    vec![VarName::new_with_start("b", Position::new(0, 4))],
                    Exp::VarName(RVVarName::new_with_start("close", Position::new(0, 8))),
                    false,
                    None,
                    StrRange::from_start("b = close", Position::new(0, 4)),
                ))),
                false,
                None,
                StrRange::from_start("a = b = close", Position::new(0, 0)),
            ))),
        );
        check_res(
            "a := close\n",
            statement_with_indent,
            Statement::VarAssignment(Box::new(VarAssignment::new(
                VarName::new_with_start("a", Position::new(0, 0)),
                Exp::VarName(RVVarName::new_with_start("close", Position::new(0, 5))),
                StrRange::from_start("a := close", Position::new(0, 0)),
            ))),
        );
    }

    #[test]
    fn prefix_exp_test() {
        let prefix_exp = Exp::PrefixExp(Box::new(PrefixExp::new(
            Exp::PrefixExp(Box::new(PrefixExp::new(
                Exp::VarName(RVVarName::new_with_start("a", Position::new(0, 0))),
                VarName::new_with_start("b", Position::new(0, 2)),
                StrRange::from_start("a.b", Position::new(0, 0)),
            ))),
            VarName::new_with_start("c", Position::new(0, 4)),
            StrRange::from_start("a.b.c", Position::new(0, 0)),
        )));
        check_res("a.b.c", prefix_ref_func_call, prefix_exp.clone());
        check_res("a.b.c", all_exp, prefix_exp.clone());

        check_res(
            "m = a.b.c \n",
            statement_with_indent,
            Statement::Assignment(Box::new(Assignment::new(
                vec![VarName::new_with_start("m", Position::new(0, 0))],
                Exp::PrefixExp(Box::new(PrefixExp::new(
                    Exp::PrefixExp(Box::new(PrefixExp::new(
                        Exp::VarName(RVVarName::new_with_start("a", Position::new(0, 4))),
                        VarName::new_with_start("b", Position::new(0, 6)),
                        StrRange::from_start("a.b", Position::new(0, 4)),
                    ))),
                    VarName::new_with_start("c", Position::new(0, 8)),
                    StrRange::from_start("a.b.c", Position::new(0, 4)),
                ))),
                false,
                None,
                StrRange::from_start("m = a.b.c", Position::new(0, 0)),
            ))),
        )
    }

    #[test]
    fn func_call_test() {
        check_res(
            "funa.funb()",
            prefix_ref_func_call,
            Exp::FuncCall(Box::new(FunctionCall::new_no_ctxid(
                Exp::PrefixExp(Box::new(PrefixExp::new(
                    Exp::VarName(RVVarName::new(VarName::new(
                        "funa",
                        StrRange::from_start("funa", Position::new(0, 0)),
                    ))),
                    VarName::new("funb", StrRange::from_start("funb", Position::new(0, 5))),
                    StrRange::from_start("funa.funb", Position::new(0, 0)),
                ))),
                vec![],
                vec![],
                StrRange::from_start("funa.funb()", Position::new(0, 0)),
            ))),
        );
    }

    #[test]
    fn block_test() {
        check_res_input(
            "    break \n    continue \n    true \nhello",
            |s, state| {
                state.enter_scope();
                block_with_indent(s, state)
            },
            Block::new(
                vec![
                    Statement::Break(StrRange::from_start("break", Position::new(0, 4))),
                    Statement::Continue(StrRange::from_start("continue", Position::new(1, 4))),
                    Statement::Exp(Exp::Bool(BoolNode::new(
                        true,
                        StrRange::from_start("true", Position::new(2, 4)),
                    ))),
                ],
                None,
                StrRange::new(Position::new(0, 4), Position::new(2, 8)),
            ),
            "hello",
        );
    }

    #[test]
    fn if_then_else_test() {
        check_res(
            "if true \n    break\n    true  \n",
            if_then_else_exp,
            IfThenElse::new_no_ctxid(
                Exp::Bool(BoolNode::new(
                    true,
                    StrRange::from_start("true", Position::new(0, 3)),
                )),
                Block::new(
                    vec![Statement::Break(StrRange::from_start(
                        "break",
                        Position::new(1, 4),
                    ))],
                    Some(Exp::Bool(BoolNode::new(
                        true,
                        StrRange::from_start("true", Position::new(2, 4)),
                    ))),
                    StrRange::new(Position::new(1, 4), Position::new(2, 8)),
                ),
                None,
                StrRange::new(Position::new(0, 0), Position::new(2, 8)),
            ),
        );
    }

    #[test]
    fn for_range_test() {
        check_res(
            "for a = 1 to 2 \n    break\n    true  \n",
            for_range_exp,
            ForRange::new_no_ctxid(
                VarName::new_with_start("a", Position::new(0, 4)),
                Exp::Num(Numeral::Int(IntNode::new(
                    1,
                    StrRange::from_start("1", Position::new(0, 8)),
                ))),
                Exp::Num(Numeral::Int(IntNode::new(
                    2,
                    StrRange::from_start("2", Position::new(0, 13)),
                ))),
                None,
                Block::new(
                    vec![Statement::Break(StrRange::from_start(
                        "break",
                        Position::new(1, 4),
                    ))],
                    Some(Exp::Bool(BoolNode::new(
                        true,
                        StrRange::from_start("true", Position::new(2, 4)),
                    ))),
                    StrRange::new(Position::new(1, 4), Position::new(2, 8)),
                ),
                StrRange::new(Position::new(0, 0), Position::new(2, 8)),
            ),
        );
    }

    #[test]
    fn func_def_test() {
        check_res(
            "a(arg1) => \n    if b\n        c \n",
            function_def_with_indent,
            FunctionDef::new(
                VarName::new_with_start("a", Position::new(0, 0)),
                vec![VarName::new_with_start("arg1", Position::new(0, 2))],
                Block::new(
                    vec![],
                    Some(Exp::Ite(Box::new(IfThenElse::new_no_ctxid(
                        Exp::VarName(RVVarName::new_with_start("b", Position::new(1, 7))),
                        Block::new(
                            vec![],
                            Some(Exp::VarName(RVVarName::new_with_start(
                                "c",
                                Position::new(2, 8),
                            ))),
                            StrRange::from_start("c", Position::new(2, 8)),
                        ),
                        None,
                        StrRange::new(Position::new(1, 4), Position::new(2, 9)),
                    )))),
                    StrRange::new(Position::new(1, 4), Position::new(2, 9)),
                ),
                StrRange::new(Position::new(0, 0), Position::new(2, 9)),
            ),
        );

        check_res(
            "a(arg1) => \n    arg1",
            function_def_with_indent,
            FunctionDef::new(
                VarName::new_with_start("a", Position::new(0, 0)),
                vec![VarName::new_with_start("arg1", Position::new(0, 2))],
                Block::new(
                    vec![],
                    Some(Exp::VarName(RVVarName::new_with_start(
                        "arg1",
                        Position::new(1, 4),
                    ))),
                    StrRange::new(Position::new(1, 4), Position::new(1, 8)),
                ),
                StrRange::new(Position::new(0, 0), Position::new(1, 8)),
            ),
        );
    }

    #[test]
    fn if_then_else_exp_test() {
        check_res(
            "if a \n    if b \n        c\n    else\n        d",
            if_then_else_exp,
            IfThenElse::new_no_ctxid(
                Exp::VarName(RVVarName::new_with_start("a", Position::new(0, 3))),
                Block::new(
                    vec![],
                    Some(Exp::Ite(Box::new(IfThenElse::new_no_ctxid(
                        Exp::VarName(RVVarName::new_with_start("b", Position::new(1, 7))),
                        Block::new(
                            vec![],
                            Some(Exp::VarName(RVVarName::new_with_start(
                                "c",
                                Position::new(2, 8),
                            ))),
                            StrRange::from_start("c", Position::new(2, 8)),
                        ),
                        Some(Block::new(
                            vec![],
                            Some(Exp::VarName(RVVarName::new_with_start(
                                "d",
                                Position::new(4, 8),
                            ))),
                            StrRange::from_start("d", Position::new(4, 8)),
                        )),
                        StrRange::new(Position::new(1, 4), Position::new(4, 9)),
                    )))),
                    StrRange::new(Position::new(1, 4), Position::new(4, 9)),
                ),
                None,
                StrRange::new(Position::new(0, 0), Position::new(4, 9)),
            ),
        );
    }

    #[test]
    fn for_range_exp_test() {
        let int_exp = |i, s, e| {
            Exp::Num(Numeral::Int(IntNode::new(
                i,
                StrRange::from_start("1", Position::new(s, e)),
            )))
        };

        check_res(
            "for i = 1 to 2\n    for i = 1 to 2\n        for i = 1 to 2\n            i",
            for_range_exp,
            ForRange::new_no_ctxid(
                VarName::new_with_start("i", Position::new(0, 4)),
                int_exp(1, 0, 8),
                int_exp(2, 0, 13),
                None,
                Block::new(
                    vec![],
                    Some(Exp::ForRange(Box::new(ForRange::new_no_ctxid(
                        VarName::new_with_start("i", Position::new(1, 8)),
                        int_exp(1, 1, 12),
                        int_exp(2, 1, 17),
                        None,
                        Block::new(
                            vec![],
                            Some(Exp::ForRange(Box::new(ForRange::new_no_ctxid(
                                VarName::new_with_start("i", Position::new(2, 12)),
                                int_exp(1, 2, 16),
                                int_exp(2, 2, 21),
                                None,
                                Block::new(
                                    vec![],
                                    Some(Exp::VarName(RVVarName::new_with_start(
                                        "i",
                                        Position::new(3, 12),
                                    ))),
                                    StrRange::from_start("i", Position::new(3, 12)),
                                ),
                                StrRange::from_start(
                                    "for i = 1 to 2\n            i",
                                    Position::new(2, 8),
                                ),
                            )))),
                            StrRange::new(Position::new(2, 8), Position::new(3, 13)),
                        ),
                        StrRange::new(Position::new(1, 4), Position::new(3, 13)),
                    )))),
                    StrRange::new(Position::new(1, 4), Position::new(3, 13)),
                ),
                StrRange::new(Position::new(0, 0), Position::new(3, 13)),
            ),
        );
    }

    #[test]
    fn prefix_ref_func_call_test() {
        check_res(
            "sma(close, 10)[1]",
            prefix_ref_func_call,
            Exp::RefCall(Box::new(RefCall::new(
                Exp::FuncCall(Box::new(FunctionCall::new(
                    Exp::VarName(RVVarName::new_with_start("sma", Position::new(0, 0))),
                    vec![
                        Exp::VarName(RVVarName::new_with_start("close", Position::new(0, 4))),
                        Exp::Num(Numeral::Int(IntNode::new(
                            10,
                            StrRange::from_start("10", Position::new(0, 11)),
                        ))),
                    ],
                    vec![],
                    0,
                    StrRange::from_start("sma(close, 10)", Position::new(0, 0)),
                ))),
                Exp::Num(Numeral::Int(IntNode::new(
                    1,
                    StrRange::from_start("1", Position::new(0, 15)),
                ))),
                StrRange::from_start("sma(close, 10)[1]", Position::new(0, 0)),
            ))),
        );

        let test_input = Input::new_with_str("sma.mm(close, 10)[1]");
        let (input, _output) = prefix_ref_func_call(test_input, &AstState::new()).unwrap();
        assert_eq!(input.src, "");

        let test_input = Input::new_with_str("sma.mm[1]");
        let (input, _output) = prefix_ref_func_call(test_input, &AstState::new()).unwrap();
        assert_eq!(input.src, "");
    }

    #[test]
    fn expr_stmt_test() {
        let test_input = Input::new_with_str("hello\nprint(ma)\n");
        let (input, _output) = block(test_input, &AstState::new()).unwrap();
        assert_eq!(input.src, "");

        let test_input = Input::new_with_str("color.new(c, 50)");
        let (input, _output) = all_exp(test_input, &AstState::new()).unwrap();
        assert_eq!(input.src, "");

        let test_input = Input::new_with_str(
            "bgColor = (dayofweek == dayofweek) ? color.new(c, 50) : color.new(color.blue, 80)",
        );
        let (input, _output) = all_exp(test_input, &AstState::new()).unwrap();
        assert_eq!(input.src, "");

        let test_input = Input::new_with_str("na(myVar) ? 0 : close");
        let (input, _output) = condition(test_input, &AstState::new()).unwrap();
        assert_eq!(input.src, "");

        let test_input = Input::new_with_str("myClose = na(myVar) ? 0 : close");
        let (input, _output) = block(test_input, &AstState::new()).unwrap();
        assert_eq!(input.src, "");

        let test_input = Input::new_with_str("sma(close, 10)[1]");
        let (input, _output) = block(test_input, &AstState::new()).unwrap();
        assert_eq!(input.src, "");
    }

    #[test]
    fn not_end_func_test() {
        let test_input = Input::new_with_str("fun(x, y) =>\n");
        let result = block(test_input, &AstState::new());
        let (input, _output) = result.unwrap();
        assert_eq!(input.src, "");
    }

    #[test]
    fn exp_error_test() {
        let test_input = Input::new_with_str("a1 = 1\na = 1;\nb = 1");
        let result = block(test_input, &AstState::new());
        let (input, _output) = result.unwrap();
        assert_eq!(input.start.get_line(), 1);
    }
}
