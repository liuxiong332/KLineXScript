use super::color::color_lit;
use super::error::{PineError, PineErrorKind, PineResult};
use super::func_call::{func_call, func_call_ws};
use super::input::{Input, StrRange};
use super::name::{varname, varname_ws, VarName};
use super::num::{int_lit_ws, num_lit_ws};
use super::op::*;
use super::stat_expr_types::*;
use super::string::string_lit;
use super::trans::flatexp_from_components;
use super::utils::{eat_sep, eat_statement, statement_end, statement_indent};
use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::{map, opt, value},
    multi::{many0, separated_list},
    sequence::{delimited, preceded, terminated, tuple},
    Err,
};

// exp2 contain the expressions that can apply the binary operators(+,-,*,/) and unary operators(+,-)
pub fn exp2(input: Input) -> PineResult<Exp2> {
    alt((
        map(eat_sep(tag("na")), |s| {
            Exp2::Na(NaNode::new(StrRange::from_input(&s)))
        }),
        map(eat_sep(tag("true")), |s| {
            Exp2::Bool(BoolNode::new(true, StrRange::from_input(&s)))
        }),
        map(eat_sep(tag("false")), |s| {
            Exp2::Bool(BoolNode::new(false, StrRange::from_input(&s)))
        }),
        map(num_lit_ws, Exp2::Num),
        map(string_lit, Exp2::Str),
        map(color_lit, Exp2::Color),
        map(bracket_expr, Exp2::Exp),
        // map(rettupledef, |varnames| Exp2::RetTuple(Box::new(varnames))), // match [a, b]
        map(tupledef, |exps| Exp2::Tuple(Box::new(exps))), // match [a, b + c]
        map(type_cast, |exp| Exp2::TypeCast(Box::new(exp))), // match float(b)
        map(prefix_exp_ws, |exp| Exp2::PrefixExp(Box::new(exp))), // match a.b.c
        map(func_call_ws, |exp| Exp2::FuncCall(Box::new(exp))), // match a(b)
        map(ref_call, |exp| Exp2::RefCall(Box::new(exp))), // match a[b]
        map(varname_ws, Exp2::VarName),                    // match a
    ))(input)
}

pub fn unopexp2(input: Input) -> PineResult<UnOpExp2> {
    let (input, (ops, exp)) = tuple((many0(unary_op), exp2))(input)?;
    let range = if ops.is_empty() {
        exp.range()
    } else {
        StrRange::new(ops[0].range.start, exp.range().end)
    };
    Ok((input, UnOpExp2::new(ops, exp, range)))
}

pub fn flatexp(input: Input) -> PineResult<FlatExp> {
    let (input, head) = unopexp2(input)?;
    let (input, binop_chain) = many0(tuple((binary_op, unopexp2)))(input)?;
    Ok((input, flatexp_from_components(head, binop_chain)))
}

pub fn exp(input: Input) -> PineResult<Exp> {
    alt((
        map(condition, |exp| Exp::Condition(Box::new(exp))), // match a ? b : c
        map(flatexp, Exp::from),
    ))(input)
}

// The left return tuple of expression `[a, b] = [1, 2]` that contain variable name between square brackets
fn rettupledef(input: Input) -> PineResult<LVTupleNode> {
    let (input, (paren_l, names, paren_r)) = eat_sep(tuple((
        eat_sep(tag("[")),
        separated_list(eat_sep(tag(",")), varname_ws),
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
fn tupledef(input: Input) -> PineResult<TupleNode> {
    let (input, (paren_l, items, paren_r)) = eat_sep(tuple((
        eat_sep(tag("[")),
        separated_list(eat_sep(tag(",")), exp),
        eat_sep(tag("]")),
    )))(input)?;
    Ok((
        input,
        TupleNode::new(items, StrRange::new(paren_l.start, paren_r.end)),
    ))
}

fn type_cast(input: Input) -> PineResult<TypeCast> {
    let (input, (data_type, _, e, end_tag)) =
        eat_sep(tuple((datatype, eat_sep(tag("(")), exp, eat_sep(tag(")")))))(input)?;
    Ok((
        input,
        TypeCast::new(
            data_type.value,
            e,
            StrRange::new(data_type.range.start, end_tag.end),
        ),
    ))
}

pub fn callable_expr(input: Input) -> PineResult<Exp> {
    alt((
        delimited(tag("("), exp, eat_sep(tag(")"))),
        map(prefix_exp, |exp| Exp::PrefixExp(Box::new(exp))), // match a.b.c
        map(varname, Exp::VarName),                           // match a
    ))(input)
}

fn ref_call(input: Input) -> PineResult<RefCall> {
    let (input, (name, (_, arg, paren_r))) = tuple((
        eat_sep(callable_expr),
        tuple((eat_sep(tag("[")), exp, eat_sep(tag("]")))),
    ))(input)?;

    let range = StrRange::new(name.range().start, paren_r.end);
    Ok((input, RefCall::new(name, arg, range)))
}

fn bracket_expr(input: Input) -> PineResult<Exp> {
    delimited(eat_sep(tag("(")), exp, eat_sep(tag(")")))(input)
}

fn condition(input: Input) -> PineResult<Condition> {
    let (input, (cond, _, exp1, _, exp2)) = tuple((
        map(flatexp, |s| Exp::from(s)),
        eat_sep(tag("?")),
        exp,
        eat_sep(tag(":")),
        exp,
    ))(input)?;

    let range = StrRange::new(cond.range().start, exp2.range().end);
    Ok((input, Condition::new(cond, exp1, exp2, range)))
}

fn prefix_exp(input: Input) -> PineResult<PrefixExp> {
    let (input, (prefix, _, names)) =
        tuple((varname, tag("."), separated_list(tag("."), varname)))(input)?;

    if names.len() == 0 {
        Err(Err::Error(PineError::from_pine_kind(
            input,
            PineErrorKind::PrefixNoNamesAfterDot,
        )))
    } else {
        let range = StrRange::new(prefix.range.start, names.last().unwrap().range.end);
        Ok((input, PrefixExp::new([vec![prefix], names].concat(), range)))
    }
}

fn prefix_exp_ws(input: Input) -> PineResult<PrefixExp> {
    eat_sep(prefix_exp)(input)
}

fn if_then_else<'a>(indent: usize) -> impl Fn(Input<'a>) -> PineResult<IfThenElse> {
    move |input: Input<'a>| {
        let (input, (if_tag, cond, _, then_block, else_block)) = tuple((
            tag("if"),
            exp,
            statement_end,
            block_with_indent(indent + 1),
            opt(tuple((
                preceded(statement_indent(indent), tag("else")),
                statement_end,
                block_with_indent(indent + 1),
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

fn if_then_else_with_indent<'a>(indent: usize) -> impl Fn(Input<'a>) -> PineResult<IfThenElse> {
    move |input: Input<'a>| preceded(statement_indent(indent), if_then_else(indent))(input)
}

fn for_range<'a>(indent: usize) -> impl Fn(Input<'a>) -> PineResult<ForRange> {
    move |input: Input<'a>| {
        let (input, (for_tag, var, _, start, _, end, by, _, do_blk)) = tuple((
            tag("for"),
            varname_ws,
            eat_sep(tag("=")),
            exp, // int_lit_ws,
            eat_sep(tag("to")),
            exp, // int_lit_ws,
            opt(tuple((eat_sep(tag("by")), exp))),
            statement_end,
            block_with_indent(indent + 1),
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

fn for_range_with_indent<'a>(indent: usize) -> impl Fn(Input<'a>) -> PineResult<ForRange> {
    move |input: Input<'a>| preceded(statement_indent(indent), for_range(indent))(input)
}

fn function_def_with_indent<'a>(indent: usize) -> impl Fn(Input<'a>) -> PineResult<FunctionDef> {
    move |input: Input<'a>| {
        let (input, (_, name, _, params, _, _, body)) = tuple((
            statement_indent(indent),
            varname,
            eat_sep(tag("(")),
            separated_list(eat_sep(tag(",")), varname_ws),
            eat_sep(tag(")")),
            eat_sep(tag("=>")),
            alt((
                preceded(statement_end, block_with_indent(indent + 1)),
                map(terminated(exp, statement_end), |s| Block {
                    stmts: vec![],
                    range: s.range(),
                    ret_stmt: Some(s),
                }),
            )),
        ))(input)?;

        let range = StrRange::new(name.range.start, body.range.end);
        Ok((
            input,
            FunctionDef {
                name,
                params,
                body,
                range,
            },
        ))
    }
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

fn datatype(input: Input) -> PineResult<DataTypeNode> {
    let (input, label) = alt((
        tag("float"),
        tag("int"),
        tag("bool"),
        tag("color"),
        tag("string"),
        tag("line"),
        tag("label"),
    ))(input)?;
    let data_type = match label.src {
        "float" => DataType::Float,
        "int" => DataType::Int,
        "bool" => DataType::Bool,
        "color" => DataType::Color,
        "string" => DataType::String,
        "line" => DataType::Line,
        "label" => DataType::Label,
        _ => unreachable!(),
    };
    Ok((
        input,
        DataTypeNode::new(data_type, StrRange::from_input(&label)),
    ))
}

pub fn exp_with_indent<'a>(indent: usize) -> impl Fn(Input<'a>) -> PineResult<Exp> {
    move |input: Input<'a>| {
        alt((
            terminated(exp, statement_end),
            map(eat_sep(if_then_else(indent)), |s| Exp::Ite(Box::new(s))),
            map(eat_sep(for_range(indent)), |s| Exp::ForRange(Box::new(s))),
        ))(input)
    }
}

fn assign_lv_names<'a>(input: Input<'a>) -> PineResult<LVTupleNode> {
    alt((
        map(varname_ws, |name| LVTupleNode::new(vec![name], name.range)),
        rettupledef,
    ))(input)
}

fn assign_with_indent<'a>(indent: usize) -> impl Fn(Input<'a>) -> PineResult<Assignment> {
    move |input: Input<'a>| {
        let exp_parser = || exp_with_indent(indent);
        alt((
            map(
                tuple((
                    tag("var"),
                    eat_sep(datatype),
                    assign_lv_names,
                    eat_sep(tag("=")),
                    exp_parser(),
                )),
                |s| {
                    let range = StrRange::new(s.0.start, s.4.range().end);
                    Assignment::new(s.2.names, s.4, true, Some(s.1.value), range)
                },
            ),
            map(
                tuple((tag("var"), assign_lv_names, eat_sep(tag("=")), exp_parser())),
                |s| {
                    let range = StrRange::new(s.0.start, s.3.range().end);
                    Assignment::new(s.1.names, s.3, true, None, range)
                },
            ),
            map(
                tuple((datatype, assign_lv_names, eat_sep(tag("=")), exp_parser())),
                |s| {
                    let range = StrRange::new(s.0.range.start, s.3.range().end);
                    Assignment::new(s.1.names, s.3, false, Some(s.0.value), range)
                },
            ),
            map(
                tuple((assign_lv_names, eat_sep(tag("=")), exp_parser())),
                |s| {
                    let range = StrRange::new(s.0.range.start, s.2.range().end);
                    Assignment::new(s.0.names, s.2, false, None, range)
                },
            ),
        ))(input)
    }
}

fn var_assign_with_indent<'a>(indent: usize) -> impl Fn(Input<'a>) -> PineResult<VarAssignment> {
    move |input: Input<'a>| {
        map(
            tuple((varname, eat_sep(tag(":=")), exp_with_indent(indent))),
            |s| {
                let range = StrRange::new(s.0.range.start, s.2.range().end);
                VarAssignment::new(s.0, s.2, range)
            },
        )(input)
    }
}

fn block_with_indent<'a>(indent: usize) -> impl Fn(Input<'a>) -> PineResult<Block> {
    move |input: Input<'a>| {
        let gen_indent = statement_indent(indent);

        let mut stmts: Vec<Statement<'a>> = vec![];
        let mut cur_input = input;
        while cur_input.len() > 0 {
            if let Ok((next_input, stas)) = statement_with_indent(indent)(cur_input) {
                stmts.push(stas);
                cur_input = next_input;
            } else {
                break;
            }
        }
        if cur_input.len() > 0 {
            if let Ok((next_input, ret_stmt)) = eat_statement(gen_indent, exp)(cur_input) {
                let range = if stmts.is_empty() {
                    ret_stmt.range()
                } else {
                    StrRange::new(stmts[0].range().start, ret_stmt.range().end)
                };
                return Ok((next_input, Block::new(stmts, Some(ret_stmt), range)));
            }
        }
        // let block = match stmts.last() {
        //     Some(&Statement::Ite(_)) | Some(&Statement::ForRange(_)) => {
        //         match stmts.pop().unwrap() {
        //             Statement::Ite(s) => Block::new(stmts, Some(Exp::Ite(s))),
        //             Statement::ForRange(s) => Block::new(stmts, Some(Exp::ForRange(s))),
        //             _ => unreachable!(),
        //         }
        //     }
        //     _ => Block::new(stmts, None),
        // };
        if stmts.is_empty() {
            Err(Err::Error(PineError::from_pine_kind(
                input,
                PineErrorKind::BlockNoStmts,
            )))
        } else {
            let range = StrRange::new(stmts[0].range().start, stmts.last().unwrap().range().end);
            Ok((cur_input, Block::new(stmts, None, range)))
        }
    }
}

fn statement_with_indent<'a>(indent: usize) -> impl Fn(Input<'a>) -> PineResult<'a, Statement<'a>> {
    let gen_indent = statement_indent(indent);
    move |input: Input<'a>| -> PineResult<'a, Statement<'a>> {
        alt((
            map(eat_statement(&gen_indent, tag("break")), |s| {
                Statement::Break(StrRange::from_input(&s))
            }),
            map(eat_statement(&gen_indent, tag("continue")), |s| {
                Statement::Continue(StrRange::from_input(&s))
            }),
            map(if_then_else_with_indent(indent), |s| {
                Statement::Ite(Box::new(s))
            }),
            map(for_range_with_indent(indent), |s| {
                Statement::ForRange(Box::new(s))
            }),
            map(statement_end, |s| Statement::None(StrRange::from_input(&s))),
            map(function_def_with_indent(indent), |s| {
                Statement::FuncDef(Box::new(s))
            }),
            map(eat_statement(&gen_indent, func_call), |s| {
                Statement::FuncCall(Box::new(s))
            }),
            map(preceded(&gen_indent, assign_with_indent(indent)), |s| {
                Statement::Assignment(Box::new(s))
            }),
            map(preceded(&gen_indent, var_assign_with_indent(indent)), |s| {
                Statement::VarAssignment(Box::new(s))
            }),
        ))(input)
    }
}

pub fn statement(input: Input) -> PineResult<Statement> {
    statement_with_indent(0)(input)
}

pub fn block(input: Input) -> PineResult<Block> {
    block_with_indent(0)(input)
}

#[cfg(test)]
mod tests {
    use super::super::input::Position;
    use super::super::num::{FloatNode, IntNode, Numeral};
    use super::*;
    use std::fmt::Debug;

    fn check_res_input<'a, F, O>(s: &'a str, handler: F, res: O, res_input: &'a str)
    where
        F: Fn(Input<'a>) -> PineResult<O>,
        O: Debug + PartialEq,
    {
        let test_input = Input::new_with_str(s);
        let input_len = test_input.len() - res_input.len();
        let consume_input = Input::new_with_start(&s[..input_len], Position::new(0, 0));
        assert_eq!(
            handler(test_input),
            Ok((
                Input::new(res_input, consume_input.end, Position::max()),
                res
            ))
        );
    }

    fn check_res<'a, F, O>(s: &'a str, handler: F, res: O)
    where
        F: Fn(Input<'a>) -> PineResult<O>,
        O: Debug + PartialEq,
    {
        check_res_input(s, handler, res, "")
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
                    Exp::VarName(VarName::new_with_start("hello", Position::new(0, 3))),
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
            ref_call,
            RefCall::new(
                Exp::VarName(VarName::new_with_start("hello", Position::new(0, 0))),
                Exp::Bool(BoolNode::new(
                    true,
                    StrRange::from_start("true", Position::new(0, 6)),
                )),
                StrRange::from_start("hello[true]", Position::new(0, 0)),
            ),
        );
    }

    #[test]
    fn condition_test() {
        check_res(
            "a ? b : c",
            condition,
            Condition::new(
                Exp::VarName(VarName::new_with_start("a", Position::new(0, 0))),
                Exp::VarName(VarName::new_with_start("b", Position::new(0, 4))),
                Exp::VarName(VarName::new_with_start("c", Position::new(0, 8))),
                StrRange::from_start("a ? b : c", Position::new(0, 0)),
            ),
        );
    }

    #[test]
    fn condition_statement_test() {
        check_res(
            "m = a ? b : c \n",
            statement_with_indent(0),
            Statement::Assignment(Box::new(Assignment::new(
                vec![VarName::new_with_start("m", Position::new(0, 0))],
                Exp::Condition(Box::new(Condition {
                    cond: Exp::VarName(VarName::new_with_start("a", Position::new(0, 4))),
                    exp1: Exp::VarName(VarName::new_with_start("b", Position::new(0, 8))),
                    exp2: Exp::VarName(VarName::new_with_start("c", Position::new(0, 12))),
                    range: StrRange::from_start("a ? b : c", Position::new(0, 4)),
                })),
                false,
                None,
                StrRange::from_start("m = a ? b : c", Position::new(0, 0)),
            ))),
        );
        check_res(
            "m = na \n",
            statement_with_indent(0),
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
            statement_with_indent(1),
            Statement::Break(StrRange::from_start("break", Position::new(0, 4))),
        );

        check_res(
            "a = b \n",
            statement_with_indent(0),
            Statement::Assignment(Box::new(Assignment::new(
                vec![VarName::new_with_start("a", Position::new(0, 0))],
                Exp::VarName(VarName::new_with_start("b", Position::new(0, 4))),
                false,
                None,
                StrRange::from_start("a = b", Position::new(0, 0)),
            ))),
        );
        check_res(
            "    a(arg1) \n",
            statement_with_indent(1),
            Statement::FuncCall(Box::new(FunctionCall::new_no_ctxid(
                Exp::VarName(VarName::new_with_start("a", Position::new(0, 4))),
                vec![Exp::VarName(VarName::new_with_start(
                    "arg1",
                    Position::new(0, 6),
                ))],
                vec![],
                StrRange::from_start("a(arg1)", Position::new(0, 4)),
            ))),
        );
        check_res(
            "    a(arg1) => b \n",
            statement_with_indent(1),
            Statement::FuncDef(Box::new(FunctionDef {
                name: VarName::new_with_start("a", Position::new(0, 4)),
                params: vec![VarName::new_with_start("arg1", Position::new(0, 6))],
                body: Block {
                    stmts: vec![],
                    ret_stmt: Some(Exp::VarName(VarName::new_with_start(
                        "b",
                        Position::new(0, 15),
                    ))),
                    range: StrRange::from_start("b", Position::new(0, 15)),
                },
                range: StrRange::from_start("a(arg1) => b", Position::new(0, 4)),
            })),
        );

        check_res(
            "    a(arg1) => \n        b \n",
            statement_with_indent(1),
            Statement::FuncDef(Box::new(FunctionDef {
                name: VarName::new_with_start("a", Position::new(0, 4)),
                params: vec![VarName::new_with_start("arg1", Position::new(0, 6))],
                body: Block {
                    stmts: vec![],
                    ret_stmt: Some(Exp::VarName(VarName::new_with_start(
                        "b",
                        Position::new(1, 8),
                    ))),
                    range: StrRange::from_start("b", Position::new(1, 8)),
                },
                range: StrRange::from_start("a(arg1) => \n        b", Position::new(0, 4)),
            })),
        );

        check_res_input(
            "    //helo world \na = close",
            statement_with_indent(1),
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
            "a = close // This is also a comment\n",
            statement_with_indent(0),
            Statement::Assignment(Box::new(Assignment::new(
                vec![VarName::new_with_start("a", Position::new(0, 0))],
                Exp::VarName(VarName::new_with_start("close", Position::new(0, 4))),
                false,
                None,
                StrRange::from_start("a = close", Position::new(0, 0)),
            ))),
        );
        check_res(
            "a := close\n",
            statement_with_indent(0),
            Statement::VarAssignment(Box::new(VarAssignment::new(
                VarName::new_with_start("a", Position::new(0, 0)),
                Exp::VarName(VarName::new_with_start("close", Position::new(0, 5))),
                StrRange::from_start("a := close", Position::new(0, 0)),
            ))),
        );
    }

    #[test]
    fn prefix_exp_test() {
        check_res(
            "m = a.b.c \n",
            statement_with_indent(0),
            Statement::Assignment(Box::new(Assignment::new(
                vec![VarName::new_with_start("m", Position::new(0, 0))],
                Exp::PrefixExp(Box::new(PrefixExp {
                    var_chain: vec![
                        VarName::new_with_start("a", Position::new(0, 4)),
                        VarName::new_with_start("b", Position::new(0, 6)),
                        VarName::new_with_start("c", Position::new(0, 8)),
                    ],
                    range: StrRange::from_start("a.b.c", Position::new(0, 4)),
                })),
                false,
                None,
                StrRange::from_start("m = a.b.c", Position::new(0, 0)),
            ))),
        )
    }

    #[test]
    fn block_test() {
        check_res_input(
            "    break \n    continue \n    true \nhello",
            block_with_indent(1),
            Block::new(
                vec![
                    Statement::Break(StrRange::from_start("break", Position::new(0, 4))),
                    Statement::Continue(StrRange::from_start("continue", Position::new(1, 4))),
                ],
                Some(Exp::Bool(BoolNode::new(
                    true,
                    StrRange::from_start("true", Position::new(2, 4)),
                ))),
                StrRange::new(Position::new(0, 4), Position::new(2, 8)),
            ),
            "hello",
        );
    }

    #[test]
    fn if_then_else_test() {
        check_res(
            "if true \n    break\n    true  \n",
            if_then_else(0),
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
            for_range(0),
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
}
