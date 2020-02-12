use super::input::StrRange;
use super::op::{BinaryOp, BinaryOpNode, UnaryOp};
use super::stat_expr_types::{
    BinaryExp, Exp, Exp2, FlatExp, OpOrExp2, RVVarName, UnOpExp2, UnOrBinOp, UnaryExp,
};
// pine language precedence table(The belower, the higher):
//   conditional ?:
//   or
//   and
//   !=    ==
//   <     >     <=    >=
//   +     -
//   *     /     %
//   unary operators (not + -)
//   reference []

lazy_static! {
    static ref UNOPS: Vec<UnaryOp> = {
        let table = &mut [UnaryOp::Plus, UnaryOp::Minus, UnaryOp::BoolNot];
        table.sort();
        table.to_vec()
    };
    static ref BINOP_PRECEDENCE: Vec<Vec<BinaryOp>> = {
        let table: &mut [&mut [BinaryOp]] = &mut [
            &mut [BinaryOp::Mul, BinaryOp::Div, BinaryOp::Mod],
            &mut [BinaryOp::Plus, BinaryOp::Minus],
            &mut [
                BinaryOp::Lt,
                BinaryOp::Gt,
                BinaryOp::Leq,
                BinaryOp::Geq,
                BinaryOp::Neq,
                BinaryOp::Eq,
            ],
            &mut [BinaryOp::BoolAnd],
            &mut [BinaryOp::BoolOr],
        ];
        let mut acc = Vec::new();
        for s in table.iter_mut() {
            s.sort();
            acc.push(s.to_vec());
        }
        acc
    };
}

fn tuple_to_flatvec<'a>(unexp: UnOpExp2<'a>) -> Vec<OpOrExp2<'a>> {
    let mut v: Vec<OpOrExp2<'a>> = unexp
        .ops
        .into_iter()
        .map(UnOrBinOp::UnaryOp)
        .map(OpOrExp2::Op)
        .collect();
    v.push(OpOrExp2::Exp2(unexp.exp));
    v
}

pub fn flatexp_from_components<'a>(
    head: UnOpExp2<'a>,
    binop_chain: Vec<(BinaryOpNode, UnOpExp2<'a>)>,
) -> FlatExp<'a> {
    let acc = tuple_to_flatvec(head);
    let res = binop_chain.into_iter().fold(acc, |mut a, (binop, t)| {
        a.push(OpOrExp2::Op(UnOrBinOp::BinaryOp(binop)));
        a.extend_from_slice(&*tuple_to_flatvec(t));
        a
    });

    let range = StrRange::new(res[0].range().start, res.last().unwrap().range().end);
    FlatExp::new(res, range)
}

// Convenience type for implementing the below conversion function
#[derive(Debug)]
enum OpOrExp<'a> {
    Op(UnOrBinOp),
    Exp(Exp<'a>),
}

impl<'a> OpOrExp<'a> {
    fn is_binop(&self) -> bool {
        if let &OpOrExp::Op(UnOrBinOp::BinaryOp(_)) = self {
            true
        } else {
            false
        }
    }

    fn is_unop(&self) -> bool {
        if let &OpOrExp::Op(UnOrBinOp::UnaryOp(_)) = self {
            true
        } else {
            false
        }
    }

    fn is_exp(&self) -> bool {
        if let &OpOrExp::Exp(_) = self {
            true
        } else {
            false
        }
    }
}

impl<'a> From<FlatExp<'a>> for Exp<'a> {
    fn from(fe: FlatExp<'a>) -> Exp<'a> {
        // Helper function. Expects a,b to be Exps and o to be a BinOp
        fn merge_nodes_binop<'a>(a: OpOrExp<'a>, o: OpOrExp<'a>, b: OpOrExp<'a>) -> OpOrExp<'a> {
            match (a, o, b) {
                (OpOrExp::Exp(a), OpOrExp::Op(UnOrBinOp::BinaryOp(o)), OpOrExp::Exp(b)) => {
                    let range = StrRange::new(a.range().start, b.range().end);
                    let merged_exp = Exp::BinaryExp(Box::new(BinaryExp::new(o.op, a, b, range)));
                    OpOrExp::Exp(merged_exp)
                }
                _ => panic!("unexpected input variants in merge_nodes_binop"),
            }
        }

        // Helper function. Expects o to be a UnOp and a to be an Exp
        fn merge_nodes_unop<'a>(o: OpOrExp, a: OpOrExp<'a>) -> OpOrExp<'a> {
            match (o, a) {
                (OpOrExp::Op(UnOrBinOp::UnaryOp(o)), OpOrExp::Exp(a)) => {
                    let range = StrRange::new(o.range.start, a.range().end);
                    let merged_exp = Exp::UnaryExp(Box::new(UnaryExp::new(o.op, a, range)));
                    OpOrExp::Exp(merged_exp)
                }
                _ => panic!("unexpected input variants in merge_nodes_unop"),
            }
        }

        // TODO: make this more efficient
        fn merge_all_binops(explist: &mut Vec<OpOrExp>, binops: &[BinaryOp]) {
            loop {
                let mut tojoin_idx: Option<usize> = None;
                for (i, oe) in explist
                    .iter()
                    .enumerate()
                    .filter(|&(_, ref oe)| oe.is_binop())
                {
                    match oe {
                        &OpOrExp::Op(UnOrBinOp::BinaryOp(ref o)) => {
                            // Found something to join
                            if binops.binary_search(&o.op).is_ok() {
                                assert!(i > 0);
                                assert!(explist[i - 1].is_exp());
                                assert!(i.checked_add(1).is_some());
                                let next = explist.get(i + 1).unwrap();

                                // If UnOps haven't been merged yet, ignore them. If there are two
                                // subsequent binops, that's an error. Otherwise we have a $ b
                                // where a and b are Exps and $ is a BinOp
                                match next {
                                    &OpOrExp::Op(UnOrBinOp::UnaryOp(_)) => continue,
                                    &OpOrExp::Op(UnOrBinOp::BinaryOp(_)) => {
                                        panic!("encountered two binops next to each other");
                                    }
                                    &OpOrExp::Exp(_) => {
                                        tojoin_idx = Some(i);
                                        break;
                                    }
                                }
                            }
                        }
                        _ => unreachable!(),
                    }
                }

                if let Some(i) = tojoin_idx {
                    let a = explist.remove(i - 1);
                    let o = explist.remove(i - 1);
                    let b = explist.remove(i - 1);
                    let merged = merge_nodes_binop(a, o, b);
                    explist.insert(i - 1, merged);
                }
                // Joined everything we could. Break
                else {
                    break;
                }
            }
        }

        fn merge_all_unops(explist: &mut Vec<OpOrExp>, unops: &[UnaryOp]) {
            loop {
                let mut tojoin_idx: Option<usize> = None;
                // Reverse iterate, since we want to apply stacked unary operators right-to-left
                for (i, oe) in explist
                    .iter()
                    .enumerate()
                    .filter(|&(_, ref oe)| oe.is_unop())
                    .rev()
                {
                    match oe {
                        &OpOrExp::Op(UnOrBinOp::UnaryOp(ref o)) => {
                            // Found something to join
                            if unops.binary_search(&o.op).is_ok() {
                                assert!(i.checked_add(1).is_some());
                                let next = explist.get(i + 1).unwrap();
                                assert!(next.is_exp());

                                tojoin_idx = Some(i);
                                break;
                            }
                        }
                        _ => unreachable!(),
                    }
                }

                if let Some(i) = tojoin_idx {
                    let o = explist.remove(i);
                    let a = explist.remove(i);
                    let merged = merge_nodes_unop(o, a);
                    explist.insert(i, merged);
                }
                // Joined everything we could. Break
                else {
                    break;
                }
            }
        }

        let mut explist: Vec<OpOrExp> = fe
            .exps
            .into_iter()
            .map(|oe| match oe {
                OpOrExp2::Op(o) => OpOrExp::Op(o),
                OpOrExp2::Exp2(e) => OpOrExp::Exp(Exp::from(e)),
            })
            .collect();

        // The unary operation is the highest preceded.
        merge_all_unops(&mut explist, &*UNOPS);

        for binops in BINOP_PRECEDENCE.iter() {
            merge_all_binops(&mut explist, &*binops);
        }

        assert_eq!(explist.len(), 1, "Exp tree construction didn't complete");
        match explist.pop().unwrap() {
            OpOrExp::Exp(e) => e,
            _ => unreachable!(),
        }
    }
}

impl<'a> From<Exp2<'a>> for Exp<'a> {
    fn from(e: Exp2<'a>) -> Exp<'a> {
        match e {
            Exp2::Na(node) => Exp::Na(node),
            Exp2::Bool(b) => Exp::Bool(b),
            Exp2::Num(n) => Exp::Num(n),
            Exp2::Str(s) => Exp::Str(s),
            Exp2::Color(s) => Exp::Color(s),
            Exp2::VarName(f) => Exp::VarName(RVVarName::new(f)),
            // Exp2::RetTuple(p) => Exp::RetTuple(p),
            Exp2::Tuple(t) => Exp::Tuple(t),
            Exp2::TypeCast(t) => Exp::TypeCast(t),
            Exp2::FuncCall(t) => Exp::FuncCall(t),
            Exp2::RefCall(f) => Exp::RefCall(f),
            Exp2::PrefixExp(f) => Exp::PrefixExp(f),
            Exp2::Exp(e) => e,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::input::{Input, Position};
    use super::super::num::IntNode;
    use super::super::op::*;
    use super::super::stat_expr_types::*;
    use super::*;
    use crate::ast::num::Numeral;

    #[test]
    fn flatexp_from_components_test() {
        let gen_unop_node = || {
            UnaryOpNode::new(
                UnaryOp::Plus,
                StrRange::from_start("+", Position::new(0, 0)),
            )
        };

        let gen_biop_node =
            || BinaryOpNode::new(BinaryOp::Eq, StrRange::from_start("+", Position::new(0, 0)));

        let gen_bool_node =
            || BoolNode::new(true, StrRange::from_start("true", Position::new(0, 1)));

        assert_eq!(
            tuple_to_flatvec(UnOpExp2::new(
                vec![gen_unop_node()],
                Exp2::Bool(gen_bool_node()),
                StrRange::from_start("+true", Position::new(0, 0))
            )),
            vec![
                OpOrExp2::Op(UnOrBinOp::UnaryOp(gen_unop_node())),
                OpOrExp2::Exp2(Exp2::Bool(gen_bool_node()))
            ]
        );

        assert_eq!(
            flatexp_from_components(
                UnOpExp2::new(
                    vec![gen_unop_node()],
                    Exp2::Bool(gen_bool_node()),
                    StrRange::from_start("+true", Position::new(0, 0))
                ),
                vec![(
                    gen_biop_node(),
                    UnOpExp2::new(
                        vec![gen_unop_node()],
                        Exp2::Bool(gen_bool_node()),
                        StrRange::from_start("+true", Position::new(0, 6))
                    )
                )],
            ),
            FlatExp::new(
                vec![
                    OpOrExp2::Op(UnOrBinOp::UnaryOp(gen_unop_node())),
                    OpOrExp2::Exp2(Exp2::Bool(gen_bool_node())),
                    OpOrExp2::Op(UnOrBinOp::BinaryOp(gen_biop_node())),
                    OpOrExp2::Op(UnOrBinOp::UnaryOp(gen_unop_node())),
                    OpOrExp2::Exp2(Exp2::Bool(gen_bool_node())),
                ],
                StrRange::new(Position::new(0, 0), Position::new(0, 5))
            )
        );

        fn int_oe2(int_lit: i64, lit_str: &'static str, pos: Position) -> OpOrExp2<'static> {
            OpOrExp2::Exp2(Exp2::Num(Numeral::Int(IntNode::new(
                int_lit,
                StrRange::from_start(lit_str, pos),
            ))))
        }

        fn int_exp(int_lit: i64, lit_str: &'static str, pos: Position) -> Exp<'static> {
            Exp::Num(Numeral::Int(IntNode::new(
                int_lit,
                StrRange::from_start(lit_str, pos),
            )))
        }

        fn unary_oe2(unary_op: UnaryOp, pos: Position) -> OpOrExp2<'static> {
            OpOrExp2::Op(UnOrBinOp::UnaryOp(UnaryOpNode::new(
                unary_op,
                StrRange::from_start("_", pos),
            )))
        }

        fn binary_oe2(binary_op: BinaryOp, pos: Position) -> OpOrExp2<'static> {
            OpOrExp2::Op(UnOrBinOp::BinaryOp(BinaryOpNode::new(
                binary_op,
                StrRange::from_start("_", pos),
            )))
        }

        // 1 + 2 * -2 - -4 / -1;
        assert_eq!(
            Exp::from(FlatExp::new(
                vec![
                    int_oe2(1, "1", Position::new(0, 0)),
                    binary_oe2(BinaryOp::Plus, Position::new(0, 1)),
                    int_oe2(2, "2", Position::new(0, 2)),
                    binary_oe2(BinaryOp::Mul, Position::new(0, 3)),
                    unary_oe2(UnaryOp::Minus, Position::new(0, 4)),
                    int_oe2(2, "2", Position::new(0, 5)),
                    binary_oe2(BinaryOp::Minus, Position::new(0, 6)),
                    unary_oe2(UnaryOp::Minus, Position::new(0, 7)),
                    int_oe2(4, "4", Position::new(0, 8)),
                    binary_oe2(BinaryOp::Div, Position::new(0, 9)),
                    unary_oe2(UnaryOp::Minus, Position::new(0, 10)),
                    int_oe2(1, "1", Position::new(0, 11)),
                ],
                StrRange::from_start("1+2*-2--4/-1", Position::new(0, 0))
            )),
            Exp::BinaryExp(Box::new(BinaryExp::new(
                BinaryOp::Minus,
                Exp::BinaryExp(Box::new(BinaryExp::new(
                    BinaryOp::Plus,
                    int_exp(1, "1", Position::new(0, 0)),
                    Exp::BinaryExp(Box::new(BinaryExp::new(
                        BinaryOp::Mul,
                        int_exp(2, "2", Position::new(0, 2)),
                        Exp::UnaryExp(Box::new(UnaryExp::new(
                            UnaryOp::Minus,
                            int_exp(2, "2", Position::new(0, 5)),
                            StrRange::from_start("-2", Position::new(0, 4))
                        ))),
                        StrRange::from_start("2*-2", Position::new(0, 2))
                    ))),
                    StrRange::from_start("1+2*-2", Position::new(0, 0))
                ))),
                Exp::BinaryExp(Box::new(BinaryExp::new(
                    BinaryOp::Div,
                    Exp::UnaryExp(Box::new(UnaryExp::new(
                        UnaryOp::Minus,
                        int_exp(4, "4", Position::new(0, 8)),
                        StrRange::from_start("-4", Position::new(0, 7))
                    ))),
                    Exp::UnaryExp(Box::new(UnaryExp::new(
                        UnaryOp::Minus,
                        int_exp(1, "1", Position::new(0, 11)),
                        StrRange::from_start("-1", Position::new(0, 10))
                    ))),
                    StrRange::from_start("-4/-1", Position::new(0, 7))
                ))),
                StrRange::from_start("1+2*-2--4/-1", Position::new(0, 0))
            ))),
        );
    }
}
