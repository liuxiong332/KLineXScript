use super::op::{BinaryOp, UnaryOp};
use super::stat_expr_types::{Exp, Exp2, FlatExp, OpOrExp2, UnOrBinOp};
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

fn tuple_to_flatvec<'a>((unops, e): (Vec<UnaryOp>, Exp2<'a>)) -> Vec<OpOrExp2<'a>> {
    let mut v: Vec<OpOrExp2<'a>> = unops
        .into_iter()
        .map(UnOrBinOp::UnaryOp)
        .map(OpOrExp2::Op)
        .collect();
    v.push(OpOrExp2::Exp2(e));
    v
}

pub fn flatexp_from_components<'a>(
    head: (Vec<UnaryOp>, Exp2<'a>),
    binop_chain: Vec<(BinaryOp, (Vec<UnaryOp>, Exp2<'a>))>,
) -> FlatExp<'a> {
    let acc = tuple_to_flatvec(head);
    let res = binop_chain.into_iter().fold(acc, |mut a, (binop, t)| {
        a.push(OpOrExp2::Op(UnOrBinOp::BinaryOp(binop)));
        a.extend_from_slice(&*tuple_to_flatvec(t));
        a
    });

    FlatExp(res)
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
                    let merged_exp = Exp::BinaryExp(o, Box::new(a), Box::new(b));
                    OpOrExp::Exp(merged_exp)
                }
                _ => panic!("unexpected input variants in merge_nodes_binop"),
            }
        }

        // Helper function. Expects o to be a UnOp and a to be an Exp
        fn merge_nodes_unop<'a>(o: OpOrExp, a: OpOrExp<'a>) -> OpOrExp<'a> {
            match (o, a) {
                (OpOrExp::Op(UnOrBinOp::UnaryOp(o)), OpOrExp::Exp(a)) => {
                    let merged_exp = Exp::UnaryExp(o, Box::new(a));
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
                            if binops.binary_search(o).is_ok() {
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
                            if unops.binary_search(o).is_ok() {
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

        let mut explist: Vec<OpOrExp> =
            fe.0.into_iter()
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
            Exp2::Na => Exp::Na,
            Exp2::Bool(b) => Exp::Bool(b),
            Exp2::Num(n) => Exp::Num(n),
            Exp2::Str(s) => Exp::Str(s),
            Exp2::Color(s) => Exp::Color(s),
            Exp2::VarName(f) => Exp::VarName(f),
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
    use super::*;
    use crate::ast::num::Numeral;

    #[test]
    fn flatexp_from_components_test() {
        assert_eq!(
            tuple_to_flatvec((vec![UnaryOp::Plus], Exp2::Bool(true))),
            vec![
                OpOrExp2::Op(UnOrBinOp::UnaryOp(UnaryOp::Plus)),
                OpOrExp2::Exp2(Exp2::Bool(true))
            ]
        );

        assert_eq!(
            flatexp_from_components(
                (vec![UnaryOp::Plus], Exp2::Bool(true)),
                vec![(BinaryOp::Eq, (vec![UnaryOp::Minus], Exp2::Bool(false)))]
            ),
            FlatExp(vec![
                OpOrExp2::Op(UnOrBinOp::UnaryOp(UnaryOp::Plus)),
                OpOrExp2::Exp2(Exp2::Bool(true)),
                OpOrExp2::Op(UnOrBinOp::BinaryOp(BinaryOp::Eq)),
                OpOrExp2::Op(UnOrBinOp::UnaryOp(UnaryOp::Minus)),
                OpOrExp2::Exp2(Exp2::Bool(false)),
            ])
        );

        fn int_oe2(int_lit: i32) -> OpOrExp2<'static> {
            OpOrExp2::Exp2(Exp2::Num(Numeral::Int(int_lit)))
        }

        fn int_exp(int_lit: i32) -> Exp<'static> {
            Exp::Num(Numeral::Int(int_lit))
        }

        fn unary_oe2(unary_op: UnaryOp) -> OpOrExp2<'static> {
            OpOrExp2::Op(UnOrBinOp::UnaryOp(unary_op))
        }

        fn binary_oe2(binary_op: BinaryOp) -> OpOrExp2<'static> {
            OpOrExp2::Op(UnOrBinOp::BinaryOp(binary_op))
        }

        // 1 + 2 * -2 - -4 / -1;
        assert_eq!(
            Exp::from(FlatExp(vec![
                int_oe2(1),
                binary_oe2(BinaryOp::Plus),
                int_oe2(2),
                binary_oe2(BinaryOp::Mul),
                unary_oe2(UnaryOp::Minus),
                int_oe2(2),
                binary_oe2(BinaryOp::Minus),
                unary_oe2(UnaryOp::Minus),
                int_oe2(4),
                binary_oe2(BinaryOp::Div),
                unary_oe2(UnaryOp::Minus),
                int_oe2(1),
            ])),
            Exp::BinaryExp(
                BinaryOp::Minus,
                Box::new(Exp::BinaryExp(
                    BinaryOp::Plus,
                    Box::new(int_exp(1)),
                    Box::new(Exp::BinaryExp(
                        BinaryOp::Mul,
                        Box::new(int_exp(2)),
                        Box::new(Exp::UnaryExp(UnaryOp::Minus, Box::new(int_exp(2))))
                    ))
                )),
                Box::new(Exp::BinaryExp(
                    BinaryOp::Div,
                    Box::new(Exp::UnaryExp(UnaryOp::Minus, Box::new(int_exp(4)))),
                    Box::new(Exp::UnaryExp(UnaryOp::Minus, Box::new(int_exp(1)))),
                ))
            )
        );
    }
}
