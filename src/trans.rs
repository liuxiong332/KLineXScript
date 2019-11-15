use crate::op::{BinaryOp, UnaryOp};
use crate::stat_expr_types::{Exp2, FlatExp, OpOrExp2, UnOrBinOp};
use nom::{multi::many0, sequence::tuple};
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

#[cfg(test)]
mod tests {
    use super::*;
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
    }
}
