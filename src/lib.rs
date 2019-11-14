// #[macro_use]
extern crate nom;

// mod comment_expr;
mod error;
mod int_expr;
mod reference_ops;
// mod identifier_expr;

// The integer literal is like \d+_\d+
// named!(pub decimal<usize>,
//     map_res!(
//         map_res!(
//             digit1,
//             str::from_utf8),
//         |s| usize::from_str_radix(s, 10)
//     )
//  );

// named!(
//     int_literal<i32>,
//     map!(
//         do_parse!(sign: opt!(alt!(tag!("+") | tag!("-"))) >> expt: decimal >> (sign, expt)),
//         |(sign, expt): (Option<&[u8]>, usize)| {
//             match sign {
//                 Some(b"+") | None => expt as i32,
//                 Some(b"-") => -(expt as i32),
//                 _ => unreachable!(),
//             }
//         }
//     )
// );

struct _FloatExpr {
    val: f64,
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {}
}
