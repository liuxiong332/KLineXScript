use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::digit1,
    combinator::{map, opt},
    multi::separated_list,
    IResult,
};

struct _IntExpr {
    val: i32,
}

fn underscore_digit_str(s: &str) -> IResult<&str, String> {
    map(separated_list(tag("_"), digit1), |s| s.join(""))(s)
}

fn unsigned_int(s: &str) -> IResult<&str, i32> {
    map(underscore_digit_str, |s| {
        i32::from_str_radix(&s, 10).unwrap()
    })(s)
}

fn signed_int(s: &str) -> IResult<&str, i32> {
    let (s, sign) = opt(alt((tag("+"), tag("-"))))(s)?;
    let (s, num_int) = unsigned_int(s)?;
    match sign {
        Some("+") | None => Ok((s, num_int)),
        Some("-") => Ok((s, -num_int)),
        _ => panic!("internal error: entered unreachable code"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn underscore_digit_str_test() {
        assert_eq!(
            underscore_digit_str("1221_121"),
            Ok(("", String::from("1221121")))
        );

        assert_eq!(
            underscore_digit_str("1221121"),
            Ok(("", String::from("1221121")))
        );
    }

    #[test]
    fn underscore_digit_test() {
        assert_eq!(unsigned_int("1221_121"), Ok(("", 1221121)));
    }

    #[test]
    fn signed_int_test() {
        assert_eq!(signed_int("-1221_121"), Ok(("", -1221121)));
        assert_eq!(signed_int("+1221_121"), Ok(("", 1221121)));
        assert_eq!(signed_int("1221_121"), Ok(("", 1221121)));
    }
}
