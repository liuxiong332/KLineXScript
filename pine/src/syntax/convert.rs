use super::{SimpleSyntaxType, SyntaxType};
use std::rc::Rc;

// Series variable can only be implicity converted to Series, Simple can be implicity converted to Simple and Series
pub fn implicity_convert<'a>(origin_type: &SyntaxType<'a>, dest_type: &SyntaxType<'a>) -> bool {
    let origin_type = origin_type.get_v_for_vf();
    let dest_type = dest_type.get_v_for_vf().get_v_for_de();

    if origin_type == dest_type {
        return true;
    }
    match origin_type {
        SyntaxType::Simple(SimpleSyntaxType::Na) => match dest_type {
            SyntaxType::Simple(_) => true,
            SyntaxType::Series(_) => true,
            _ => false,
        },
        SyntaxType::Series(SimpleSyntaxType::Na) => match dest_type {
            SyntaxType::Series(_) => true,
            _ => false,
        },

        SyntaxType::Simple(SimpleSyntaxType::Int) => match dest_type {
            SyntaxType::Simple(SimpleSyntaxType::Bool)
            | SyntaxType::Simple(SimpleSyntaxType::Int)
            | SyntaxType::Simple(SimpleSyntaxType::Float) => true,

            SyntaxType::Series(SimpleSyntaxType::Bool)
            | SyntaxType::Series(SimpleSyntaxType::Float)
            | SyntaxType::Series(SimpleSyntaxType::Int) => true,

            _ => false,
        },
        SyntaxType::Series(SimpleSyntaxType::Int) => match dest_type {
            SyntaxType::Series(SimpleSyntaxType::Bool)
            | SyntaxType::Series(SimpleSyntaxType::Float)
            | SyntaxType::Series(SimpleSyntaxType::Int) => true,

            _ => false,
        },

        SyntaxType::Simple(SimpleSyntaxType::Float) => match dest_type {
            SyntaxType::Simple(SimpleSyntaxType::Bool)
            | SyntaxType::Simple(SimpleSyntaxType::Float) => true,

            SyntaxType::Series(SimpleSyntaxType::Bool)
            | SyntaxType::Series(SimpleSyntaxType::Float) => true,

            _ => false,
        },
        SyntaxType::Series(SimpleSyntaxType::Float) => match dest_type {
            SyntaxType::Series(SimpleSyntaxType::Bool)
            | SyntaxType::Series(SimpleSyntaxType::Float) => true,

            _ => false,
        },

        SyntaxType::Simple(SimpleSyntaxType::Bool) => match dest_type {
            SyntaxType::Simple(SimpleSyntaxType::Bool)
            | SyntaxType::Series(SimpleSyntaxType::Bool) => true,
            _ => false,
        },
        SyntaxType::Series(SimpleSyntaxType::Bool) => match dest_type {
            SyntaxType::Series(SimpleSyntaxType::Bool) => true,
            _ => false,
        },

        SyntaxType::Simple(SimpleSyntaxType::Color) => match dest_type {
            SyntaxType::Simple(SimpleSyntaxType::Color)
            | SyntaxType::Series(SimpleSyntaxType::Color) => true,
            _ => false,
        },
        SyntaxType::Series(SimpleSyntaxType::Color) => match dest_type {
            SyntaxType::Series(SimpleSyntaxType::Color) => true,
            _ => false,
        },

        SyntaxType::Simple(SimpleSyntaxType::String) => match dest_type {
            SyntaxType::Simple(SimpleSyntaxType::String)
            | SyntaxType::Series(SimpleSyntaxType::String) => true,
            _ => false,
        },
        SyntaxType::Series(SimpleSyntaxType::String) => match dest_type {
            SyntaxType::Series(SimpleSyntaxType::String) => true,
            _ => false,
        },

        SyntaxType::Tuple(types) => match dest_type {
            // Tuple(Int) can convert to List(Int)
            SyntaxType::List(SimpleSyntaxType::Int) => types
                .iter()
                .all(|s| implicity_convert(&s, &SyntaxType::Simple(SimpleSyntaxType::Int))),
            // Tuple(Float) can convert to List(Float)
            SyntaxType::List(SimpleSyntaxType::Float) => types
                .iter()
                .all(|s| implicity_convert(&s, &SyntaxType::Simple(SimpleSyntaxType::Float))),
            // Tuple(String) can convert to List(String)
            SyntaxType::List(SimpleSyntaxType::String) => types
                .iter()
                .all(|s| implicity_convert(&s, &SyntaxType::Simple(SimpleSyntaxType::String))),
            // Tuple(Color) can convert to List(Color)
            SyntaxType::List(SimpleSyntaxType::Color) => types
                .iter()
                .all(|s| implicity_convert(&s, &SyntaxType::Simple(SimpleSyntaxType::Color))),
            _ => false,
        },
        _ => false,
    }
}

// Get common simple syntax type for type1 and type2
fn common_simple_type(
    type1: &SimpleSyntaxType,
    type2: &SimpleSyntaxType,
) -> Option<SimpleSyntaxType> {
    match (type1, type2) {
        (SimpleSyntaxType::Bool, SimpleSyntaxType::Bool)
        | (SimpleSyntaxType::Bool, SimpleSyntaxType::Na)
        | (SimpleSyntaxType::Bool, SimpleSyntaxType::Float)
        | (SimpleSyntaxType::Bool, SimpleSyntaxType::Int)
        | (SimpleSyntaxType::Na, SimpleSyntaxType::Bool)
        | (SimpleSyntaxType::Float, SimpleSyntaxType::Bool)
        | (SimpleSyntaxType::Int, SimpleSyntaxType::Bool) => Some(SimpleSyntaxType::Bool),

        (SimpleSyntaxType::Float, SimpleSyntaxType::Float)
        | (SimpleSyntaxType::Float, SimpleSyntaxType::Int)
        | (SimpleSyntaxType::Float, SimpleSyntaxType::Na)
        | (SimpleSyntaxType::Int, SimpleSyntaxType::Float)
        | (SimpleSyntaxType::Na, SimpleSyntaxType::Float) => Some(SimpleSyntaxType::Float),

        (SimpleSyntaxType::Int, SimpleSyntaxType::Int)
        | (SimpleSyntaxType::Int, SimpleSyntaxType::Na)
        | (SimpleSyntaxType::Na, SimpleSyntaxType::Int) => Some(SimpleSyntaxType::Int),

        (SimpleSyntaxType::Na, SimpleSyntaxType::Na) => Some(SimpleSyntaxType::Na),

        (SimpleSyntaxType::Color, SimpleSyntaxType::Color)
        | (SimpleSyntaxType::Color, SimpleSyntaxType::Na)
        | (SimpleSyntaxType::Na, SimpleSyntaxType::Color) => Some(SimpleSyntaxType::Color),

        (SimpleSyntaxType::String, SimpleSyntaxType::String)
        | (SimpleSyntaxType::String, SimpleSyntaxType::Na)
        | (SimpleSyntaxType::Na, SimpleSyntaxType::String) => Some(SimpleSyntaxType::String),

        _ => None,
    }
}

// Get the common type of type1 and type2
pub fn common_type<'a>(type1: &SyntaxType<'a>, type2: &SyntaxType<'a>) -> Option<SyntaxType<'a>> {
    let type1 = type1.get_v_for_vf();
    let type2 = type2.get_v_for_vf();
    if type1 == type2 {
        return Some(type1.clone());
    }
    match (type1, type2) {
        (SyntaxType::Simple(t1), SyntaxType::Simple(t2)) => {
            let simple_type = common_simple_type(t1, t2);
            match simple_type {
                None => None,
                Some(t) => Some(SyntaxType::Simple(t)),
            }
        }
        (SyntaxType::Series(t1), SyntaxType::Simple(t2))
        | (SyntaxType::Simple(t1), SyntaxType::Series(t2))
        | (SyntaxType::Series(t1), SyntaxType::Series(t2)) => {
            let simple_type = common_simple_type(t1, t2);
            match simple_type {
                None => None,
                Some(t) => Some(SyntaxType::Series(t)),
            }
        }
        (SyntaxType::Tuple(tuple1), SyntaxType::Tuple(tuple2)) => {
            if tuple1.len() != tuple2.len() {
                None
            } else {
                let res_tuple: Vec<_> = tuple1
                    .iter()
                    .zip(tuple2.iter())
                    .map(|d| common_type(d.0, d.1))
                    .collect();
                if res_tuple.iter().any(|d| d.is_none()) {
                    None
                } else {
                    let res: Vec<_> = res_tuple.into_iter().map(|d| d.unwrap()).collect();
                    Some(SyntaxType::Tuple(Rc::new(res)))
                }
            }
        }
        _ => None,
    }
}

pub fn similar_simple_type(
    type1: &SimpleSyntaxType,
    type2: &SimpleSyntaxType,
) -> Option<SimpleSyntaxType> {
    if type1 == type2 {
        return Some(type1.clone());
    }
    match (type1, type2) {
        (SimpleSyntaxType::Int, SimpleSyntaxType::Float)
        | (SimpleSyntaxType::Float, SimpleSyntaxType::Int) => Some(SimpleSyntaxType::Float),

        (t, SimpleSyntaxType::Na) | (SimpleSyntaxType::Na, t) => Some(t.clone()),

        _ => None,
    }
}

// The variable types must be simalar.
pub fn similar_type<'a>(type1: &SyntaxType<'a>, type2: &SyntaxType<'a>) -> Option<SyntaxType<'a>> {
    let type1 = type1.get_v_for_vf();
    let type2 = type2.get_v_for_vf();
    match (type1, type2) {
        (SyntaxType::Simple(t1), SyntaxType::Simple(t2)) => {
            let simple_type = similar_simple_type(t1, t2);
            match simple_type {
                None => None,
                Some(t) => Some(SyntaxType::Simple(t)),
            }
        }
        (SyntaxType::Series(t1), SyntaxType::Simple(t2))
        | (SyntaxType::Simple(t1), SyntaxType::Series(t2))
        | (SyntaxType::Series(t1), SyntaxType::Series(t2)) => {
            let simple_type = similar_simple_type(t1, t2);
            match simple_type {
                None => None,
                Some(t) => Some(SyntaxType::Series(t)),
            }
        }
        _ => None,
    }
}

pub fn simple_to_series<'a>(origin_type: SyntaxType<'a>) -> SyntaxType<'a> {
    match origin_type.into_v_for_vf() {
        SyntaxType::Simple(t) => SyntaxType::Series(t),
        vtype => vtype,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn implicity_convert_test() {
        // na => simple any type
        assert!(implicity_convert(
            &SyntaxType::Simple(SimpleSyntaxType::Na),
            &SyntaxType::Simple(SimpleSyntaxType::Na),
        ));
        assert!(implicity_convert(
            &SyntaxType::Simple(SimpleSyntaxType::Na),
            &SyntaxType::Simple(SimpleSyntaxType::Int),
        ));
        assert!(implicity_convert(
            &SyntaxType::Simple(SimpleSyntaxType::Na),
            &SyntaxType::Simple(SimpleSyntaxType::Float),
        ));
        assert!(implicity_convert(
            &SyntaxType::Simple(SimpleSyntaxType::Na),
            &SyntaxType::Simple(SimpleSyntaxType::Bool),
        ));
        assert!(implicity_convert(
            &SyntaxType::Simple(SimpleSyntaxType::Na),
            &SyntaxType::Simple(SimpleSyntaxType::Color),
        ));
        assert!(implicity_convert(
            &SyntaxType::Simple(SimpleSyntaxType::Na),
            &SyntaxType::Simple(SimpleSyntaxType::String),
        ));

        // na => series any type
        assert!(implicity_convert(
            &SyntaxType::Simple(SimpleSyntaxType::Na),
            &SyntaxType::Series(SimpleSyntaxType::Na),
        ));
        assert!(implicity_convert(
            &SyntaxType::Simple(SimpleSyntaxType::Na),
            &SyntaxType::Series(SimpleSyntaxType::Int),
        ));
        assert!(implicity_convert(
            &SyntaxType::Simple(SimpleSyntaxType::Na),
            &SyntaxType::Series(SimpleSyntaxType::Float),
        ));
        assert!(implicity_convert(
            &SyntaxType::Simple(SimpleSyntaxType::Na),
            &SyntaxType::Series(SimpleSyntaxType::Bool),
        ));
        assert!(implicity_convert(
            &SyntaxType::Simple(SimpleSyntaxType::Na),
            &SyntaxType::Series(SimpleSyntaxType::Color),
        ));
        assert!(implicity_convert(
            &SyntaxType::Simple(SimpleSyntaxType::Na),
            &SyntaxType::Series(SimpleSyntaxType::String),
        ));

        // int => simple any type
        assert!(implicity_convert(
            &SyntaxType::Simple(SimpleSyntaxType::Int),
            &SyntaxType::Simple(SimpleSyntaxType::Float),
        ));
        assert!(implicity_convert(
            &SyntaxType::Simple(SimpleSyntaxType::Int),
            &SyntaxType::Simple(SimpleSyntaxType::Bool),
        ));
        assert!(!implicity_convert(
            &SyntaxType::Simple(SimpleSyntaxType::Int),
            &SyntaxType::Simple(SimpleSyntaxType::Na),
        ));

        assert!(implicity_convert(
            &SyntaxType::Simple(SimpleSyntaxType::Int),
            &SyntaxType::Series(SimpleSyntaxType::Float),
        ));
        assert!(implicity_convert(
            &SyntaxType::Simple(SimpleSyntaxType::Int),
            &SyntaxType::Series(SimpleSyntaxType::Bool),
        ));

        // float => simple any type
        assert!(!implicity_convert(
            &SyntaxType::Simple(SimpleSyntaxType::Float),
            &SyntaxType::Simple(SimpleSyntaxType::Int),
        ));
        assert!(implicity_convert(
            &SyntaxType::Simple(SimpleSyntaxType::Float),
            &SyntaxType::Simple(SimpleSyntaxType::Bool),
        ));
        assert!(!implicity_convert(
            &SyntaxType::Simple(SimpleSyntaxType::Float),
            &SyntaxType::Simple(SimpleSyntaxType::Na),
        ));
        assert!(implicity_convert(
            &SyntaxType::Simple(SimpleSyntaxType::Float),
            &SyntaxType::Series(SimpleSyntaxType::Float),
        ));
    }
}
