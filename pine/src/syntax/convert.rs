use super::{SimpleSyntaxType, SyntaxType};

pub fn implicity_convert<'a>(origin_type: &SyntaxType<'a>, dest_type: &SyntaxType<'a>) -> bool {
    if origin_type == dest_type {
        return true;
    }
    match origin_type {
        SyntaxType::Series(SimpleSyntaxType::Na) => match dest_type {
            SyntaxType::Series(_) => true,
            _ => false,
        },
        SyntaxType::Simple(SimpleSyntaxType::Na) => match dest_type {
            SyntaxType::Simple(_) => true,
            SyntaxType::Series(_) => true,
            _ => false,
        },
        SyntaxType::Series(SimpleSyntaxType::Int) => match dest_type {
            SyntaxType::Series(SimpleSyntaxType::Bool)
            | SyntaxType::Series(SimpleSyntaxType::Float)
            | SyntaxType::Series(SimpleSyntaxType::Int) => true,
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
        SyntaxType::Series(SimpleSyntaxType::Float) => match dest_type {
            SyntaxType::Series(SimpleSyntaxType::Bool)
            | SyntaxType::Series(SimpleSyntaxType::Float) => true,

            _ => false,
        },
        SyntaxType::Simple(SimpleSyntaxType::Float) => match dest_type {
            SyntaxType::Simple(SimpleSyntaxType::Bool)
            | SyntaxType::Simple(SimpleSyntaxType::Float) => true,

            SyntaxType::Series(SimpleSyntaxType::Bool)
            | SyntaxType::Series(SimpleSyntaxType::Float) => true,

            _ => false,
        },
        _ => false,
    }
}

fn common_simple_type(
    type1: &SimpleSyntaxType,
    type2: &SimpleSyntaxType,
) -> Option<SimpleSyntaxType> {
    match (type1, type2) {
        (SimpleSyntaxType::Bool, SimpleSyntaxType::Bool)
        | (SimpleSyntaxType::Bool, SimpleSyntaxType::Float)
        | (_, SimpleSyntaxType::Bool) => Some(SimpleSyntaxType::Bool),
        (SimpleSyntaxType::Float, _) | (_, SimpleSyntaxType::Float) => {
            Some(SimpleSyntaxType::Float)
        }
        (SimpleSyntaxType::Int, _) | (_, SimpleSyntaxType::Int) => Some(SimpleSyntaxType::Int),
        (SimpleSyntaxType::Color, _) | (_, SimpleSyntaxType::Color) => {
            Some(SimpleSyntaxType::Color)
        }
        (SimpleSyntaxType::String, _) | (_, SimpleSyntaxType::String) => {
            Some(SimpleSyntaxType::String)
        }
        (SimpleSyntaxType::Na, _) | (_, SimpleSyntaxType::Na) => Some(SimpleSyntaxType::Na),
    }
}

pub fn common_type<'a>(type1: &SyntaxType<'a>, type2: &SyntaxType<'a>) -> Option<SyntaxType<'a>> {}

pub fn simple_to_series<'a>(origin_type: SyntaxType<'a>) -> SyntaxType<'a> {
    match origin_type {
        SyntaxType::Simple(t) => SyntaxType::Series(t),
        _ => origin_type,
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
