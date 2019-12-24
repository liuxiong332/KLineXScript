use crate::ast::syntax_type::SyntaxType;
use std::collections::HashMap;

pub struct TypesIdGen<'a> {
    counter: i32,
    types: HashMap<Vec<SyntaxType<'a>>, i32>,
}

impl<'a> TypesIdGen<'a> {
    pub fn new() -> TypesIdGen<'a> {
        TypesIdGen {
            counter: 0,
            types: HashMap::new(),
        }
    }

    pub fn get(&mut self, types: &Vec<SyntaxType<'a>>) -> i32 {
        match self.types.get(types) {
            Some(val) => *val,
            None => {
                self.counter += 1;
                self.types.insert(types.clone(), self.counter);
                self.counter
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::syntax_type::SimpleSyntaxType;

    #[test]
    fn types_id_gen_test() {
        let mut types_id_gen = TypesIdGen::new();
        assert_eq!(
            types_id_gen.get(&vec![SyntaxType::Series(SimpleSyntaxType::Int)]),
            1
        );
        assert_eq!(
            types_id_gen.get(&vec![SyntaxType::Series(SimpleSyntaxType::Float)]),
            2
        );
        assert_eq!(
            types_id_gen.get(&vec![
                SyntaxType::Series(SimpleSyntaxType::Int),
                SyntaxType::Series(SimpleSyntaxType::Float)
            ],),
            3
        );
        assert_eq!(
            types_id_gen.get(&vec![SyntaxType::Series(SimpleSyntaxType::Int)]),
            1
        );
    }
}
