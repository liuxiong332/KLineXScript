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
