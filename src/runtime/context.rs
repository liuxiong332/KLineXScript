use crate::types::{ConvertErr, Object, PineType};
use std::collections::{HashMap, HashSet};

pub struct Context<'a> {
    // input: &'a str,
    pub vars: HashMap<&'a str, Box<dyn PineType<'a> + 'a>>,
    pub objects: HashMap<&'a str, Box<Object<'a>>>,
    pub declare_vars: HashSet<&'a str>,
}

impl<'a> Context<'a> {
    pub fn new(objects: HashMap<&'a str, Box<Object<'a>>>) -> Context<'a> {
        Context {
            objects,
            vars: HashMap::new(),
            declare_vars: HashSet::new(),
        }
    }
}

pub trait Runner<'a> {
    fn run(&self, context: &mut Context<'a>) -> Result<Box<dyn PineType<'a> + 'a>, ConvertErr>;
}

pub trait StmtRunner<'a> {
    fn run(&self, context: &mut Context<'a>) -> Result<(), ConvertErr>;
}
