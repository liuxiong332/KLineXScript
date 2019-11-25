use crate::types::{ConvertErr, Object, PineType};
use std::collections::HashMap;

pub struct Context<'a> {
    // input: &'a str,
    // vars: HashMap<&'a str, Box<dyn PineType<'a> + 'a>>,
    pub objects: HashMap<&'a str, Box<Object<'a>>>,
}

impl<'a> Context<'a> {
    pub fn new(objects: HashMap<&'a str, Box<Object<'a>>>) -> Context<'a> {
        Context { objects }
    }
}

pub trait Runner<'a> {
    fn run(&self, context: &mut Context<'a>) -> Result<Box<dyn PineType<'a> + 'a>, ConvertErr>;
}
