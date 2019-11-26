use crate::types::{ConvertErr, Object, PineType, NA};
use std::collections::{HashMap, HashSet};

pub struct Context<'a> {
    // input: &'a str,
    parent: Option<&'a mut Context<'a>>,

    vars: HashMap<&'a str, Box<dyn PineType<'a> + 'a>>,
    declare_vars: HashSet<&'a str>,
}

impl<'a> Context<'a> {
    pub fn new(parent: Option<&'a mut Context<'a>>) -> Context<'a> {
        Context {
            parent,
            vars: HashMap::new(),
            declare_vars: HashSet::new(),
        }
    }

    pub fn create_var(&mut self, name: &'a str, val: Box<dyn PineType<'a> + 'a>) {
        self.vars.insert(name, val);
    }

    pub fn update_var(&mut self, name: &'a str, val: Box<dyn PineType<'a> + 'a>) {
        if self.vars.contains_key(name) {
            self.vars.insert(name, val);
        } else if let Some(ref mut parent) = self.parent {
            parent.update_var(name, val);
        }
    }

    // Move the value for the specific name from this context or the parent context.
    pub fn move_var(&mut self, name: &'a str) -> Option<Box<dyn PineType<'a> + 'a>> {
        // Insert the temporary NA into the name and move the original value out.
        if self.vars.contains_key(name) {
            self.vars.insert(name, Box::new(NA))
        } else if let Some(ref mut parent) = self.parent {
            parent.vars.insert(name, Box::new(NA))
        } else {
            None
        }
    }

    pub fn contains_var(&self, name: &'a str) -> bool {
        if self.vars.contains_key(name) {
            true
        } else if let Some(ref parent) = self.parent {
            parent.vars.contains_key(name)
        } else {
            false
        }
    }

    pub fn map_var<F>(&mut self, name: &'a str, f: F)
    where
        F: Fn(Option<Box<dyn PineType<'a> + 'a>>) -> Option<Box<dyn PineType<'a> + 'a>>,
    {
        // Find in  current context
        let val = self.vars.remove(name);
        if let Some(_) = val {
            if let Some(ret_val) = f(val) {
                self.vars.insert(name, ret_val);
            }
            return ();
        }
        // Find in parent context if this context don't has this var
        if let Some(ref mut parent) = self.parent {
            let val = parent.vars.remove(name);
            if let Some(ret_val) = f(val) {
                parent.vars.insert(name, ret_val);
            }
        }
    }

    pub fn insert_declare(&mut self, name: &'a str) {
        self.declare_vars.insert(name);
    }

    pub fn contains_declare(&self, name: &'a str) -> bool {
        self.declare_vars.contains(name)
    }

    pub fn clear_declare(&mut self) {
        self.declare_vars.clear();
    }
}

pub trait Runner<'a> {
    fn run(&self, context: &mut Context<'a>) -> Result<Box<dyn PineType<'a> + 'a>, ConvertErr>;
}

pub trait StmtRunner<'a> {
    fn run(&self, context: &mut Context<'a>) -> Result<(), ConvertErr>;
}

#[cfg(test)]
mod tests {
    use super::*;
}
