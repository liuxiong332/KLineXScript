use super::data_src::Callback;
use crate::types::{
    Bool, Callable, Color, DataType, Float, Int, PineFrom, PineRef, PineStaticType, PineType,
    RefData, RuntimeErr, SecondType, Series, NA,
};
use std::collections::{HashMap, HashSet};
use std::fmt::Debug;
use std::mem;

pub trait Ctx<'a> {
    fn create_var(&mut self, name: &'a str, val: PineRef<'a>) -> Option<PineRef<'a>>;

    fn update_var(&mut self, name: &'a str, val: PineRef<'a>);

    fn move_var(&mut self, name: &'a str) -> Option<PineRef<'a>>;

    fn contains_var(&self, name: &'a str) -> bool;

    fn create_callable(&mut self, call: RefData<Callable<'a>>);

    fn create_declare(&mut self, name: &'a str);

    fn contains_declare(&self, name: &'a str) -> bool;

    fn clear_declare(&mut self);

    fn get_type(&self) -> ContextType;

    fn get_callback(&self) -> Option<&'a dyn Callback>;
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ContextType {
    Normal,
    IfElseBlock,
    ForRangeBlock,
    FuncDefBlock,
}

// T is Context<_, _>, but in this situation, we cannot give Context<'c, 'd>
// because there is no place to declare 'c and 'd lifetime.
pub struct Context<'a, 'b> {
    // input: &'a str,
    parent: Option<&'b mut (dyn 'b + Ctx<'a>)>,
    context_type: ContextType,

    // variable map that defined by user and library.
    vars: HashMap<&'a str, PineRef<'a>>,
    // All the Series type variable name
    _series: Vec<&'a str>,
    callables: Vec<RefData<Callable<'a>>>,
    declare_vars: HashSet<&'a str>,

    callback: Option<&'a dyn Callback>,
    first_commit: bool,
}

fn commit_series<'a, D>(val: PineRef<'a>) -> PineRef<'a>
where
    D: Default + PartialEq + PineStaticType + PineType<'a> + Clone + Debug + 'a,
{
    let mut series: RefData<Series<D>> = Series::implicity_from(val).unwrap();
    series.commit();
    series.into_pf()
}

fn roll_back_series<'a, D>(val: PineRef<'a>) -> PineRef<'a>
where
    D: Default + PartialEq + PineStaticType + PineType<'a> + Clone + Debug + 'a,
{
    let mut series: RefData<Series<D>> = Series::implicity_from(val).unwrap();
    series.roll_back();
    series.into_pf()
}

impl<'a, 'b> Context<'a, 'b> {
    pub fn new(parent: Option<&'b mut (dyn 'b + Ctx<'a>)>, t: ContextType) -> Context<'a, 'b> {
        Context {
            parent,
            context_type: t,
            vars: HashMap::new(),
            _series: vec![],
            callables: vec![],
            declare_vars: HashSet::new(),
            callback: None,
            first_commit: false,
        }
    }

    pub fn new_with_callback(callback: &'a dyn Callback) -> Context<'a, 'b> {
        Context {
            parent: None,
            context_type: ContextType::Normal,
            vars: HashMap::new(),
            _series: vec![],
            callables: vec![],
            declare_vars: HashSet::new(),
            callback: Some(callback),
            first_commit: false,
        }
    }

    pub fn map_var<F>(&mut self, name: &'a str, f: F)
    where
        F: Fn(Option<PineRef<'a>>) -> Option<PineRef<'a>>,
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
            let val = parent.move_var(name);
            if let Some(ret_val) = f(val) {
                parent.create_var(name, ret_val);
            }
        }
    }

    pub fn commit(&mut self) {
        let keys: Vec<&'a str> = self.vars.keys().cloned().collect();
        for k in keys {
            let val = self.move_var(k).unwrap();
            let ret_val = match val.get_type() {
                (DataType::Float, SecondType::Series) => commit_series::<Float>(val),
                (DataType::Int, SecondType::Series) => commit_series::<Int>(val),
                (DataType::Color, SecondType::Series) => commit_series::<Color>(val),
                (DataType::Bool, SecondType::Series) => commit_series::<Bool>(val),
                _ => val,
            };
            self.update_var(k, ret_val);
        }
        if !self.first_commit {
            self.first_commit = true;
        }
    }

    pub fn roll_back(&mut self) -> Result<(), RuntimeErr> {
        let keys: Vec<&'a str> = self.vars.keys().cloned().collect();
        for k in keys {
            let val = self.move_var(k).unwrap();
            let ret_val = match val.get_type() {
                (DataType::Float, SecondType::Series) => roll_back_series::<Float>(val),
                (DataType::Int, SecondType::Series) => roll_back_series::<Int>(val),
                (DataType::Color, SecondType::Series) => roll_back_series::<Color>(val),
                (DataType::Bool, SecondType::Series) => roll_back_series::<Bool>(val),
                _ => val,
            };
            self.update_var(k, ret_val);
        }
        let callables = mem::replace(&mut self.callables, vec![]);
        for callable in callables.iter() {
            callable.back(self)?;
        }
        mem::replace(&mut self.callables, callables);
        Ok(())
    }

    pub fn run_callbacks(&mut self) -> Result<(), RuntimeErr> {
        let callables = mem::replace(&mut self.callables, vec![]);
        for callable in callables.iter() {
            callable.run(self)?;
        }
        mem::replace(&mut self.callables, callables);
        Ok(())
    }
}

impl<'a, 'b> Ctx<'a> for Context<'a, 'b> {
    fn create_var(&mut self, name: &'a str, val: PineRef<'a>) -> Option<PineRef<'a>> {
        self.vars.insert(name, val)
    }

    fn update_var(&mut self, name: &'a str, val: PineRef<'a>) {
        if self.vars.contains_key(name) {
            self.vars.insert(name, val);
        } else if let Some(ref mut parent) = self.parent {
            parent.update_var(name, val);
        }
    }

    // Move the value for the specific name from this context or the parent context.
    fn move_var(&mut self, name: &'a str) -> Option<PineRef<'a>> {
        // Insert the temporary NA into the name and move the original value out.
        if self.vars.contains_key(name) {
            self.vars.insert(name, PineRef::new_box(NA))
        } else if let Some(ref mut parent) = self.parent {
            parent.create_var(name, PineRef::new_box(NA))
        } else {
            None
        }
    }

    fn contains_var(&self, name: &'a str) -> bool {
        if self.vars.contains_key(name) {
            true
        } else if let Some(ref parent) = self.parent {
            parent.contains_var(name)
        } else {
            false
        }
    }

    fn create_callable(&mut self, call: RefData<Callable<'a>>) {
        if let Some(ref mut v) = self.parent {
            v.create_callable(call);
        } else if !self.first_commit {
            self.callables.push(call);
        }
    }

    fn create_declare(&mut self, name: &'a str) {
        self.declare_vars.insert(name);
    }

    fn contains_declare(&self, name: &'a str) -> bool {
        self.declare_vars.contains(name)
    }

    fn clear_declare(&mut self) {
        self.declare_vars.clear();
    }

    fn get_type(&self) -> ContextType {
        self.context_type
    }

    fn get_callback(&self) -> Option<&'a dyn Callback> {
        self.callback
    }
}

pub trait Runner<'a> {
    fn run(&'a self, context: &mut dyn Ctx<'a>) -> Result<PineRef<'a>, RuntimeErr>;
}

// Evaluate  the expression with right-value.
pub trait RVRunner<'a> {
    fn rv_run(&'a self, context: &mut dyn Ctx<'a>) -> Result<PineRef<'a>, RuntimeErr>;
}

pub trait StmtRunner<'a> {
    fn st_run(&'a self, context: &mut dyn Ctx<'a>) -> Result<(), RuntimeErr>;
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{Int, PineFrom};

    #[test]
    fn context_test() {
        let mut context1 = Context::new(None, ContextType::Normal);
        context1.create_declare("hello");
        assert!(context1.contains_declare("hello"));

        context1.clear_declare();
        assert!(!context1.contains_declare("hello"));

        context1.create_var("hello", PineRef::new_box(Some(1)));
        assert_eq!(
            Int::implicity_from(context1.move_var("hello").unwrap()),
            Ok(RefData::new_box(Some(1)))
        );

        context1.update_var("hello", PineRef::new_box(Some(10)));
        assert_eq!(
            Int::implicity_from(context1.move_var("hello").unwrap()),
            Ok(RefData::new_box(Some(10)))
        );
        assert!(context1.contains_var("hello"));

        context1.map_var("hello", |_| Some(PineRef::new_box(Some(100))));
        assert_eq!(
            Int::implicity_from(context1.move_var("hello").unwrap()),
            Ok(RefData::new_box(Some(100)))
        );
    }

    #[test]
    fn callable_context_test() {
        // Parent context create callable
        let mut context1 = Context::new(None, ContextType::Normal);
        context1.create_callable(RefData::new_rc(Callable::new(None, None, vec![])));
        assert_eq!(context1.callables.len(), 1);

        {
            // Child context create callable
            let mut context2 = Context::new(Some(&mut context1), ContextType::Normal);
            context2.create_callable(RefData::new_rc(Callable::new(None, None, vec![])));
        }
        assert_eq!(context1.callables.len(), 2);

        context1.commit();

        // After commit, parent context and child context should not add callable by create callable
        context1.create_callable(RefData::new_rc(Callable::new(None, None, vec![])));
        {
            let mut context2 = Context::new(Some(&mut context1), ContextType::Normal);
            context2.create_callable(RefData::new_rc(Callable::new(None, None, vec![])));
        }
        assert_eq!(context1.callables.len(), 2);

        assert_eq!(context1.roll_back(), Ok(()));
        assert_eq!(context1.run_callbacks(), Ok(()));
        assert_eq!(context1.callables.len(), 2);
    }

    #[test]
    fn derive_context_test() {
        let mut context1 = Context::new(None, ContextType::Normal);
        context1.create_var("hello", PineRef::new_box(Some(1)));

        let mut context2 = Context::new(Some(&mut context1), ContextType::Normal);
        context2.create_var("hello2", PineRef::new_box(Some(2)));

        let mov_res = context2.move_var("hello").unwrap();
        context2.update_var("hello", mov_res);

        assert!(context2.vars.get("hello").is_none());
        let parent = context2.parent.as_mut().unwrap();
        assert!(parent.move_var("hello").is_some());

        assert!(context2.contains_var("hello"));
        assert!(context2.contains_var("hello2"));
    }
}
