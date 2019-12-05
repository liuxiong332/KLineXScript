use super::data_src::Callback;
use crate::types::{
    Bool, Callable, Color, DataType, Float, Int, PineFrom, PineRef, PineStaticType, PineType,
    RefData, RuntimeErr, SecondType, Series, NA,
};
use std::cell::{Cell, RefCell};
use std::collections::{HashMap, HashSet};
use std::fmt::Debug;
use std::rc::{Rc, Weak};

pub trait VarOperate<'a> {
    fn create_var(&self, name: &'a str, val: PineRef<'a>) -> Option<PineRef<'a>>;

    fn update_var(&self, name: &'a str, val: PineRef<'a>);

    fn move_var(&self, name: &'a str) -> Option<PineRef<'a>>;

    fn get_var_keys(&self) -> Vec<&'a str>;
}

// lifetime 'a is the lifetime of Exp, 'c is the lifetime of Ctx Self's lifetime
pub trait Ctx<'a>: VarOperate<'a> {
    fn contains_var(&self, name: &'a str) -> bool;

    fn create_callable(&self, call: RefData<Callable<'a>>);

    fn create_declare(&self, name: &'a str);

    fn contains_declare(&self, name: &'a str) -> bool;

    fn contains_declare_scope(&self, name: &'a str) -> bool;

    fn clear_declare(&self);

    fn any_declare(&self) -> bool;

    fn set_is_run(&self, is_run: bool);

    fn get_is_run(&self) -> bool;

    fn clear_is_run(&self);

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

// 'a is the lifetime of Exp, 'b is the parent context's lifetime, 'c is the context self's lifetime
pub struct Context<'a> {
    // input: &'a str,
    parent: Cell<Option<Weak<Context<'a>>>>,
    context_type: Cell<ContextType>,

    // Child contexts that with parent self lifetime 'c
    sub_contexts: RefCell<HashMap<String, Rc<Context<'a>>>>,

    // variable map that defined by user and library.
    vars: RefCell<HashMap<&'a str, PineRef<'a>>>,
    // All the Series type variable name
    // _series: Vec<&'a str>,
    callables: RefCell<Vec<RefData<Callable<'a>>>>,
    declare_vars: RefCell<HashSet<&'a str>>,

    callback: Cell<Option<&'a dyn Callback>>,
    first_commit: Cell<bool>,

    is_run: Cell<bool>,
}

pub fn downcast_ctx<'a>(item: Rc<dyn Ctx<'a> + 'a>) -> Result<Rc<Context<'a>>, RuntimeErr> {
    unsafe {
        let raw: *const dyn Ctx<'a> = Rc::into_raw(item);
        let t = raw as *const Context<'a>;
        Ok(Rc::from_raw(t))
    }
}

pub fn upcast_ctx<'a>(item: Rc<Context<'a>>) -> Result<Rc<dyn Ctx<'a> + 'a>, RuntimeErr> {
    unsafe {
        let raw: *const Context<'a> = Rc::into_raw(item);
        let t = raw as *const (dyn Ctx<'a> + 'a);
        Ok(Rc::from_raw(t))
    }
}

// pub fn rc_downgrade<T>(item: Rc<T>) -> Weak<T> {
//     item.inc_weak();
//     // Make sure we do not create a dangling Weak
//     debug_assert!(!is_dangling(this.ptr));
//     Weak { ptr: this.ptr }
// }

fn commit_series<'a, D>(val: PineRef<'a>) -> PineRef<'a>
where
    D: Default + PartialEq + PineStaticType + PineType<'a> + PineFrom<'a, D> + Clone + Debug + 'a,
{
    let mut series: RefData<Series<D>> = Series::implicity_from(val).unwrap();
    series.commit();
    series.into_pf()
}

pub fn commit_series_for_operator<'a>(operator: &dyn VarOperate<'a>) {
    let keys: Vec<&'a str> = operator.get_var_keys();
    // The committed set used to make sure only one instance of series commmit.
    let mut commited: HashSet<*const (dyn PineType<'a> + 'a)> = HashSet::new();
    for k in keys {
        let val = operator.move_var(k).unwrap();
        if commited.contains(&val.as_ptr()) {
            continue;
        }
        commited.insert(val.as_ptr());
        let ret_val = match val.get_type() {
            (DataType::Float, SecondType::Series) => commit_series::<Float>(val),
            (DataType::Int, SecondType::Series) => commit_series::<Int>(val),
            (DataType::Color, SecondType::Series) => commit_series::<Color>(val),
            (DataType::Bool, SecondType::Series) => commit_series::<Bool>(val),
            _ => val,
        };
        operator.update_var(k, ret_val);
    }
}

fn roll_back_series<'a, D>(val: PineRef<'a>) -> PineRef<'a>
where
    D: Default + PartialEq + PineStaticType + PineType<'a> + PineFrom<'a, D> + Clone + Debug + 'a,
{
    let mut series: RefData<Series<D>> = Series::implicity_from(val).unwrap();
    series.roll_back();
    series.into_pf()
}

impl<'a> Context<'a> {
    pub fn new(parent: Option<Weak<Context<'a>>>, t: ContextType) -> Context<'a> {
        Context {
            parent: Cell::new(parent),
            context_type: Cell::new(t),
            sub_contexts: RefCell::new(HashMap::new()),
            vars: RefCell::new(HashMap::new()),
            callables: RefCell::new(vec![]),
            declare_vars: RefCell::new(HashSet::new()),
            callback: Cell::new(None),
            first_commit: Cell::new(false),
            is_run: Cell::new(false),
        }
    }

    pub fn new_with_callback(callback: &'a dyn Callback) -> Context<'a> {
        Context {
            parent: Cell::new(None),
            context_type: Cell::new(ContextType::Normal),
            sub_contexts: RefCell::new(HashMap::new()),
            vars: RefCell::new(HashMap::new()),
            callables: RefCell::new(vec![]),
            declare_vars: RefCell::new(HashSet::new()),
            callback: Cell::new(Some(callback)),
            first_commit: Cell::new(false),
            is_run: Cell::new(false),
        }
    }

    pub fn create_sub_context(self: Rc<Self>, name: String, t: ContextType) -> Rc<Context<'a>> {
        let mut subctx = Rc::new(Context::new(Some(Rc::downgrade(&self)), t));
        self.sub_contexts.borrow_mut().insert(name.clone(), subctx);
        // *self.get_sub_context(&name).unwrap()
        self.move_sub_context(&name).unwrap()
    }

    pub fn map_var<F>(self: Rc<Self>, name: &'a str, f: F)
    where
        F: Fn(Option<PineRef<'a>>) -> Option<PineRef<'a>>,
    {
        // Find in  current context
        let val = self.vars.borrow_mut().remove(name);
        if let Some(_) = val {
            if let Some(ret_val) = f(val) {
                self.vars.borrow_mut().insert(name, ret_val);
            }
            return ();
        }
        // Find in parent context if this context don't has this var
        if let Some(parent) = self.parent.take() {
            let parent = parent.upgrade().unwrap();
            let val = parent.move_var(name);
            if let Some(ret_val) = f(val) {
                parent.create_var(name, ret_val);
            }
            self.parent.replace(Some(Rc::downgrade(&parent)));
        }
    }

    pub fn commit(self: Rc<Self>) {
        commit_series_for_operator(&*self);

        // Commit the Series for all of the sub context.
        let keys = self.sub_contexts.borrow().keys();

        for key in keys.into_iter() {
            // If this context does not declare variables, so this context is not run,
            // we need not commit the series.
            let (name, ctx) = self.sub_contexts.borrow_mut().remove_entry(key).unwrap();
            if ctx.get_is_run() {
                downcast_ctx(ctx).unwrap().commit();
            }
            self.sub_contexts.borrow_mut().insert(name, ctx);
        }

        if !self.first_commit.get() {
            self.first_commit.replace(true);
        }
    }

    pub fn roll_back(self: Rc<Self>) -> Result<(), RuntimeErr> {
        let keys: Vec<&'a str> = self.vars.borrow().keys().cloned().collect();
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
        let callables = self.callables.replace(vec![]);
        for callable in callables.iter() {
            callable.back(self)?;
        }
        // mem::replace(&mut self.callables, callables);
        self.callables.replace(callables);
        Ok(())
    }

    pub fn run_callbacks(self: Rc<Self>) -> Result<(), RuntimeErr> {
        let callables = self.callables.replace(vec![]);
        for callable in callables.iter() {
            callable.run(Rc::clone(&self))?;
        }
        self.callables.replace(callables);
        Ok(())
    }

    pub fn contains_sub_context(self: Rc<Self>, name: &String) -> bool {
        self.sub_contexts.borrow().contains_key(name)
    }

    pub fn get_sub_context<'b>(&'b self, name: &String) -> Option<&'b Rc<Context<'a>>> {
        self.sub_contexts.borrow().get(name)
    }

    pub fn set_sub_context(self: Rc<Self>, name: String, sub_context: Rc<Context<'a>>) {
        self.sub_contexts.borrow_mut().insert(name, sub_context);
    }

    pub fn move_sub_context(self: Rc<Self>, name: &String) -> Option<Rc<Context<'a>>> {
        self.sub_contexts.borrow_mut().remove(name)
    }

    pub fn update_sub_context(self: Rc<Self>, name: String, subctx: Rc<Context<'a>>) {
        self.sub_contexts.borrow_mut().insert(name, subctx);
    }
}

impl<'a> VarOperate<'a> for Context<'a> {
    fn create_var(&self, name: &'a str, val: PineRef<'a>) -> Option<PineRef<'a>> {
        self.vars.borrow_mut().insert(name, val)
    }

    fn update_var(&self, name: &'a str, val: PineRef<'a>) {
        if self.vars.borrow().contains_key(name) {
            self.vars.borrow_mut().insert(name, val);
        } else if let Some(parent) = self.parent.take() {
            let parent = parent.upgrade().unwrap();
            if parent.contains_var(name) {
                parent.update_var(name, val);
            } else {
                self.vars.borrow_mut().insert(name, val);
            }
            self.parent.replace(Some(Rc::downgrade(&parent)));
        } else {
            self.vars.borrow_mut().insert(name, val);
        }
    }

    // Move the value for the specific name from this context or the parent context.
    fn move_var(&self, name: &'a str) -> Option<PineRef<'a>> {
        // Insert the temporary NA into the name and move the original value out.
        if self.vars.borrow().contains_key(name) {
            self.vars.borrow_mut().insert(name, PineRef::new_box(NA))
        } else if let Some(parent) = self.parent.take() {
            let parent = parent.upgrade().unwrap();
            let var = parent.move_var(name);
            self.parent.replace(Some(Rc::downgrade(&parent)));
            var
        } else {
            None
        }
    }

    fn get_var_keys(&self) -> Vec<&'a str> {
        self.vars.borrow().keys().cloned().collect()
    }
}

impl<'a> Ctx<'a> for Context<'a> {
    fn contains_var(&self, name: &'a str) -> bool {
        if self.vars.borrow().contains_key(name) {
            true
        } else if let Some(parent) = self.parent.take() {
            let parent = parent.upgrade().unwrap();
            let result = parent.contains_var(name);
            self.parent.replace(Some(Rc::downgrade(&parent)));
            result
        } else {
            false
        }
    }

    fn create_callable(&self, call: RefData<Callable<'a>>) {
        if let Some(v) = self.parent.take() {
            let val = v.upgrade().unwrap();
            val.create_callable(call);
            self.parent.replace(Some(Rc::downgrade(&val)));
        } else if !self.first_commit.get() {
            self.callables.borrow_mut().push(call);
        }
    }

    fn create_declare(&self, name: &'a str) {
        self.declare_vars.borrow_mut().insert(name);
    }

    fn contains_declare(&self, name: &'a str) -> bool {
        if self.declare_vars.borrow().contains(name) {
            true
        } else if let Some(parent) = self.parent.take() {
            let parent = parent.upgrade().unwrap();
            let result = parent.contains_declare(name);
            self.parent.replace(Some(Rc::downgrade(&parent)));
            result
        } else {
            false
        }
    }

    fn contains_declare_scope(&self, name: &'a str) -> bool {
        self.declare_vars.borrow().contains(name)
    }

    fn clear_declare(&self) {
        self.declare_vars.borrow_mut().clear();
        for (_, v) in self.sub_contexts.borrow().iter() {
            v.clear_declare();
        }
    }

    fn any_declare(&self) -> bool {
        !self.declare_vars.borrow().is_empty()
    }

    fn set_is_run(&self, is_run: bool) {
        self.is_run.replace(is_run);
    }

    fn get_is_run(&self) -> bool {
        self.is_run.get()
    }

    fn clear_is_run(&self) {
        self.is_run.set(false);
        for (_, subctx) in self.sub_contexts.borrow().iter() {
            subctx.clear_is_run();
        }
    }

    fn get_type(&self) -> ContextType {
        self.context_type.get()
    }

    fn get_callback(&self) -> Option<&'a dyn Callback> {
        self.callback.get()
    }
}

pub trait Runner<'a> {
    fn run(&'a self, context: &Rc<Context<'a>>) -> Result<PineRef<'a>, RuntimeErr>;
}

// Evaluate  the expression with right-value.
pub trait RVRunner<'a> {
    fn rv_run(&'a self, context: &Rc<Context<'a>>) -> Result<PineRef<'a>, RuntimeErr>;
}

pub trait StmtRunner<'a> {
    fn st_run(&'a self, context: &Rc<Context<'a>>) -> Result<(), RuntimeErr>;
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{Int, PineFrom};

    #[test]
    fn context_test() {
        let mut context1 = Rc::new(Context::new(None, ContextType::Normal));
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
        let mut context1 = Rc::new(Context::new(None, ContextType::Normal));
        context1.create_callable(RefData::new_rc(Callable::new(None, None, vec![])));
        assert_eq!(context1.callables.borrow().len(), 1);

        {
            // Child context create callable
            let mut context2 = Context::new(Some(Rc::downgrade(&context1)), ContextType::Normal);
            context2.create_callable(RefData::new_rc(Callable::new(None, None, vec![])));
        }
        assert_eq!(context1.callables.borrow().len(), 2);

        context1.commit();

        // After commit, parent context and child context should not add callable by create callable
        context1.create_callable(RefData::new_rc(Callable::new(None, None, vec![])));
        {
            let mut context2 = Context::new(Some(Rc::downgrade(&context1)), ContextType::Normal);
            context2.create_callable(RefData::new_rc(Callable::new(None, None, vec![])));
        }
        assert_eq!(context1.callables.borrow().len(), 2);

        assert_eq!(context1.roll_back(), Ok(()));
        assert_eq!(context1.run_callbacks(), Ok(()));
        assert_eq!(context1.callables.borrow().len(), 2);
    }

    #[test]
    fn derive_context_test() {
        // hello is owned by context1, hello2 is owned by context2, hello3 is not owned by both context
        let mut context1 = Rc::new(Context::new(None, ContextType::Normal));
        context1.create_var("hello", PineRef::new_box(Some(1)));

        let mut context2 = Context::new(Some(Rc::downgrade(&context1)), ContextType::Normal);
        context2.create_var("hello2", PineRef::new_box(Some(2)));

        assert_eq!(context2.contains_var("hello"), true);
        let mov_res1 = context2.move_var("hello").unwrap();
        assert_eq!(mov_res1, PineRef::new(Some(1)));

        assert_eq!(context2.contains_var("hello2"), true);
        let mov_res2 = context2.move_var("hello2").unwrap();
        assert_eq!(mov_res2, PineRef::new(Some(2)));

        assert_eq!(context2.contains_var("hello3"), false);
        assert_eq!(context2.move_var("hello3"), None);

        context2.update_var("hello", mov_res1);
        assert!(context2.vars.borrow().get("hello").is_none());
        {
            let parent = context2.parent.take().unwrap();
            let p = parent.upgrade().unwrap();
            assert!(p.move_var("hello").is_some());
            context2.parent.replace(Some(Rc::downgrade(&p)));
        }

        context2.update_var("hello2", mov_res2);
        assert!(context2.vars.borrow().get("hello2").is_some());

        context2.update_var("hello3", PineRef::new(Some(10)));
        assert!(context2.vars.borrow().get("hello3").is_some());

        assert!(context2.contains_var("hello"));
        assert!(context2.contains_var("hello2"));
    }
}
