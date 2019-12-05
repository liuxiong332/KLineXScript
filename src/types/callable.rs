use super::{
    Bool, Category, ComplexType, DataType, Float, Int, PineFrom, PineRef, PineStaticType, PineType,
    RefData, RuntimeErr, SecondType, NA,
};
use crate::runtime::context::{commit_series_for_operator, Context, Ctx, VarOperate};
use crate::runtime::statement::process_assign_val;
use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

pub trait SeriesCall<'a> {
    fn step(
        &self,
        _context: &Rc<Context<'a>>,
        _p: HashMap<&'a str, PineRef<'a>>,
    ) -> Result<PineRef<'a>, RuntimeErr> {
        Ok(PineRef::Box(Box::new(NA)))
    }

    fn run(&self, _context: &Rc<Context<'a>>) -> Result<(), RuntimeErr> {
        Ok(())
    }

    fn back(&self, _context: &Rc<Context<'a>>) -> Result<(), RuntimeErr> {
        Ok(())
    }

    fn copy(&self) -> Box<dyn SeriesCall<'a> + 'a>;
}

impl<'a> fmt::Debug for dyn SeriesCall<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s: &ParamCollectCall;
        unsafe {
            let raw: *const dyn SeriesCall<'a> = self;
            let t = raw as *const ParamCollectCall;
            s = t.as_ref().unwrap();
        }
        s.fmt(f)
    }
}

pub struct ParamCollectCall<'a> {
    func: fn(
        context: &Rc<Context<'a>>,
        HashMap<&'a str, PineRef<'a>>,
    ) -> Result<PineRef<'a>, RuntimeErr>,
    params: RefCell<HashMap<&'a str, PineRef<'a>>>,
}

impl<'a> fmt::Debug for ParamCollectCall<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let val = self.params.borrow();
        let res = write!(f, "{:?}", val);
        res
    }
}

impl<'a> ParamCollectCall<'a> {
    pub fn new(
        func: fn(
            context: &Rc<Context<'a>>,
            HashMap<&'a str, PineRef<'a>>,
        ) -> Result<PineRef<'a>, RuntimeErr>,
    ) -> ParamCollectCall<'a> {
        ParamCollectCall {
            params: RefCell::new(HashMap::new()),
            func,
        }
    }
}

impl<'a> VarOperate<'a> for RefCell<HashMap<&'a str, PineRef<'a>>> {
    fn create_var(&self, name: &'a str, val: PineRef<'a>) -> Option<PineRef<'a>> {
        self.borrow_mut().insert(name, val)
    }

    fn update_var(&self, name: &'a str, val: PineRef<'a>) {
        self.borrow_mut().insert(name, val);
    }

    fn move_var(&self, name: &'a str) -> Option<PineRef<'a>> {
        self.borrow_mut().remove(name)
    }

    fn get_var_keys(&self) -> Vec<&'a str> {
        self.borrow().keys().cloned().collect()
    }
}

impl<'a> SeriesCall<'a> for ParamCollectCall<'a> {
    fn step(
        &self,
        _context: &Rc<Context<'a>>,
        pmap: HashMap<&'a str, PineRef<'a>>,
    ) -> Result<PineRef<'a>, RuntimeErr> {
        // self.params.set(map);
        for (k, v) in pmap {
            // Merge all of the series variable into the exists series variable
            process_assign_val(v, &self.params, k)?;
        }
        // Commit all of the series variables.
        commit_series_for_operator(&self.params);
        Ok(PineRef::Box(Box::new(NA)))
    }

    fn run(&self, _context: &Rc<Context<'a>>) -> Result<(), RuntimeErr> {
        let val = self.params.replace(HashMap::new());
        (self.func)(_context, val)?;
        Ok(())
    }

    fn copy(&self) -> Box<dyn SeriesCall<'a> + 'a> {
        let map = self.params.borrow();
        let new_map: HashMap<&'a str, PineRef<'a>> =
            map.iter().map(|(&k, v)| (k, v.copy())).collect();
        // self.params.set(map);
        Box::new(ParamCollectCall {
            params: RefCell::new(new_map),
            func: self.func,
        })
    }
}

// pine callable type
pub struct Callable<'a> {
    func: Option<
        fn(
            context: &Rc<Context<'a>>,
            HashMap<&'a str, PineRef<'a>>,
        ) -> Result<PineRef<'a>, RuntimeErr>,
    >,
    caller: Option<Box<dyn SeriesCall<'a> + 'a>>,
    param_names: Vec<&'static str>,
}

impl<'a> fmt::Debug for Callable<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "param names: {:?}", self.param_names)
    }
}

impl<'a> Clone for Callable<'a> {
    fn clone(&self) -> Self {
        let caller = match self.caller {
            Some(ref c) => Some(c.copy()),
            None => None,
        };
        Callable {
            func: self.func,
            caller: caller,
            param_names: self.param_names.clone(),
        }
    }
}

impl<'a> PineStaticType for Callable<'a> {
    fn static_type() -> (DataType, SecondType) {
        (DataType::Callable, SecondType::Simple)
    }
}
impl<'a> PineType<'a> for Callable<'a> {
    fn get_type(&self) -> (DataType, SecondType) {
        <Self as PineStaticType>::static_type()
    }

    fn category(&self) -> Category {
        Category::Complex
    }

    fn copy(&self) -> PineRef<'a> {
        PineRef::Box(Box::new(self.clone()))
    }
}
impl<'a> PineFrom<'a, Callable<'a>> for Callable<'a> {}

impl<'a> PartialEq for Callable<'a> {
    fn eq(&self, other: &Callable<'a>) -> bool {
        self.param_names == other.param_names
    }
}

impl<'a> ComplexType for Callable<'a> {}

impl<'a> Callable<'a> {
    pub fn new(
        func: Option<
            fn(
                context: &Rc<Context<'a>>,
                HashMap<&'a str, PineRef<'a>>,
            ) -> Result<PineRef<'a>, RuntimeErr>,
        >,
        caller: Option<Box<dyn SeriesCall<'a> + 'a>>,
        param_names: Vec<&'static str>,
    ) -> Callable<'a> {
        Callable {
            func,
            caller,
            param_names,
        }
    }

    pub fn call(
        &self,
        context: &Rc<Context<'a>>,
        pos_args: Vec<PineRef<'a>>,
        dict_args: Vec<(&'a str, PineRef<'a>)>,
    ) -> Result<PineRef<'a>, RuntimeErr> {
        if pos_args.len() > self.param_names.len() {
            return Err(RuntimeErr::NotValidParam);
        }

        let mut all_args: HashMap<&'a str, PineRef<'a>> = HashMap::new();
        for (i, val) in pos_args.into_iter().enumerate() {
            let name = self.param_names[i];
            all_args.insert(name, val);
        }
        for (name, val) in dict_args.into_iter() {
            match self.param_names.iter().any(|&v| name == v) {
                false => return Err(RuntimeErr::NotValidParam),
                true => {
                    all_args.insert(name, val);
                }
            }
        }
        if let Some(func) = self.func {
            func(context, all_args)
        } else if let Some(ref caller) = self.caller {
            caller.step(context, all_args)
        } else {
            Ok(PineRef::Box(Box::new(NA)))
        }
    }

    pub fn back(&self, context: &Rc<Context<'a>>) -> Result<(), RuntimeErr> {
        if let Some(ref caller) = self.caller {
            caller.back(context)
        } else {
            Ok(())
        }
    }

    pub fn run(&self, context: &Rc<Context<'a>>) -> Result<(), RuntimeErr> {
        if let Some(ref caller) = self.caller {
            caller.run(context)
        } else {
            Ok(())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::primitive::Int;
    use super::super::Arithmetic;
    use super::*;
    use crate::runtime::context::{Context, ContextType};
    use crate::types::{downcast, downcast_pf, Series};

    fn test_func<'a>(
        _context: &Rc<Context<'a>>,
        mut args: HashMap<&'a str, PineRef<'a>>,
    ) -> Result<PineRef<'a>, RuntimeErr> {
        let (arg1, arg2) = (args.remove("arg1").unwrap(), args.remove("arg2").unwrap());
        match (arg1, arg2) {
            (PineRef::Box(a1), PineRef::Box(a2)) => {
                let s: Int = Some(
                    downcast::<Int>(a1).unwrap().unwrap() + downcast::<Int>(a2).unwrap().unwrap(),
                );
                Ok(PineRef::Box(Box::new(s) as Box<dyn PineType>))
            }
            _ => Err(RuntimeErr::NotSupportOperator),
        }
    }

    #[test]
    fn callable_test() {
        let callable = Callable::new(Some(test_func), None, vec!["arg1", "arg2"]);
        let mut context = Rc::new(Context::new(None, ContextType::Normal));

        let call_res = callable.call(
            &context,
            vec![
                PineRef::Box(Box::new(Some(1)) as Box<dyn PineType>),
                PineRef::Box(Box::new(Some(2)) as Box<dyn PineType>),
            ],
            vec![],
        );

        let val = downcast_pf::<Int>(call_res.unwrap()).unwrap();
        assert_eq!(val.into_inner(), Some(3));
    }

    fn add_test_func<'a>(
        _context: &Rc<Context<'a>>,
        mut args: HashMap<&'a str, PineRef<'a>>,
    ) -> Result<PineRef<'a>, RuntimeErr> {
        println!("Get args {:?}", args);
        let (arg1, arg2) = (args.remove("arg1").unwrap(), args.remove("arg2").unwrap());
        let arg1_val = downcast_pf::<Series<Int>>(arg1).unwrap();
        let arg2_val = downcast_pf::<Series<Int>>(arg2).unwrap();
        assert_eq!(arg1_val.get_history(), &vec![Some(1), Some(3)]);
        assert_eq!(arg2_val.get_history(), &vec![Some(2), Some(4)]);
        Ok(PineRef::new(NA))
    }

    #[test]
    fn series_call_test() {
        let callable = Callable::new(
            None,
            Some(Box::new(ParamCollectCall::new(add_test_func))),
            vec!["arg1", "arg2"],
        );
        let gen_params = |val1, val2| {
            vec![
                PineRef::new(Series::from(Some(val1))),
                PineRef::new(Series::from(Some(val2))),
            ]
        };

        let mut context = Rc::new(Context::new(None, ContextType::Normal));

        let call_res = callable.call(&context, gen_params(1, 2), vec![]).unwrap();
        assert_eq!(call_res.get_type(), (DataType::NA, SecondType::Simple));

        callable.call(&context, gen_params(3, 4), vec![]).unwrap();
        callable.back(&context).unwrap();
        callable.run(&context).unwrap();
    }

    // This test case is not used! If the callable receive simple int, that must be const!.

    // #[test]
    // fn series_array_test() {
    //     fn test_func<'a>(
    //         _context: &mut dyn Ctx<'a>,
    //         mut args: HashMap<&'a str, PineRef<'a>>,
    //     ) -> Result<PineRef<'a>, RuntimeErr> {
    //         println!("args {:?}", args);
    //         let arg1 = args.remove("arg").unwrap();
    //         let arg1_val = downcast_pf::<Vec<Int>>(arg1).unwrap();
    //         assert_eq!(arg1_val.into_inner(), vec![Some(100), Some(10)]);
    //         Ok(PineRef::new_box(NA))
    //     }

    //     let call = ParamCollectCall::new(test_func);
    //     let mut context = Context::new(None, ContextType::Normal);

    //     let mut map = HashMap::new();
    //     map.insert("arg", PineRef::new_box(Some(100)));
    //     assert!(call.step(&mut context, map).is_ok());

    //     let mut map = HashMap::new();
    //     map.insert("arg", PineRef::new_box(Some(10)));
    //     assert!(call.step(&mut context, map).is_ok());

    //     assert_eq!(call.run(&mut context), Ok(()));
    // }
}
