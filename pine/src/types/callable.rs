use super::{
    Bool, Category, ComplexType, DataType, Float, Int, PineFrom, PineRef, PineStaticType, PineType,
    RefData, RuntimeErr, SecondType, NA,
};
use crate::ast::stat_expr_types::VarIndex;
use crate::ast::syntax_type::{FunctionType, SyntaxType};
use crate::runtime::context::{commit_series_for_operator, Ctx, VarOperate};
use crate::runtime::statement::process_assign_val;
use crate::syntax::SyntaxCtx;
use std::cell::{Cell, RefCell};
use std::fmt;
use std::mem;

pub trait SeriesCall<'a> {
    fn init_param_len(&self, len: usize) {}

    fn step(
        &self,
        _context: &mut dyn Ctx<'a>,
        _p: Vec<Option<PineRef<'a>>>,
        _func_type: FunctionType<'a>,
    ) -> Result<PineRef<'a>, RuntimeErr> {
        Ok(PineRef::Box(Box::new(NA)))
    }

    fn run(&self, _context: &mut dyn Ctx<'a>) -> Result<(), RuntimeErr> {
        Ok(())
    }

    fn back(&self, _context: &mut dyn Ctx<'a>) -> Result<(), RuntimeErr> {
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
        context: &mut dyn Ctx<'a>,
        Vec<Option<PineRef<'a>>>,
        FunctionType<'a>,
    ) -> Result<(), RuntimeErr>,
    params: RefCell<Vec<Option<PineRef<'a>>>>,
    func_type: Cell<Option<FunctionType<'a>>>,
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
            context: &mut dyn Ctx<'a>,
            Vec<Option<PineRef<'a>>>,
            FunctionType<'a>,
        ) -> Result<(), RuntimeErr>,
    ) -> ParamCollectCall<'a> {
        ParamCollectCall {
            params: RefCell::new(vec![]),
            func,
            func_type: Cell::new(None),
        }
    }
}

impl<'a> VarOperate<'a> for Vec<Option<PineRef<'a>>> {
    fn create_var(&mut self, index: i32, val: PineRef<'a>) -> Option<PineRef<'a>> {
        mem::replace(&mut self[index as usize], Some(val))
    }

    fn update_var(&mut self, index: VarIndex, val: PineRef<'a>) {
        debug_assert!(index.rel_ctx == 0);
        self[index.varid as usize] = Some(val);
    }

    fn move_var(&mut self, index: VarIndex) -> Option<PineRef<'a>> {
        mem::replace(&mut self[index.varid as usize], None)
    }

    fn get_var(&self, index: VarIndex) -> &Option<PineRef<'a>> {
        &self[index.varid as usize]
    }

    fn var_len(&self) -> i32 {
        self.len() as i32
    }
}

impl<'a> SeriesCall<'a> for ParamCollectCall<'a> {
    fn init_param_len(&self, len: usize) {
        let mut params = Vec::with_capacity(len);
        params.resize_with(len, || None);
        self.params.replace(params);
    }

    fn step(
        &self,
        _context: &mut dyn Ctx<'a>,
        pmap: Vec<Option<PineRef<'a>>>,
        func_type: FunctionType<'a>,
    ) -> Result<PineRef<'a>, RuntimeErr> {
        // self.params.set(map);
        for (i, v) in pmap.into_iter().enumerate() {
            if let Some(v) = v {
                // Merge all of the series variable into the exists series variable
                process_assign_val(v, &mut *self.params.borrow_mut(), i as i32)?;
            }
        }
        // Commit all of the series variables.
        commit_series_for_operator(&mut *self.params.borrow_mut());
        self.func_type.replace(Some(func_type));
        Ok(PineRef::Box(Box::new(NA)))
    }

    fn run(&self, _context: &mut dyn Ctx<'a>) -> Result<(), RuntimeErr> {
        let val = self.params.replace(vec![]);
        (self.func)(_context, val, self.func_type.take().unwrap())?;
        Ok(())
    }

    fn copy(&self) -> Box<dyn SeriesCall<'a> + 'a> {
        let map = self.params.borrow();
        let new_map: Vec<_> = map
            .iter()
            .map(|v| match v {
                None => None,
                Some(v) => Some(v.copy()),
            })
            .collect();
        // self.params.set(map);
        let func_type = self.func_type.take();
        self.func_type.replace(func_type.clone());
        Box::new(ParamCollectCall {
            params: RefCell::new(new_map),
            func: self.func,
            func_type: Cell::new(func_type),
        })
    }
}

// pine callable type
pub struct Callable<'a> {
    func: Option<
        fn(
            context: &mut dyn Ctx<'a>,
            Vec<Option<PineRef<'a>>>,
            FunctionType<'a>,
        ) -> Result<PineRef<'a>, RuntimeErr>,
    >,
    caller: Option<Box<dyn SeriesCall<'a> + 'a>>,
    param_names: Option<Vec<&'a str>>,
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
                context: &mut dyn Ctx<'a>,
                Vec<Option<PineRef<'a>>>,
                FunctionType<'a>,
            ) -> Result<PineRef<'a>, RuntimeErr>,
        >,
        caller: Option<Box<dyn SeriesCall<'a> + 'a>>,
    ) -> Callable<'a> {
        Callable {
            func,
            caller,
            param_names: None,
        }
    }

    pub fn call(
        &mut self,
        context: &mut dyn Ctx<'a>,
        pos_args: Vec<PineRef<'a>>,
        dict_args: Vec<(&'a str, PineRef<'a>)>,
        func_type: FunctionType<'a>,
    ) -> Result<PineRef<'a>, RuntimeErr> {
        if self.param_names.is_none() {
            self.param_names = Some(func_type.arg_names());
            if let Some(ref caller) = self.caller {
                caller.init_param_len(self.param_names.as_ref().unwrap().len());
            }
        }
        let param_names = self.param_names.as_ref().unwrap();
        let param_len = param_names.len();
        if pos_args.len() > param_len {
            return Err(RuntimeErr::NotValidParam);
        }

        let mut all_args: Vec<Option<PineRef<'a>>> = Vec::with_capacity(param_len);
        all_args.resize_with(param_len, || None);
        for (i, val) in pos_args.into_iter().enumerate() {
            all_args[i] = Some(val);
        }
        for (name, val) in dict_args.into_iter() {
            match param_names.iter().position(|&v| name == v) {
                None => return Err(RuntimeErr::NotValidParam),
                Some(pos) => {
                    all_args[pos] = Some(val);
                }
            }
        }
        if let Some(func) = self.func {
            func(context, all_args, func_type)
        } else if let Some(ref caller) = self.caller {
            caller.step(context, all_args, func_type)
        } else {
            Ok(PineRef::Box(Box::new(NA)))
        }
    }

    pub fn back(&self, context: &mut dyn Ctx<'a>) -> Result<(), RuntimeErr> {
        if let Some(ref caller) = self.caller {
            caller.back(context)
        } else {
            Ok(())
        }
    }

    pub fn run(&self, context: &mut dyn Ctx<'a>) -> Result<(), RuntimeErr> {
        if let Some(ref caller) = self.caller {
            caller.run(context)
        } else {
            Ok(())
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct CallableFactory<'a> {
    create_func: fn() -> Callable<'a>,
}

impl<'a> PineFrom<'a, CallableFactory<'a>> for CallableFactory<'a> {}

impl<'a> PineStaticType for CallableFactory<'a> {
    fn static_type() -> (DataType, SecondType) {
        (DataType::CallableFactory, SecondType::Simple)
    }
}
impl<'a> PineType<'a> for CallableFactory<'a> {
    fn get_type(&self) -> (DataType, SecondType) {
        <Self as PineStaticType>::static_type()
    }

    fn category(&self) -> Category {
        Category::Complex
    }

    fn copy(&self) -> PineRef<'a> {
        PineRef::new_rc(self.clone())
    }
}

impl<'a> ComplexType for CallableFactory<'a> {}

impl<'a> CallableFactory<'a> {
    pub fn new(create_func: fn() -> Callable<'a>) -> CallableFactory<'a> {
        CallableFactory { create_func }
    }

    pub fn create(&self) -> Callable<'a> {
        (self.create_func)()
    }
}

#[cfg(test)]
mod tests {
    use super::super::primitive::Int;
    use super::*;
    use crate::ast::syntax_type::{SimpleSyntaxType, SyntaxType};
    use crate::runtime::context::{Context, ContextType};
    use crate::types::{downcast, downcast_pf, Series};

    fn test_func<'a>(
        _context: &mut dyn Ctx<'a>,
        mut args: Vec<Option<PineRef<'a>>>,
        _func_type: FunctionType<'a>,
    ) -> Result<PineRef<'a>, RuntimeErr> {
        let (arg1, arg2) = (
            mem::replace(&mut args[0], None),
            mem::replace(&mut args[1], None),
        );
        match (arg1, arg2) {
            (Some(PineRef::Box(a1)), Some(PineRef::Box(a2))) => {
                let s: Int = Some(
                    downcast::<Int>(a1).unwrap().unwrap() + downcast::<Int>(a2).unwrap().unwrap(),
                );
                Ok(PineRef::Box(Box::new(s) as Box<dyn PineType>))
            }
            _ => Err(RuntimeErr::NotSupportOperator),
        }
    }

    const INT_TYPE: SyntaxType = SyntaxType::Simple(SimpleSyntaxType::Int);

    #[test]
    fn callable_test() {
        let mut callable = Callable::new(Some(test_func), None);
        let mut context = Context::new(None, ContextType::Normal);

        let call_res = callable.call(
            &mut context,
            vec![
                PineRef::Box(Box::new(Some(1)) as Box<dyn PineType>),
                PineRef::Box(Box::new(Some(2)) as Box<dyn PineType>),
            ],
            vec![],
            FunctionType((vec![("arg1", INT_TYPE), ("arg2", INT_TYPE)], INT_TYPE)),
        );

        let val = downcast_pf::<Int>(call_res.unwrap()).unwrap();
        assert_eq!(val.into_inner(), Some(3));
    }

    fn add_test_func<'a>(
        _context: &mut dyn Ctx<'a>,
        mut args: Vec<Option<PineRef<'a>>>,
        _func_type: FunctionType<'a>,
    ) -> Result<(), RuntimeErr> {
        // println!("Get args {:?}", args);
        let (arg1, arg2) = (
            mem::replace(&mut args[0], None),
            mem::replace(&mut args[1], None),
        );
        let arg1_val = downcast_pf::<Series<Int>>(arg1.unwrap()).unwrap();
        let arg2_val = downcast_pf::<Series<Int>>(arg2.unwrap()).unwrap();
        assert_eq!(arg1_val.get_history(), &vec![Some(1), Some(3)]);
        assert_eq!(arg2_val.get_history(), &vec![Some(2), Some(4)]);
        Ok(())
    }

    #[test]
    fn series_call_test() {
        let mut callable =
            Callable::new(None, Some(Box::new(ParamCollectCall::new(add_test_func))));
        let gen_params = |val1, val2| {
            vec![
                PineRef::new(Series::from(Some(val1))),
                PineRef::new(Series::from(Some(val2))),
            ]
        };

        let mut context = Context::new(None, ContextType::Normal);
        let func_type = FunctionType((vec![("arg1", INT_TYPE), ("arg2", INT_TYPE)], INT_TYPE));

        let call_res = callable
            .call(&mut context, gen_params(1, 2), vec![], func_type.clone())
            .unwrap();
        assert_eq!(call_res.get_type(), (DataType::NA, SecondType::Simple));

        callable
            .call(&mut context, gen_params(3, 4), vec![], func_type.clone())
            .unwrap();
        callable.back(&mut context).unwrap();
        callable.run(&mut context).unwrap();
    }

    // This test case is not used! If the callable receive simple int, that must be const!.

    #[test]
    fn series_array_test() {
        fn test_func<'a>(
            _context: &mut dyn Ctx<'a>,
            mut args: Vec<Option<PineRef<'a>>>,
            _func_type: FunctionType<'a>,
        ) -> Result<(), RuntimeErr> {
            // println!("args {:?}", args);
            let arg1 = mem::replace(&mut args[0], None).unwrap();
            let arg1_val = downcast_pf::<Series<Int>>(arg1).unwrap();
            assert_eq!(arg1_val.get_history(), &vec![Some(100), Some(10)]);
            Ok(())
        }
        let func_type = FunctionType((vec![("arg1", INT_TYPE)], INT_TYPE));

        let call = ParamCollectCall::new(test_func);
        call.init_param_len(1);
        let mut context = Context::new(None, ContextType::Normal);

        assert!(call
            .step(
                &mut context,
                vec![Some(PineRef::new_rc(Series::from(Some(100))))],
                func_type.clone()
            )
            .is_ok());
        assert!(call
            .step(
                &mut context,
                vec![Some(PineRef::new_rc(Series::from(Some(10))))],
                func_type.clone()
            )
            .is_ok());

        assert_eq!(call.run(&mut context), Ok(()));
    }
}
