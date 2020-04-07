use super::{
    Bool, Category, ComplexType, DataType, Float, Int, PineFrom, PineRef, PineStaticType, PineType,
    RefData, Runnable, RuntimeErr, SecondType, NA,
};
use crate::ast::stat_expr_types::VarIndex;
use crate::ast::syntax_type::FunctionType;
use crate::runtime::context::{
    commit_series_for_operator, rollback_series_for_operator, Ctx, VarOperate,
};
use crate::runtime::statement::process_assign_val;
use std::cell::{Cell, RefCell};
use std::fmt;
use std::mem;

pub trait SeriesCall<'a> {
    fn init_param_len(&mut self, _len: usize) {}

    fn step(
        &mut self,
        _context: &mut dyn Ctx<'a>,
        _p: Vec<Option<PineRef<'a>>>,
        _func_type: FunctionType<'a>,
    ) -> Result<PineRef<'a>, RuntimeErr> {
        Ok(PineRef::Box(Box::new(NA)))
    }

    fn run(&mut self, _context: &mut dyn Ctx<'a>) -> Result<(), RuntimeErr> {
        Ok(())
    }

    fn run_with_cd(
        &mut self,
        _context: &mut dyn Ctx<'a>,
        _params: Vec<Option<PineRef<'a>>>,
        _func_type: FunctionType<'a>,
    ) -> Result<(), RuntimeErr> {
        Ok(())
    }

    fn back(&mut self, _context: &mut dyn Ctx<'a>) -> Result<(), RuntimeErr> {
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

pub type StepHandleFunc<'a> = fn(
    context: &mut dyn Ctx<'a>,
    Vec<Option<PineRef<'a>>>,
    FunctionType<'a>,
) -> Result<PineRef<'a>, RuntimeErr>;

pub type RunHandleFunc<'a> = fn(
    context: &mut dyn Ctx<'a>,
    Vec<Option<PineRef<'a>>>,
    FunctionType<'a>,
) -> Result<(), RuntimeErr>;

pub struct ParamCollectCall<'a> {
    step_func: Option<StepHandleFunc<'a>>,
    run_func: Option<RunHandleFunc<'a>>,
    caller: Option<Box<dyn SeriesCall<'a> + 'a>>,
    params: Vec<Option<PineRef<'a>>>,
    func_type: Option<FunctionType<'a>>,
}

impl<'a> fmt::Debug for ParamCollectCall<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.params)
    }
}

impl<'a> ParamCollectCall<'a> {
    pub fn new(func: RunHandleFunc<'a>) -> ParamCollectCall<'a> {
        ParamCollectCall {
            params: vec![],
            step_func: None,
            run_func: Some(func),
            caller: None,
            func_type: None,
        }
    }

    pub fn new_with_fns(
        step_func: Option<StepHandleFunc<'a>>,
        run_func: Option<RunHandleFunc<'a>>,
    ) -> ParamCollectCall<'a> {
        ParamCollectCall {
            params: vec![],
            step_func,
            run_func,
            caller: None,
            func_type: None,
        }
    }

    pub fn new_with_caller(caller: Box<dyn SeriesCall<'a> + 'a>) -> ParamCollectCall<'a> {
        ParamCollectCall {
            params: vec![],
            step_func: None,
            run_func: None,
            caller: Some(caller),
            func_type: None,
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
    fn init_param_len(&mut self, len: usize) {
        let mut params = Vec::with_capacity(len);
        params.resize_with(len, || None);
        self.params = params;
    }

    fn step(
        &mut self,
        _context: &mut dyn Ctx<'a>,
        pmap: Vec<Option<PineRef<'a>>>,
        func_type: FunctionType<'a>,
    ) -> Result<PineRef<'a>, RuntimeErr> {
        // self.params.set(map);
        for (i, v) in pmap.into_iter().enumerate() {
            if let Some(v) = v {
                let syntax_type = &func_type.signature.0[i].1;
                // Merge all of the series variable into the exists series variable
                process_assign_val(v, &mut self.params, i as i32, Some(syntax_type))?;
            }
        }

        self.func_type = Some(func_type.clone());
        // If the step function is specified, then we merge the series parameters.
        if let Some(step) = self.step_func {
            let ret_val = step(_context, self.params.clone(), func_type);
            commit_series_for_operator(&mut self.params);
            ret_val
        } else if let Some(caller) = &mut self.caller {
            let ret_val = caller.step(_context, self.params.clone(), func_type);
            commit_series_for_operator(&mut self.params);
            ret_val
        } else {
            // Commit all of the series variables.
            commit_series_for_operator(&mut self.params);
            Ok(PineRef::Box(Box::new(NA)))
        }
    }

    fn run(&mut self, _context: &mut dyn Ctx<'a>) -> Result<(), RuntimeErr> {
        if let Some(run_func) = self.run_func {
            let val = self.params.clone();
            run_func(_context, val, self.func_type.take().unwrap())?;
        } else if let Some(caller) = &mut self.caller {
            caller.run(_context)?;
            caller.run_with_cd(
                _context,
                self.params.clone(),
                self.func_type.take().unwrap(),
            )?;
        }
        Ok(())
    }

    fn back(&mut self, _context: &mut dyn Ctx<'a>) -> Result<(), RuntimeErr> {
        rollback_series_for_operator(&mut self.params);
        if let Some(caller) = &mut self.caller {
            caller.back(_context)?;
        }
        Ok(())
    }

    fn copy(&self) -> Box<dyn SeriesCall<'a> + 'a> {
        let new_map: Vec<_> = self
            .params
            .iter()
            .map(|v| match v {
                None => None,
                Some(v) => Some(v.copy()),
            })
            .collect();

        Box::new(ParamCollectCall {
            params: new_map,
            step_func: self.step_func.clone(),
            run_func: self.run_func.clone(),
            func_type: self.func_type.clone(),
            caller: match &self.caller {
                Some(caller) => Some(caller.copy()),
                None => None,
            },
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
            if let Some(ref mut caller) = self.caller {
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
        } else if let Some(ref mut caller) = self.caller {
            caller.step(context, all_args, func_type)
        } else {
            Ok(PineRef::Box(Box::new(NA)))
        }
    }
}

impl<'a> Runnable<'a> for Callable<'a> {
    fn back(&mut self, context: &mut dyn Ctx<'a>) -> Result<(), RuntimeErr> {
        if let Some(ref mut caller) = self.caller {
            caller.back(context)
        } else {
            Ok(())
        }
    }

    fn run(&mut self, context: &mut dyn Ctx<'a>) -> Result<(), RuntimeErr> {
        if let Some(ref mut caller) = self.caller {
            caller.run(context)
        } else {
            Ok(())
        }
    }
}

pub trait CallableCreator<'a> {
    fn create(&self) -> Callable<'a>;

    fn copy(&self) -> Box<dyn CallableCreator<'a>>;
}

impl<'a> fmt::Debug for dyn CallableCreator<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        "Callable Factory".fmt(f)
    }
}

#[derive(Debug)]
pub struct CallableFactory<'a> {
    create_func: Option<fn() -> Callable<'a>>,
    creator: Option<Box<dyn CallableCreator<'a>>>,
}

impl<'a> PineFrom<'a, CallableFactory<'a>> for CallableFactory<'a> {}

impl<'a> PartialEq for CallableFactory<'a> {
    fn eq(&self, _: &CallableFactory<'a>) -> bool {
        true
    }
}

impl<'a> Clone for CallableFactory<'a> {
    fn clone(&self) -> Self {
        let creator = match self.creator {
            Some(ref c) => Some(c.copy()),
            None => None,
        };

        CallableFactory {
            create_func: self.create_func.clone(),
            creator,
        }
    }
}

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
        CallableFactory {
            create_func: Some(create_func),
            creator: None,
        }
    }

    pub fn new_with_creator(creator: Box<dyn CallableCreator<'a>>) -> CallableFactory<'a> {
        CallableFactory {
            create_func: None,
            creator: Some(creator),
        }
    }

    pub fn create(&self) -> Callable<'a> {
        if let Some(func) = self.create_func {
            func()
        } else if let Some(creator) = &self.creator {
            creator.create()
        } else {
            unreachable!()
        }
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
                PineRef::Box(Box::new(Some(1i64)) as Box<dyn PineType>),
                PineRef::Box(Box::new(Some(2i64)) as Box<dyn PineType>),
            ],
            vec![],
            FunctionType::new((
                vec![
                    ("arg1", SyntaxType::int_series()),
                    ("arg2", SyntaxType::int_series()),
                ],
                SyntaxType::int_series(),
            )),
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
        assert_eq!(arg1_val.get_history(), &vec![Some(1)]);
        assert_eq!(arg2_val.get_history(), &vec![Some(2)]);
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
        let func_type = FunctionType::new((
            vec![
                ("arg1", SyntaxType::int_series()),
                ("arg2", SyntaxType::int_series()),
            ],
            SyntaxType::int_series(),
        ));

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
        let func_type = FunctionType::new((
            vec![("arg1", SyntaxType::int_series())],
            SyntaxType::int_series(),
        ));

        let mut call = ParamCollectCall::new(test_func);
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
