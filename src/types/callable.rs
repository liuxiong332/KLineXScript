use super::downcast::downcast_ref;
use super::{DataType, PineFrom, PineStaticType, PineType, RuntimeErr, SecondType, NA};
use crate::runtime::context::Ctx;
use std::cell::Cell;
use std::collections::HashMap;
use std::fmt;

pub trait SeriesCall<'a> {
    fn step(
        &self,
        _context: &mut dyn Ctx<'a>,
        _p: HashMap<&'a str, Box<dyn PineType<'a> + 'a>>,
    ) -> Result<Box<dyn PineType<'a> + 'a>, RuntimeErr> {
        Ok(Box::new(NA))
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
        let s: &SeriesToArrayCall;
        unsafe {
            let raw: *const dyn SeriesCall<'a> = self;
            let t = raw as *const SeriesToArrayCall;
            s = t.as_ref().unwrap();
        }
        s.fmt(f)
    }
}

pub struct SeriesToArrayCall<'a> {
    func: fn(
        context: &mut dyn Ctx<'a>,
        HashMap<&'a str, Box<dyn PineType<'a> + 'a>>,
    ) -> Result<Box<dyn PineType<'a> + 'a>, RuntimeErr>,
    params: Cell<HashMap<&'a str, Box<dyn PineType<'a> + 'a>>>,
}

impl<'a> fmt::Debug for SeriesToArrayCall<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let val = self.params.take();
        let res = write!(f, "{:?}", val);
        self.params.set(val);
        res
    }
}

impl<'a> SeriesToArrayCall<'a> {
    pub fn new(
        func: fn(
            context: &mut dyn Ctx<'a>,
            HashMap<&'a str, Box<dyn PineType<'a> + 'a>>,
        ) -> Result<Box<dyn PineType<'a> + 'a>, RuntimeErr>,
    ) -> SeriesToArrayCall<'a> {
        SeriesToArrayCall {
            params: Cell::new(HashMap::new()),
            func,
        }
    }
}

impl<'a> SeriesCall<'a> for SeriesToArrayCall<'a> {
    fn step(
        &self,
        _context: &mut dyn Ctx<'a>,
        _p: HashMap<&'a str, Box<dyn PineType<'a> + 'a>>,
    ) -> Result<Box<dyn PineType<'a> + 'a>, RuntimeErr> {
        self.params.set(_p);
        let newv = self.params.take();
        self.params.set(newv);
        Ok(Box::new(NA))
    }

    fn run(&self, _context: &mut dyn Ctx<'a>) -> Result<(), RuntimeErr> {
        let val = self.params.take();
        (self.func)(_context, val)?;
        Ok(())
    }

    fn copy(&self) -> Box<dyn SeriesCall<'a> + 'a> {
        let map = self.params.take();
        let new_map: HashMap<&'a str, Box<dyn PineType<'a> + 'a>> =
            map.iter().map(|(&k, v)| (k, v.copy())).collect();
        self.params.set(map);
        Box::new(SeriesToArrayCall {
            params: Cell::new(new_map),
            func: self.func,
        })
    }
}

// pine callable type
pub struct Callable<'a> {
    func: Option<
        fn(
            context: &mut dyn Ctx<'a>,
            HashMap<&'a str, Box<dyn PineType<'a> + 'a>>,
        ) -> Result<Box<dyn PineType<'a> + 'a>, RuntimeErr>,
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

    fn copy(&self) -> Box<dyn PineType<'a> + 'a> {
        Box::new(self.clone())
    }
}
impl<'a> PineFrom<'a, Callable<'a>> for Callable<'a> {}

impl<'a> Callable<'a> {
    pub fn new(
        func: Option<
            fn(
                context: &mut dyn Ctx<'a>,
                HashMap<&'a str, Box<dyn PineType<'a> + 'a>>,
            ) -> Result<Box<dyn PineType<'a> + 'a>, RuntimeErr>,
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
        context: &mut dyn Ctx<'a>,
        pos_args: Vec<Box<dyn PineType<'a> + 'a>>,
        dict_args: Vec<(&'a str, Box<dyn PineType<'a> + 'a>)>,
    ) -> Result<Box<dyn PineType<'a> + 'a>, RuntimeErr> {
        if pos_args.len() > self.param_names.len() {
            return Err(RuntimeErr::NotValidParam);
        }

        let mut all_args: HashMap<&'a str, Box<dyn PineType<'a> + 'a>> = HashMap::new();
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
            Ok(Box::new(NA))
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

#[cfg(test)]
mod tests {
    use super::super::primitive::Int;
    use super::*;
    use crate::runtime::context::{Context, ContextType};
    use crate::types::downcast;

    fn test_func<'a>(
        _context: &mut dyn Ctx<'a>,
        mut args: HashMap<&'a str, Box<dyn PineType<'a> + 'a>>,
    ) -> Result<Box<dyn PineType<'a> + 'a>, RuntimeErr> {
        let (arg1, arg2) = (args.remove("arg1").unwrap(), args.remove("arg2").unwrap());
        let s: Int =
            Some(downcast::<Int>(arg1).unwrap().unwrap() + downcast::<Int>(arg2).unwrap().unwrap());
        Ok(Box::new(s) as Box<dyn PineType>)
    }

    #[test]
    fn callable_test() {
        let callable = Callable::new(Some(test_func), None, vec!["arg1", "arg2"]);
        let mut context = Context::new(None, ContextType::Normal);

        let call_res = callable.call(
            &mut context,
            vec![
                Box::new(Some(1)) as Box<dyn PineType>,
                Box::new(Some(2)) as Box<dyn PineType>,
            ],
            vec![],
        );
        assert_eq!(
            downcast::<Int>(call_res.unwrap()).unwrap(),
            Box::new(Some(3))
        );
    }

    #[test]
    fn series_call_test() {
        let callable = Callable::new(
            None,
            Some(Box::new(SeriesToArrayCall::new(test_func))),
            vec!["arg1", "arg2"],
        );
        let gen_params = || {
            vec![
                Box::new(Some(1)) as Box<dyn PineType>,
                Box::new(Some(2)) as Box<dyn PineType>,
            ]
        };

        let mut context = Context::new(None, ContextType::Normal);

        let call_res = callable.call(&mut context, gen_params(), vec![]).unwrap();
        assert_eq!(call_res.get_type(), (DataType::NA, SecondType::Simple));

        callable.back(&mut context).unwrap();
        callable.run(&mut context).unwrap();
    }

    #[test]
    fn series_array_test() {
        fn test_func<'a>(
            _context: &mut dyn Ctx<'a>,
            mut args: HashMap<&'a str, Box<dyn PineType<'a> + 'a>>,
        ) -> Result<Box<dyn PineType<'a> + 'a>, RuntimeErr> {
            let arg1 = args.remove("arg").unwrap();
            let s: Box<Int> = downcast::<Int>(arg1).unwrap();
            assert_eq!(s, Box::new(Some(10)));
            Ok(s as Box<dyn PineType>)
        }

        let call = SeriesToArrayCall::new(test_func);
        let mut context = Context::new(None, ContextType::Normal);

        let mut map = HashMap::new();
        map.insert("arg", Box::new(Some(100)) as Box<dyn PineType>);
        assert!(call.step(&mut context, map).is_ok());

        let mut map = HashMap::new();
        map.insert("arg", Box::new(Some(10)) as Box<dyn PineType>);
        assert!(call.step(&mut context, map).is_ok());

        assert_eq!(call.run(&mut context), Ok(()));
    }
}
