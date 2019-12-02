use super::{
    Bool, Category, ComplexType, DataType, Float, Int, PineFrom, PineRef, PineStaticType, PineType,
    RefData, RuntimeErr, SecondType, NA,
};
use crate::runtime::context::Ctx;
use std::cell::Cell;
use std::collections::HashMap;
use std::fmt;

pub trait SeriesCall<'a> {
    fn step(
        &self,
        _context: &mut dyn Ctx<'a>,
        _p: HashMap<&'a str, PineRef<'a>>,
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
        HashMap<&'a str, PineRef<'a>>,
    ) -> Result<PineRef<'a>, RuntimeErr>,
    params: Cell<HashMap<&'a str, PineRef<'a>>>,
    serie_names: Cell<Vec<&'a str>>,
    simple_names: Cell<Vec<&'a str>>,
    parsed: Cell<bool>,
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
            HashMap<&'a str, PineRef<'a>>,
        ) -> Result<PineRef<'a>, RuntimeErr>,
    ) -> SeriesToArrayCall<'a> {
        SeriesToArrayCall {
            params: Cell::new(HashMap::new()),
            func,
            serie_names: Cell::new(vec![]),
            simple_names: Cell::new(vec![]),
            parsed: Cell::new(false),
        }
    }
}

fn merge_array<'a>(
    cur_array: Option<PineRef<'a>>,
    v: PineRef<'a>,
) -> Result<PineRef<'a>, RuntimeErr> {
    match cur_array {
        None => match v.get_type() {
            (DataType::Int, _) => Ok(PineRef::new(vec![Int::implicity_from(v)?.into_inner()])),
            (DataType::Float, _) => Ok(PineRef::new(vec![Float::implicity_from(v)?.into_inner()])),
            (DataType::Bool, _) => Ok(PineRef::new(vec![Bool::implicity_from(v)?.into_inner()])),
            _t => Err(RuntimeErr::NotCompatible(format!(
                "The array type only support int, float, bool, bug get {:?}",
                _t
            ))),
        },
        Some(val) => match val.get_type() {
            (DataType::Int, SecondType::Array) => {
                let mut vec: RefData<Vec<Int>> = Vec::implicity_from(val)?;
                vec.push(Int::implicity_from(v)?.into_inner());
                Ok(vec.into_pf())
            }
            (DataType::Float, SecondType::Array) => {
                let mut vec: RefData<Vec<Float>> = Vec::implicity_from(val)?;
                vec.push(Float::implicity_from(v)?.into_inner());
                Ok(vec.into_pf())
            }
            (DataType::Bool, SecondType::Array) => {
                let mut vec: RefData<Vec<Bool>> = Vec::implicity_from(val)?;
                vec.push(Bool::implicity_from(v)?.into_inner());
                Ok(vec.into_pf())
            }
            _t => Err(RuntimeErr::NotCompatible(format!(
                "The array type only support int, float, bool, bug get {:?}",
                _t
            ))),
        },
    }
}

impl<'a> SeriesCall<'a> for SeriesToArrayCall<'a> {
    fn step(
        &self,
        _context: &mut dyn Ctx<'a>,
        mut _p: HashMap<&'a str, PineRef<'a>>,
    ) -> Result<PineRef<'a>, RuntimeErr> {
        if !self.parsed.get() {
            let mut serie_names: Vec<&'a str> = vec![];
            let mut simple_names: Vec<&'a str> = vec![];
            for (k, v) in _p.iter() {
                match v.get_type() {
                    (_, SecondType::Simple) => simple_names.push(*k),
                    (_, SecondType::Series) => serie_names.push(*k),
                    _ => unreachable!(),
                }
            }
            println!("series {:?} {:?}", serie_names, simple_names);

            self.serie_names.replace(serie_names);
            self.simple_names.replace(simple_names);
            self.parsed.replace(true);
        }
        let mut map = self.params.take();
        let serie_names = self.serie_names.take();
        for n in serie_names.iter() {
            match _p.remove(n) {
                None => {}
                Some(v) => {
                    map.insert(n, v);
                }
            }
        }
        self.serie_names.replace(serie_names);
        let simple_names = self.simple_names.take();
        for n in simple_names.iter() {
            match _p.remove(n) {
                None => {}
                Some(v) => {
                    let cur_val = map.remove(n);
                    let arr = merge_array(cur_val, v)?;
                    println!("cur array {:?}", arr);
                    map.insert(n, arr);
                }
            }
        }
        self.simple_names.replace(simple_names);
        self.params.set(map);
        Ok(PineRef::Box(Box::new(NA)))
    }

    fn run(&self, _context: &mut dyn Ctx<'a>) -> Result<(), RuntimeErr> {
        let val = self.params.take();
        (self.func)(_context, val)?;
        Ok(())
    }

    fn copy(&self) -> Box<dyn SeriesCall<'a> + 'a> {
        let map = self.params.take();
        let new_map: HashMap<&'a str, PineRef<'a>> =
            map.iter().map(|(&k, v)| (k, v.copy())).collect();
        self.params.set(map);
        Box::new(SeriesToArrayCall {
            params: Cell::new(new_map),
            func: self.func,
            serie_names: Cell::new(vec![]),
            simple_names: Cell::new(vec![]),
            parsed: Cell::new(false),
        })
    }
}

// pine callable type
pub struct Callable<'a> {
    func: Option<
        fn(
            context: &mut dyn Ctx<'a>,
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
                context: &mut dyn Ctx<'a>,
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
        context: &mut dyn Ctx<'a>,
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
    use super::super::Arithmetic;
    use super::*;
    use crate::runtime::context::{Context, ContextType};
    use crate::types::{downcast, downcast_pf};

    fn test_func<'a>(
        _context: &mut dyn Ctx<'a>,
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
        let mut context = Context::new(None, ContextType::Normal);

        let call_res = callable.call(
            &mut context,
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
        _context: &mut dyn Ctx<'a>,
        mut args: HashMap<&'a str, PineRef<'a>>,
    ) -> Result<PineRef<'a>, RuntimeErr> {
        println!("Get args {:?}", args);
        let (arg1, arg2) = (args.remove("arg1").unwrap(), args.remove("arg2").unwrap());
        let arg1_val = downcast_pf::<Vec<Int>>(arg1).unwrap();
        let arg2_val = downcast_pf::<Vec<Int>>(arg2).unwrap();
        let res: Vec<Int> = arg1_val
            .iter()
            .zip(arg2_val.iter())
            .map(|(v1, v2)| v1.add(v2.clone()))
            .collect();
        Ok(PineRef::new(res))
    }

    #[test]
    fn series_call_test() {
        let callable = Callable::new(
            None,
            Some(Box::new(SeriesToArrayCall::new(add_test_func))),
            vec!["arg1", "arg2"],
        );
        let gen_params = || {
            vec![
                PineRef::Box(Box::new(Some(1))),
                PineRef::Box(Box::new(Some(2))),
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
            mut args: HashMap<&'a str, PineRef<'a>>,
        ) -> Result<PineRef<'a>, RuntimeErr> {
            println!("args {:?}", args);
            let arg1 = args.remove("arg").unwrap();
            let arg1_val = downcast_pf::<Vec<Int>>(arg1).unwrap();
            assert_eq!(arg1_val.into_inner(), vec![Some(100), Some(10)]);
            Ok(PineRef::new_box(NA))
        }

        let call = SeriesToArrayCall::new(test_func);
        let mut context = Context::new(None, ContextType::Normal);

        let mut map = HashMap::new();
        map.insert("arg", PineRef::new_box(Some(100)));
        assert!(call.step(&mut context, map).is_ok());

        let mut map = HashMap::new();
        map.insert("arg", PineRef::new_box(Some(10)));
        assert!(call.step(&mut context, map).is_ok());

        assert_eq!(call.run(&mut context), Ok(()));
    }
}
