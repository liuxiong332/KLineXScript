use super::{
    Callable, Category, ComplexType, DataType, PineClass, PineFrom, PineRef, PineStaticType,
    PineType, RuntimeErr, SecondType,
};
use crate::runtime::Ctx;

#[derive(Debug)]
pub struct SimpleCallableObject<'a> {
    obj: Box<dyn PineClass<'a> + 'a>,
    create_func: fn() -> Callable<'a>,
}

impl<'a> PartialEq for SimpleCallableObject<'a> {
    fn eq(&self, other: &SimpleCallableObject<'a>) -> bool {
        PartialEq::eq(&*self.obj, &*other.obj) && self.create_func == other.create_func
    }
}

impl<'a> PineStaticType for SimpleCallableObject<'a> {
    fn static_type() -> (DataType, SecondType) {
        (DataType::SimpleCallableObject, SecondType::Simple)
    }
}

impl<'a> PineType<'a> for SimpleCallableObject<'a> {
    fn get_type(&self) -> (DataType, SecondType) {
        <Self as PineStaticType>::static_type()
    }

    fn category(&self) -> Category {
        Category::Complex
    }

    fn copy(&self) -> PineRef<'a> {
        PineRef::new_rc(SimpleCallableObject::new(self.obj.copy(), self.create_func))
    }
}

impl<'a> PineFrom<'a, SimpleCallableObject<'a>> for SimpleCallableObject<'a> {}

impl<'a> ComplexType for SimpleCallableObject<'a> {}

impl<'a> SimpleCallableObject<'a> {
    pub fn new(
        obj: Box<dyn PineClass<'a> + 'a>,
        create_func: fn() -> Callable<'a>,
    ) -> SimpleCallableObject<'a> {
        SimpleCallableObject { obj, create_func }
    }

    pub fn get(&self, context: &mut dyn Ctx<'a>, name: &str) -> Result<PineRef<'a>, RuntimeErr> {
        self.obj.get(context, name)
    }

    pub fn set(&self, name: &str, property: PineRef<'a>) -> Result<(), RuntimeErr> {
        self.obj.set(name, property)
    }

    pub fn create(&self) -> Callable<'a> {
        (self.create_func)()
    }
}

#[cfg(test)]
mod tests {
    use super::super::{downcast_pf, Callable, Int, Object, RefData};
    use super::*;
    use crate::ast::syntax_type::FunctionType;
    use crate::ast::syntax_type::{SimpleSyntaxType, SyntaxType};
    use crate::runtime::context::{Context, ContextType, Ctx};
    use std::mem;

    struct A;
    impl<'a> PineClass<'a> for A {
        fn custom_type(&self) -> &str {
            "Custom A"
        }

        fn get(&self, _ctx: &mut dyn Ctx<'a>, name: &str) -> Result<PineRef<'a>, RuntimeErr> {
            match name {
                "int1" => Ok(PineRef::new_box(Some(1i64))),
                "int2" => Ok(PineRef::new_box(Some(2i64))),
                "float1" => Ok(PineRef::new_box(Some(1f64))),
                "float2" => Ok(PineRef::new_box(Some(2f64))),
                _ => Err(RuntimeErr::NotSupportOperator),
            }
        }

        fn set(&self, _n: &str, _p: PineRef<'a>) -> Result<(), RuntimeErr> {
            Err(RuntimeErr::NotSupportOperator)
        }

        fn copy(&self) -> Box<dyn PineClass<'a> + 'a> {
            Box::new(A)
        }
    }

    fn test_func<'a>(
        _context: &mut dyn Ctx<'a>,
        mut args: Vec<Option<PineRef<'a>>>,
        _func_type: FunctionType<'a>,
    ) -> Result<PineRef<'a>, RuntimeErr> {
        Ok(mem::replace(&mut args[0], None).unwrap())
    }
    const INT_TYPE: SyntaxType = SyntaxType::Simple(SimpleSyntaxType::Int);

    #[test]
    fn object_test() {
        let obj = SimpleCallableObject::new(Box::new(A), || Callable::new(Some(test_func), None));
        let mut context = Context::new(None, ContextType::Normal);

        assert_eq!(
            obj.get_type(),
            (DataType::SimpleCallableObject, SecondType::Simple)
        );
        assert_eq!(
            downcast_pf::<Int>(obj.get(&mut context, "int1").unwrap()).unwrap(),
            RefData::new_box(Some(1))
        );
        let mut callable = obj.create();

        assert_eq!(
            downcast_pf::<Int>(
                callable
                    .call(
                        &mut context,
                        vec![PineRef::new_box(Some(1))],
                        vec![],
                        FunctionType::new((vec![("arg1", INT_TYPE)], INT_TYPE))
                    )
                    .unwrap()
            )
            .unwrap(),
            RefData::new_box(Some(1))
        );
    }
}
