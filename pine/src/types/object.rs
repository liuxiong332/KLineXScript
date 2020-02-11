use super::{
    Category, ComplexType, DataType, PineClass, PineFrom, PineRef, PineStaticType, PineType,
    RuntimeErr, SecondType,
};

#[derive(Debug)]
pub struct Object<'a> {
    obj: Box<dyn PineClass<'a> + 'a>,
}

impl<'a> PartialEq for Object<'a> {
    fn eq(&self, other: &Object<'a>) -> bool {
        PartialEq::eq(&*self.obj, &*other.obj)
    }
}

impl<'a> PineStaticType for Object<'a> {
    fn static_type() -> (DataType, SecondType) {
        (DataType::Object, SecondType::Simple)
    }
}
impl<'a> PineType<'a> for Object<'a> {
    fn get_type(&self) -> (DataType, SecondType) {
        <Self as PineStaticType>::static_type()
    }

    fn category(&self) -> Category {
        Category::Complex
    }

    fn copy(&self) -> PineRef<'a> {
        PineRef::new_rc(Object::new(self.obj.copy()))
    }
}

impl<'a> PineFrom<'a, Object<'a>> for Object<'a> {}

impl<'a> ComplexType for Object<'a> {}

impl<'a> Object<'a> {
    pub fn new(obj: Box<dyn PineClass<'a> + 'a>) -> Object<'a> {
        Object { obj }
    }

    pub fn get(&self, name: &str) -> Result<PineRef<'a>, RuntimeErr> {
        self.obj.get(name)
    }

    pub fn set(&self, name: &str, property: PineRef<'a>) -> Result<(), RuntimeErr> {
        self.obj.set(name, property)
    }
}

#[cfg(test)]
mod tests {
    use super::super::{downcast_pf, Int, RefData};
    use super::*;

    struct A;
    impl<'a> PineClass<'a> for A {
        fn custom_type(&self) -> &str {
            "Custom A"
        }

        fn get(&self, name: &str) -> Result<PineRef<'a>, RuntimeErr> {
            match name {
                "int1" => Ok(PineRef::new_box(Some(1i32))),
                "int2" => Ok(PineRef::new_box(Some(2i32))),
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

    #[test]
    fn object_test() {
        let obj = Object::new(Box::new(A));
        assert_eq!(obj.get_type(), (DataType::Object, SecondType::Simple));
        assert_eq!(
            downcast_pf::<Int>(obj.get("int1").unwrap()).unwrap(),
            RefData::new_box(Some(1))
        );
    }
}
