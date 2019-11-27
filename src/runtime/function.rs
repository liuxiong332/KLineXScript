use super::context::{Context, ContextType, Ctx, Runner};
use crate::ast::stat_expr_types::FunctionDef;
use crate::types::{ConvertErr, DataType, PineFrom, PineStaticType, PineType, SecondType};
use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub struct Function<'a> {
    def: &'a FunctionDef<'a>,
}

impl<'a> PineStaticType for Function<'a> {
    fn static_type() -> (DataType, SecondType) {
        (DataType::Function, SecondType::Simple)
    }
}
impl<'a> PineType<'a> for Function<'a> {
    fn get_type(&self) -> (DataType, SecondType) {
        <Self as PineStaticType>::static_type()
    }

    fn copy(&'a self) -> Box<dyn PineType<'a> + 'a> {
        Box::new(self.clone())
    }
}

impl<'a> PineFrom<'a, Function<'a>> for Function<'a> {}

impl<'a> Function<'a> {
    pub fn new(def: &'a FunctionDef<'a>) -> Function<'a> {
        Function { def }
    }

    pub fn call(
        &self,
        context: &mut dyn Ctx<'a>,
        pos_args: Vec<Box<dyn PineType<'a> + 'a>>,
        dict_args: Vec<(&'a str, Box<dyn PineType<'a> + 'a>)>,
    ) -> Result<Box<dyn PineType<'a> + 'a>, ConvertErr> {
        if pos_args.len() > self.def.params.len() {
            return Err(ConvertErr::NotValidParam);
        }

        let mut all_args: HashMap<&'a str, Box<dyn PineType<'a> + 'a>> = HashMap::new();
        for (i, val) in pos_args.into_iter().enumerate() {
            let name = self.def.params[i].0;
            all_args.insert(name, val);
        }
        for (name, val) in dict_args.into_iter() {
            match self.def.params.iter().any(|&v| name == v.0) {
                false => return Err(ConvertErr::NotValidParam),
                true => {
                    all_args.insert(name, val);
                }
            }
        }
        let mut new_context = Context::new(Some(context), ContextType::FuncDefBlock);
        for (k, v) in all_args {
            new_context.create_var(k, v);
        }
        self.def.body.run(&mut new_context)
    }
}
