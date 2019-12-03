use super::context::{Context, ContextType, Ctx, Runner};
use super::statement::process_assign_val;
use crate::ast::stat_expr_types::FunctionDef;
use crate::types::{
    Category, ComplexType, DataType, PineFrom, PineRef, PineStaticType, PineType, RuntimeErr,
    SecondType,
};
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

    fn category(&self) -> Category {
        Category::Complex
    }

    fn copy(&self) -> PineRef<'a> {
        PineRef::new_rc(self.clone())
    }
}

impl<'a> PineFrom<'a, Function<'a>> for Function<'a> {}

impl<'a> ComplexType for Function<'a> {}

impl<'a> Function<'a> {
    pub fn new(def: &'a FunctionDef<'a>) -> Function<'a> {
        Function { def }
    }

    pub fn call(
        &self,
        context: &mut dyn Ctx<'a>,
        pos_args: Vec<PineRef<'a>>,
        dict_args: Vec<(&'a str, PineRef<'a>)>,
    ) -> Result<PineRef<'a>, RuntimeErr> {
        if pos_args.len() > self.def.params.len() {
            return Err(RuntimeErr::NotValidParam);
        }

        let mut all_args: HashMap<&'a str, PineRef<'a>> = HashMap::new();
        for (i, val) in pos_args.into_iter().enumerate() {
            let name = self.def.params[i].0;
            all_args.insert(name, val);
        }
        for (name, val) in dict_args.into_iter() {
            match self.def.params.iter().any(|&v| name == v.0) {
                false => return Err(RuntimeErr::NotValidParam),
                true => {
                    all_args.insert(name, val);
                }
            }
        }
        // let mut new_context = Context::new(Some(context), ContextType::FuncDefBlock);
        for (k, v) in all_args {
            // context.create_var(k, v);
            process_assign_val(v, context, k)?;
        }
        self.def.body.run(context)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::name::VarName;
    use crate::ast::stat_expr_types::{Block, Exp};
    use crate::types::series::Series;

    #[test]
    fn func_test() {
        let func_def = FunctionDef {
            name: VarName("hello"),
            params: vec![VarName("arg1")],
            body: Block::new(vec![], Some(Exp::VarName(VarName("arg1")))),
        };
        let func = Function::new(&func_def);
        let mut ctx = Context::new(None, ContextType::FuncDefBlock);
        assert_eq!(
            func.call(&mut ctx, vec![PineRef::new(Series::from(Some(1)))], vec![]),
            Ok(PineRef::new(Series::from(Some(1))))
        );
        assert_eq!(
            func.call(&mut ctx, vec![PineRef::new(Series::from(Some(10)))], vec![]),
            Ok(PineRef::new(Series::from(Some(10))))
        );
        ctx.commit();
        assert_eq!(
            func.call(
                &mut ctx,
                vec![PineRef::new(Series::from(Some(100)))],
                vec![]
            ),
            Ok(PineRef::new(Series::from(Some(100))))
        );
    }
}
