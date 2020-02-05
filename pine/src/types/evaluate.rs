use crate::runtime::context::{Ctx, PineRuntimeError};
use crate::syntax::SyntaxCtx;
use crate::types::traits::{Category, ComplexType, DataType, PineStaticType, PineType, SecondType};
use crate::types::PineRef;
use std::fmt;

pub trait EvaluateVal<'a> {
    fn prepare(&mut self, syntax_ctx: &mut dyn SyntaxCtx<'a>);

    fn run(&self, ctx: &mut dyn Ctx<'a>) -> Result<PineRef<'a>, PineRuntimeError>;

    fn copy(&self) -> Box<dyn EvaluateVal<'a>>;

    fn eq(&self, other: &dyn EvaluateVal<'a>) -> bool;

    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result;
}

pub fn downcast_evaluate_val_ref<'a, 'b, T: EvaluateVal<'a>>(
    item: &'b (dyn EvaluateVal<'a>),
) -> &'b T {
    unsafe {
        let raw: *const dyn EvaluateVal<'a> = item;
        let t = raw as *const T;
        t.as_ref().unwrap()
    }
}

pub struct Evaluate<'a> {
    val: Box<dyn EvaluateVal<'a>>,
}

impl<'a> Evaluate<'a> {
    pub fn new(val: Box<dyn EvaluateVal<'a>>) -> Evaluate<'a> {
        Evaluate { val }
    }

    pub fn prepare(&mut self, syntax_ctx: &mut dyn SyntaxCtx<'a>) {
        self.val.prepare(syntax_ctx)
    }

    pub fn run(&self, ctx: &mut dyn Ctx<'a>) -> Result<PineRef<'a>, PineRuntimeError> {
        self.val.run(ctx)
    }
}

impl<'a> PineStaticType for Evaluate<'a> {
    fn static_type() -> (DataType, SecondType) {
        (DataType::Evaluate, SecondType::Simple)
    }
}

impl<'a> PineType<'a> for Evaluate<'a> {
    fn get_type(&self) -> (DataType, SecondType) {
        (DataType::Evaluate, SecondType::Simple)
    }

    fn category(&self) -> Category {
        Category::Complex
    }

    fn copy(&self) -> PineRef<'a> {
        PineRef::new_rc(Evaluate {
            val: self.val.copy(),
        })
    }
}

impl<'a> ComplexType for Evaluate<'a> {}

impl<'a> PartialEq for Evaluate<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.val.eq(&*other.val)
    }
}

impl<'a> fmt::Debug for Evaluate<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.val.fmt(f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::stat_expr_types::VarIndex;
    use crate::runtime::context::{Context, ContextType as RunContextType, VarOperate};
    use crate::syntax::{ContextType, SyntaxContext};
    use crate::types::downcast_pf_ref;
    use crate::types::{Arithmetic, Float, PineFrom, RefData, RuntimeErr, Series};

    #[derive(Debug, Clone, PartialEq)]
    struct MyVal {
        close_index: VarIndex,
        open_index: VarIndex,
    }

    impl MyVal {
        pub fn new() -> MyVal {
            MyVal {
                close_index: VarIndex::new(0, 0),
                open_index: VarIndex::new(0, 0),
            }
        }
    }

    impl<'a> EvaluateVal<'a> for MyVal {
        fn prepare(&mut self, syntax_ctx: &mut dyn SyntaxCtx<'a>) {
            self.close_index = syntax_ctx.get_var_index("close");
            self.open_index = syntax_ctx.get_var_index("open");
        }

        fn run(&self, ctx: &mut dyn Ctx<'a>) -> Result<PineRef<'a>, PineRuntimeError> {
            match (ctx.get_var(self.close_index), ctx.get_var(self.open_index)) {
                (Some(close_val), Some(open_val)) => {
                    let close = downcast_pf_ref::<Series<Float>>(close_val).unwrap();
                    let open = downcast_pf_ref::<Series<Float>>(open_val).unwrap();
                    Ok(PineRef::new_rc(Series::from(
                        close.get_current().add(open.get_current()),
                    )))
                }
                _ => Err(PineRuntimeError::new_no_range(RuntimeErr::VarNotFound)),
            }
        }

        fn copy(&self) -> Box<dyn EvaluateVal<'a>> {
            Box::new(self.clone())
        }

        fn eq(&self, other: &dyn EvaluateVal<'a>) -> bool {
            PartialEq::eq(self, downcast_evaluate_val_ref::<MyVal>(other))
        }

        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            fmt::Debug::fmt(self, f)
        }
    }

    #[test]
    fn evaluate_test() {
        let mut evaluate = Evaluate::new(Box::new(MyVal::new()));

        let mut syntax_ctx = SyntaxContext::new(None, ContextType::Normal);
        syntax_ctx.gen_var_index("close");
        syntax_ctx.gen_var_index("open");

        evaluate.prepare(&mut syntax_ctx);

        let mut context = Context::new(None, RunContextType::Normal);
        context.init(2, 0, 0);
        context.create_var(0, PineRef::new(Series::from(Some(1f64))));
        context.create_var(1, PineRef::new(Series::from(Some(2f64))));

        assert_eq!(
            evaluate.run(&mut context),
            Ok(PineRef::new_rc(Series::from(Some(3f64))))
        );
    }
}
