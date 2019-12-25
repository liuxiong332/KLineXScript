use super::context::{Context, ContextType, Ctx, PineRuntimeError, Runner, VarOperate};
// use super::ctxid_parser::CtxIdParser;
use crate::ast::input::{Position, StrRange};
use crate::ast::stat_expr_types::{Block, VarIndex};
use crate::types::{Float, PineFrom, PineRef, PineType, RefData, RuntimeErr, Series};
use std::collections::HashMap;

pub trait Callback {
    fn print(&self, _str: String) {}

    fn plot(&self, floats: Vec<f64>) {}
}

pub struct DataSrc<'a, 'b, 'c> {
    context: Context<'a, 'b, 'c>,
    blk: &'a Block<'a>,
    pub callback: &'a dyn Callback,
}

fn get_len(data: &HashMap<&'static str, Vec<Float>>) -> Result<usize, PineRuntimeError> {
    let lens: Vec<usize> = data.iter().map(|(_, v)| v.len()).collect();
    if lens.len() == 0 {
        return Err(PineRuntimeError::new_no_range(RuntimeErr::NotValidParam));
    }
    for l in &lens[1..] {
        if *l != lens[0] {
            return Err(PineRuntimeError::new_no_range(RuntimeErr::NotValidParam));
        }
    }
    Ok(lens[0])
}

impl<'a, 'b, 'c> DataSrc<'a, 'b, 'c> {
    pub fn new(
        blk: &'a mut Block<'a>,
        lib_vars: HashMap<&'a str, PineRef<'a>>,
        callback: &'a dyn Callback,
    ) -> DataSrc<'a, 'b, 'c> {
        let mut context = Context::new_with_callback(callback);
        let mut vars: Vec<Option<PineRef<'a>>> = Vec::with_capacity(blk.var_count as usize);
        vars.resize(blk.var_count as usize, None);
        context.init_vars(vars);

        let ctx_count = blk.subctx_count as usize;
        let mut ctxs: Vec<Option<Box<dyn 'c + Ctx<'a>>>> = Vec::with_capacity(ctx_count);
        ctxs.resize_with(ctx_count, || None);
        context.init_sub_contexts(ctxs);

        for (i, (_k, v)) in lib_vars.into_iter().enumerate() {
            context.create_var(i as i32, v);
        }
        DataSrc {
            blk,
            context,
            callback,
        }
    }

    fn run_data(
        &mut self,
        data: HashMap<&'static str, Vec<Float>>,
        len: usize,
    ) -> Result<(), PineRuntimeError> {
        for i in 0..len {
            // Extract data into context
            for (i, (_k, v)) in data.iter().enumerate() {
                let index = VarIndex::new(i as i32, 0);
                let series = self.context.move_var(index).unwrap();
                let mut float_s: RefData<Series<Float>> = Series::implicity_from(series).unwrap();
                float_s.update(v[i]);
                self.context.update_var(index, float_s.into_pf());
            }
            self.blk.run(&mut self.context)?;
            self.context.commit();
            // self.context.clear_declare();
            self.context.clear_is_run();
        }
        match self.context.run_callbacks() {
            Err(err) => Err(PineRuntimeError::new_no_range(err)),
            _ => Ok(()),
        }
    }

    pub fn run(&mut self, data: HashMap<&'static str, Vec<Float>>) -> Result<(), PineRuntimeError> {
        let len = get_len(&data)?;

        // Create variable from the hash map
        for (i, (k, _)) in data.iter().enumerate() {
            let s: Series<Float> = Series::new();
            self.context.create_var(i as i32, PineRef::new_rc(s));
        }
        self.run_data(data, len)
    }

    pub fn update(
        &mut self,
        data: HashMap<&'static str, Vec<Float>>,
    ) -> Result<(), PineRuntimeError> {
        let len = get_len(&data)?;
        self.context.roll_back()?;
        self.run_data(data, len)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::input::{Position, StrRange};
    use crate::ast::name::VarName;
    use crate::ast::stat_expr_types::{Assignment, Exp, RVVarName, Statement};
    use crate::ast::syntax_type::{SimpleSyntaxType, SyntaxType};
    use crate::syntax::SyntaxParser;

    struct MyCallback;
    impl Callback for MyCallback {}

    #[test]
    fn datasrc_test() {
        let mut blk = Block::new(
            vec![Statement::Assignment(Box::new(Assignment::new(
                vec![VarName::new_with_start("hello", Position::new(0, 0))],
                Exp::VarName(RVVarName::new_with_start("close", Position::new(0, 6))),
                false,
                None,
                StrRange::from_start("hello=close", Position::new(0, 0)),
            )))],
            None,
            StrRange::from_start("hello=close", Position::new(0, 0)),
        );
        let mut typemap: HashMap<&str, SyntaxType> = HashMap::new();
        typemap.insert("close", SyntaxType::Series(SimpleSyntaxType::Float));
        SyntaxParser::new_with_vars(typemap);

        let mut datasrc = DataSrc::new(&mut blk, HashMap::new(), &MyCallback);

        let mut data = HashMap::new();
        data.insert("close", vec![Some(10f64), Some(100f64)]);

        assert_eq!(datasrc.run(data), Ok(()));
        datasrc.context.map_var(VarIndex::new(1, 0), |hv| match hv {
            None => None,
            Some(v) => {
                let ser: RefData<Series<Float>> = Series::implicity_from(v).unwrap();
                let expect_ser = RefData::new_rc(Series::from_vec(vec![Some(10f64), Some(100f64)]));
                assert_eq!(ser, expect_ser);
                Some(ser.into_pf())
            }
        });
    }
}
