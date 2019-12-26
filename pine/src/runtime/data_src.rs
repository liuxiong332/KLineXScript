use super::context::{Context, ContextType, Ctx, PineRuntimeError, Runner, VarOperate};
// use super::ctxid_parser::CtxIdParser;
use crate::ast::input::{Position, StrRange};
use crate::ast::stat_expr_types::{Block, VarIndex};
use crate::types::{Float, PineFrom, PineRef, PineType, RefData, RuntimeErr, Series};
use std::collections::HashMap;

pub trait Callback {
    fn print(&self, _str: String) {}

    fn plot(&self, _floats: Vec<f64>) {}
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
        context.init(blk.var_count, blk.subctx_count);

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
            for (index, (_k, v)) in data.iter().enumerate() {
                let var_index = VarIndex::new(index as i32, 0);
                let series = self.context.move_var(var_index).unwrap();
                let mut float_s: RefData<Series<Float>> = Series::implicity_from(series).unwrap();
                float_s.update(v[i]);
                self.context.update_var(var_index, float_s.into_pf());
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
        for (i, (_, _)) in data.iter().enumerate() {
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
    use crate::syntax::{ParseValue, SyntaxParser};

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
        let mut typemap = vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))];
        assert_eq!(
            SyntaxParser::new_with_vars(typemap).parse_blk(&mut blk),
            Ok(ParseValue::new_with_type(SyntaxType::Void))
        );
        assert_eq!(blk.var_count, 2);
        assert_eq!(blk.subctx_count, 0);

        let mut datasrc = DataSrc::new(&mut blk, HashMap::new(), &MyCallback);
        assert_eq!(datasrc.context.var_len(), 2);

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
