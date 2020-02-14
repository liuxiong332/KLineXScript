use super::context::{Context, Ctx, PineRuntimeError, Runner, VarOperate};
// use super::ctxid_parser::CtxIdParser;
use super::output::{InputVal, SymbolInfo};
use super::{AnySeries, AnySeriesType};
use crate::ast::stat_expr_types::{Block, VarIndex};
use crate::types::{
    DataType, Float, Int, PineFrom, PineRef, PineType, RefData, RuntimeErr, Series,
};
use std::rc::Rc;

pub trait Callback {
    fn print(&self, _str: String) {}

    fn plot(&self, _floats: Vec<f64>) {}
}

pub struct NoneCallback();
impl Callback for NoneCallback {}

pub struct DataSrc<'a, 'b, 'c> {
    context: Context<'a, 'b, 'c>,
    blk: &'a Block<'a>,
    input_index: i32,
    input_names: Vec<(&'a str, AnySeriesType)>,
    pub callback: &'a dyn Callback,
}

fn get_len(data: &Vec<(&'static str, AnySeries)>) -> Result<usize, PineRuntimeError> {
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
        blk: &'a Block<'a>,
        lib_vars: Vec<(&'a str, PineRef<'a>)>,
        input_names: Vec<(&'a str, AnySeriesType)>,
        callback: &'a dyn Callback,
    ) -> DataSrc<'a, 'b, 'c> {
        let mut context = Context::new_with_callback(callback);
        context.init(blk.var_count, blk.subctx_count, blk.libfun_count);

        let input_index = lib_vars.len() as i32;

        for (i, (_k, v)) in lib_vars.into_iter().enumerate() {
            context.create_var(i as i32, v);
            context.set_varname_index(_k, i as i32);
        }

        // Create variable from the hash map
        for (i, (name, input_type)) in input_names.iter().enumerate() {
            match input_type {
                AnySeriesType::Int => {
                    let s: Series<Int> = Series::new();
                    context.create_var(input_index + i as i32, PineRef::new_rc(s));
                    context.set_varname_index(*name, input_index + i as i32);
                }
                AnySeriesType::Float => {
                    let s: Series<Float> = Series::new();
                    context.create_var(input_index + i as i32, PineRef::new_rc(s));
                    context.set_varname_index(*name, input_index + i as i32);
                }
            }
        }
        DataSrc {
            blk,
            context,
            input_names,
            input_index,
            callback,
        }
    }

    pub fn change_inputs(&mut self, inputs: Vec<Option<InputVal>>) {
        self.context.change_inputs(inputs);
    }

    pub fn set_input_srcs(&mut self, srcs: Vec<String>) {
        self.context.set_input_srcs(srcs);
    }

    fn run_data(
        &mut self,
        data: &Vec<(&'static str, AnySeries)>,
        len: usize,
    ) -> Result<(), PineRuntimeError> {
        let bar_index = self.input_names.iter().position(|(s, _)| *s == "bar_index");
        let name_indexs: Vec<Option<usize>> = data
            .iter()
            .map(|(k, _)| self.input_names.iter().position(|(s, _)| *s == *k))
            .collect();
        for i in 0..len {
            // Extract data into context
            for (index, (_k, v)) in data.iter().enumerate() {
                if let Some(name_index) = name_indexs[index] {
                    let var_index = VarIndex::new(self.input_index + name_index as i32, 0);
                    let series = self.context.move_var(var_index).unwrap();
                    match series.get_type().0 {
                        DataType::Float => {
                            let mut float_s: RefData<Series<Float>> =
                                Series::implicity_from(series).unwrap();
                            float_s.update(v.index(i as isize));
                            self.context.update_var(var_index, float_s.into_pf());
                        }
                        DataType::Int => {
                            let mut int_s: RefData<Series<Int>> =
                                Series::implicity_from(series).unwrap();
                            int_s.update(v.index(i as isize));
                            self.context.update_var(var_index, int_s.into_pf());
                        }
                        _ => unreachable!(),
                    }
                }
            }

            if let Some(bar_index) = bar_index {
                let var_index = VarIndex::new(self.input_index + bar_index as i32, 0);
                let series = self.context.move_var(var_index).unwrap();
                let mut index_s: RefData<Series<Int>> = Series::implicity_from(series).unwrap();
                index_s.update(Some(i as i64 + 1));
                self.context.update_var(var_index, index_s.into_pf());
            }

            self.blk.run(&mut self.context)?;
            self.context.commit();
            // self.context.clear_declare();
            self.context.clear_is_run();
            self.context.reset_input_index();
            self.context.let_input_info_ready();
        }
        match self.context.run_callbacks() {
            Err(err) => Err(PineRuntimeError::new_no_range(err)),
            _ => {
                self.context.let_output_info_ready();
                Ok(())
            }
        }
    }

    pub fn run(
        &mut self,
        data: &Vec<(&'static str, AnySeries)>,
        syminfo: Option<Rc<SymbolInfo>>,
    ) -> Result<(), PineRuntimeError> {
        let len = get_len(data)?;
        // Update the range of data.
        self.context.update_data_range((Some(0), Some(len as i32)));
        if let Some(syminfo) = syminfo {
            self.context.set_syminfo(syminfo);
        }
        self.run_data(data, len)
    }

    pub fn update(
        &mut self,
        data: &Vec<(&'static str, AnySeries)>,
    ) -> Result<(), PineRuntimeError> {
        let len = get_len(data)?;

        // Get the range of exist running data.
        let range = self.context.get_data_range();
        // The new data's start index.
        let start = range.1.unwrap() - 1;
        self.context
            .update_data_range((Some(start), Some(start + len as i32)));
        self.context.roll_back()?;
        self.run_data(data, len)
    }

    pub fn update_from(
        &mut self,
        data: &Vec<(&'static str, AnySeries)>,
        from: i32,
    ) -> Result<(), PineRuntimeError> {
        let len = get_len(data)?;

        let range = self.context.get_data_range();
        // Calculate the count of roll_back invocation
        let roll_count = range.1.unwrap() - from;
        self.context
            .update_data_range((Some(from), Some(from + len as i32)));

        for _ in 0..roll_count {
            self.context.roll_back()?;
        }
        self.run_data(data, len)
    }

    pub fn get_context(&mut self) -> &mut Context<'a, 'b, 'c> {
        &mut self.context
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
        let typemap = vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))];
        assert_eq!(
            SyntaxParser::new_with_vars(&typemap).parse_blk(&mut blk),
            Ok(ParseValue::new_with_type(SyntaxType::Void))
        );
        assert_eq!(blk.var_count, 2);
        assert_eq!(blk.subctx_count, 0);

        let mut datasrc = DataSrc::new(
            &blk,
            vec![],
            vec![("close", AnySeriesType::Float)],
            &MyCallback,
        );
        assert_eq!(datasrc.context.var_len(), 2);

        let data = vec![(
            "close",
            AnySeries::from_float_vec(vec![Some(10f64), Some(100f64)]),
        )];

        assert_eq!(datasrc.run(&data, None), Ok(()));
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
