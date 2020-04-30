use super::context::{
    downcast_ctx, Context, ContextType, Ctx, PineRuntimeError, Runner, VarOperate,
};
// use super::ctxid_parser::CtxIdParser;
use super::output::{InputSrc, InputVal, SymbolInfo};
use super::{AnySeries, AnySeriesType};
use crate::ast::stat_expr_types::{Block, VarIndex};
use crate::types::{
    DataType, Float, Int, PineFrom, PineRef, PineType, RefData, RuntimeErr, Series,
};
use std::mem;
use std::rc::Rc;

pub trait Callback {
    fn print(&self, _str: String) {}

    fn plot(&self, _floats: Vec<f64>) {}
}

pub struct NoneCallback();
impl Callback for NoneCallback {}

pub struct DataSrc<'a> {
    lib_context: Box<dyn Ctx<'a> + 'a>,
    context: Box<dyn Ctx<'a> + 'a>,
    blk: &'a Block<'a>,
    input_index: i32,
    input_names: Vec<(&'a str, AnySeriesType)>,
    pub callback: &'a dyn Callback,
    inputs: Vec<Option<InputVal>>,
    input_srcs: Option<InputSrc>,
    has_run: bool,
}

pub fn parse_datalen<'a>(
    data: &Vec<(&'static str, AnySeries)>,
    names: &Vec<(&'a str, AnySeriesType)>,
) -> Result<usize, PineRuntimeError> {
    let lens: Vec<usize> = data
        .iter()
        .filter_map(|(name, v)| match names.iter().find(|(n, _)| n == name) {
            Some(_) => Some(v.len()),
            None => None,
        })
        .collect();
    if lens.len() == 0 {
        return Ok(0);
        // return Err(PineRuntimeError::new_no_range(RuntimeErr::NotValidParam));
    }
    for l in &lens[1..] {
        if *l != lens[0] {
            return Err(PineRuntimeError::new_no_range(RuntimeErr::NotValidParam));
        }
    }
    Ok(lens[0])
}

impl<'a> DataSrc<'a> {
    pub fn new(
        blk: &'a Block<'a>,
        lib_vars: Vec<(&'a str, PineRef<'a>)>,
        input_names: Vec<(&'a str, AnySeriesType)>,
        callback: &'a dyn Callback,
    ) -> DataSrc<'a> {
        let input_index = lib_vars.len() as i32;

        let mut context = Box::new(Context::new(None, ContextType::Library));
        let libvar_count = input_index + input_names.len() as i32;
        context.init(libvar_count, 1, 0);

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

        let libctx_ptr = Box::into_raw(context);
        let mut main_ctx = Box::new(Context::new(
            Some(unsafe { libctx_ptr.as_mut().unwrap() }),
            ContextType::Main,
        ));
        main_ctx.init(blk.var_count, blk.subctx_count, blk.libfun_count);

        DataSrc {
            blk,
            context: main_ctx,
            lib_context: unsafe { Box::from_raw(libctx_ptr) },
            input_names,
            input_index,
            callback,
            inputs: vec![],
            input_srcs: None,
            has_run: false,
        }
    }

    pub fn reset_vars(&mut self) {
        if !self.has_run {
            return;
        }
        println!("Will reset vars");
        let parent = unsafe { mem::transmute::<_, &mut (dyn Ctx<'a>)>(self.lib_context.as_mut()) };
        let mut main_ctx = Context::new(Some(parent), ContextType::Main);
        // Set the inputs and input sources.
        main_ctx.change_inputs(self.inputs.clone());
        if let Some(input_src) = self.input_srcs.as_ref() {
            main_ctx.add_input_src(input_src.clone());
        }

        // let libvar_count = self.input_index + self.input_names.len() as i32;
        main_ctx.init(
            self.blk.var_count,
            self.blk.subctx_count,
            self.blk.libfun_count,
        );
        self.context = Box::new(main_ctx);
    }

    pub fn change_inputs(&mut self, inputs: Vec<Option<InputVal>>) {
        self.inputs = inputs;
        downcast_ctx(self.context.as_mut()).change_inputs(self.inputs.clone());
    }

    pub fn set_input_srcs(&mut self, srcs: Vec<String>) {
        self.input_srcs = Some(InputSrc::new(None, srcs));
        downcast_ctx(self.context.as_mut())
            .add_input_src(self.input_srcs.as_ref().unwrap().clone());
    }

    fn run_data(
        &mut self,
        data: &Vec<(&'static str, AnySeries)>,
        start: i64,
        len: usize,
    ) -> Result<(), PineRuntimeError> {
        let bar_index = self.input_names.iter().position(|(s, _)| *s == "bar_index");
        let name_indexs: Vec<Option<usize>> = data
            .iter()
            .map(|(k, _)| self.input_names.iter().position(|(s, _)| *s == *k))
            .collect();
        for iter_i in start..(start + len as i64) {
            // Extract data into context
            for (index, (_k, v)) in data.iter().enumerate() {
                if let Some(name_index) = name_indexs[index] {
                    let var_index = VarIndex::new(self.input_index + name_index as i32, 0);
                    let series = self.lib_context.move_var(var_index).unwrap();
                    match series.get_type().0 {
                        DataType::Float => {
                            let mut float_s: RefData<Series<Float>> =
                                Series::implicity_from(series).unwrap();
                            float_s.update(v.index((iter_i - start) as isize));
                            self.lib_context.update_var(var_index, float_s.into_pf());
                        }
                        DataType::Int => {
                            let mut int_s: RefData<Series<Int>> =
                                Series::implicity_from(series).unwrap();
                            int_s.update(v.index((iter_i - start) as isize));
                            self.lib_context.update_var(var_index, int_s.into_pf());
                        }
                        _ => unreachable!(),
                    }
                } else {
                    // TODO: Remove this data copy.
                    downcast_ctx(self.lib_context.as_mut())
                        .insert_input_data(String::from(*_k), v.clone());
                }
            }

            if let Some(bar_index) = bar_index {
                let var_index = VarIndex::new(self.input_index + bar_index as i32, 0);
                let series = self.lib_context.move_var(var_index).unwrap();
                let mut index_s: RefData<Series<Int>> = Series::implicity_from(series).unwrap();
                index_s.update(Some(iter_i as i64));
                self.lib_context.update_var(var_index, index_s.into_pf());
            }

            self.context.set_iterindex(iter_i as i32);
            self.blk.run(self.context.as_mut())?;

            let lib_ctx = downcast_ctx(self.lib_context.as_mut());
            // main context is not children of Library context, so commit it alone.
            lib_ctx.commit();

            let main_ctx = downcast_ctx(self.context.as_mut());
            main_ctx.commit();
            main_ctx.clear_is_run();
            // self.context.clear_declare();
            main_ctx.reset_input_index();
            main_ctx.let_input_info_ready();
        }
        let main_ctx = downcast_ctx(self.context.as_mut());
        match downcast_ctx(main_ctx).run_callbacks() {
            Err(err) => Err(PineRuntimeError::new_no_range(err)),
            _ => {
                main_ctx.let_output_info_ready();
                Ok(())
            }
        }
    }

    pub fn run(
        &mut self,
        data: &Vec<(&'static str, AnySeries)>,
        syminfo: Option<Rc<SymbolInfo>>,
    ) -> Result<(), PineRuntimeError> {
        let len = parse_datalen(data, &self.input_names)?;
        self.runl(data, len, syminfo)
    }

    pub fn runl(
        &mut self,
        data: &Vec<(&'static str, AnySeries)>,
        len: usize,
        syminfo: Option<Rc<SymbolInfo>>,
    ) -> Result<(), PineRuntimeError> {
        // Update the range of data.
        self.reset_vars();
        let main_ctx = downcast_ctx(self.context.as_mut());
        main_ctx.update_data_range((Some(0), Some(len as i32)));
        if let Some(syminfo) = syminfo {
            main_ctx.set_syminfo(syminfo);
        }
        self.has_run = true;
        self.run_data(data, 0, len)
    }

    pub fn update(
        &mut self,
        data: &Vec<(&'static str, AnySeries)>,
    ) -> Result<(), PineRuntimeError> {
        let len = parse_datalen(data, &self.input_names)?;

        self.updatel(data, len)
    }

    pub fn updatel(
        &mut self,
        data: &Vec<(&'static str, AnySeries)>,
        len: usize,
    ) -> Result<(), PineRuntimeError> {
        let main_ctx = downcast_ctx(self.context.as_mut());

        // Get the range of exist running data.
        let range = main_ctx.get_data_range();
        // The new data's start index.
        let start = range.1.unwrap() - 1;
        main_ctx.update_data_range((Some(start), Some(start + len as i32)));
        main_ctx.roll_back()?;
        self.run_data(data, start as i64, len)
    }

    pub fn update_from(
        &mut self,
        data: &Vec<(&'static str, AnySeries)>,
        from: i32,
    ) -> Result<(), PineRuntimeError> {
        let len = parse_datalen(data, &self.input_names)?;

        self.update_froml(data, from, len)
    }

    pub fn update_froml(
        &mut self,
        data: &Vec<(&'static str, AnySeries)>,
        from: i32,
        len: usize,
    ) -> Result<(), PineRuntimeError> {
        let main_ctx = downcast_ctx(self.context.as_mut());

        let range = main_ctx.get_data_range();
        // Calculate the count of roll_back invocation
        let roll_count = range.1.unwrap() - from;
        main_ctx.update_data_range((Some(from), Some(from + len as i32)));

        for _ in 0..roll_count {
            main_ctx.roll_back()?;
        }
        self.run_data(data, from as i64, len)
    }

    pub fn get_context(&mut self) -> &mut dyn Ctx<'a> {
        unsafe { mem::transmute::<_, &mut dyn Ctx<'a>>(self.context.as_mut()) }
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
            SyntaxParser::new_with_libvars(&typemap).parse_blk(&mut blk),
            Ok(ParseValue::new_with_type(SyntaxType::Void))
        );
        assert_eq!(blk.var_count, 1);
        assert_eq!(blk.subctx_count, 0);

        let mut datasrc = DataSrc::new(
            &blk,
            vec![],
            vec![("close", AnySeriesType::Float)],
            &MyCallback,
        );
        assert_eq!(datasrc.context.var_len(), 1);

        let data = vec![(
            "close",
            AnySeries::from_float_vec(vec![Some(10f64), Some(100f64)]),
        )];

        assert_eq!(datasrc.run(&data, None), Ok(()));
        downcast_ctx(datasrc.context.as_mut()).map_var(VarIndex::new(0, 0), |hv| match hv {
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
