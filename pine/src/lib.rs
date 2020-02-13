// #[macro_use]
extern crate nom;
use nom::Err;

#[macro_use]
extern crate lazy_static;

#[macro_use]
extern crate assert_matches;

#[macro_use]
extern crate serde_derive;

extern crate regex;

pub mod ast;

#[macro_use]
pub mod helper;

pub mod libs;
pub mod runtime;
pub mod syntax;
pub mod types;

use ast::error::PineErrorKind;
use ast::input::{Input, Position, StrRange};
use ast::stat_expr::block;
use ast::stat_expr_types::Block;
use ast::state::{AstState, PineInputError};
use ast::syntax_type::{SimpleSyntaxType, SyntaxType};

use syntax::SyntaxParser;

use libs::{declare_vars, VarResult};
use runtime::context::{Context, PineRuntimeError};
use runtime::data_src::{Callback, DataSrc};
use runtime::error_format::{ErrorFormater, PineFormatError};
use runtime::output::{IOInfo, InputVal, OutputData, OutputDataCollect};
use std::mem;
use types::{Float, PineRef};

#[derive(Debug, Clone)]
pub struct LibInfo<'a> {
    var_types: Vec<(&'a str, SyntaxType<'a>)>,
    var_values: Vec<(&'a str, PineRef<'a>)>,
    input_names: Vec<&'a str>, // The input varnames include bar_index
    client_input_names: Vec<&'a str>, // The input varnames user client should pass in
}

const BAR_INDEX: &'static str = "bar_index";

impl<'a> LibInfo<'a> {
    pub fn new(
        lib_vars: Vec<VarResult<'a>>,
        input_vars: Vec<(&'static str, SyntaxType<'a>)>,
    ) -> LibInfo<'a> {
        let (types, values, input_names, client_input_names) = extract_vars(lib_vars, input_vars);
        // println!("now input names {:?}", input_names);
        LibInfo {
            var_types: types,
            var_values: values,
            input_names,
            client_input_names,
        }
    }
}

pub struct PineParser<'a, 'b> {
    src: &'a str,
    var_types: &'b Vec<(&'a str, SyntaxType<'a>)>,
    client_input_names: &'b Vec<&'a str>,
}

impl<'a, 'b> PineParser<'a, 'b> {
    pub fn new(src: &'a str, lib_info: &'b LibInfo<'a>) -> PineParser<'a, 'b> {
        PineParser {
            src,
            var_types: &lib_info.var_types,
            client_input_names: &lib_info.client_input_names,
        }
    }

    pub fn parse(
        &mut self,
    ) -> Result<(Block<'a>, SyntaxParser<'a>, Vec<PineInputError>), Vec<PineInputError>> {
        let mut all_errs = vec![];
        let mut blk = match parse_ast(self.src) {
            Ok(blk) => blk,
            Err((Some(blk), errs)) => {
                all_errs = errs;
                blk
            }
            Err((None, errs)) => return Err(errs),
        };
        let syntax_parser;
        match parse_syntax(&mut blk, &self.var_types, &self.client_input_names) {
            Ok(parser) => syntax_parser = parser,
            Err((parser, errs)) => {
                all_errs.extend(errs);
                if parser.is_none() {
                    return Err(all_errs);
                }
                syntax_parser = parser.unwrap();
            }
        }
        Ok((blk, syntax_parser, all_errs))
    }

    pub fn parse_blk(&mut self) -> Result<Block<'a>, Vec<PineInputError>> {
        match self.parse() {
            Ok((blk, _, errs)) => {
                if errs.is_empty() {
                    Ok(blk)
                } else {
                    Err(errs)
                }
            }
            Err(errs) => Err(errs),
        }
    }
}

pub struct PineRunner<'a, 'b, 'c> {
    datasrc: DataSrc<'a, 'b, 'c>,
}

impl<'a, 'b, 'c> PineRunner<'a, 'b, 'c> {
    pub fn new(
        lib_info: &LibInfo<'a>,
        blk: &Block<'a>,
        callback: &'a dyn Callback,
    ) -> PineRunner<'a, 'b, 'c> {
        let var_values = lib_info.var_values.clone();
        let input_names = lib_info.input_names.clone();

        let blk_ref = unsafe { mem::transmute::<&Block<'a>, &'a Block<'a>>(blk) };
        let datasrc = DataSrc::new(blk_ref, var_values, input_names, callback);
        PineRunner { datasrc }
    }

    pub fn run(&mut self, data: &Vec<(&'static str, Vec<Float>)>) -> Result<(), PineRuntimeError> {
        self.datasrc.run(data)
    }

    pub fn update(
        &mut self,
        data: &Vec<(&'static str, Vec<Float>)>,
    ) -> Result<(), PineRuntimeError> {
        self.datasrc.update(data)
    }

    pub fn update_from(
        &mut self,
        data: &Vec<(&'static str, Vec<Float>)>,
        from: i32,
    ) -> Result<(), PineRuntimeError> {
        self.datasrc.update_from(data, from)
    }

    pub fn set_input_srcs(&mut self, srcs: Vec<String>) {
        self.datasrc.set_input_srcs(srcs);
    }

    pub fn change_inputs(&mut self, inputs: Vec<Option<InputVal>>) {
        self.datasrc.change_inputs(inputs);
    }

    pub fn get_context(&mut self) -> &mut Context<'a, 'b, 'c> {
        self.datasrc.get_context()
    }
}

pub struct PineScript<'pa, 'li, 'ra, 'rb, 'rc> {
    lib_info: LibInfo<'li>,
    blk: Block<'pa>,
    syntax_parser: Option<SyntaxParser<'pa>>,
    callback: Option<&'ra dyn Callback>,
    runner: Option<PineRunner<'ra, 'rb, 'rc>>,
    data: Vec<(&'static str, Vec<Float>)>,
    error_format: ErrorFormater,
}

const SERIES_FLOAT: SyntaxType = SyntaxType::Series(SimpleSyntaxType::Float);
const SERIES_INT: SyntaxType = SyntaxType::Series(SimpleSyntaxType::Int);

impl<'pa, 'li, 'ra, 'rb, 'rc> PineScript<'pa, 'li, 'ra, 'rb, 'rc> {
    pub fn new(callback: Option<&'ra dyn Callback>) -> PineScript<'pa, 'li, 'ra, 'rb, 'rc> {
        let lib_info = LibInfo::new(
            declare_vars(),
            vec![
                ("close", SERIES_FLOAT.clone()),
                ("open", SERIES_FLOAT.clone()),
                ("high", SERIES_FLOAT.clone()),
                ("low", SERIES_FLOAT.clone()),
                (BAR_INDEX, SERIES_INT.clone()),
            ],
        );
        PineScript {
            lib_info,
            blk: Block::new_no_input(vec![], None),
            syntax_parser: None,
            callback,
            runner: None,
            data: vec![],
            error_format: ErrorFormater::new(),
        }
    }

    pub fn new_with_libinfo(
        lib_info: LibInfo<'li>,
        callback: Option<&'ra dyn Callback>,
    ) -> PineScript<'pa, 'li, 'ra, 'rb, 'rc> {
        PineScript {
            lib_info,
            blk: Block::new_no_input(vec![], None),
            syntax_parser: None,
            callback,
            runner: None,
            data: vec![],
            error_format: ErrorFormater::new(),
        }
    }

    pub fn parse_src<'s, 'a, 'pb>(&'s mut self, src: &'a str) -> Result<(), Vec<PineFormatError>>
    where
        's: 'pb,
        'li: 'pa,
        'a: 'pa,
    {
        let mut parser: PineParser<'pa, 'pb>;
        unsafe {
            let lib_ref = mem::transmute::<&LibInfo<'li>, &'pb LibInfo<'pa>>(&self.lib_info);
            let src_ref = mem::transmute::<&'a str, &'pa str>(src);
            parser = PineParser::new(src_ref, lib_ref);
        }
        // parser = PineParser::new(src, &self.lib_info);
        match parser.parse() {
            Ok((blk, parser, errs)) => {
                self.blk = blk;
                self.syntax_parser = Some(parser);
                self.runner = None;
                if errs.is_empty() {
                    Ok(())
                } else {
                    Err(errs
                        .into_iter()
                        .map(|err| PineFormatError::from_input_error(&self.error_format, err))
                        .collect())
                }
            }
            Err(errs) => Err(errs
                .into_iter()
                .map(|err| PineFormatError::from_input_error(&self.error_format, err))
                .collect()),
        }
    }

    fn get_runner(&mut self) -> &mut PineRunner<'ra, 'rb, 'rc> {
        if self.runner.is_none() {
            let mut runner: PineRunner<'ra, 'rb, 'rc>;
            unsafe {
                let blk_ref: &'ra Block<'ra> =
                    mem::transmute::<&Block<'pa>, &'ra Block<'ra>>(&self.blk);
                let lib_ref = mem::transmute::<&LibInfo<'li>, &LibInfo<'ra>>(&self.lib_info);
                runner = PineRunner::new(lib_ref, blk_ref, self.callback.unwrap());
                let names = self.syntax_parser.as_ref().unwrap().get_inputnames();
                runner.set_input_srcs(names.into_iter().map(|s| String::from(s)).collect());
            }
            self.runner = Some(runner);
        }
        self.runner.as_mut().unwrap()
    }

    // Run the script with the experimental data to generate IOInfo data
    pub fn gen_io_info(&mut self) -> Result<IOInfo, PineFormatError> {
        match self.get_runner().run(&vec![
            ("close", vec![Some(0f64)]),
            ("open", vec![Some(0f64)]),
            ("high", vec![Some(0f64)]),
            ("low", vec![Some(0f64)]),
        ]) {
            Err(err) => Err(PineFormatError::from_runtime_error(&self.error_format, err)),
            Ok(_) => {
                self.move_output_data();
                Ok(self.get_runner().get_context().get_io_info().clone())
            }
        }
    }

    fn move_output_data(&mut self) -> OutputDataCollect {
        let context = self.get_runner().get_context();
        let (start, end) = context.get_data_range();
        OutputDataCollect::new(start.unwrap(), end.unwrap(), context.move_output_data())
    }

    // Run the script with new input settings and old data
    pub fn run_with_input(
        &mut self,
        input: Vec<Option<InputVal>>,
    ) -> Result<OutputDataCollect, PineFormatError> {
        let runner = self.get_runner();
        runner.change_inputs(input);
        match self.runner.as_mut().unwrap().run(&self.data) {
            Ok(_) => Ok(self.move_output_data()),
            Err(err) => Err(PineFormatError::from_runtime_error(&self.error_format, err)),
        }
    }

    // Run the script with new data
    pub fn run_with_data(
        &mut self,
        data: Vec<(&'static str, Vec<Float>)>,
    ) -> Result<OutputDataCollect, PineFormatError>
    where
        'li: 'ra,
        'pa: 'ra,
    {
        self.data = data;
        self.get_runner();
        match self.runner.as_mut().unwrap().run(&self.data) {
            Ok(_) => Ok(self.move_output_data()),
            Err(err) => Err(PineFormatError::from_runtime_error(&self.error_format, err)),
        }
    }

    // Run the script with new data
    pub fn run(
        &mut self,
        input: Vec<Option<InputVal>>,
        data: Vec<(&'static str, Vec<Float>)>,
    ) -> Result<OutputDataCollect, PineFormatError>
    where
        'li: 'ra,
        'pa: 'ra,
    {
        self.data = data;
        self.get_runner().change_inputs(input);
        match self.runner.as_mut().unwrap().run(&self.data) {
            Ok(_) => Ok(self.move_output_data()),
            Err(err) => Err(PineFormatError::from_runtime_error(&self.error_format, err)),
        }
    }

    fn merge_data(&mut self, new_data: &Vec<(&'static str, Vec<Float>)>, from: usize) {
        let origin_data = mem::replace(&mut self.data, vec![]);

        self.data = origin_data
            .into_iter()
            .zip(new_data.clone())
            .map(|(mut v1, v2)| {
                debug_assert!(v1.0 == v2.0);
                v1.1.splice(from..v1.1.len(), vec![]);
                (v1.0, [v1.1, v2.1].concat())
            })
            .collect();
    }

    // Run the script with updated data(The last data included).
    pub fn update(
        &mut self,
        data: Vec<(&'static str, Vec<Float>)>,
    ) -> Result<OutputDataCollect, PineFormatError> {
        self.merge_data(&data, self.data[0].1.len() - 1);
        match self.runner.as_mut().unwrap().update(&data) {
            Ok(_) => Ok(self.move_output_data()),
            Err(err) => Err(PineFormatError::from_runtime_error(&self.error_format, err)),
        }
    }

    pub fn update_from(
        &mut self,
        data: Vec<(&'static str, Vec<Float>)>,
        from: i32,
    ) -> Result<OutputDataCollect, PineFormatError> {
        self.merge_data(&data, from as usize);
        match self.runner.as_mut().unwrap().update_from(&data, from) {
            Ok(_) => Ok(self.move_output_data()),
            Err(err) => Err(PineFormatError::from_runtime_error(&self.error_format, err)),
        }
    }

    pub fn move_parser(&mut self) -> Option<SyntaxParser<'pa>> {
        mem::replace(&mut self.syntax_parser, None)
    }
}

pub fn parse_ast(in_str: &str) -> Result<Block, (Option<Block>, Vec<PineInputError>)> {
    let input = Input::new(in_str, Position::new(0, 0), Position::max());
    let state = AstState::new();
    match block(input.clone(), &state) {
        Ok((input, parsed)) => {
            if input.len() != 0 {
                state.catch(PineInputError::new(
                    PineErrorKind::NonRecongnizeStmt,
                    StrRange::new(input.end, Position::max()),
                ));
            }
            if state.is_ok() {
                Ok(parsed)
            } else {
                Err((Some(parsed), state.into_inner()))
            }
        }
        Err(Err::Error(pine_error)) => {
            state.merge_pine_error(pine_error);
            Err((None, state.into_inner()))
        }
        _ => {
            state.catch(PineInputError::new(
                PineErrorKind::UnknownErr,
                StrRange::new(Position::new(0, 0), Position::max()),
            ));
            Err((None, state.into_inner()))
        }
    }
}

pub fn parse_syntax<'a>(
    blk: &mut Block<'a>,
    vars: &Vec<(&'a str, SyntaxType<'a>)>,
    input_names: &Vec<&'a str>,
) -> Result<SyntaxParser<'a>, (Option<SyntaxParser<'a>>, Vec<PineInputError>)> {
    let mut syntax_parser = SyntaxParser::new_with_vars(vars);
    syntax_parser.init_input_options(input_names.clone());
    match syntax_parser.parse_blk(blk) {
        Ok(_) => {
            if syntax_parser.is_ok() {
                Ok(syntax_parser)
            } else {
                let errs = syntax_parser.move_errors();
                Err((Some(syntax_parser), errs))
            }
        }
        Err(e) => {
            syntax_parser.catch(e);
            Err((None, syntax_parser.move_errors()))
        }
    }
}

pub fn extract_vars<'a>(
    vars: Vec<VarResult<'a>>,
    input_vars: Vec<(&'static str, SyntaxType<'a>)>,
) -> (
    Vec<(&'a str, SyntaxType<'a>)>,
    Vec<(&'a str, PineRef<'a>)>,
    Vec<&'a str>,
    Vec<&'a str>,
) {
    let mut types = Vec::with_capacity(vars.len() + input_vars.len());
    let mut values = Vec::with_capacity(vars.len());
    let mut input_names = Vec::with_capacity(input_vars.len());
    let mut user_input_names = Vec::with_capacity(input_vars.len());
    for var in vars {
        types.push((var.name, var.syntax_type));
        values.push((var.name, var.value));
    }
    for (n, t) in input_vars {
        input_names.push(n);
        types.push((n, t));
        if n != BAR_INDEX {
            user_input_names.push(n);
        }
    }
    // debug_assert!(check_names(&types));
    // map.insert(print::VAR_NAME, print::declare_var());
    // map.insert(plot::VAR_NAME, plot::declare_var());
    (types, values, input_names, user_input_names)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::libs::input;
    use crate::libs::plot;
    use crate::runtime::data_src::NoneCallback;
    use crate::runtime::output::{InputInfo, IntInputInfo, OutputInfo, PlotInfo};

    #[test]
    fn script_test() {
        let lib_info = LibInfo::new(
            vec![input::declare_var(), plot::declare_var()],
            vec![
                ("close", SERIES_FLOAT.clone()),
                ("open", SERIES_FLOAT.clone()),
                ("high", SERIES_FLOAT.clone()),
                ("low", SERIES_FLOAT.clone()),
                ("bar_index", SERIES_INT.clone()),
            ],
        );
        let mut parser = PineScript::new_with_libinfo(lib_info, Some(&NoneCallback()));
        parser
            .parse_src("m = input(1, 'hello', 'int')\nplot(close + m)")
            .unwrap();
        assert_eq!(
            parser.gen_io_info(),
            Ok(IOInfo::new_with_io(
                vec![InputInfo::Int(IntInputInfo {
                    defval: Some(1),
                    title: Some(String::from("hello")),
                    input_type: String::from("int"),
                    confirm: None,
                    minval: None,
                    maxval: None,
                    step: None,
                    options: None,
                })],
                vec![OutputInfo::Plot(PlotInfo {
                    title: None,
                    color: None,
                    linewidth: None,
                    style: None,
                    transp: None,
                    trackprice: None,
                    histbase: None,
                    offset: None,
                    join: None,
                    editable: None,
                    show_last: None,
                    display: None,
                })],
                vec![String::from("close")]
            ))
        );
        let data = vec![("close", vec![Some(1f64), Some(2f64)])];
        assert_eq!(
            parser.run_with_data(data.clone()),
            Ok(OutputDataCollect::new_with_one(
                0,
                2,
                vec![Some(2f64), Some(3f64)]
            ))
        );

        assert_eq!(
            parser.run_with_input(vec![Some(InputVal::Int(10))]),
            Ok(OutputDataCollect::new_with_one(
                0,
                2,
                vec![Some(11f64), Some(12f64)]
            ))
        );

        assert_eq!(
            parser.run_with_data(data.clone()),
            Ok(OutputDataCollect::new_with_one(
                0,
                2,
                vec![Some(11f64), Some(12f64)]
            ))
        );

        assert_eq!(
            parser.update(vec![("close", vec![Some(10f64), Some(11f64)])]),
            Ok(OutputDataCollect::new_with_one(
                1,
                3,
                vec![Some(20f64), Some(21f64)]
            ))
        );
        assert_eq!(
            parser.update_from(vec![("close", vec![Some(10f64), Some(11f64)])], 2),
            Ok(OutputDataCollect::new_with_one(
                2,
                4,
                vec![Some(20f64), Some(21f64)]
            ))
        );

        assert_eq!(
            parser.run_with_input(vec![Some(InputVal::Int(100))]),
            Ok(OutputDataCollect::new_with_one(
                0,
                4,
                vec![Some(101f64), Some(110f64), Some(110f64), Some(111f64)]
            ))
        );
    }
}
