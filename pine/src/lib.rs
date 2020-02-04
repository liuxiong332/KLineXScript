// #[macro_use]
extern crate nom;
use nom::Err;

#[macro_use]
extern crate lazy_static;

#[macro_use]
extern crate assert_matches;

pub mod ast;
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
use runtime::context::{InputVal, PineRuntimeError};
use runtime::data_src::{Callback, DataSrc};
use runtime::error_format::{ErrorFormater, PineFormatError};
use std::mem;
use types::{Float, PineRef};

#[derive(Debug, Clone)]
pub struct LibInfo<'a> {
    var_types: Vec<(&'a str, SyntaxType<'a>)>,
    var_values: Vec<(&'a str, PineRef<'a>)>,
    input_names: Vec<&'a str>,
}

impl<'a> LibInfo<'a> {
    pub fn new(
        lib_vars: Vec<VarResult<'a>>,
        input_vars: Vec<(&'static str, SyntaxType<'a>)>,
    ) -> LibInfo<'a> {
        let (types, values, input_names) = extract_vars(lib_vars, input_vars);
        // println!("now input names {:?}", input_names);
        LibInfo {
            var_types: types,
            var_values: values,
            input_names,
        }
    }
}

pub struct PineParser<'a, 'b> {
    src: &'a str,
    var_types: &'b Vec<(&'a str, SyntaxType<'a>)>,
}

impl<'a, 'b> PineParser<'a, 'b> {
    pub fn new(src: &'a str, lib_info: &'b LibInfo<'a>) -> PineParser<'a, 'b> {
        PineParser {
            src,
            var_types: &lib_info.var_types,
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
        match parse_syntax(&mut blk, &self.var_types) {
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
}

pub struct PineRunner<'a, 'b, 'c> {
    datasrc: DataSrc<'a, 'b, 'c>,
}

impl<'a, 'b, 'c> PineRunner<'a, 'b, 'c> {
    pub fn new(
        lib_info: &LibInfo<'a>,
        blk: &'a Block<'a>,
        callback: &'a dyn Callback,
    ) -> PineRunner<'a, 'b, 'c> {
        let var_values = lib_info.var_values.clone();
        let input_names = lib_info.input_names.clone();
        let datasrc = DataSrc::new(blk, var_values, input_names, callback);
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

    pub fn change_inputs(&mut self, inputs: Vec<InputVal>) {
        self.datasrc.change_inputs(inputs);
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
        PineScript {
            lib_info: LibInfo::new(
                declare_vars(),
                vec![
                    ("close", SERIES_FLOAT.clone()),
                    ("open", SERIES_FLOAT.clone()),
                    ("high", SERIES_FLOAT.clone()),
                    ("low", SERIES_FLOAT.clone()),
                    ("bar_index", SERIES_INT.clone()),
                ],
            ),
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

    pub fn run_with_input(&mut self, input: Vec<InputVal>) -> Result<(), PineFormatError> {
        let runner = self.runner.as_mut().unwrap();
        runner.change_inputs(input);
        match runner.run(&self.data) {
            Ok(val) => Ok(val),
            Err(err) => Err(PineFormatError::from_runtime_error(&self.error_format, err)),
        }
    }

    // Run the script with new data
    pub fn run(&mut self, data: Vec<(&'static str, Vec<Float>)>) -> Result<(), PineFormatError>
    where
        'li: 'ra,
        'pa: 'ra,
    {
        if self.runner.is_none() {
            let runner: PineRunner<'ra, 'rb, 'rc>;
            unsafe {
                let blk_ref: &'ra Block<'ra> =
                    mem::transmute::<&Block<'pa>, &'ra Block<'ra>>(&self.blk);
                let lib_ref = mem::transmute::<&LibInfo<'li>, &LibInfo<'ra>>(&self.lib_info);
                runner = PineRunner::new(lib_ref, blk_ref, self.callback.unwrap());
            }
            self.runner = Some(runner);
        }
        self.data = data;
        match self.runner.as_mut().unwrap().run(&self.data) {
            Ok(val) => Ok(val),
            Err(err) => Err(PineFormatError::from_runtime_error(&self.error_format, err)),
        }
    }

    // Run the script with updated data(The last data included).
    pub fn update(&mut self, data: Vec<(&'static str, Vec<Float>)>) -> Result<(), PineFormatError> {
        let origin_data = mem::replace(&mut self.data, vec![]);
        self.data = origin_data
            .into_iter()
            .zip(data.clone())
            .map(|(mut v1, v2)| {
                debug_assert!(v1.0 == v2.0);
                v1.1.pop();
                (v1.0, [v1.1, v2.1].concat())
            })
            .collect();
        match self.runner.as_mut().unwrap().update(&data) {
            Ok(val) => Ok(val),
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
) -> Result<SyntaxParser<'a>, (Option<SyntaxParser<'a>>, Vec<PineInputError>)> {
    let mut syntax_parser = SyntaxParser::new_with_vars(vars);
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
) {
    let mut types = Vec::with_capacity(vars.len() + input_vars.len());
    let mut values = Vec::with_capacity(vars.len());
    let mut input_names = Vec::with_capacity(input_vars.len());
    for var in vars {
        types.push((var.name, var.syntax_type));
        values.push((var.name, var.value));
    }
    for (n, t) in input_vars {
        input_names.push(n);
        types.push((n, t));
    }
    // debug_assert!(check_names(&types));
    // map.insert(print::VAR_NAME, print::declare_var());
    // map.insert(plot::VAR_NAME, plot::declare_var());
    (types, values, input_names)
}
