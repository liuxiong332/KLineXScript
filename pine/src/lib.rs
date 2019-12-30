// #[macro_use]
extern crate nom;
use nom::Err;

#[macro_use]
extern crate lazy_static;

#[macro_use]
extern crate assert_matches;

pub mod ast;
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
use runtime::context::PineRuntimeError;
use runtime::data_src::{Callback, DataSrc};
use std::mem;
use types::{Float, PineRef};

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
        LibInfo {
            var_types: types,
            var_values: values,
            input_names,
        }
    }
}

pub struct PineParser<'a> {
    src: &'a str,
    lib_info: &'a LibInfo<'a>,
}

impl<'a> PineParser<'a> {
    pub fn new(src: &'a str, lib_info: &'a LibInfo<'a>) -> PineParser<'a> {
        PineParser { src, lib_info }
    }

    pub fn parse(&mut self) -> Result<Block<'a>, Vec<PineInputError>> {
        let mut blk = parse_ast(self.src)?;
        parse_syntax(&mut blk, &self.lib_info.var_types)?;
        Ok(blk)
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

    pub fn run(&mut self, data: Vec<(&'static str, Vec<Float>)>) -> Result<(), PineRuntimeError> {
        self.datasrc.run(data)
    }
}

pub struct PineScript<'p, 'li, 'ra, 'rb, 'rc> {
    lib_info: LibInfo<'li>,
    blk: Block<'p>,
    callback: &'ra dyn Callback,
    runner: Option<PineRunner<'ra, 'rb, 'rc>>,
}

const SERIES_FLOAT: SyntaxType = SyntaxType::Series(SimpleSyntaxType::Float);
const SERIES_INT: SyntaxType = SyntaxType::Series(SimpleSyntaxType::Int);

impl<'p, 'li, 'ra, 'rb, 'rc> PineScript<'p, 'li, 'ra, 'rb, 'rc> {
    pub fn new(callback: &'ra dyn Callback) -> PineScript<'p, 'li, 'ra, 'rb, 'rc> {
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
            callback,
            runner: None,
        }
    }

    pub fn parse_src<'a, 's>(&'s mut self, src: &'a str) -> Result<(), Vec<PineInputError>>
    where
        'li: 'p,
        's: 'p,
        'a: 'p,
    {
        let mut parser: PineParser<'p>;
        unsafe {
            let lib_ref = mem::transmute::<&LibInfo<'li>, &'p LibInfo<'p>>(&self.lib_info);
            // let src_ref = mem::transmute::<&'a str, &'p str>(src);
            parser = PineParser::new(src, lib_ref);
        }
        // parser = PineParser::new(src, &self.lib_info);
        let blk = parser.parse()?;
        self.blk = blk;
        Ok(())
    }

    pub fn run<'s>(
        &'s mut self,
        data: Vec<(&'static str, Vec<Float>)>,
    ) -> Result<(), PineRuntimeError>
    where
        's: 'ra,
        'li: 'ra,
        'p: 'ra,
    {
        let runner: PineRunner<'ra, 'rb, 'rc>;
        unsafe {
            let blk_ref: &'ra Block<'ra> = mem::transmute::<&Block<'p>, &'ra Block<'ra>>(&self.blk);
            let lib_ref = mem::transmute::<&LibInfo<'li>, &LibInfo<'ra>>(&self.lib_info);
            runner = PineRunner::new(lib_ref, blk_ref, self.callback);
        }
        self.runner = Some(runner);
        self.runner.as_mut().unwrap().run(data)
    }
}

pub fn parse_ast(in_str: &str) -> Result<Block, Vec<PineInputError>> {
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
                Err(state.into_inner())
            }
        }
        Err(Err::Error(pine_error)) => {
            state.merge_pine_error(pine_error);
            Err(state.into_inner())
        }
        _ => {
            state.catch(PineInputError::new(
                PineErrorKind::UnknownErr,
                StrRange::new(Position::new(0, 0), Position::max()),
            ));
            Err(state.into_inner())
        }
    }
}

pub fn parse_syntax<'a>(
    blk: &mut Block<'a>,
    vars: &Vec<(&'a str, SyntaxType<'a>)>,
) -> Result<(), Vec<PineInputError>> {
    let mut syntax_parser = SyntaxParser::new_with_vars(vars);
    match syntax_parser.parse_blk(blk) {
        Ok(_) => {
            if syntax_parser.is_ok() {
                Ok(())
            } else {
                Err(syntax_parser.move_errors())
            }
        }
        Err(e) => {
            syntax_parser.catch(e);
            Err(syntax_parser.move_errors())
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
