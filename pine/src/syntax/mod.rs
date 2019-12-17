use crate::ast::error::PineErrorKind;
use crate::ast::name::VarName;
use crate::ast::num::Numeral;
use crate::ast::stat_expr_types::{
    Block, Condition, DataType, Exp, ForRange, FunctionCall, FunctionDef, IfThenElse, RefCall,
    Statement, TupleNode, TypeCast,
};
use crate::ast::state::PineInputError;
use std::collections::HashMap;
use std::rc::Rc;

mod convert;

use convert::implicity_convert;

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionType<'a> {
    types: Vec<(Vec<(&'a str, SyntaxType<'a>)>, SyntaxType<'a>)>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum SimpleSyntaxType {
    Int,
    Float,
    Bool,
    Na,
    String,
    Color,
}

#[derive(Debug, PartialEq, Clone)]
pub enum SyntaxType<'a> {
    Simple(SimpleSyntaxType),
    Series(SimpleSyntaxType),
    Tuple(Rc<Vec<SyntaxType<'a>>>),
    Object(Rc<HashMap<&'a str, SyntaxType<'a>>>),
    Function(Rc<FunctionType<'a>>),
    UserFunction(Rc<Vec<&'a str>>),
    Any,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ContextType {
    Normal,
    IfElseBlock,
    ForRangeBlock,
    FuncDefBlock,
}

pub trait SyntaxCtx<'a> {
    fn declare_var(&mut self, name: &'a str, t: SyntaxType<'a>);

    fn update_var(&mut self, name: &'a str, t: SyntaxType<'a>);

    fn get_var(&self, name: &'a str) -> Option<&SyntaxType<'a>>;

    fn get_var_scope(&self, name: &'a str) -> Option<&SyntaxType<'a>>;
}

pub struct SyntaxContext<'a> {
    parent: Option<*mut (dyn SyntaxCtx<'a> + 'a)>,
    context_type: ContextType,

    vars: HashMap<&'a str, SyntaxType<'a>>,
}

pub fn downcast_ctx<'a>(item: *mut (dyn SyntaxCtx<'a> + 'a)) -> &mut SyntaxContext<'a> {
    unsafe {
        let raw: *mut dyn SyntaxCtx<'a> = item;
        let t = raw as *mut SyntaxContext<'a>;
        t.as_mut().unwrap()
    }
}

impl<'a> SyntaxCtx<'a> for SyntaxContext<'a> {
    fn declare_var(&mut self, name: &'a str, t: SyntaxType<'a>) {
        self.vars.insert(name, t);
    }

    fn update_var(&mut self, name: &'a str, t: SyntaxType<'a>) {
        if self.vars.contains_key(name) {
            self.vars.insert(name, t);
        } else if let Some(p) = self.parent {
            downcast_ctx(p).update_var(name, t);
        } else {
            unreachable!();
        }
    }

    fn get_var(&self, name: &'a str) -> Option<&SyntaxType<'a>> {
        if self.vars.contains_key(name) {
            self.vars.get(name)
        } else if let Some(p) = self.parent {
            downcast_ctx(p).get_var(name)
        } else {
            None
        }
    }

    fn get_var_scope(&self, name: &'a str) -> Option<&SyntaxType<'a>> {
        self.vars.get(name)
    }
}

impl<'a> SyntaxContext<'a> {
    pub fn new(
        parent: Option<*mut (dyn SyntaxCtx<'a> + 'a)>,
        context_type: ContextType,
    ) -> SyntaxContext<'a> {
        SyntaxContext {
            parent,
            context_type,
            vars: HashMap::new(),
        }
    }
}

pub struct SyntaxParser<'a> {
    ctxs: HashMap<i32, Box<dyn SyntaxCtx<'a> + 'a>>,
    context: *mut (dyn SyntaxCtx<'a> + 'a),
    user_funcs: HashMap<&'a str, *mut FunctionDef<'a>>,
    errors: Vec<PineInputError>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParseValue<'a> {
    pub syntax_type: SyntaxType<'a>,
    pub varname: Option<&'a str>,
}

impl<'a> ParseValue<'a> {
    pub fn new(syntax_type: SyntaxType<'a>, varname: &'a str) -> ParseValue<'a> {
        ParseValue {
            syntax_type,
            varname: Some(varname),
        }
    }
    pub fn new_with_type(syntax_type: SyntaxType<'a>) -> ParseValue<'a> {
        ParseValue {
            syntax_type,
            varname: None,
        }
    }
}

type ParseResult<'a> = Result<ParseValue<'a>, PineInputError>;

impl<'a> SyntaxParser<'a> {
    pub fn new() -> SyntaxParser<'a> {
        let mut ctxs: HashMap<i32, Box<dyn SyntaxCtx<'a> + 'a>> = HashMap::new();
        let top_ctx: Box<dyn SyntaxCtx<'a> + 'a> =
            Box::new(SyntaxContext::new(None, ContextType::Normal));
        ctxs.insert(0, top_ctx);
        SyntaxParser {
            context: &mut **ctxs.get_mut(&0).unwrap(),
            ctxs,
            user_funcs: HashMap::new(),
            errors: vec![],
        }
    }

    // pub fn gen_ctx_id(&mut self) -> i32 {
    //     self.ctxid += 1;
    //     self.ctxid
    // }

    pub fn catch(&mut self, err: PineInputError) {
        self.errors.push(err)
    }

    fn parse_std_func_call(
        &mut self,
        func_call: &mut FunctionCall<'a>,
        fun_type: &Rc<FunctionType<'a>>,
    ) -> ParseResult<'a> {
        let mut pos_arg_type = vec![];
        for arg in func_call.pos_args.iter_mut() {
            pos_arg_type.push(self.parse_exp(arg)?);
        }
        let mut dict_arg_type = vec![];
        for (name, exp) in func_call.dict_args.iter_mut() {
            dict_arg_type.push((name, self.parse_exp(exp)?));
        }
        let res_fun = fun_type.types.iter().find(|(args, _)| {
            if args.len() >= pos_arg_type.len() {
                let pos_match = pos_arg_type.iter().zip(args.iter()).all(|(x1, x2)| {
                    x1.syntax_type == x2.1 || implicity_convert(&x1.syntax_type, &x2.1)
                });
                let dict_match = dict_arg_type.iter().all(|(name, t)| {
                    match args.iter().find(|s| s.0 == name.value) {
                        None => false,
                        Some(val) => {
                            t.syntax_type == val.1 || implicity_convert(&t.syntax_type, &val.1)
                        }
                    }
                });
                pos_match && dict_match
            } else {
                false
            }
        });
        match res_fun {
            None => Err(PineInputError::new(
                PineErrorKind::FuncCallSignatureNotMatch,
                func_call.range,
            )),
            Some(d) => Ok(ParseValue::new_with_type(d.1.clone())),
        }
    }

    fn parse_user_func_call(
        &mut self,
        func_call: &mut FunctionCall<'a>,
        names: &Rc<Vec<&'a str>>,
        method_name: &'a str,
    ) -> ParseResult<'a> {
        if func_call.dict_args.len() > 0 {
            Err(PineInputError::new(
                PineErrorKind::ForbiddenDictArgsForUserFunc,
                func_call.range,
            ))
        } else if func_call.pos_args.len() != names.len() {
            Err(PineInputError::new(
                PineErrorKind::FuncCallSignatureNotMatch,
                func_call.range,
            ))
        } else {
            let mut pos_arg_type = vec![];
            for arg in func_call.pos_args.iter_mut() {
                pos_arg_type.push(self.parse_exp(arg)?);
            }
            let mut sub_ctx = Box::new(SyntaxContext::new(
                Some(self.context),
                ContextType::FuncDefBlock,
            ));
            names
                .iter()
                .zip(pos_arg_type.iter())
                .for_each(|(&n, t)| sub_ctx.declare_var(n, t.syntax_type.clone()));
            self.context = &mut *sub_ctx;

            let body;
            unsafe {
                body = &mut self.user_funcs[method_name].as_mut().unwrap().body;
            };
            let parse_res = self.parse_blk(body)?;

            self.context = sub_ctx.parent.unwrap();
            self.ctxs.insert(func_call.ctxid, sub_ctx);
            Ok(ParseValue::new_with_type(parse_res.syntax_type))
        }
    }

    fn parse_func_call(&mut self, func_call: &mut FunctionCall<'a>) -> ParseResult<'a> {
        let method_type = self.parse_exp(&mut func_call.method)?;
        if let SyntaxType::Function(fun_type) = method_type.syntax_type {
            self.parse_std_func_call(func_call, &fun_type)
        } else if let SyntaxType::UserFunction(names) = method_type.syntax_type {
            self.parse_user_func_call(func_call, &names, method_type.varname.unwrap())
        } else {
            Err(PineInputError::new(
                PineErrorKind::VarNotCallable,
                func_call.range,
            ))
        }
    }

    fn parse_tuple(&mut self, tuple: &mut TupleNode<'a>) -> ParseResult<'a> {
        let mut tuple_type: Vec<SyntaxType<'a>> = vec![];
        for arg in tuple.exps.iter_mut() {
            tuple_type.push(self.parse_exp(arg)?.syntax_type);
        }
        Ok(ParseValue::new_with_type(SyntaxType::Tuple(Rc::new(
            tuple_type,
        ))))
    }

    fn parse_ref_call(&mut self, ref_call: &mut RefCall<'a>) -> ParseResult<'a> {
        // let name_res = self.parse_exp(&mut ref_call.name)?;

        // self.parse_exp(&mut ref_call.arg);
        Err(PineInputError::new(
            PineErrorKind::VarNotCallable,
            ref_call.range,
        ))
    }

    fn parse_condition(&mut self, condition: &mut Condition<'a>) -> ParseResult<'a> {
        // self.parse_exp(&mut condition.cond);
        // self.parse_exp(&mut condition.exp1);
        // self.parse_exp(&mut condition.exp2);
        Err(PineInputError::new(
            PineErrorKind::VarNotCallable,
            condition.range,
        ))
    }

    fn parse_ifthenelse(&mut self, ite: &mut IfThenElse<'a>) -> ParseResult<'a> {
        // ite.then_ctxid = self.gen_ctx_id();
        // ite.else_ctxid = self.gen_ctx_id();

        // self.parse_exp(&mut ite.cond);
        // self.parse_blk(&mut ite.then_blk);
        // if let Some(else_blk) = &mut ite.else_blk {
        //     self.parse_blk(else_blk);
        // }
        Err(PineInputError::new(
            PineErrorKind::VarNotCallable,
            ite.range,
        ))
    }

    fn parse_forrange(&mut self, for_range: &mut ForRange<'a>) -> ParseResult<'a> {
        // for_range.ctxid = self.gen_ctx_id();

        // self.parse_exp(&mut for_range.start);
        // self.parse_exp(&mut for_range.end);
        // if let Some(step) = &mut for_range.step {
        //     self.parse_exp(step);
        // }
        // self.parse_blk(&mut for_range.do_blk);
        Err(PineInputError::new(
            PineErrorKind::VarNotCallable,
            for_range.range,
        ))
    }

    fn parse_varname(&mut self, varname: &mut VarName<'a>) -> ParseResult<'a> {
        match downcast_ctx(self.context).get_var(varname.value) {
            None => Err(PineInputError::new(
                PineErrorKind::VarNotDeclare,
                varname.range,
            )),
            Some(val) => Ok(ParseValue::new(val.clone(), varname.value)),
        }
    }

    fn parse_type_cast(&mut self, type_cast: &mut TypeCast<'a>) -> ParseResult<'a> {
        let mut is_cast_err = false;
        let origin_type = self.parse_exp(&mut type_cast.exp)?.syntax_type;
        let result = match type_cast.data_type {
            DataType::Bool => match origin_type {
                SyntaxType::Series(SimpleSyntaxType::Int)
                | SyntaxType::Series(SimpleSyntaxType::Float)
                | SyntaxType::Series(SimpleSyntaxType::Bool)
                | SyntaxType::Series(SimpleSyntaxType::Na) => {
                    SyntaxType::Series(SimpleSyntaxType::Bool)
                }
                SyntaxType::Series(_) => {
                    is_cast_err = true;
                    SyntaxType::Series(SimpleSyntaxType::Bool)
                }
                SyntaxType::Simple(SimpleSyntaxType::Int)
                | SyntaxType::Simple(SimpleSyntaxType::Float)
                | SyntaxType::Simple(SimpleSyntaxType::Bool)
                | SyntaxType::Simple(SimpleSyntaxType::Na) => {
                    SyntaxType::Simple(SimpleSyntaxType::Bool)
                }
                _ => {
                    is_cast_err = true;
                    SyntaxType::Simple(SimpleSyntaxType::Bool)
                }
            },
            DataType::Int => match origin_type {
                SyntaxType::Series(SimpleSyntaxType::Int)
                | SyntaxType::Series(SimpleSyntaxType::Float)
                | SyntaxType::Series(SimpleSyntaxType::Na) => {
                    SyntaxType::Series(SimpleSyntaxType::Int)
                }
                SyntaxType::Series(_) => {
                    is_cast_err = true;
                    SyntaxType::Series(SimpleSyntaxType::Int)
                }
                SyntaxType::Simple(SimpleSyntaxType::Int)
                | SyntaxType::Simple(SimpleSyntaxType::Float)
                | SyntaxType::Simple(SimpleSyntaxType::Na) => {
                    SyntaxType::Simple(SimpleSyntaxType::Int)
                }
                _ => {
                    is_cast_err = true;
                    SyntaxType::Simple(SimpleSyntaxType::Int)
                }
            },
            DataType::Float => match origin_type {
                SyntaxType::Series(SimpleSyntaxType::Int)
                | SyntaxType::Series(SimpleSyntaxType::Float)
                | SyntaxType::Series(SimpleSyntaxType::Na) => {
                    SyntaxType::Series(SimpleSyntaxType::Float)
                }
                SyntaxType::Series(_) => {
                    is_cast_err = true;
                    SyntaxType::Series(SimpleSyntaxType::Float)
                }

                SyntaxType::Simple(SimpleSyntaxType::Int)
                | SyntaxType::Simple(SimpleSyntaxType::Float)
                | SyntaxType::Simple(SimpleSyntaxType::Na) => {
                    SyntaxType::Simple(SimpleSyntaxType::Float)
                }
                _ => {
                    is_cast_err = true;
                    SyntaxType::Simple(SimpleSyntaxType::Float)
                }
            },
            DataType::Color => match origin_type {
                SyntaxType::Series(SimpleSyntaxType::Color) => {
                    SyntaxType::Series(SimpleSyntaxType::Color)
                }
                SyntaxType::Series(_) => {
                    is_cast_err = true;
                    SyntaxType::Series(SimpleSyntaxType::Color)
                }
                SyntaxType::Simple(SimpleSyntaxType::Color) => {
                    SyntaxType::Simple(SimpleSyntaxType::Color)
                }
                _ => {
                    is_cast_err = true;
                    SyntaxType::Simple(SimpleSyntaxType::Color)
                }
            },
            DataType::String => match origin_type {
                SyntaxType::Series(SimpleSyntaxType::String) => {
                    SyntaxType::Series(SimpleSyntaxType::String)
                }
                SyntaxType::Series(_) => {
                    is_cast_err = true;
                    SyntaxType::Series(SimpleSyntaxType::String)
                }
                SyntaxType::Simple(SimpleSyntaxType::String) => {
                    SyntaxType::Simple(SimpleSyntaxType::String)
                }
                _ => {
                    is_cast_err = true;
                    SyntaxType::Simple(SimpleSyntaxType::String)
                }
            },
        };
        if is_cast_err {
            self.catch(PineInputError::new(
                PineErrorKind::InvalidTypeCast {
                    origin: format!("{:?}", origin_type),
                    cast: format!("{:?}", type_cast.data_type),
                },
                type_cast.range,
            ));
        }
        Ok(ParseValue::new_with_type(result))
    }

    fn parse_exp(&mut self, exp: &mut Exp<'a>) -> ParseResult<'a> {
        match exp {
            Exp::Na(_) => Ok(ParseValue::new_with_type(SyntaxType::Simple(
                SimpleSyntaxType::Na,
            ))),
            Exp::Bool(_) => Ok(ParseValue::new_with_type(SyntaxType::Simple(
                SimpleSyntaxType::Bool,
            ))),
            Exp::Num(Numeral::Int(_)) => Ok(ParseValue::new_with_type(SyntaxType::Simple(
                SimpleSyntaxType::Int,
            ))),
            Exp::Num(Numeral::Float(_)) => Ok(ParseValue::new_with_type(SyntaxType::Simple(
                SimpleSyntaxType::Float,
            ))),
            Exp::Str(_) => Ok(ParseValue::new_with_type(SyntaxType::Simple(
                SimpleSyntaxType::String,
            ))),
            Exp::Color(_) => Ok(ParseValue::new_with_type(SyntaxType::Simple(
                SimpleSyntaxType::Color,
            ))),
            Exp::VarName(name) => self.parse_varname(name),
            Exp::Tuple(tuple) => self.parse_tuple(tuple),
            Exp::TypeCast(type_cast) => self.parse_type_cast(type_cast),
            Exp::FuncCall(func_call) => self.parse_func_call(func_call),
            Exp::RefCall(ref_call) => self.parse_ref_call(ref_call),
            Exp::Condition(condition) => self.parse_condition(condition),
            Exp::Ite(ite) => self.parse_ifthenelse(ite),
            Exp::ForRange(fr) => self.parse_forrange(fr),
            Exp::UnaryExp(node) => self.parse_exp(&mut node.exp),
            // Exp::BinaryExp(node) => {
            //     self.parse_exp(&mut node.exp1);
            //     self.parse_exp(&mut node.exp2);
            // }
            t => Err(PineInputError::new(
                PineErrorKind::VarNotCallable,
                t.range(),
            )),
        }
    }

    fn parse_func_def(&mut self, func_def: &mut FunctionDef<'a>) -> ParseResult<'a> {
        let names: Vec<&'a str> = func_def.params.iter().map(|s| s.value).collect();
        let stype = SyntaxType::UserFunction(Rc::new(names));

        downcast_ctx(self.context).declare_var(func_def.name.value, stype.clone());
        self.user_funcs.insert(func_def.name.value, func_def);
        Ok(ParseValue::new_with_type(stype))
    }

    fn parse_stmt(&mut self, stmt: &mut Statement<'a>) -> ParseResult<'a> {
        match stmt {
            Statement::FuncCall(func_call) => self.parse_func_call(func_call),
            Statement::Ite(ite) => self.parse_ifthenelse(ite),
            Statement::ForRange(fr) => self.parse_forrange(fr),
            // Statement::Assignment(assign) => {
            //     self.parse_exp(&mut assign.val);
            // }
            // Statement::VarAssignment(assign) => {
            //     self.parse_exp(&mut assign.val);
            // }
            // Statement::FuncDef(func_def) => {
            //     self.parse_blk(&mut func_def.body);
            // }
            t => Err(PineInputError::new(
                PineErrorKind::VarNotCallable,
                t.range(),
            )),
        }
    }

    pub fn parse_blk(&mut self, blk: &mut Block<'a>) -> ParseResult<'a> {
        // for stmt in blk.stmts.iter_mut() {
        //     self.parse_stmt(stmt)
        // }
        if let Some(ref mut exp) = blk.ret_stmt {
            self.parse_exp(exp)
        } else {
            Ok(ParseValue::new_with_type(SyntaxType::Simple(
                SimpleSyntaxType::Na,
            )))
        }
        // Err(PineInputError::new(
        //     PineErrorKind::VarNotCallable,
        //     blk.range,
        // ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::input::{Position, StrRange};
    use crate::ast::name::VarName;
    use crate::ast::stat_expr_types::{Assignment, DataType, RefCall, TypeCast, VarAssignment};

    fn varname(n: &str) -> VarName {
        VarName::new(n, StrRange::new_empty())
    }

    #[test]
    fn func_def_test() {
        let mut parser = SyntaxParser::new();
        let mut func_def = FunctionDef {
            name: varname("funa"),
            params: vec![varname("hello"), varname("hello2")],
            body: Block::new(vec![], None, StrRange::new_empty()),
            range: StrRange::new_empty(),
        };
        assert_eq!(
            parser.parse_func_def(&mut func_def),
            Ok(ParseValue::new_with_type(SyntaxType::UserFunction(
                Rc::new(vec!["hello", "hello2"])
            )))
        );
        assert_eq!(parser.user_funcs["funa"], &mut func_def as *mut FunctionDef);
        assert_eq!(downcast_ctx(parser.context).vars.contains_key("funa"), true);
    }

    const INT_TYPE: SyntaxType = SyntaxType::Simple(SimpleSyntaxType::Int);
    const FLOAT_TYPE: SyntaxType = SyntaxType::Simple(SimpleSyntaxType::Float);

    fn int_exp<'a>(i: i32) -> Exp<'a> {
        Exp::Num(Numeral::from_i32(i))
    }

    fn float_exp<'a>(i: f64) -> Exp<'a> {
        Exp::Num(Numeral::from_f64(i))
    }

    fn fun_nm<'a>() -> Exp<'a> {
        Exp::VarName(varname("func"))
    }

    #[test]
    fn func_call_test() {
        let mut parser = SyntaxParser::new();
        downcast_ctx(parser.context).declare_var(
            "func",
            SyntaxType::Function(Rc::new(FunctionType {
                types: vec![
                    (vec![("arg1", INT_TYPE), ("arg2", INT_TYPE)], INT_TYPE),
                    (vec![("arg1", FLOAT_TYPE), ("arg2", FLOAT_TYPE)], FLOAT_TYPE),
                ],
            })),
        );

        let mut func_call = FunctionCall::new(
            fun_nm(),
            vec![int_exp(1), int_exp(2)],
            vec![],
            1,
            StrRange::new_empty(),
        );
        assert_eq!(
            parser.parse_func_call(&mut func_call),
            Ok(ParseValue::new_with_type(INT_TYPE))
        );

        let mut func_call =
            FunctionCall::new(fun_nm(), vec![int_exp(1)], vec![], 1, StrRange::new_empty());
        assert_eq!(
            parser.parse_func_call(&mut func_call),
            Ok(ParseValue::new_with_type(INT_TYPE))
        );

        let mut func_call = FunctionCall::new(
            fun_nm(),
            vec![int_exp(1)],
            vec![(varname("arg2"), int_exp(1))],
            1,
            StrRange::new_empty(),
        );
        assert_eq!(
            parser.parse_func_call(&mut func_call),
            Ok(ParseValue::new_with_type(INT_TYPE))
        );

        let mut func_call = FunctionCall::new(
            fun_nm(),
            vec![int_exp(1)],
            vec![(varname("arg2"), float_exp(1f64))],
            1,
            StrRange::new_empty(),
        );
        assert_eq!(
            parser.parse_func_call(&mut func_call),
            Ok(ParseValue::new_with_type(FLOAT_TYPE))
        );

        let mut func_call = FunctionCall::new(
            fun_nm(),
            vec![float_exp(1f64)],
            vec![(varname("arg2"), int_exp(1))],
            1,
            StrRange::new_empty(),
        );
        assert_eq!(
            parser.parse_func_call(&mut func_call),
            Ok(ParseValue::new_with_type(FLOAT_TYPE))
        );

        let mut func_call = FunctionCall::new(
            fun_nm(),
            vec![float_exp(1f64)],
            vec![(varname("arg3"), int_exp(1))],
            1,
            StrRange::new_empty(),
        );
        assert_eq!(
            parser.parse_func_call(&mut func_call),
            Err(PineInputError::new(
                PineErrorKind::FuncCallSignatureNotMatch,
                StrRange::new_empty()
            ))
        );
    }

    #[test]
    fn user_func_call_test() {
        let mut parser = SyntaxParser::new();
        let mut func_def = FunctionDef {
            name: varname("fun"),
            params: vec![varname("a1"), varname("a2")],
            body: Block::new(
                vec![],
                Some(Exp::VarName(varname("a2"))),
                StrRange::new_empty(),
            ),
            range: StrRange::new_empty(),
        };
        assert!(parser.parse_func_def(&mut func_def).is_ok());

        let mut func_call = FunctionCall::new(
            Exp::VarName(varname("fun")),
            vec![int_exp(1), float_exp(2f64)],
            vec![],
            1,
            StrRange::new_empty(),
        );
        assert_eq!(
            parser.parse_func_call(&mut func_call),
            Ok(ParseValue::new_with_type(FLOAT_TYPE))
        );
        assert_eq!(downcast_ctx(parser.context).parent, None);
        assert_eq!(
            downcast_ctx(&mut **parser.ctxs.get_mut(&1).unwrap())
                .vars
                .contains_key("a1"),
            true
        );
    }
}
