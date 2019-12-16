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

pub struct SyntaxContext<'a, 'b> {
    parent: Option<&'b mut (dyn SyntaxCtx<'a> + 'b)>,
    context_type: ContextType,

    vars: HashMap<&'a str, SyntaxType<'a>>,
}

pub fn downcast_ctx<'a, 'b, 'c>(
    item: &'c mut (dyn SyntaxCtx<'a>),
) -> &'c mut SyntaxContext<'a, 'b> {
    unsafe {
        let raw: *mut dyn SyntaxCtx<'a> = item;
        let t = raw as *mut SyntaxContext<'a, 'b>;
        t.as_mut().unwrap()
    }
}

impl<'a, 'b> SyntaxCtx<'a> for SyntaxContext<'a, 'b> {
    fn declare_var(&mut self, name: &'a str, t: SyntaxType<'a>) {
        self.vars.insert(name, t);
    }

    fn update_var(&mut self, name: &'a str, t: SyntaxType<'a>) {
        if self.vars.contains_key(name) {
            self.vars.insert(name, t);
        } else if let Some(p) = &mut self.parent {
            p.update_var(name, t);
        } else {
            unreachable!();
        }
    }

    fn get_var(&self, name: &'a str) -> Option<&SyntaxType<'a>> {
        if self.vars.contains_key(name) {
            self.vars.get(name)
        } else if let Some(p) = &self.parent {
            p.get_var(name)
        } else {
            None
        }
    }

    fn get_var_scope(&self, name: &'a str) -> Option<&SyntaxType<'a>> {
        self.vars.get(name)
    }
}

impl<'a, 'b> SyntaxContext<'a, 'b> {
    pub fn new(
        parent: Option<&'b mut (dyn SyntaxCtx<'a> + 'b)>,
        context_type: ContextType,
    ) -> SyntaxContext<'a, 'b> {
        SyntaxContext {
            parent,
            context_type,
            vars: HashMap::new(),
        }
    }
}

pub struct SyntaxParser<'a, 'b> {
    ctxid: i32,
    ctxs: HashMap<i32, Box<dyn SyntaxCtx<'a> + 'a>>,
    current_ctxid: i32,
    context: &'b mut (dyn SyntaxCtx<'a> + 'a),
    errors: Vec<PineInputError>,
}

pub struct ParseValue<'a> {
    syntax_type: Rc<SyntaxType<'a>>,
}

type ParseResult<'a> = Result<SyntaxType<'a>, PineInputError>;

impl<'a, 'b> SyntaxParser<'a, 'b> {
    pub fn new() -> SyntaxParser<'a, 'b> {
        let mut ctxs: HashMap<i32, Box<dyn SyntaxCtx<'a> + 'a>> = HashMap::new();
        let top_ctx = Box::new(SyntaxContext::new(None, ContextType::Normal));
        ctxs.insert(0, top_ctx);
        SyntaxParser {
            ctxid: 0,
            current_ctxid: 0,
            context: &mut *top_ctx,
            ctxs,
            errors: vec![],
        }
    }

    pub fn gen_ctx_id(&mut self) -> i32 {
        self.ctxid += 1;
        self.ctxid
    }

    pub fn catch(&mut self, err: PineInputError) {
        self.errors.push(err)
    }

    fn implicity_convert(origin_type: SyntaxType<'a>, dest_type: SyntaxType<'a>) -> bool {
        if origin_type == dest_type {
            return true;
        }
        match origin_type {
            SyntaxType::Series(SimpleSyntaxType::Na) => match dest_type {
                SyntaxType::Series(t) => true,
            },
            SyntaxType::Simple(SimpleSyntaxType::Na) => match dest_type {
                SyntaxType::Simple(t) => true,
                SyntaxType::Series(t) => true,
            },
            SyntaxType::Series(SimpleSyntaxType::Int) => match dest_type {
                SyntaxType::Series(SimpleSyntaxType::Bool)
                | SyntaxType::Series(SimpleSyntaxType::Float)
                | SyntaxType::Series(SimpleSyntaxType::Int) => true,
            },
            SyntaxType::Simple(SimpleSyntaxType::Int) => match dest_type {
                SyntaxType::Simple(SimpleSyntaxType::Bool)
                | SyntaxType::Simple(SimpleSyntaxType::Int)
                | SyntaxType::Simple(SimpleSyntaxType::Float) => true,

                SyntaxType::Series(SimpleSyntaxType::Bool)
                | SyntaxType::Series(SimpleSyntaxType::Float)
                | SyntaxType::Series(SimpleSyntaxType::Int) => true,
            },
            SyntaxType::Series(SimpleSyntaxType::Float) => match dest_type {
                SyntaxType::Series(SimpleSyntaxType::Bool)
                | SyntaxType::Series(SimpleSyntaxType::Float) => true,
            },
            SyntaxType::Simple(SimpleSyntaxType::Float) => match dest_type {
                SyntaxType::Simple(SimpleSyntaxType::Bool)
                | SyntaxType::Simple(SimpleSyntaxType::Float) => true,

                SyntaxType::Series(SimpleSyntaxType::Bool)
                | SyntaxType::Series(SimpleSyntaxType::Float) => true,
            },
        }
    }

    fn parse_func_call(&mut self, func_call: &mut FunctionCall<'a>) -> ParseResult<'a> {
        func_call.ctxid = self.gen_ctx_id();

        let method_type = self.parse_exp(&mut func_call.method)?;
        if let SyntaxType::Function(fun_type) = method_type {
            let mut pos_arg_type = vec![];
            for arg in func_call.pos_args.iter_mut() {
                pos_arg_type.push(self.parse_exp(arg)?);
            }
            let mut dict_arg_type = vec![];
            for (name, exp) in func_call.dict_args.iter_mut() {
                dict_arg_type.push((name, self.parse_exp(exp)?));
            }
            let res_fun = fun_type.types.iter().find(|(args, ret_type)| {
                if args.len() >= pos_arg_type.len() {
                    let pos_match = pos_arg_type
                        .iter()
                        .zip(args.iter())
                        .all(|(x1, x2)| *x1 == x2.1 || SyntaxParser::implicity_convert(*x1, x2.1));
                    let dict_match = dict_arg_type.iter().all(|(name, t)| {
                        match args.iter().find(|s| s.0 == name.value) {
                            None => false,
                            Some(val) => val.1 == *t || SyntaxParser::implicity_convert(val.1, *t),
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
                Some(d) => Ok(d.1),
            }
        } else if let SyntaxType::UserFunction(names) = method_type {
            let sub_ctx = SyntaxContext::new(Some(self.context), ContextType::FuncDefBlock);
        // sub_ctx
        } else {
            Err(PineInputError::new(
                PineErrorKind::VarNotCallable,
                func_call.range,
            ))
        }
    }

    fn parse_tuple(&mut self, tuple: &mut TupleNode<'a>) -> ParseResult<'a> {
        let tuple_type: Rc<Vec<SyntaxType<'a>>> = Rc::new(vec![]);
        for arg in tuple.exps.iter_mut() {
            tuple_type.push(self.parse_exp(arg)?);
        }
        Ok(SyntaxType::Tuple(tuple_type))
    }

    fn parse_ref_call(&mut self, ref_call: &mut RefCall<'a>) {
        self.parse_exp(&mut ref_call.arg);
        self.parse_exp(&mut ref_call.name);
    }

    fn parse_condition(&mut self, condition: &mut Condition<'a>) {
        self.parse_exp(&mut condition.cond);
        self.parse_exp(&mut condition.exp1);
        self.parse_exp(&mut condition.exp2);
    }

    fn parse_ifthenelse(&mut self, ite: &mut IfThenElse<'a>) {
        ite.then_ctxid = self.gen_ctx_id();
        ite.else_ctxid = self.gen_ctx_id();

        self.parse_exp(&mut ite.cond);
        self.parse_blk(&mut ite.then_blk);
        if let Some(else_blk) = &mut ite.else_blk {
            self.parse_blk(else_blk);
        }
    }

    fn parse_forrange(&mut self, for_range: &mut ForRange<'a>) {
        for_range.ctxid = self.gen_ctx_id();

        self.parse_exp(&mut for_range.start);
        self.parse_exp(&mut for_range.end);
        if let Some(step) = &mut for_range.step {
            self.parse_exp(step);
        }
        self.parse_blk(&mut for_range.do_blk);
    }

    fn parse_varname(&mut self, varname: &mut VarName<'a>) -> ParseResult<'a> {
        match self.ctxs[&self.current_ctxid].get_var(varname.value) {
            None => Err(PineInputError::new(
                PineErrorKind::VarNotDeclare,
                varname.range,
            )),
            Some(val) => Ok(val.clone()),
        }
    }

    fn parse_type_cast(&mut self, type_cast: &mut TypeCast<'a>) -> ParseResult<'a> {
        let mut is_cast_err = false;
        let origin_type = self.parse_exp(&mut type_cast.exp)?;
        let result = match type_cast.data_type {
            DataType::Bool => match origin_type {
                SyntaxType::Series(SimpleSyntaxType::Int)
                | SyntaxType::Series(SimpleSyntaxType::Float)
                | SyntaxType::Series(SimpleSyntaxType::Bool)
                | SyntaxType::Series(SimpleSyntaxType::Na) => {
                    Ok(SyntaxType::Series(SimpleSyntaxType::Bool))
                }
                SyntaxType::Series(_) => {
                    is_cast_err = true;
                    Ok(SyntaxType::Series(SimpleSyntaxType::Bool))
                }
                SyntaxType::Simple(SimpleSyntaxType::Int)
                | SyntaxType::Simple(SimpleSyntaxType::Float)
                | SyntaxType::Simple(SimpleSyntaxType::Bool)
                | SyntaxType::Simple(SimpleSyntaxType::Na) => {
                    Ok(SyntaxType::Simple(SimpleSyntaxType::Bool))
                }
                _ => {
                    is_cast_err = true;
                    Ok(SyntaxType::Simple(SimpleSyntaxType::Bool))
                }
            },
            DataType::Int => match origin_type {
                SyntaxType::Series(SimpleSyntaxType::Int)
                | SyntaxType::Series(SimpleSyntaxType::Float)
                | SyntaxType::Series(SimpleSyntaxType::Na) => {
                    Ok(SyntaxType::Series(SimpleSyntaxType::Int))
                }
                SyntaxType::Series(_) => {
                    is_cast_err = true;
                    Ok(SyntaxType::Series(SimpleSyntaxType::Int))
                }
                SyntaxType::Simple(SimpleSyntaxType::Int)
                | SyntaxType::Simple(SimpleSyntaxType::Float)
                | SyntaxType::Simple(SimpleSyntaxType::Na) => {
                    Ok(SyntaxType::Simple(SimpleSyntaxType::Int))
                }
                _ => {
                    is_cast_err = true;
                    Ok(SyntaxType::Simple(SimpleSyntaxType::Int))
                }
            },
            DataType::Float => match origin_type {
                SyntaxType::Series(SimpleSyntaxType::Int)
                | SyntaxType::Series(SimpleSyntaxType::Float)
                | SyntaxType::Series(SimpleSyntaxType::Na) => {
                    Ok(SyntaxType::Series(SimpleSyntaxType::Float))
                }
                SyntaxType::Series(_) => {
                    is_cast_err = true;
                    Ok(SyntaxType::Series(SimpleSyntaxType::Float))
                }

                SyntaxType::Simple(SimpleSyntaxType::Int)
                | SyntaxType::Simple(SimpleSyntaxType::Float)
                | SyntaxType::Simple(SimpleSyntaxType::Na) => {
                    Ok(SyntaxType::Simple(SimpleSyntaxType::Float))
                }
                _ => {
                    is_cast_err = true;
                    Ok(SyntaxType::Simple(SimpleSyntaxType::Float))
                }
            },
            DataType::Color => match origin_type {
                SyntaxType::Series(SimpleSyntaxType::Color) => {
                    Ok(SyntaxType::Series(SimpleSyntaxType::Color))
                }
                SyntaxType::Series(_) => {
                    is_cast_err = true;
                    Ok(SyntaxType::Series(SimpleSyntaxType::Color))
                }
                SyntaxType::Simple(SimpleSyntaxType::Color) => {
                    Ok(SyntaxType::Simple(SimpleSyntaxType::Color))
                }
                _ => {
                    is_cast_err = true;
                    Ok(SyntaxType::Simple(SimpleSyntaxType::Color))
                }
            },
            DataType::String => match origin_type {
                SyntaxType::Series(SimpleSyntaxType::String) => {
                    Ok(SyntaxType::Series(SimpleSyntaxType::String))
                }
                SyntaxType::Series(_) => {
                    is_cast_err = true;
                    Ok(SyntaxType::Series(SimpleSyntaxType::String))
                }
                SyntaxType::Simple(SimpleSyntaxType::String) => {
                    Ok(SyntaxType::Simple(SimpleSyntaxType::String))
                }
                _ => {
                    is_cast_err = true;
                    Ok(SyntaxType::Simple(SimpleSyntaxType::String))
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
        result
    }

    fn parse_exp(&mut self, exp: &mut Exp<'a>) -> ParseResult<'a> {
        match exp {
            Exp::Na(_) => Ok(SyntaxType::Simple(SimpleSyntaxType::Na)),
            Exp::Bool(_) => Ok(SyntaxType::Simple(SimpleSyntaxType::Bool)),
            Exp::Num(Numeral::Int(_)) => Ok(SyntaxType::Simple(SimpleSyntaxType::Int)),
            Exp::Num(Numeral::Float(_)) => Ok(SyntaxType::Simple(SimpleSyntaxType::Float)),
            Exp::Str(_) => Ok(SyntaxType::Simple(SimpleSyntaxType::String)),
            Exp::Color(_) => Ok(SyntaxType::Simple(SimpleSyntaxType::Color)),
            Exp::VarName(name) => self.parse_varname(name),
            Exp::Tuple(tuple) => self.parse_tuple(tuple),
            Exp::TypeCast(type_cast) => self.parse_type_cast(type_cast),
            Exp::FuncCall(func_call) => {
                self.parse_func_call(func_call);
                Ok(SyntaxType::Na)
            }
            Exp::RefCall(ref_call) => self.parse_ref_call(ref_call),
            Exp::Condition(condition) => self.parse_condition(condition),
            Exp::Ite(ite) => self.parse_ifthenelse(ite),
            Exp::ForRange(fr) => self.parse_forrange(fr),
            Exp::UnaryExp(node) => self.parse_exp(&mut node.exp),
            Exp::BinaryExp(node) => {
                self.parse_exp(&mut node.exp1);
                self.parse_exp(&mut node.exp2);
            }
            _ => {}
        }
    }

    fn parse_stmt(&mut self, stmt: &mut Statement<'a>) {
        match stmt {
            Statement::FuncCall(func_call) => self.parse_func_call(func_call),
            Statement::Ite(ite) => self.parse_ifthenelse(ite),
            Statement::ForRange(fr) => self.parse_forrange(fr),
            Statement::Assignment(assign) => {
                self.parse_exp(&mut assign.val);
            }
            Statement::VarAssignment(assign) => {
                self.parse_exp(&mut assign.val);
            }
            Statement::FuncDef(func_def) => {
                self.parse_blk(&mut func_def.body);
            }
            _ => {}
        }
    }

    pub fn parse_blk(&mut self, blk: &mut Block<'a>) {
        for stmt in blk.stmts.iter_mut() {
            self.parse_stmt(stmt);
        }
        if let Some(ref mut exp) = blk.ret_stmt {
            self.parse_exp(exp);
        }
    }
}

// #[cfg(test)]
// mod tests {
//     use super::*;
//     use crate::ast::input::{Position, StrRange};
//     use crate::ast::name::VarName;
//     use crate::ast::stat_expr_types::{Assignment, DataType, RefCall, TypeCast, VarAssignment};

//     fn name<'a>(n: &'a str) -> Exp<'a> {
//         Exp::VarName(VarName::new_with_start(n, Position::new(0, 0)))
//     }

//     fn func_call<'a>(
//         method: Exp<'a>,
//         pos_args: Vec<Exp<'a>>,
//         dict_args: Vec<(VarName<'a>, Exp<'a>)>,
//     ) -> Exp<'a> {
//         Exp::FuncCall(Box::new(FunctionCall::new_no_ctxid(
//             method,
//             pos_args,
//             dict_args,
//             StrRange::new_empty(),
//         )))
//     }

//     fn func_call_stmt<'a>(
//         method: Exp<'a>,
//         pos_args: Vec<Exp<'a>>,
//         dict_args: Vec<(VarName<'a>, Exp<'a>)>,
//     ) -> Statement<'a> {
//         Statement::FuncCall(Box::new(FunctionCall::new_no_ctxid(
//             method,
//             pos_args,
//             dict_args,
//             StrRange::new_empty(),
//         )))
//     }

//     #[test]
//     fn func_call_test() {
//         let mut call_exp = FunctionCall::new_no_ctxid(
//             name("func"),
//             vec![func_call(name("arg"), vec![], vec![])],
//             vec![(
//                 VarName::new_no_input("arg2"),
//                 func_call(name("arg2"), vec![], vec![]),
//             )],
//             StrRange::new_empty(),
//         );
//         let mut parser = CtxIdParser::new();
//         parser.parse_func_call(&mut call_exp);
//         assert_eq!(call_exp.ctxid, 0);
//         assert_eq!(parser.ctxid, 3);
//     }

//     #[test]
//     fn ref_call_test() {
//         let mut ref_exp = RefCall {
//             name: func_call(name("ref"), vec![], vec![]),
//             arg: func_call(name("arg"), vec![], vec![]),
//             range: StrRange::new_empty(),
//         };
//         let mut parser = CtxIdParser::new();
//         parser.parse_ref_call(&mut ref_exp);
//         assert_eq!(parser.ctxid, 2);
//     }

//     #[test]
//     fn condition_test() {
//         let mut cond = Condition {
//             cond: func_call(name("cond"), vec![], vec![]),
//             exp1: func_call(name("exp1"), vec![], vec![]),
//             exp2: func_call(name("exp2"), vec![], vec![]),
//             range: StrRange::new_empty(),
//         };
//         let mut parser = CtxIdParser::new();
//         parser.parse_condition(&mut cond);
//         assert_eq!(parser.ctxid, 3);
//     }

//     #[test]
//     fn type_cast_test() {
//         let mut cast = Exp::TypeCast(Box::new(TypeCast {
//             data_type: DataType::Bool,
//             exp: func_call(name("cond"), vec![], vec![]),
//             range: StrRange::new_empty(),
//         }));
//         let mut parser = CtxIdParser::new();
//         parser.parse_exp(&mut cast);
//         assert_eq!(parser.ctxid, 1);
//     }

//     #[test]
//     fn assign_test() {
//         let mut cast = Statement::Assignment(Box::new(Assignment::new(
//             vec![VarName::new_no_input("n")],
//             func_call(name("func"), vec![], vec![]),
//             false,
//             None,
//             StrRange::new_empty(),
//         )));
//         let mut parser = CtxIdParser::new();
//         parser.parse_stmt(&mut cast);
//         assert_eq!(parser.ctxid, 1);
//     }

//     #[test]
//     fn var_assign_test() {
//         let mut cast = Statement::VarAssignment(Box::new(VarAssignment::new_no_input(
//             VarName::new_no_input("n"),
//             func_call(name("func"), vec![], vec![]),
//         )));
//         let mut parser = CtxIdParser::new();
//         parser.parse_stmt(&mut cast);
//         assert_eq!(parser.ctxid, 1);
//     }

//     #[test]
//     fn ife_test() {
//         let mut ite = IfThenElse::new_no_ctxid(
//             func_call(name("cond"), vec![], vec![]),
//             Block::new_no_input(
//                 vec![func_call_stmt(name("t1"), vec![], vec![])],
//                 Some(func_call(name("then"), vec![], vec![])),
//             ),
//             Some(Block::new_no_input(
//                 vec![func_call_stmt(name("e1"), vec![], vec![])],
//                 Some(func_call(name("else"), vec![], vec![])),
//             )),
//             StrRange::new_empty(),
//         );
//         let mut parser = CtxIdParser::new();
//         parser.parse_ifthenelse(&mut ite);
//         assert_eq!(ite.then_ctxid, 0);
//         assert_eq!(ite.else_ctxid, 1);
//         assert_eq!(parser.ctxid, 7);
//     }

//     #[test]
//     fn for_range_test() {
//         let mut fr = ForRange::new_no_ctxid(
//             VarName::new_no_input("fr"),
//             func_call(name("start"), vec![], vec![]),
//             func_call(name("end"), vec![], vec![]),
//             Some(func_call(name("step"), vec![], vec![])),
//             Block::new_no_input(
//                 vec![func_call_stmt(name("e1"), vec![], vec![])],
//                 Some(func_call(name("else"), vec![], vec![])),
//             ),
//             StrRange::new_empty(),
//         );
//         let mut parser = CtxIdParser::new();
//         parser.parse_forrange(&mut fr);
//         assert_eq!(fr.ctxid, 0);
//         assert_eq!(parser.ctxid, 6);
//     }

//     #[test]
//     fn func_def_test() {
//         let mut def = Statement::FuncDef(Box::new(FunctionDef {
//             name: VarName::new_no_input("fr"),
//             params: vec![VarName::new_no_input("arg1")],
//             body: Block::new_no_input(
//                 vec![func_call_stmt(name("e1"), vec![], vec![])],
//                 Some(func_call(name("else"), vec![], vec![])),
//             ),
//             range: StrRange::new_empty(),
//         }));
//         let mut parser = CtxIdParser::new();
//         parser.parse_stmt(&mut def);
//         assert_eq!(parser.ctxid, 2);
//     }
// }
