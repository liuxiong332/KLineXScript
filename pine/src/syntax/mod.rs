use crate::ast::error::PineErrorKind;
use crate::ast::name::VarName;
use crate::ast::num::Numeral;
use crate::ast::op::{BinaryOp, UnaryOp};
use crate::ast::stat_expr_types::{
    BinaryExp, Block, Condition, DataType, Exp, ForRange, FunctionCall, FunctionDef, IfThenElse,
    RefCall, Statement, TupleNode, TypeCast, UnaryExp,
};
use crate::ast::state::PineInputError;
use std::collections::HashMap;
use std::rc::Rc;

mod convert;
mod type_cast;

use convert::{implicity_convert, simple_to_series};
use type_cast::parse_type_cast;

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
    Void,
    Simple(SimpleSyntaxType),
    Series(SimpleSyntaxType),
    Tuple(Rc<Vec<SyntaxType<'a>>>),
    Object(Rc<HashMap<&'a str, SyntaxType<'a>>>),
    Function(Rc<FunctionType<'a>>),
    UserFunction(Rc<Vec<&'a str>>),
    Any,
}

impl<'a> SyntaxType<'a> {
    pub fn is_na(&self) -> bool {
        match self {
            SyntaxType::Simple(SimpleSyntaxType::Na) | SyntaxType::Series(SimpleSyntaxType::Na) => {
                true
            }
            _ => false,
        }
    }

    pub fn is_void(&self) -> bool {
        self == &SyntaxType::Void
    }

    pub fn is_int(&self) -> bool {
        match self {
            SyntaxType::Simple(SimpleSyntaxType::Int)
            | SyntaxType::Series(SimpleSyntaxType::Int) => true,
            _ => false,
        }
    }

    pub fn is_num(&self) -> bool {
        match self {
            SyntaxType::Simple(SimpleSyntaxType::Int)
            | SyntaxType::Series(SimpleSyntaxType::Int)
            | SyntaxType::Simple(SimpleSyntaxType::Float)
            | SyntaxType::Series(SimpleSyntaxType::Float) => true,
            _ => false,
        }
    }

    pub fn is_string(&self) -> bool {
        match self {
            SyntaxType::Simple(SimpleSyntaxType::String)
            | SyntaxType::Series(SimpleSyntaxType::String) => true,
            _ => false,
        }
    }
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

    fn parse_ref_call_arg(
        &mut self,
        ref_call: &mut RefCall<'a>,
        name_type: SyntaxType<'a>,
    ) -> ParseResult<'a> {
        let arg_res = self.parse_exp(&mut ref_call.arg)?;
        match arg_res.syntax_type {
            SyntaxType::Simple(SimpleSyntaxType::Int)
            | SyntaxType::Series(SimpleSyntaxType::Int) => Ok(ParseValue::new_with_type(name_type)),
            _ => {
                self.catch(PineInputError::new(
                    PineErrorKind::RefIndexNotInt,
                    ref_call.range,
                ));
                Ok(ParseValue::new_with_type(name_type))
            }
        }
    }

    fn parse_ref_call(&mut self, ref_call: &mut RefCall<'a>) -> ParseResult<'a> {
        let name_res = self.parse_exp(&mut ref_call.name)?;

        match name_res.syntax_type {
            // If the type is simple, then we need to update the var to series.
            SyntaxType::Simple(t) => {
                if let Some(name) = name_res.varname {
                    let series_type = SyntaxType::Series(t);
                    downcast_ctx(self.context).update_var(name, series_type.clone());
                    self.parse_ref_call_arg(ref_call, series_type)
                } else {
                    Err(PineInputError::new(
                        PineErrorKind::VarNotSeriesInRef,
                        ref_call.range,
                    ))
                }
            }
            SyntaxType::Series(_) => self.parse_ref_call_arg(ref_call, name_res.syntax_type),
            _ => Err(PineInputError::new(
                PineErrorKind::VarNotSeriesInRef,
                ref_call.range,
            )),
        }
    }

    fn parse_condition(&mut self, condition: &mut Condition<'a>) -> ParseResult<'a> {
        let cond_res = self.parse_exp(&mut condition.cond)?;
        if implicity_convert(
            &cond_res.syntax_type,
            &SyntaxType::Series(SimpleSyntaxType::Bool),
        ) {
            let exp1_res = self.parse_exp(&mut condition.exp1)?;
            let exp2_res = self.parse_exp(&mut condition.exp2)?;
            if implicity_convert(&exp1_res.syntax_type, &exp2_res.syntax_type) {
                Ok(ParseValue::new_with_type(simple_to_series(
                    exp2_res.syntax_type,
                )))
            } else if implicity_convert(&exp2_res.syntax_type, &exp1_res.syntax_type) {
                Ok(ParseValue::new_with_type(simple_to_series(
                    exp1_res.syntax_type,
                )))
            } else {
                Err(PineInputError::new(
                    PineErrorKind::CondExpTypesNotSame,
                    condition.range,
                ))
            }
        } else {
            Err(PineInputError::new(
                PineErrorKind::CondNotBool,
                condition.range,
            ))
        }
    }

    fn parse_ifthenelse_exp(&mut self, ite: &mut IfThenElse<'a>) -> ParseResult<'a> {
        let cond_res = self.parse_exp(&mut ite.cond)?;
        if !implicity_convert(
            &cond_res.syntax_type,
            &SyntaxType::Series(SimpleSyntaxType::Bool),
        ) {
            return Err(PineInputError::new(PineErrorKind::CondNotBool, ite.range));
        }
        // Create new context for if block
        let mut if_ctx = Box::new(SyntaxContext::new(
            Some(self.context),
            ContextType::IfElseBlock,
        ));
        self.context = &mut *if_ctx;

        let then_res = self.parse_blk(&mut ite.then_blk)?;
        self.context = if_ctx.parent.unwrap();
        self.ctxs.insert(ite.then_ctxid, if_ctx);

        if then_res.syntax_type.is_void() {
            return Err(PineInputError::new(
                PineErrorKind::ExpNoReturn,
                ite.then_blk.range,
            ));
        }
        if then_res.syntax_type.is_na() {
            return Err(PineInputError::new(
                PineErrorKind::ExpReturnNa,
                ite.then_blk.range,
            ));
        }
        if let Some(else_blk) = &mut ite.else_blk {
            // Create new context for if block
            let mut else_ctx = Box::new(SyntaxContext::new(
                Some(self.context),
                ContextType::IfElseBlock,
            ));
            self.context = &mut *else_ctx;

            let else_res = self.parse_blk(else_blk)?;
            self.context = else_ctx.parent.unwrap();
            self.ctxs.insert(ite.else_ctxid, else_ctx);

            if else_res.syntax_type.is_void() {
                return Err(PineInputError::new(
                    PineErrorKind::ExpNoReturn,
                    else_blk.range,
                ));
            }
            if else_res.syntax_type.is_na() {
                return Err(PineInputError::new(
                    PineErrorKind::ExpReturnNa,
                    else_blk.range,
                ));
            }
            // Find the common type that can satisfy the then type and else type
            if implicity_convert(&then_res.syntax_type, &else_res.syntax_type) {
                // The return type of if-then-else block must be series.
                return Ok(ParseValue::new_with_type(simple_to_series(
                    else_res.syntax_type,
                )));
            } else if implicity_convert(&else_res.syntax_type, &then_res.syntax_type) {
                return Ok(ParseValue::new_with_type(simple_to_series(
                    then_res.syntax_type,
                )));
            } else {
                return Err(PineInputError::new(PineErrorKind::TypeMismatch, ite.range));
            }
        } else {
            Ok(ParseValue::new_with_type(then_res.syntax_type))
        }
    }

    fn parse_forrange_exp(&mut self, for_range: &mut ForRange<'a>) -> ParseResult<'a> {
        let start_res = self.parse_exp(&mut for_range.start)?;
        if !start_res.syntax_type.is_int() {
            self.catch(PineInputError::new(
                PineErrorKind::ForRangeIndexNotInt,
                for_range.start.range(),
            ));
        }
        let end_res = self.parse_exp(&mut for_range.end)?;
        if !end_res.syntax_type.is_int() {
            self.catch(PineInputError::new(
                PineErrorKind::ForRangeIndexNotInt,
                for_range.end.range(),
            ));
        }
        if let Some(step) = &mut for_range.step {
            let step_res = self.parse_exp(step)?;
            if !step_res.syntax_type.is_int() {
                self.catch(PineInputError::new(
                    PineErrorKind::ForRangeIndexNotInt,
                    step.range(),
                ));
            }
        }
        let mut for_ctx = Box::new(SyntaxContext::new(
            Some(self.context),
            ContextType::ForRangeBlock,
        ));
        for_ctx.declare_var(
            for_range.var.value,
            SyntaxType::Simple(SimpleSyntaxType::Int),
        );
        self.context = &mut *for_ctx;

        let blk_res = self.parse_blk(&mut for_range.do_blk)?;
        self.context = for_ctx.parent.unwrap();
        self.ctxs.insert(for_range.ctxid, for_ctx);

        if blk_res.syntax_type.is_void() {
            return Err(PineInputError::new(
                PineErrorKind::ExpNoReturn,
                for_range.do_blk.range,
            ));
        }
        if blk_res.syntax_type.is_na() {
            return Err(PineInputError::new(
                PineErrorKind::ExpReturnNa,
                for_range.do_blk.range,
            ));
        }
        Ok(ParseValue::new_with_type(simple_to_series(
            blk_res.syntax_type,
        )))
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
        let origin_type = self.parse_exp(&mut type_cast.exp)?.syntax_type;
        let (is_cast_err, result) = parse_type_cast(&origin_type, &type_cast.data_type);
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

    fn parse_unary(&mut self, unary: &mut UnaryExp<'a>) -> ParseResult<'a> {
        let exp_type = self.parse_exp(&mut unary.exp)?.syntax_type;
        match unary.op {
            UnaryOp::Plus | UnaryOp::Minus => {
                if !exp_type.is_num() {
                    Err(PineInputError::new(
                        PineErrorKind::UnaryTypeNotNum,
                        unary.range,
                    ))
                } else {
                    Ok(ParseValue::new_with_type(exp_type))
                }
            }
            UnaryOp::BoolNot => {
                if implicity_convert(&exp_type, &SyntaxType::Series(SimpleSyntaxType::Bool)) {
                    Ok(ParseValue::new_with_type(SyntaxType::Series(
                        SimpleSyntaxType::Bool,
                    )))
                } else if implicity_convert(&exp_type, &SyntaxType::Simple(SimpleSyntaxType::Bool))
                {
                    Ok(ParseValue::new_with_type(SyntaxType::Simple(
                        SimpleSyntaxType::Bool,
                    )))
                } else {
                    Err(PineInputError::new(
                        PineErrorKind::UnaryTypeNotNum,
                        unary.range,
                    ))
                }
            }
        }
    }

    fn parse_binary(&mut self, binary: &mut BinaryExp<'a>) -> ParseResult<'a> {
        let exp1_type = self.parse_exp(&mut binary.exp1)?.syntax_type;
        let exp2_type = self.parse_exp(&mut binary.exp2)?.syntax_type;
        let gen_bool = |exp1_type, exp2_type| match (exp1_type, exp2_type) {
            (SyntaxType::Series(_), _) | (_, SyntaxType::Series(_)) => {
                return Ok(ParseValue::new_with_type(SyntaxType::Series(
                    SimpleSyntaxType::Bool,
                )));
            }
            _ => {
                return Ok(ParseValue::new_with_type(SyntaxType::Simple(
                    SimpleSyntaxType::Bool,
                )));
            }
        };
        match binary.op {
            BinaryOp::Plus | BinaryOp::Minus | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => {
                if binary.op == BinaryOp::Plus && exp1_type.is_string() && exp2_type.is_string() {
                    match (exp1_type, exp2_type) {
                        (SyntaxType::Series(_), _) | (_, SyntaxType::Series(_)) => {
                            return Ok(ParseValue::new_with_type(SyntaxType::Series(
                                SimpleSyntaxType::String,
                            )));
                        }
                        _ => {
                            return Ok(ParseValue::new_with_type(SyntaxType::Simple(
                                SimpleSyntaxType::String,
                            )));
                        }
                    }
                }
                if !(exp1_type.is_num() && exp2_type.is_num()) {
                    return Err(PineInputError::new(
                        PineErrorKind::BinaryTypeNotNum,
                        binary.range,
                    ));
                }
                if implicity_convert(&exp1_type, &exp2_type) {
                    Ok(ParseValue::new_with_type(exp2_type))
                } else if implicity_convert(&exp2_type, &exp1_type) {
                    Ok(ParseValue::new_with_type(exp1_type))
                } else {
                    Err(PineInputError::new(
                        PineErrorKind::TypeMismatch,
                        binary.range,
                    ))
                }
            }
            BinaryOp::Gt | BinaryOp::Geq | BinaryOp::Lt | BinaryOp::Leq => {
                if !(exp1_type.is_num() && exp2_type.is_num()) {
                    return Err(PineInputError::new(
                        PineErrorKind::BinaryTypeNotNum,
                        binary.range,
                    ));
                }
                gen_bool(exp1_type, exp2_type)
            }
            BinaryOp::Eq | BinaryOp::Neq => {
                if implicity_convert(&exp1_type, &exp2_type)
                    || implicity_convert(&exp2_type, &exp1_type)
                {
                    gen_bool(exp1_type, exp2_type)
                } else {
                    Err(PineInputError::new(
                        PineErrorKind::TypeMismatch,
                        binary.range,
                    ))
                }
            }
            BinaryOp::BoolAnd | BinaryOp::BoolOr => {
                let bool_type = SyntaxType::Series(SimpleSyntaxType::Bool);
                if implicity_convert(&exp1_type, &bool_type)
                    && implicity_convert(&exp2_type, &bool_type)
                {
                    gen_bool(exp1_type, exp2_type)
                } else {
                    Err(PineInputError::new(
                        PineErrorKind::BoolExpTypeNotBool,
                        binary.range,
                    ))
                }
            }
        }
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
            Exp::Ite(ite) => self.parse_ifthenelse_exp(ite),
            Exp::ForRange(fr) => self.parse_forrange_exp(fr),
            Exp::UnaryExp(node) => self.parse_unary(node),
            Exp::BinaryExp(node) => self.parse_binary(node),
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
            // Statement::Ite(ite) => self.parse_ifthenelse(ite),
            // Statement::ForRange(fr) => self.parse_forrange(fr),
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
    use crate::ast::color::ColorNode;
    use crate::ast::input::{Input, Position, StrRange};
    use crate::ast::name::VarName;
    use crate::ast::stat_expr_types::{
        Assignment, BoolNode, DataType, NaNode, RefCall, TypeCast, VarAssignment,
    };
    use crate::ast::string::StringNode;

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

    #[test]
    fn simple_test() {
        let mut parser = SyntaxParser::new();
        assert_eq!(
            parser.parse_exp(&mut Exp::Na(NaNode::new(StrRange::new_empty()))),
            Ok(ParseValue::new_with_type(SyntaxType::Simple(
                SimpleSyntaxType::Na
            )))
        );

        assert_eq!(
            parser.parse_exp(&mut Exp::Bool(BoolNode::new_no_range(true))),
            Ok(ParseValue::new_with_type(SyntaxType::Simple(
                SimpleSyntaxType::Bool
            )))
        );

        assert_eq!(
            parser.parse_exp(&mut Exp::Num(Numeral::from_f64(1f64))),
            Ok(ParseValue::new_with_type(SyntaxType::Simple(
                SimpleSyntaxType::Float
            )))
        );

        assert_eq!(
            parser.parse_exp(&mut Exp::Num(Numeral::from_i32(1))),
            Ok(ParseValue::new_with_type(SyntaxType::Simple(
                SimpleSyntaxType::Int
            )))
        );

        assert_eq!(
            parser.parse_exp(&mut Exp::Color(ColorNode::from_str("#AAAAAA"))),
            Ok(ParseValue::new_with_type(SyntaxType::Simple(
                SimpleSyntaxType::Color
            )))
        );

        assert_eq!(
            parser.parse_exp(&mut Exp::Str(StringNode::new(
                String::from("hello"),
                StrRange::new_empty()
            ))),
            Ok(ParseValue::new_with_type(SyntaxType::Simple(
                SimpleSyntaxType::String
            )))
        );
    }

    #[test]
    fn varname_test() {
        let mut parser = SyntaxParser::new();
        downcast_ctx(parser.context).declare_var("hello", INT_TYPE);
        assert_eq!(
            parser.parse_exp(&mut Exp::VarName(varname("hello"))),
            Ok(ParseValue::new(INT_TYPE, "hello"))
        );
    }

    #[test]
    fn tuple_test() {
        let mut parser = SyntaxParser::new();
        downcast_ctx(parser.context).declare_var("arg1", INT_TYPE);
        downcast_ctx(parser.context).declare_var("arg2", FLOAT_TYPE);
        assert_eq!(
            parser.parse_exp(&mut Exp::Tuple(Box::new(TupleNode::new(
                vec![Exp::VarName(varname("arg1")), Exp::VarName(varname("arg2"))],
                StrRange::new_empty()
            )))),
            Ok(ParseValue::new_with_type(SyntaxType::Tuple(Rc::new(vec![
                INT_TYPE, FLOAT_TYPE
            ]))))
        );
    }

    #[test]
    fn tuple_type_cast() {
        let mut parser = SyntaxParser::new();
        downcast_ctx(parser.context)
            .declare_var("arg1", SyntaxType::Series(SimpleSyntaxType::Float));

        assert_eq!(
            parser.parse_exp(&mut Exp::TypeCast(Box::new(TypeCast::new_no_input(
                DataType::Int,
                Exp::VarName(varname("arg1"))
            )))),
            Ok(ParseValue::new_with_type(SyntaxType::Series(
                SimpleSyntaxType::Int
            )))
        );
    }

    #[test]
    fn ref_call_cast() {
        let mut parser = SyntaxParser::new();
        let context = downcast_ctx(parser.context);
        context.declare_var("series", SyntaxType::Series(SimpleSyntaxType::Float));
        context.declare_var("simple", SyntaxType::Simple(SimpleSyntaxType::Float));
        context.declare_var("arg", SyntaxType::Series(SimpleSyntaxType::Float));

        assert_eq!(
            parser.parse_exp(&mut Exp::RefCall(Box::new(RefCall::new_no_input(
                Exp::VarName(varname("series")),
                Exp::VarName(varname("arg"))
            )))),
            Ok(ParseValue::new_with_type(SyntaxType::Series(
                SimpleSyntaxType::Float
            )))
        );
        assert_eq!(parser.errors.len(), 1);

        assert_eq!(
            parser.parse_exp(&mut Exp::RefCall(Box::new(RefCall::new_no_input(
                Exp::VarName(varname("simple")),
                Exp::VarName(varname("arg"))
            )))),
            Ok(ParseValue::new_with_type(SyntaxType::Series(
                SimpleSyntaxType::Float
            )))
        );
        assert_eq!(
            context.get_var_scope("simple"),
            Some(&SyntaxType::Series(SimpleSyntaxType::Float))
        );
    }

    #[test]
    fn condition_test() {
        let mut parser = SyntaxParser::new();
        let context = downcast_ctx(parser.context);
        context.declare_var("cond", SyntaxType::Series(SimpleSyntaxType::Float));
        context.declare_var("arg1", SyntaxType::Series(SimpleSyntaxType::Float));
        context.declare_var("arg2", SyntaxType::Series(SimpleSyntaxType::Int));

        assert_eq!(
            parser.parse_exp(&mut Exp::Condition(Box::new(Condition::new_no_input(
                Exp::VarName(varname("cond")),
                Exp::VarName(varname("arg1")),
                Exp::VarName(varname("arg2"))
            )))),
            Ok(ParseValue::new_with_type(SyntaxType::Series(
                SimpleSyntaxType::Float
            )))
        );

        assert_eq!(
            parser.parse_exp(&mut Exp::Condition(Box::new(Condition::new_no_input(
                Exp::Num(Numeral::from_i32(1)),
                Exp::Num(Numeral::from_i32(2)),
                Exp::Num(Numeral::from_i32(3))
            )))),
            Ok(ParseValue::new_with_type(SyntaxType::Series(
                SimpleSyntaxType::Int
            )))
        );
    }

    #[test]
    fn if_then_else_exp_test() {
        use crate::ast::stat_expr::if_then_else_exp;
        use crate::ast::state::AstState;

        let mut parser = SyntaxParser::new();
        let context = downcast_ctx(parser.context);
        context.declare_var("var", SyntaxType::Simple(SimpleSyntaxType::Int));

        let input = Input::new_with_str("if var\n    1\nelse\n    1.0");
        assert_eq!(
            parser
                .parse_ifthenelse_exp(&mut if_then_else_exp(0)(input, &AstState::new()).unwrap().1),
            Ok(ParseValue::new_with_type(SyntaxType::Series(
                SimpleSyntaxType::Float
            )))
        );

        // let input = Input::new_with_str("if var\n    var = 1\n    var\nelse\n    var=1.0\n    var");
        // assert_eq!(
        //     parser
        //         .parse_ifthenelse_exp(&mut if_then_else_exp(0)(input, &AstState::new()).unwrap().1),
        //     Ok(ParseValue::new_with_type(SyntaxType::Simple(
        //         SimpleSyntaxType::Float
        //     )))
        // )
    }

    #[test]
    fn for_range_exp_test() {
        use crate::ast::stat_expr::for_range_exp;
        use crate::ast::state::AstState;

        let mut parser = SyntaxParser::new();
        let context = downcast_ctx(parser.context);
        context.declare_var("var", SyntaxType::Simple(SimpleSyntaxType::Int));

        let input = Input::new_with_str("for i = 1 to 2\n    i");
        assert_eq!(
            parser.parse_forrange_exp(&mut for_range_exp(0)(input, &AstState::new()).unwrap().1),
            Ok(ParseValue::new_with_type(SyntaxType::Series(
                SimpleSyntaxType::Int
            )))
        );
    }
}
