use crate::ast::stat_expr_types::{
    Block, Condition, Exp, ForRange, FunctionCall, FunctionDef, IfThenElse, RefCall, Statement,
};

pub struct CtxIdParser {
    ctxid: i32,
}

impl CtxIdParser {
    pub fn new() -> CtxIdParser {
        CtxIdParser { ctxid: 0 }
    }

    fn parse_func_call<'a>(&mut self, func_call: &mut FunctionCall<'a>) {
        func_call.ctxid = self.ctxid;
        self.ctxid += 1;

        self.parse_exp(&mut func_call.method);
        for arg in func_call.pos_args.iter_mut() {
            self.parse_exp(arg);
        }
        for (_, exp) in func_call.dict_args.iter_mut() {
            self.parse_exp(exp);
        }
    }

    fn parse_tuple<'a>(&mut self, tuple: &mut Vec<Exp<'a>>) {
        for arg in tuple.iter_mut() {
            self.parse_exp(arg);
        }
    }

    fn parse_ref_call<'a>(&mut self, ref_call: &mut RefCall<'a>) {
        self.parse_exp(&mut ref_call.arg);
        self.parse_exp(&mut ref_call.name);
    }

    fn parse_condition<'a>(&mut self, condition: &mut Condition<'a>) {
        self.parse_exp(&mut condition.cond);
        self.parse_exp(&mut condition.exp1);
        self.parse_exp(&mut condition.exp2);
    }

    fn parse_ifthenelse<'a>(&mut self, ite: &mut IfThenElse<'a>) {
        ite.then_ctxid = self.ctxid;
        self.ctxid += 1;
        ite.else_ctxid = self.ctxid;
        self.ctxid += 1;

        self.parse_exp(&mut ite.cond);
        self.parse_blk(&mut ite.then_blk);
        if let Some(else_blk) = &mut ite.else_blk {
            self.parse_blk(else_blk);
        }
    }

    fn parse_forrange<'a>(&mut self, for_range: &mut ForRange<'a>) {
        for_range.ctxid = self.ctxid;
        self.ctxid += 1;

        self.parse_exp(&mut for_range.start);
        self.parse_exp(&mut for_range.end);
        if let Some(step) = &mut for_range.step {
            self.parse_exp(step);
        }
        self.parse_blk(&mut for_range.do_blk);
    }

    fn parse_exp<'a>(&mut self, exp: &mut Exp<'a>) {
        match exp {
            Exp::Tuple(tuple) => {
                self.parse_tuple(tuple);
            }
            Exp::TypeCast(type_cast) => self.parse_exp(&mut type_cast.exp),
            Exp::FuncCall(func_call) => {
                self.parse_func_call(func_call);
            }
            Exp::RefCall(ref_call) => self.parse_ref_call(ref_call),
            Exp::Condition(condition) => self.parse_condition(condition),
            Exp::Ite(ite) => self.parse_ifthenelse(ite),
            Exp::ForRange(fr) => self.parse_forrange(fr),
            Exp::UnaryExp(_, exp) => self.parse_exp(exp),
            Exp::BinaryExp(_, exp1, exp2) => {
                self.parse_exp(exp1);
                self.parse_exp(exp2);
            }
            _ => {}
        }
    }

    fn parse_stmt<'a>(&mut self, stmt: &mut Statement<'a>) {
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

    pub fn parse_blk<'a>(&mut self, blk: &mut Block<'a>) {
        for stmt in blk.stmts.iter_mut() {
            self.parse_stmt(stmt);
        }
        if let Some(ref mut exp) = blk.ret_stmt {
            self.parse_exp(exp);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::name::VarName;
    use crate::ast::stat_expr_types::{Assignment, DataType, RefCall, TypeCast, VarAssignment};

    fn name<'a>(n: &'a str) -> Exp<'a> {
        Exp::VarName(VarName(n))
    }

    fn func_call<'a>(
        method: Exp<'a>,
        pos_args: Vec<Exp<'a>>,
        dict_args: Vec<(VarName<'a>, Exp<'a>)>,
    ) -> Exp<'a> {
        Exp::FuncCall(Box::new(FunctionCall::new_no_ctxid(
            method, pos_args, dict_args,
        )))
    }

    fn func_call_stmt<'a>(
        method: Exp<'a>,
        pos_args: Vec<Exp<'a>>,
        dict_args: Vec<(VarName<'a>, Exp<'a>)>,
    ) -> Statement<'a> {
        Statement::FuncCall(Box::new(FunctionCall::new_no_ctxid(
            method, pos_args, dict_args,
        )))
    }

    #[test]
    fn func_call_test() {
        let mut call_exp = FunctionCall::new_no_ctxid(
            name("func"),
            vec![func_call(name("arg"), vec![], vec![])],
            vec![(VarName("arg2"), func_call(name("arg2"), vec![], vec![]))],
        );
        let mut parser = CtxIdParser::new();
        parser.parse_func_call(&mut call_exp);
        assert_eq!(call_exp.ctxid, 0);
        assert_eq!(parser.ctxid, 3);
    }

    #[test]
    fn ref_call_test() {
        let mut ref_exp = RefCall {
            name: func_call(name("ref"), vec![], vec![]),
            arg: func_call(name("arg"), vec![], vec![]),
        };
        let mut parser = CtxIdParser::new();
        parser.parse_ref_call(&mut ref_exp);
        assert_eq!(parser.ctxid, 2);
    }

    #[test]
    fn condition_test() {
        let mut cond = Condition {
            cond: func_call(name("cond"), vec![], vec![]),
            exp1: func_call(name("exp1"), vec![], vec![]),
            exp2: func_call(name("exp2"), vec![], vec![]),
        };
        let mut parser = CtxIdParser::new();
        parser.parse_condition(&mut cond);
        assert_eq!(parser.ctxid, 3);
    }

    #[test]
    fn type_cast_test() {
        let mut cast = Exp::TypeCast(Box::new(TypeCast {
            data_type: DataType::Bool,
            exp: func_call(name("cond"), vec![], vec![]),
        }));
        let mut parser = CtxIdParser::new();
        parser.parse_exp(&mut cast);
        assert_eq!(parser.ctxid, 1);
    }

    #[test]
    fn assign_test() {
        let mut cast = Statement::Assignment(Box::new(Assignment::new(
            vec![VarName("n")],
            func_call(name("func"), vec![], vec![]),
            false,
            None,
        )));
        let mut parser = CtxIdParser::new();
        parser.parse_stmt(&mut cast);
        assert_eq!(parser.ctxid, 1);
    }

    #[test]
    fn var_assign_test() {
        let mut cast = Statement::VarAssignment(Box::new(VarAssignment::new(
            VarName("n"),
            func_call(name("func"), vec![], vec![]),
        )));
        let mut parser = CtxIdParser::new();
        parser.parse_stmt(&mut cast);
        assert_eq!(parser.ctxid, 1);
    }

    #[test]
    fn ife_test() {
        let mut ite = IfThenElse::new_no_ctxid(
            func_call(name("cond"), vec![], vec![]),
            Block::new(
                vec![func_call_stmt(name("t1"), vec![], vec![])],
                Some(func_call(name("then"), vec![], vec![])),
            ),
            Some(Block::new(
                vec![func_call_stmt(name("e1"), vec![], vec![])],
                Some(func_call(name("else"), vec![], vec![])),
            )),
        );
        let mut parser = CtxIdParser::new();
        parser.parse_ifthenelse(&mut ite);
        assert_eq!(ite.then_ctxid, 0);
        assert_eq!(ite.else_ctxid, 1);
        assert_eq!(parser.ctxid, 7);
    }

    #[test]
    fn for_range_test() {
        let mut fr = ForRange::new_no_ctxid(
            VarName("fr"),
            func_call(name("start"), vec![], vec![]),
            func_call(name("end"), vec![], vec![]),
            Some(func_call(name("step"), vec![], vec![])),
            Block::new(
                vec![func_call_stmt(name("e1"), vec![], vec![])],
                Some(func_call(name("else"), vec![], vec![])),
            ),
        );
        let mut parser = CtxIdParser::new();
        parser.parse_forrange(&mut fr);
        assert_eq!(fr.ctxid, 0);
        assert_eq!(parser.ctxid, 6);
    }

    #[test]
    fn func_def_test() {
        let mut def = Statement::FuncDef(Box::new(FunctionDef {
            name: VarName("fr"),
            params: vec![VarName("arg1")],
            body: Block::new(
                vec![func_call_stmt(name("e1"), vec![], vec![])],
                Some(func_call(name("else"), vec![], vec![])),
            ),
        }));
        let mut parser = CtxIdParser::new();
        parser.parse_stmt(&mut def);
        assert_eq!(parser.ctxid, 2);
    }
}
