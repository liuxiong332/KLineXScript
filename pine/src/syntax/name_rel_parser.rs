use super::{downcast_ctx, SyntaxCtx};
use crate::ast::input::StrRange;
use crate::ast::name::VarName;
use crate::ast::stat_expr_types::*;
use std::collections::HashMap;
use std::collections::HashSet;

// Find the dependency names for the expression and statement
pub trait DepNameFinder<'a> {
    fn find_names(&self) -> Vec<&'a str>;
}

// Find the generated variable names for expressions and statements
pub trait GenNameFinder<'a> {
    fn find_gen_names(&self) -> Vec<&'a str>;
}

impl<'a> DepNameFinder<'a> for Assignment<'a> {
    fn find_names(&self) -> Vec<&'a str> {
        self.val.find_names()
    }
}

impl<'a> GenNameFinder<'a> for Assignment<'a> {
    fn find_gen_names(&self) -> Vec<&'a str> {
        self.names.iter().map(|s| s.value).collect()
    }
}

impl<'a> DepNameFinder<'a> for VarAssignment<'a> {
    fn find_names(&self) -> Vec<&'a str> {
        self.val.find_names()
        // vec![vec![self.name.value], ].concat()
    }
}

impl<'a> GenNameFinder<'a> for VarAssignment<'a> {
    fn find_gen_names(&self) -> Vec<&'a str> {
        vec![self.name.value]
    }
}

impl<'a> DepNameFinder<'a> for ForRange<'a> {
    fn find_names(&self) -> Vec<&'a str> {
        vec![
            self.start.find_names(),
            self.end.find_names(),
            self.step.as_ref().map_or(vec![], |e| e.find_names()),
        ]
        .concat()
    }
}

impl<'a> DepNameFinder<'a> for FunctionCall<'a> {
    fn find_names(&self) -> Vec<&'a str> {
        [
            self.method.find_names(),
            self.pos_args
                .iter()
                .map(|e| e.find_names())
                .flatten()
                .collect(),
            self.dict_args
                .iter()
                .map(|(_, e)| e.find_names())
                .flatten()
                .collect(),
        ]
        .concat()
    }
}

impl<'a> DepNameFinder<'a> for Exp<'a> {
    fn find_names(&self) -> Vec<&'a str> {
        match self {
            Exp::Na(_) | Exp::Bool(_) | Exp::Num(_) | Exp::Str(_) | Exp::Color(_) => vec![],
            Exp::VarName(name) => vec![name.name.value],
            Exp::Tuple(t) => t.exps.iter().map(|e| e.find_names()).flatten().collect(),
            Exp::TypeCast(t) => t.exp.find_names(),
            Exp::FuncCall(c) => c.find_names(),
            Exp::RefCall(r) => vec![r.name.find_names(), r.arg.find_names()].concat(),
            Exp::PrefixExp(p) => p.left_exp.find_names(),
            Exp::Condition(c) => vec![
                c.cond.find_names(),
                c.exp1.find_names(),
                c.exp2.find_names(),
            ]
            .concat(),
            Exp::Ite(ite) => ite.cond.find_names(),

            Exp::ForRange(fr) => fr.find_names(),

            Exp::Assignment(assign) => assign.find_names(),

            Exp::VarAssignment(assign) => assign.find_names(),
            Exp::UnaryExp(exp) => exp.exp.find_names(),
            Exp::BinaryExp(exp) => vec![exp.exp1.find_names(), exp.exp2.find_names()].concat(),
        }
    }
}

impl<'a> GenNameFinder<'a> for Exp<'a> {
    fn find_gen_names(&self) -> Vec<&'a str> {
        match self {
            Exp::Assignment(assign) => assign.find_gen_names(),
            Exp::VarAssignment(assign) => assign.find_gen_names(),
            _ => vec![],
        }
    }
}

impl<'a> DepNameFinder<'a> for Statement<'a> {
    fn find_names(&self) -> Vec<&'a str> {
        match self {
            Statement::Break(_) | Statement::Continue(_) | Statement::None(_) => vec![],
            Statement::Assignment(assign) => assign.find_names(),
            Statement::VarAssignment(assign) => assign.find_names(),
            Statement::Ite(ite) => ite.cond.find_names(),
            Statement::ForRange(fr) => fr.find_names(),
            Statement::FuncCall(f) => f.find_names(),
            Statement::FuncDef(_) => vec![],
            Statement::Exp(e) => e.find_names(),
        }
    }
}

impl<'a> GenNameFinder<'a> for Statement<'a> {
    fn find_gen_names(&self) -> Vec<&'a str> {
        match self {
            Statement::Break(_) | Statement::Continue(_) | Statement::None(_) => vec![],
            Statement::Assignment(assign) => assign.find_gen_names(),
            Statement::VarAssignment(assign) => assign.find_gen_names(),
            Statement::Ite(_) => vec![],
            Statement::ForRange(_) => vec![],
            Statement::FuncCall(_) => vec![],
            Statement::FuncDef(d) => vec![d.name.value],
            Statement::Exp(e) => e.find_gen_names(),
        }
    }
}

// impl<'a> ExpNameExtractor<'a> for Block<'a> {
//     fn extract(&self) -> Vec<&'a str> {
//         vec![]
//     }
// }

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct CtxVarName<'a> {
    ctxid: i32,
    name: &'a str,
}

impl<'a> CtxVarName<'a> {
    pub fn new(ctxid: i32, name: &'a str) -> CtxVarName<'a> {
        CtxVarName { ctxid, name }
    }
}

pub struct ExpNameRelParser<'a> {
    pub stmt_dep_names: HashMap<*const Statement<'a>, Vec<CtxVarName<'a>>>,
    pub name_gen_stmts: HashMap<CtxVarName<'a>, Vec<*const Statement<'a>>>,
    pub ctxid_stack: Vec<i32>,
    pub ctx: Option<*mut (dyn SyntaxCtx<'a> + 'a)>,
    pub exp_stmt: HashMap<*const Exp<'a>, Box<Statement<'a>>>,
    pub gen_index: i32,
}

impl<'a> ExpNameRelParser<'a> {
    pub fn new() -> ExpNameRelParser<'a> {
        ExpNameRelParser {
            stmt_dep_names: HashMap::new(),
            name_gen_stmts: HashMap::new(),
            ctxid_stack: vec![],
            ctx: None,
            exp_stmt: HashMap::new(),
            gen_index: -1,
        }
    }

    pub fn enter_ctx(&mut self, ctx: *mut (dyn SyntaxCtx<'a> + 'a), ctxid: i32) {
        self.ctxid_stack.push(ctxid);
        self.ctx = Some(ctx);
    }

    pub fn exit_ctx(&mut self) {
        self.ctxid_stack.pop();
        self.ctx = downcast_ctx(self.ctx.unwrap()).get_parent();
    }

    pub fn gen_stmt_from_exp(&mut self, exp: *const Exp<'a>) -> Box<Statement<'a>> {
        if !self.exp_stmt.contains_key(&exp) {
            let new_exp = unsafe { exp.as_ref().unwrap().clone() };
            self.exp_stmt.insert(exp, Box::new(Statement::Exp(new_exp)));
        }
        self.exp_stmt.remove(&exp).unwrap()
    }

    fn get_ctx_varname(
        &self,
        ctx: *mut (dyn SyntaxCtx<'a> + 'a),
        name: &'a str,
    ) -> Option<CtxVarName<'a>> {
        match downcast_ctx(ctx).get_var_scope(name) {
            Some(_) => Some(CtxVarName::new(*self.ctxid_stack.last().unwrap(), name)),
            None => {
                if let Some(parent) = downcast_ctx(ctx).get_parent() {
                    self.get_ctx_varname(parent, name)
                } else {
                    None
                }
            }
        }
    }

    pub fn parse_exp(&mut self, exp: &Exp<'a>) {
        let stmt = self.gen_stmt_from_exp(exp);
        self.parse_stmt(&*stmt);
        self.exp_stmt.insert(exp, stmt);
    }

    pub fn parse_stmt(&mut self, stmt: &Statement<'a>) {
        let mut depnames = stmt.find_names();
        depnames.sort();
        depnames.dedup();
        let nameset: Vec<_> = depnames
            .into_iter()
            .filter_map(|n| self.get_ctx_varname(self.ctx.unwrap(), n))
            .collect();

        self.stmt_dep_names.insert(stmt, nameset);

        stmt.find_gen_names().into_iter().for_each(|n| {
            let n = self.get_ctx_varname(self.ctx.unwrap(), n);
            if let Some(n) = n {
                match self.name_gen_stmts.get_mut(&n) {
                    Some(vec) => {
                        vec.push(stmt);
                    }
                    None => {
                        self.name_gen_stmts
                            .insert(n, vec![stmt as *const Statement<'a>]);
                    }
                }
            }
        });
    }

    fn _gen_dep_stmts(
        &self,
        stmt: &Statement<'a>,
    ) -> (Vec<CtxVarName<'a>>, Vec<*const Statement<'a>>) {
        let mut handle_stmts: Vec<*const Statement<'a>> = vec![stmt];
        let mut res_stmts: Vec<*const Statement<'a>> = vec![];
        let mut lib_names: Vec<CtxVarName<'a>> = vec![];
        let mut processd_names: HashSet<CtxVarName<'a>> = HashSet::new();
        let mut processd_stmts: HashSet<*const Statement<'a>> = HashSet::new();
        processd_stmts.insert(stmt);

        while !handle_stmts.is_empty() {
            let stmt = handle_stmts.remove(0);
            res_stmts.push(stmt);
            match self.stmt_dep_names.get(&(stmt as *const Statement<'a>)) {
                Some(names) => {
                    let stmts: Vec<_> = names
                        .iter()
                        .filter(|n| !processd_names.contains(n))
                        .map(|n| {
                            match self.name_gen_stmts.get(&n).cloned() {
                                Some(v) => {
                                    if v.is_empty() {
                                        lib_names.push(n.clone());
                                    }
                                    v
                                }
                                None => {
                                    // This name is not generated by statements, so it's library name
                                    lib_names.push(n.clone());
                                    vec![]
                                }
                            }
                        })
                        .flatten()
                        .filter(|st| {
                            if !processd_stmts.contains(st) {
                                processd_stmts.insert(st.clone());
                                true
                            } else {
                                false
                            }
                        })
                        .rev()
                        .collect();
                    processd_names = processd_names.iter().chain(names.iter()).cloned().collect();
                    handle_stmts.splice(handle_stmts.len().., stmts);
                }
                None => {}
            }
        }
        res_stmts.reverse();
        (lib_names, res_stmts)
    }

    fn _gen_block(
        &mut self,
        lib_names: Vec<CtxVarName<'a>>,
        stmts: Vec<*const Statement<'a>>,
    ) -> (Vec<&'a str>, FunctionDef<'a>) {
        let names: Vec<_> = lib_names
            .into_iter()
            .filter_map(|n| {
                let ctx = downcast_ctx(self.ctx.unwrap());
                if n.ctxid == 0 {
                    ctx.map_input_src(n.name)
                } else {
                    None
                }
            })
            .collect();

        let mut new_stmts: Vec<_> = stmts
            .into_iter()
            .map(|s| unsafe { s.as_ref().unwrap().clone() })
            .collect();

        let expr_st = new_stmts.pop().unwrap();
        let expr = match expr_st {
            Statement::Exp(e) => e,
            _ => unreachable!(),
        };

        self.gen_index += 1;
        let func_def = FunctionDef::new_with_gen_name(
            format!("gen@{}", self.gen_index),
            names.iter().map(|n| VarName::new_no_input(n)).collect(),
            Block::new(new_stmts, Some(expr), StrRange::new_empty()),
            StrRange::new_empty(),
        );
        (names, func_def)
    }

    pub fn gen_dep_stmts_for_exp(&mut self, exp: &Exp<'a>) -> (Vec<&'a str>, FunctionDef<'a>) {
        let stmt = self.gen_stmt_from_exp(exp);
        let (names, stmts) = self._gen_dep_stmts(&*stmt);
        self.exp_stmt.insert(exp, stmt);
        self._gen_block(names, stmts)
    }

    pub fn gen_dep_stmts(&mut self, stmt: &Statement<'a>) -> (Vec<&'a str>, FunctionDef<'a>) {
        let (names, stmts) = self._gen_dep_stmts(stmt);
        self._gen_block(names, stmts)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::input::*;
    use crate::ast::stat_expr::*;
    use crate::ast::state::*;
    use crate::ast::syntax_type::*;
    use crate::syntax::SimpleInputSrcDetector;
    use crate::syntax::{ContextType, SyntaxContext, SyntaxParser};
    use std::rc::Rc;

    #[test]
    fn parse_stmt_test() {
        let mut parser = ExpNameRelParser::new();
        let mut ctx = SyntaxContext::new(None, ContextType::Normal);
        ctx.declare_var_with_index("close", SyntaxType::int());
        ctx.declare_var_with_index("m", SyntaxType::int());
        parser.enter_ctx(&mut ctx, 0);

        let (_, res) = block(
            Input::new_with_str("m = close + close\nm"),
            &AstState::new(),
        )
        .unwrap();

        parser.parse_stmt(&res.stmts[0]);

        let stmt0 = &res.stmts[0] as *const Statement;
        let depnames = [(stmt0, vec![CtxVarName::new(0, "close")])];
        let gennames = [(CtxVarName::new(0, "m"), vec![stmt0])];
        assert_eq!(parser.stmt_dep_names, depnames.iter().cloned().collect());
        assert_eq!(parser.name_gen_stmts, gennames.iter().cloned().collect());

        parser.parse_stmt(&res.stmts[1]);
        let stmt1 = &res.stmts[1] as *const Statement;
        let depnames = [
            (stmt0, vec![CtxVarName::new(0, "close")]),
            (stmt1, vec![CtxVarName::new(0, "m")]),
        ];
        let gennames = [(CtxVarName::new(0, "m"), vec![stmt0])];
        assert_eq!(parser.stmt_dep_names, depnames.iter().cloned().collect());
        assert_eq!(parser.name_gen_stmts, gennames.iter().cloned().collect());
    }

    #[test]
    fn tuple_stmt_test() {
        let mut syntax_parser = SyntaxParser::new();
        let mut parser = ExpNameRelParser::new();
        let ctx = downcast_ctx(syntax_parser.get_context());
        ctx.declare_var_with_index("close", SyntaxType::int_series());
        ctx.declare_var_with_index(
            "funca",
            SyntaxType::Function(Rc::new(FunctionTypes(vec![FunctionType::new((
                vec![("arg1", SyntaxType::int_series())],
                SyntaxType::int_series(),
            ))]))),
        );
        parser.enter_ctx(ctx, 0);

        let src = "[m, n] = [close, close]\n";
        let (_, mut res) = block(Input::new_with_str(src), &AstState::new()).unwrap();
        assert!(syntax_parser.parse_blk(&mut res).is_ok());

        parser.parse_stmt(&res.stmts[0]);
        let stmt0 = &res.stmts[0] as *const Statement;
        let depnames = [(stmt0, vec![CtxVarName::new(0, "close")])];
        let gennames = [
            (CtxVarName::new(0, "m"), vec![stmt0]),
            (CtxVarName::new(0, "n"), vec![stmt0]),
        ];
        assert_eq!(parser.stmt_dep_names, depnames.iter().cloned().collect());
        assert_eq!(parser.name_gen_stmts, gennames.iter().cloned().collect());
    }

    #[test]
    fn func_call_stmt_test() {
        let mut syntax_parser = SyntaxParser::new();
        let mut parser = ExpNameRelParser::new();
        let ctx = downcast_ctx(syntax_parser.get_context());
        ctx.declare_var_with_index("close", SyntaxType::int_series());
        ctx.declare_var_with_index(
            "funca",
            SyntaxType::Function(Rc::new(FunctionTypes(vec![FunctionType::new((
                vec![("arg1", SyntaxType::int_series())],
                SyntaxType::int_series(),
            ))]))),
        );
        parser.enter_ctx(ctx, 0);

        let src = "m = funca(close + 122)\n";
        let (_, mut res) = block(Input::new_with_str(src), &AstState::new()).unwrap();
        assert!(syntax_parser.parse_blk(&mut res).is_ok());

        parser.parse_stmt(&res.stmts[0]);
        let stmt0 = &res.stmts[0] as *const Statement;
        let depnames = [(
            stmt0,
            vec![CtxVarName::new(0, "close"), CtxVarName::new(0, "funca")],
        )];
        let gennames = [(CtxVarName::new(0, "m"), vec![stmt0])];
        assert_eq!(parser.stmt_dep_names, depnames.iter().cloned().collect());
        assert_eq!(parser.name_gen_stmts, gennames.iter().cloned().collect());
    }

    #[test]
    fn ref_call_stmt_test() {
        let mut syntax_parser = SyntaxParser::new();
        let mut parser = ExpNameRelParser::new();
        let ctx = downcast_ctx(syntax_parser.get_context());
        ctx.declare_var_with_index("close", SyntaxType::int_series());
        parser.enter_ctx(ctx, 0);

        let src = "m=1\nn = close[m]\n";
        let (_, mut res) = block(Input::new_with_str(src), &AstState::new()).unwrap();
        assert!(syntax_parser.parse_blk(&mut res).is_ok());

        parser.parse_stmt(&res.stmts[0]);
        parser.parse_stmt(&res.stmts[1]);
        let stmt0 = &res.stmts[0] as *const Statement;
        let stmt1 = &res.stmts[1] as *const Statement;
        let depnames = [
            (stmt0, vec![]),
            (
                stmt1,
                vec![CtxVarName::new(0, "close"), CtxVarName::new(0, "m")],
            ),
        ];
        let gennames = [
            (CtxVarName::new(0, "m"), vec![stmt0]),
            (CtxVarName::new(0, "n"), vec![stmt1]),
        ];
        assert_eq!(parser.stmt_dep_names, depnames.iter().cloned().collect());
        assert_eq!(parser.name_gen_stmts, gennames.iter().cloned().collect());
    }

    #[test]
    fn condition_stmt_test() {
        let mut syntax_parser = SyntaxParser::new();
        let mut parser = ExpNameRelParser::new();
        let ctx = downcast_ctx(syntax_parser.get_context());
        ctx.declare_var_with_index("close", SyntaxType::int_series());
        parser.enter_ctx(ctx, 0);

        let src = "m=1\nn = m ? close : close\n";
        let (_, mut res) = block(Input::new_with_str(src), &AstState::new()).unwrap();
        assert!(syntax_parser.parse_blk(&mut res).is_ok());

        parser.parse_stmt(&res.stmts[0]);
        parser.parse_stmt(&res.stmts[1]);
        let stmt0 = &res.stmts[0] as *const Statement;
        let stmt1 = &res.stmts[1] as *const Statement;
        let depnames = [
            (stmt0, vec![]),
            (
                stmt1,
                vec![CtxVarName::new(0, "close"), CtxVarName::new(0, "m")],
            ),
        ];
        let gennames = [
            (CtxVarName::new(0, "m"), vec![stmt0]),
            (CtxVarName::new(0, "n"), vec![stmt1]),
        ];
        assert_eq!(parser.stmt_dep_names, depnames.iter().cloned().collect());
        assert_eq!(parser.name_gen_stmts, gennames.iter().cloned().collect());
    }

    #[test]
    fn ite_stmt_test() {
        let mut syntax_parser = SyntaxParser::new();
        let mut parser = ExpNameRelParser::new();
        let ctx = downcast_ctx(syntax_parser.get_context());
        ctx.declare_var_with_index("close", SyntaxType::int_series());
        parser.enter_ctx(ctx, 0);

        let src = "if close\n    close\nelse\n    close";
        let (_, mut res) = block(Input::new_with_str(src), &AstState::new()).unwrap();
        assert!(syntax_parser.parse_blk(&mut res).is_ok());

        parser.parse_stmt(&res.stmts[0]);
        let stmt0 = &res.stmts[0] as *const Statement;

        let depnames = [(stmt0, vec![]), (stmt0, vec![CtxVarName::new(0, "close")])];
        let gennames = [];
        assert_eq!(parser.stmt_dep_names, depnames.iter().cloned().collect());
        assert_eq!(parser.name_gen_stmts, gennames.iter().cloned().collect());
    }

    #[test]
    fn gen_stmt_test() {
        let mut syntax_parser = SyntaxParser::new();
        let input_detector = SimpleInputSrcDetector::new(vec!["close", "time"]);
        syntax_parser.init_input_detector(&input_detector);

        let mut parser = ExpNameRelParser::new();
        let ctx = downcast_ctx(syntax_parser.get_context());
        ctx.declare_var_with_index("close", SyntaxType::int_series());
        ctx.declare_var_with_index("time", SyntaxType::int_series());
        parser.enter_ctx(ctx, 0);

        let src = "m=1\nn = close[m]\nn:=1\nm+n+time";
        let (_, mut res) = block(Input::new_with_str(src), &AstState::new()).unwrap();
        assert!(syntax_parser.parse_blk(&mut res).is_ok());

        res.stmts.iter().for_each(|m| {
            parser.parse_stmt(m);
        });
        let (names, new_blk) = parser.gen_dep_stmts(&res.stmts[3]);
        assert_eq!(names, vec!["_time", "close"]);
        assert_eq!(
            new_blk.params.iter().map(|v| v.value).collect::<Vec<_>>(),
            vec!["_time", "close"]
        );
        assert_eq!(
            new_blk.body.stmts,
            vec![
                res.stmts[0].clone(),
                res.stmts[1].clone(),
                res.stmts[2].clone()
            ]
        );
    }
}
