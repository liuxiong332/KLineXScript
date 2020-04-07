use crate::ast::input::Position;
use crate::ast::stat_expr_types::*;

#[derive(PartialEq, Clone, Debug)]
pub struct FindVal<'a> {
    name: Option<&'a str>,
    lib_name: Option<&'a str>,
    ctx_index: i32,
}

impl<'a> FindVal<'a> {
    pub fn new(name: &'a str, ctx_index: i32) -> FindVal<'a> {
        FindVal {
            name: Some(name),
            lib_name: None,
            ctx_index,
        }
    }

    pub fn new_with_lib_name(lib_name: &'a str) -> FindVal<'a> {
        FindVal {
            name: None,
            lib_name: Some(lib_name),
            ctx_index: -1,
        }
    }
}

struct FindState<'a> {
    ctx_index: i32,
    lib_names: Vec<&'a str>,
}

impl<'a> FindState<'a> {
    pub fn new() -> FindState<'a> {
        FindState {
            ctx_index: 0,
            lib_names: vec![],
        }
    }

    pub fn new_with_lib_names(names: Vec<&'a str>) -> FindState<'a> {
        FindState {
            ctx_index: 0,
            lib_names: names,
        }
    }

    pub fn new_with_ctx_index(ctx_index: i32) -> FindState<'a> {
        FindState {
            ctx_index,
            lib_names: vec![],
        }
    }
}

trait NodeFinder<'a> {
    fn find(&self, pos: Position, state: &mut FindState) -> Option<FindVal<'a>>;
}

trait LibNameForPrefix {
    fn gen_lib_name(&self) -> String;
}

impl<'a> NodeFinder<'a> for TupleNode<'a> {
    fn find(&self, pos: Position, state: &mut FindState) -> Option<FindVal<'a>> {
        for exp in self.exps.iter() {
            if let Some(res) = exp.find(pos, state) {
                return Some(res);
            }
        }
        Some(FindVal::new_with_lib_name("[]"))
    }
}

impl<'a> NodeFinder<'a> for TypeCast<'a> {
    fn find(&self, pos: Position, state: &mut FindState) -> Option<FindVal<'a>> {
        if let Some(res) = self.exp.find(pos, state) {
            return Some(res);
        }
        let type_str = match self.data_type {
            DataType::Bool => "bool",
            DataType::Color => "color",
            DataType::Int => "int",
            DataType::Float => "float",
            DataType::String => "string",
            DataType::Custom(t) => t,
        };
        Some(FindVal::new_with_lib_name(type_str))
    }
}

impl<'a> NodeFinder<'a> for FunctionCall<'a> {
    fn find(&self, pos: Position, state: &mut FindState) -> Option<FindVal<'a>> {
        if let Some(res) = self.method.find(pos, state) {
            return Some(res);
        }
        for exp in self.pos_args.iter() {
            if let Some(res) = exp.find(pos, state) {
                return Some(res);
            }
        }
        for (_, exp) in self.dict_args.iter() {
            if let Some(res) = exp.find(pos, state) {
                return Some(res);
            }
        }
        None
    }
}

impl<'a> NodeFinder<'a> for RefCall<'a> {
    fn find(&self, pos: Position, state: &mut FindState) -> Option<FindVal<'a>> {
        if let Some(res) = self.name.find(pos, state) {
            return Some(res);
        }
        if let Some(res) = self.arg.find(pos, state) {
            return Some(res);
        }
        Some(FindVal::new_with_lib_name("[]"))
    }
}

impl<'a> NodeFinder<'a> for PrefixExp<'a> {
    fn find(&self, pos: Position, state: &mut FindState) -> Option<FindVal<'a>> {
        if let Some(res) = self.left_exp.find(pos, state) {
            return Some(res);
        }
        None
    }
}

impl<'a> NodeFinder<'a> for Condition<'a> {
    fn find(&self, pos: Position, state: &mut FindState) -> Option<FindVal<'a>> {
        if let Some(res) = self.cond.find(pos, state) {
            return Some(res);
        }
        if let Some(res) = self.exp1.find(pos, state) {
            return Some(res);
        }
        if let Some(res) = self.exp2.find(pos, state) {
            return Some(res);
        }
        None
    }
}

impl<'a> NodeFinder<'a> for IfThenElse<'a> {
    fn find(&self, pos: Position, state: &mut FindState) -> Option<FindVal<'a>> {
        if let Some(res) = self.cond.find(pos, state) {
            return Some(res);
        }
        if let Some(res) = self.then_blk.find(pos, state) {
            return Some(res);
        }
        if let Some(blk) = &self.else_blk {
            if let Some(e) = blk.find(pos, state) {
                return Some(e);
            }
        }
        None
    }
}

impl<'a> NodeFinder<'a> for ForRange<'a> {
    fn find(&self, pos: Position, state: &mut FindState) -> Option<FindVal<'a>> {
        if self.var.range.contain(pos) {
            return Some(FindVal::new(self.var.value, self.ctxid));
        }
        if let Some(res) = self.start.find(pos, state) {
            return Some(res);
        }
        if let Some(res) = self.end.find(pos, state) {
            return Some(res);
        }
        if let Some(exp) = &self.step {
            if let Some(e) = exp.find(pos, state) {
                return Some(e);
            }
        }
        if let Some(e) = self.do_blk.find(pos, state) {
            return Some(e);
        }
        None
    }
}

impl<'a> NodeFinder<'a> for UnaryExp<'a> {
    fn find(&self, pos: Position, state: &mut FindState) -> Option<FindVal<'a>> {
        if let Some(v) = self.exp.find(pos, state) {
            return Some(v);
        }
        None
    }
}

impl<'a> NodeFinder<'a> for BinaryExp<'a> {
    fn find(&self, pos: Position, state: &mut FindState) -> Option<FindVal<'a>> {
        if let Some(v) = self.exp1.find(pos, state) {
            return Some(v);
        }
        if let Some(v) = self.exp2.find(pos, state) {
            return Some(v);
        }
        None
    }
}

impl<'a> NodeFinder<'a> for Assignment<'a> {
    fn find(&self, pos: Position, state: &mut FindState) -> Option<FindVal<'a>> {
        for (n, _id) in self.names.iter().zip(self.varids.as_ref().unwrap().iter()) {
            if n.range.contain(pos) {
                return Some(FindVal::new(n.value, state.ctx_index));
            }
        }
        if let Some(e) = self.val.find(pos, state) {
            return Some(e);
        }
        None
    }
}

impl<'a> NodeFinder<'a> for VarAssignment<'a> {
    fn find(&self, pos: Position, state: &mut FindState) -> Option<FindVal<'a>> {
        if self.name.range.contain(pos) {
            return Some(FindVal::new(self.name.value, state.ctx_index));
        }
        if let Some(exp) = self.val.find(pos, state) {
            return Some(exp);
        }
        None
    }
}

impl<'a> NodeFinder<'a> for FunctionDef<'a> {
    fn find(&self, _pos: Position, _state: &mut FindState) -> Option<FindVal<'a>> {
        None
    }
}

impl<'a> NodeFinder<'a> for Exp<'a> {
    fn find(&self, pos: Position, state: &mut FindState) -> Option<FindVal<'a>> {
        if !self.range().contain(pos) {
            return None;
        }
        match self {
            Exp::Na(_) | Exp::Bool(_) | Exp::Num(_) | Exp::Str(_) | Exp::Color(_) => None,
            Exp::VarName(name) => Some(FindVal::new(name.name.value, state.ctx_index)),
            Exp::Tuple(tuple) => tuple.find(pos, state),
            Exp::TypeCast(cast) => cast.find(pos, state),
            Exp::FuncCall(call) => call.find(pos, state),
            Exp::RefCall(call) => call.find(pos, state),
            Exp::PrefixExp(prefix) => prefix.find(pos, state),
            Exp::Condition(cond) => cond.find(pos, state),
            Exp::Ite(ite) => ite.find(pos, state),
            Exp::ForRange(for_range) => for_range.find(pos, state),
            Exp::Assignment(assign) => assign.find(pos, state),
            Exp::VarAssignment(assign) => assign.find(pos, state),
            Exp::UnaryExp(exp) => exp.find(pos, state),
            Exp::BinaryExp(exp) => exp.find(pos, state),
        }
    }
}

impl<'a> NodeFinder<'a> for Statement<'a> {
    fn find(&self, pos: Position, state: &mut FindState) -> Option<FindVal<'a>> {
        if !self.range().contain(pos) {
            return None;
        }
        match self {
            Statement::Assignment(assign) => assign.find(pos, state),
            Statement::VarAssignment(assign) => assign.find(pos, state),
            Statement::Ite(ite) => ite.find(pos, state),
            Statement::ForRange(for_range) => for_range.find(pos, state),
            Statement::FuncCall(call) => call.find(pos, state),
            Statement::FuncDef(def) => def.find(pos, state),
            Statement::Exp(exp) => exp.find(pos, state),
            _ => None,
        }
        // Ite(Box<IfThenElse<'a>>),
        // ForRange(Box<ForRange<'a>>),
        // FuncCall(Box<FunctionCall<'a>>),
        // FuncDef(Box<FunctionDef<'a>>),
        // Exp(Exp<'a>),
    }
}

impl<'a> NodeFinder<'a> for Block<'a> {
    fn find(&self, pos: Position, state: &mut FindState) -> Option<FindVal<'a>> {
        if !self.range.contain(pos) {
            return None;
        }
        for stmt in self.stmts.iter() {
            if let Some(exp) = stmt.find(pos, state) {
                return Some(exp);
            }
        }
        if let Some(exp) = &self.ret_stmt {
            if let Some(e) = exp.find(pos, state) {
                return Some(e);
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::input::StrRange;
    use crate::ast::name::VarName;

    fn gen_var_exp(name: &str, pos: Position) -> Exp {
        Exp::VarName(RVVarName::new_with_start(name, pos))
    }

    #[test]
    fn find_exp_test() {
        let exp = gen_var_exp("hello", Position::new(1, 10));
        assert_eq!(
            exp.find(Position::new(1, 11), &mut FindState::new()),
            Some(FindVal::new("hello", 0))
        );

        let tuple_exp = Exp::Tuple(Box::new(TupleNode::new(
            vec![
                gen_var_exp("he", Position::new(0, 1)),
                gen_var_exp("wo", Position::new(0, 3)),
            ],
            StrRange::from_start("he, wo", Position::new(0, 0)),
        )));
        assert_eq!(
            tuple_exp.find(Position::new(0, 4), &mut FindState::new_with_ctx_index(1)),
            Some(FindVal::new("wo", 01))
        );
        assert_eq!(
            tuple_exp.find(Position::new(0, 0), &mut FindState::new_with_ctx_index(1)),
            Some(FindVal::new_with_lib_name("[]"))
        );

        let type_cast_exp = Exp::TypeCast(Box::new(TypeCast::new(
            DataType::Int,
            gen_var_exp("hello", Position::new(0, 10)),
            StrRange::from_start("int(hello)", Position::new(0, 5)),
        )));
        assert_eq!(
            type_cast_exp.find(Position::new(0, 11), &mut FindState::new_with_ctx_index(1)),
            Some(FindVal::new("hello", 1))
        );

        let func_call_exp = Exp::FuncCall(Box::new(FunctionCall::new(
            gen_var_exp("hello", Position::new(0, 0)),
            vec![gen_var_exp("arg1", Position::new(0, 10))],
            vec![(
                VarName::new_with_start("key", Position::new(0, 20)),
                gen_var_exp("arg2", Position::new(0, 30)),
            )],
            0,
            StrRange::new(Position::new(0, 0), Position::new(0, 40)),
        )));
        assert_eq!(
            func_call_exp.find(Position::new(0, 1), &mut FindState::new_with_ctx_index(1)),
            Some(FindVal::new("hello", 1))
        );
        assert_eq!(
            func_call_exp.find(Position::new(0, 11), &mut FindState::new_with_ctx_index(1)),
            Some(FindVal::new("arg1", 1))
        );
        assert_eq!(
            func_call_exp.find(Position::new(0, 31), &mut FindState::new_with_ctx_index(1)),
            Some(FindVal::new("arg2", 1))
        );

        let ref_call_exp = Exp::RefCall(Box::new(RefCall::new(
            gen_var_exp("refcall", Position::new(0, 0)),
            gen_var_exp("num", Position::new(0, 10)),
            StrRange::new(Position::new(0, 0), Position::new(0, 20)),
        )));
        assert_eq!(
            ref_call_exp.find(Position::new(0, 0), &mut FindState::new()),
            Some(FindVal::new("refcall", 0))
        );
        assert_eq!(
            ref_call_exp.find(Position::new(0, 11), &mut FindState::new()),
            Some(FindVal::new("num", 0))
        );

        let prefix_exp = Exp::PrefixExp(Box::new(PrefixExp::new(
            gen_var_exp("prefix", Position::new(0, 0)),
            VarName::new_with_start("a", Position::new(0, 0)),
            StrRange::from_start("prefix.a", Position::new(0, 0)),
        )));
        assert_eq!(
            prefix_exp.find(Position::new(0, 1), &mut FindState::new()),
            Some(FindVal::new("prefix", 0))
        );
    }
}
