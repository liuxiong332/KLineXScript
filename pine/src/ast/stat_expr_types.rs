use super::color::ColorNode;
use super::input::{Input, Position, StrRange};
use super::name::VarName;
use super::num::Numeral;
use super::op::{BinaryOp, BinaryOpNode, UnaryOp, UnaryOpNode};
use super::string::StringNode;
use super::syntax_type::{FunctionType, SyntaxType};

#[derive(Clone, Debug, PartialEq, Copy)]
pub struct VarIndex {
    pub varid: i32,
    pub rel_ctx: i32,
}

impl VarIndex {
    pub fn new(varid: i32, rel_ctx: i32) -> VarIndex {
        VarIndex { varid, rel_ctx }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionCall<'a> {
    pub method: Exp<'a>,
    pub pos_args: Vec<Exp<'a>>,
    pub dict_args: Vec<(VarName<'a>, Exp<'a>)>,
    pub ctxid: i32,
    pub range: StrRange,
    pub func_type: Option<FunctionType<'a>>,
    // The index in function definition's specfic definitions
    pub spec_index: i32,
}

impl<'a> FunctionCall<'a> {
    #[inline]
    pub fn new(
        method: Exp<'a>,
        pos_args: Vec<Exp<'a>>,
        dict_args: Vec<(VarName<'a>, Exp<'a>)>,
        ctxid: i32,
        range: StrRange,
    ) -> Self {
        FunctionCall {
            method,
            pos_args,
            dict_args,
            ctxid,
            range,
            func_type: None,
            spec_index: 0,
        }
    }

    pub fn new_no_ctxid(
        method: Exp<'a>,
        pos_args: Vec<Exp<'a>>,
        dict_args: Vec<(VarName<'a>, Exp<'a>)>,
        range: StrRange,
    ) -> Self {
        FunctionCall {
            method,
            pos_args,
            dict_args,
            ctxid: 0,
            range,
            func_type: None,
            spec_index: 0,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct RefCall<'a> {
    pub name: Exp<'a>,
    pub arg: Exp<'a>,
    pub range: StrRange,
}

impl<'a> RefCall<'a> {
    #[inline]
    pub fn new(name: Exp<'a>, arg: Exp<'a>, range: StrRange) -> RefCall<'a> {
        RefCall { name, arg, range }
    }

    pub fn new_no_input(name: Exp<'a>, arg: Exp<'a>) -> RefCall<'a> {
        RefCall {
            name,
            arg,
            range: StrRange::new_empty(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Condition<'a> {
    pub cond: Exp<'a>,
    pub exp1: Exp<'a>,
    pub exp2: Exp<'a>,
    pub range: StrRange,
    pub result_type: SyntaxType<'a>,
}

impl<'a> Condition<'a> {
    #[inline]
    pub fn new(cond: Exp<'a>, exp1: Exp<'a>, exp2: Exp<'a>, range: StrRange) -> Condition<'a> {
        Condition {
            cond,
            exp1,
            exp2,
            range,
            result_type: SyntaxType::Any,
        }
    }

    #[inline]
    pub fn new_no_input(cond: Exp<'a>, exp1: Exp<'a>, exp2: Exp<'a>) -> Condition<'a> {
        Condition {
            cond,
            exp1,
            exp2,
            range: StrRange::new_empty(),
            result_type: SyntaxType::Any,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct NaNode {
    pub range: StrRange,
}

impl NaNode {
    pub fn new(range: StrRange) -> NaNode {
        NaNode { range }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct BoolNode {
    pub value: bool,
    pub range: StrRange,
}

impl BoolNode {
    pub fn new(value: bool, range: StrRange) -> BoolNode {
        BoolNode { value, range }
    }

    pub fn new_no_range(value: bool) -> BoolNode {
        BoolNode {
            value,
            range: StrRange::new_empty(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct UnaryExp<'a> {
    pub op: UnaryOp,
    pub exp: Exp<'a>,
    pub range: StrRange,
}

impl<'a> UnaryExp<'a> {
    pub fn new(op: UnaryOp, exp: Exp<'a>, range: StrRange) -> UnaryExp<'a> {
        UnaryExp { op, exp, range }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct BinaryExp<'a> {
    pub op: BinaryOp,
    pub exp1: Exp<'a>,
    pub exp2: Exp<'a>,
    pub range: StrRange,
    pub ref_type: SyntaxType<'a>,
    pub result_type: SyntaxType<'a>,
}

impl<'a> BinaryExp<'a> {
    pub fn new(op: BinaryOp, exp1: Exp<'a>, exp2: Exp<'a>, range: StrRange) -> BinaryExp<'a> {
        BinaryExp {
            op,
            exp1,
            exp2,
            range,
            ref_type: SyntaxType::Any,
            result_type: SyntaxType::Any,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TupleNode<'a> {
    pub exps: Vec<Exp<'a>>,
    pub range: StrRange,
}

impl<'a> TupleNode<'a> {
    pub fn new(exps: Vec<Exp<'a>>, range: StrRange) -> TupleNode<'a> {
        TupleNode { exps, range }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct LVTupleNode<'a> {
    pub names: Vec<VarName<'a>>,
    pub range: StrRange,
}

impl<'a> LVTupleNode<'a> {
    pub fn new(names: Vec<VarName<'a>>, range: StrRange) -> LVTupleNode<'a> {
        LVTupleNode { names, range }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct RVVarName<'a> {
    pub name: VarName<'a>,
    pub var_index: VarIndex,
}

impl<'a> RVVarName<'a> {
    pub fn new(name: VarName<'a>) -> RVVarName<'a> {
        RVVarName {
            name,
            var_index: VarIndex::new(0, 0),
        }
    }

    pub fn new_with_str(n: &'a str, range: StrRange) -> RVVarName<'a> {
        RVVarName {
            name: VarName::new(n, range),
            var_index: VarIndex::new(0, 0),
        }
    }

    pub fn new_with_start(value: &'a str, start: Position) -> RVVarName<'a> {
        RVVarName {
            name: VarName::new_with_start(value, start),
            var_index: VarIndex::new(0, 0),
        }
    }

    pub fn new_no_range(n: &'a str) -> RVVarName<'a> {
        RVVarName {
            name: VarName::new_no_input(n),
            var_index: VarIndex::new(0, 0),
        }
    }

    pub fn new_with_index(n: &'a str, index: VarIndex) -> RVVarName<'a> {
        RVVarName {
            name: VarName::new_no_input(n),
            var_index: index,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Exp<'a> {
    Na(NaNode),
    Bool(BoolNode),
    Num(Numeral),
    Str(StringNode),
    Color(ColorNode<'a>),
    VarName(RVVarName<'a>),
    // RetTuple(Box<Vec<VarName<'a>>>),
    Tuple(Box<TupleNode<'a>>),
    TypeCast(Box<TypeCast<'a>>),
    FuncCall(Box<FunctionCall<'a>>),
    RefCall(Box<RefCall<'a>>),
    PrefixExp(Box<PrefixExp<'a>>),
    Condition(Box<Condition<'a>>),
    Ite(Box<IfThenElse<'a>>),
    ForRange(Box<ForRange<'a>>),
    Assignment(Box<Assignment<'a>>),
    VarAssignment(Box<VarAssignment<'a>>),
    UnaryExp(Box<UnaryExp<'a>>),
    BinaryExp(Box<BinaryExp<'a>>),
}

impl<'a> Exp<'a> {
    pub fn range(&self) -> StrRange {
        match self {
            Exp::Na(na) => na.range,
            Exp::Bool(node) => node.range,
            Exp::Num(node) => node.range(),
            Exp::Str(node) => node.range,
            Exp::Color(node) => node.range,
            Exp::VarName(node) => node.name.range,
            Exp::Tuple(node) => node.range,
            Exp::TypeCast(node) => node.range,
            Exp::FuncCall(node) => node.range,
            Exp::RefCall(node) => node.range,
            Exp::PrefixExp(node) => node.range,
            Exp::Condition(node) => node.range,
            Exp::Ite(node) => node.range,
            Exp::ForRange(node) => node.range,
            Exp::Assignment(node) => node.range,
            Exp::VarAssignment(node) => node.range,
            Exp::UnaryExp(node) => node.range,
            Exp::BinaryExp(node) => node.range,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum OpOrExp2<'a> {
    Op(UnOrBinOp),
    Exp2(Exp2<'a>),
}

impl<'a> OpOrExp2<'a> {
    pub fn range(&self) -> StrRange {
        match self {
            OpOrExp2::Op(op) => op.range(),
            OpOrExp2::Exp2(exp) => exp.range(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum UnOrBinOp {
    UnaryOp(UnaryOpNode),
    BinaryOp(BinaryOpNode),
}

impl UnOrBinOp {
    pub fn range(&self) -> StrRange {
        match self {
            UnOrBinOp::UnaryOp(node) => node.range,
            UnOrBinOp::BinaryOp(node) => node.range,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FlatExp<'a> {
    pub exps: Vec<OpOrExp2<'a>>,
    pub range: StrRange,
}

pub struct UnOpExp2<'a> {
    pub ops: Vec<UnaryOpNode>,
    pub exp: Exp2<'a>,
    pub range: StrRange,
}

impl<'a> UnOpExp2<'a> {
    pub fn new(ops: Vec<UnaryOpNode>, exp: Exp2<'a>, range: StrRange) -> UnOpExp2<'a> {
        UnOpExp2 { ops, exp, range }
    }
}

impl<'a> FlatExp<'a> {
    pub fn new(exps: Vec<OpOrExp2<'a>>, range: StrRange) -> FlatExp<'a> {
        FlatExp { exps, range }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Exp2<'a> {
    Na(NaNode),
    Bool(BoolNode),
    Num(Numeral),
    Str(StringNode),
    Color(ColorNode<'a>),
    VarName(VarName<'a>),
    // RetTuple(Box<Vec<VarName<'a>>>),
    Tuple(Box<TupleNode<'a>>),
    TypeCast(Box<TypeCast<'a>>),
    FuncCall(Box<FunctionCall<'a>>),
    RefCall(Box<RefCall<'a>>),
    PrefixExp(Box<PrefixExp<'a>>),
    Exp(Exp<'a>),
}

impl<'a> Exp2<'a> {
    pub fn range(&self) -> StrRange {
        match self {
            Exp2::Na(na) => na.range,
            Exp2::Bool(node) => node.range,
            Exp2::Num(node) => node.range(),
            Exp2::Str(node) => node.range,
            Exp2::Color(node) => node.range,
            Exp2::VarName(node) => node.range,
            Exp2::Tuple(node) => node.range,
            Exp2::TypeCast(node) => node.range,
            Exp2::FuncCall(node) => node.range,
            Exp2::RefCall(node) => node.range,
            Exp2::PrefixExp(node) => node.range,
            Exp2::Exp(node) => node.range(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeCast<'a> {
    pub data_type: DataType,
    pub exp: Exp<'a>,
    pub range: StrRange,
}

impl<'a> TypeCast<'a> {
    pub fn new(data_type: DataType, exp: Exp<'a>, range: StrRange) -> TypeCast<'a> {
        TypeCast {
            data_type,
            exp,
            range,
        }
    }

    pub fn new_no_input(data_type: DataType, exp: Exp<'a>) -> TypeCast<'a> {
        TypeCast {
            data_type,
            exp,
            range: StrRange::new_empty(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct PrefixExp<'a> {
    pub var_chain: Vec<VarName<'a>>,
    pub range: StrRange,
    pub var_index: VarIndex,
}

impl<'a> PrefixExp<'a> {
    pub fn new(var_chain: Vec<VarName<'a>>, range: StrRange) -> PrefixExp<'a> {
        PrefixExp {
            var_chain,
            range,
            var_index: VarIndex::new(0, 0),
        }
    }

    pub fn new_no_input(var_chain: Vec<VarName<'a>>) -> PrefixExp<'a> {
        PrefixExp {
            var_chain,
            range: StrRange::new_empty(),
            var_index: VarIndex::new(0, 0),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum DataType {
    Float,
    Int,
    Bool,
    Color,
    String,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Assignment<'a> {
    pub names: Vec<VarName<'a>>,
    pub val: Exp<'a>,
    pub var_type: Option<DataType>,
    pub var: bool,
    pub range: StrRange,
    pub varids: Option<Vec<i32>>,
}

impl<'a> Assignment<'a> {
    pub fn new(
        names: Vec<VarName<'a>>,
        val: Exp<'a>,
        var: bool,
        var_type: Option<DataType>,
        range: StrRange,
    ) -> Assignment<'a> {
        Assignment {
            names,
            val,
            var,
            var_type,
            range,
            varids: None,
        }
    }

    pub fn new_no_input(
        names: Vec<VarName<'a>>,
        val: Exp<'a>,
        var: bool,
        var_type: Option<DataType>,
    ) -> Assignment<'a> {
        Assignment {
            names,
            val,
            var,
            var_type,
            range: StrRange::new_empty(),
            varids: None,
        }
    }

    pub fn new_with_varids(
        names: Vec<VarName<'a>>,
        val: Exp<'a>,
        var: bool,
        var_type: Option<DataType>,
        varids: Vec<i32>,
    ) -> Assignment<'a> {
        Assignment {
            names,
            val,
            var,
            var_type,
            range: StrRange::new_empty(),
            varids: Some(varids),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarAssignment<'a> {
    pub name: VarName<'a>,
    pub val: Exp<'a>,
    pub range: StrRange,
    pub var_index: VarIndex,
}

impl<'a> VarAssignment<'a> {
    pub fn new(name: VarName<'a>, val: Exp<'a>, range: StrRange) -> VarAssignment<'a> {
        VarAssignment {
            name,
            val,
            range,
            var_index: VarIndex::new(0, 0),
        }
    }

    pub fn new_no_input(name: VarName<'a>, val: Exp<'a>) -> VarAssignment<'a> {
        VarAssignment {
            name,
            val,
            range: StrRange::new_empty(),
            var_index: VarIndex::new(0, 0),
        }
    }

    pub fn new_with_index(
        name: VarName<'a>,
        val: Exp<'a>,
        var_index: VarIndex,
    ) -> VarAssignment<'a> {
        VarAssignment {
            name,
            val,
            range: StrRange::new_empty(),
            var_index,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Block<'a> {
    pub stmts: Vec<Statement<'a>>,
    pub ret_stmt: Option<Exp<'a>>,
    pub range: StrRange,
    pub var_count: i32,
    pub subctx_count: i32,
}

impl<'a> Block<'a> {
    pub fn new(stmts: Vec<Statement<'a>>, ret_stmt: Option<Exp<'a>>, range: StrRange) -> Block<'a> {
        Block {
            stmts,
            ret_stmt,
            range,
            var_count: 0,
            subctx_count: 0,
        }
    }

    pub fn new_no_input(stmts: Vec<Statement<'a>>, ret_stmt: Option<Exp<'a>>) -> Block<'a> {
        Block {
            stmts,
            ret_stmt,
            range: StrRange::new_empty(),
            var_count: 0,
            subctx_count: 0,
        }
    }

    pub fn new_with_count(
        stmts: Vec<Statement<'a>>,
        ret_stmt: Option<Exp<'a>>,
        var_count: i32,
        subctx_count: i32,
    ) -> Block<'a> {
        Block {
            stmts,
            ret_stmt,
            range: StrRange::new_empty(),
            var_count,
            subctx_count,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct IfThenElse<'a> {
    pub cond: Exp<'a>,
    pub then_blk: Block<'a>,
    pub then_ctxid: i32,
    // pub elseifs: Vec<(Exp<'a>, Block<'a>)>,
    pub else_blk: Option<Block<'a>>,
    pub else_ctxid: i32,
    pub range: StrRange,
    pub result_type: SyntaxType<'a>,
}

impl<'a> IfThenElse<'a> {
    pub fn new(
        cond: Exp<'a>,
        then_blk: Block<'a>,
        else_blk: Option<Block<'a>>,
        then_ctxid: i32,
        else_ctxid: i32,
        range: StrRange,
    ) -> Self {
        IfThenElse {
            cond,
            then_blk,
            then_ctxid,
            else_blk,
            else_ctxid,
            range,
            result_type: SyntaxType::Any,
        }
    }

    pub fn new_no_ctxid(
        cond: Exp<'a>,
        then_blk: Block<'a>,
        else_blk: Option<Block<'a>>,
        range: StrRange,
    ) -> Self {
        IfThenElse {
            cond,
            then_blk,
            else_blk,
            then_ctxid: 0,
            else_ctxid: 1,
            range,
            result_type: SyntaxType::Any,
        }
    }

    pub fn get_then_var_count(&self) -> i32 {
        self.then_blk.var_count
    }

    pub fn get_then_subctx_count(&self) -> i32 {
        self.then_blk.subctx_count
    }

    pub fn get_else_var_count(&self) -> i32 {
        self.else_blk.as_ref().unwrap().var_count
    }

    pub fn get_else_subctx_count(&self) -> i32 {
        self.else_blk.as_ref().unwrap().subctx_count
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ForRange<'a> {
    pub var: VarName<'a>,
    pub start: Exp<'a>,
    pub end: Exp<'a>,
    pub step: Option<Exp<'a>>,
    pub do_blk: Block<'a>,
    pub ctxid: i32,
    pub varid: i32,
    pub range: StrRange,
    pub result_type: SyntaxType<'a>,
}

impl<'a> ForRange<'a> {
    pub fn new(
        var: VarName<'a>,
        start: Exp<'a>,
        end: Exp<'a>,
        step: Option<Exp<'a>>,
        do_blk: Block<'a>,
        ctxid: i32,
        range: StrRange,
    ) -> Self {
        ForRange {
            var,
            start,
            end,
            step,
            do_blk,
            ctxid,
            varid: 0,
            range,
            result_type: SyntaxType::Any,
        }
    }

    pub fn new_no_ctxid(
        var: VarName<'a>,
        start: Exp<'a>,
        end: Exp<'a>,
        step: Option<Exp<'a>>,
        do_blk: Block<'a>,
        range: StrRange,
    ) -> Self {
        ForRange {
            var,
            start,
            end,
            step,
            do_blk,
            ctxid: 0,
            varid: 0,
            range,
            result_type: SyntaxType::Any,
        }
    }

    pub fn new_with_ctxid(
        var: VarName<'a>,
        start: Exp<'a>,
        end: Exp<'a>,
        step: Option<Exp<'a>>,
        do_blk: Block<'a>,
        range: StrRange,
        ctxid: i32,
        result_type: SyntaxType<'a>,
    ) -> Self {
        ForRange {
            var,
            start,
            end,
            step,
            do_blk,
            ctxid,
            varid: 0,
            range,
            result_type,
        }
    }

    pub fn get_var_count(&self) -> i32 {
        self.do_blk.var_count
    }

    pub fn get_subctx_count(&self) -> i32 {
        self.do_blk.subctx_count
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionDef<'a> {
    pub name: VarName<'a>,
    pub params: Vec<VarName<'a>>,
    pub body: Block<'a>,
    pub range: StrRange,
    // The index in global name context
    pub name_varid: i32,
    // The argument indexs for this function definition
    pub varids: Option<Vec<i32>>,
    // The function definition with specific types.
    pub spec_defs: Option<Box<Vec<FunctionDef<'a>>>>,
}

impl<'a> FunctionDef<'a> {
    pub fn new(
        name: VarName<'a>,
        params: Vec<VarName<'a>>,
        body: Block<'a>,
        range: StrRange,
    ) -> FunctionDef<'a> {
        FunctionDef {
            name,
            params,
            body,
            range,
            name_varid: 0,
            varids: None,
            spec_defs: Some(Box::new(vec![])),
        }
    }

    pub fn gen_spec_def(&self) -> FunctionDef<'a> {
        FunctionDef {
            name: self.name.clone(),
            params: self.params.clone(),
            body: self.body.clone(),
            range: self.range.clone(),
            name_varid: 0,
            varids: None,
            spec_defs: None,
        }
    }

    pub fn get_var_count(&self) -> i32 {
        self.body.var_count
    }

    pub fn get_subctx_count(&self) -> i32 {
        self.body.subctx_count
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Statement<'a> {
    Break(StrRange),
    Continue(StrRange),
    None(StrRange),
    Assignment(Box<Assignment<'a>>),
    VarAssignment(Box<VarAssignment<'a>>),
    Ite(Box<IfThenElse<'a>>),
    ForRange(Box<ForRange<'a>>),
    FuncCall(Box<FunctionCall<'a>>),
    FuncDef(Box<FunctionDef<'a>>),
    Exp(Exp<'a>),
}

impl<'a> Statement<'a> {
    pub fn range(&self) -> StrRange {
        match self {
            &Statement::Break(range) => range,
            &Statement::Continue(range) => range,
            &Statement::None(range) => range,
            Statement::Assignment(assign) => assign.range,
            Statement::VarAssignment(assign) => assign.range,
            Statement::Ite(ite) => ite.range,
            Statement::ForRange(for_range) => for_range.range,
            Statement::FuncCall(func_call) => func_call.range,
            Statement::FuncDef(func_def) => func_def.range,
            Statement::Exp(exp) => exp.range(),
        }
    }
}
