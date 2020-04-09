use super::data_src::Callback;
use super::output::InputVal;
use super::output::{
    IOInfo, InputInfo, InputSrc, OutputData, OutputInfo, ScriptPurpose, SymbolInfo,
};
use crate::ast::input::{Position, StrRange};
use crate::ast::stat_expr_types::VarIndex;
use crate::runtime::AnySeries;
use crate::types::{
    Bool, Callable, Color, DataType, Float, Int, PineFrom, PineRef, PineStaticType, PineType,
    RefData, Runnable, RuntimeErr, SecondType, Series, NA,
};
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt::Debug;
use std::mem;
use std::rc::Rc;

pub trait VarOperate<'a> {
    fn create_var(&mut self, index: i32, val: PineRef<'a>) -> Option<PineRef<'a>>;

    fn update_var(&mut self, index: VarIndex, val: PineRef<'a>);

    fn move_var(&mut self, index: VarIndex) -> Option<PineRef<'a>>;

    fn get_var(&self, index: VarIndex) -> &Option<PineRef<'a>>;

    fn var_len(&self) -> i32;
}

// lifetime 'a is the lifetime of Exp, 'c is the lifetime of Ctx Self's lifetime
pub trait Ctx<'a>: VarOperate<'a> {
    fn contains_var(&self, index: VarIndex) -> bool;

    fn contains_var_scope(&self, index: i32) -> bool;

    fn set_varname_index(&mut self, name: &'a str, index: i32);

    fn get_varname_index(&self, name: &str) -> Option<&i32>;

    fn get_top_varname_index(&self, name: &str) -> Option<VarIndex>;

    fn create_runnable(&mut self, call: Rc<RefCell<dyn Runnable<'a> + 'a>>);

    fn move_fun_instance(&mut self, index: i32) -> Option<RefData<Callable<'a>>>;

    fn create_fun_instance(&mut self, index: i32, val: RefData<Callable<'a>>);

    // fn create_declare(&mut self, name: &'a str);

    // fn contains_declare(&self, name: &'a str) -> bool;

    // fn contains_declare_scope(&self, name: &'a str) -> bool;

    // fn clear_declare(&mut self);

    // fn any_declare(&self) -> bool;

    fn has_parent(&self) -> bool;

    fn get_top_ctx(&mut self) -> &mut dyn Ctx<'a>;

    fn get_rel_top_ctx(&mut self) -> (i32, &mut dyn Ctx<'a>);

    fn set_is_run(&mut self, is_run: bool);

    fn get_is_run(&self) -> bool;

    fn clear_is_run(&mut self);

    fn get_type(&self) -> ContextType;

    fn get_callback(&self) -> Option<&'a dyn Callback>;
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ContextType {
    Normal,
    IfElseBlock,
    ForRangeBlock,
    FuncDefBlock,
}

// 'a is the lifetime of Exp, 'b is the parent context's lifetime, 'c is the context self's lifetime
pub struct Context<'a, 'b, 'c> {
    // input: &'a str,
    parent: Option<&'b mut (dyn 'b + Ctx<'a>)>,
    context_type: ContextType,

    // Child contexts that with parent self lifetime 'c
    sub_contexts: Vec<Option<Box<dyn 'c + Ctx<'a>>>>,

    // variable map that defined by user and library.
    vars: Vec<Option<PineRef<'a>>>,

    varname_indexs: HashMap<&'a str, i32>,

    // function instances
    fun_instances: Vec<Option<RefData<Callable<'a>>>>,

    runnables: Vec<Rc<RefCell<dyn Runnable<'a> + 'a>>>,
    // declare_vars: HashSet<&'a str>,

    // The input value from user
    inputs: Vec<Option<InputVal>>,
    // The input index will increment after input function is invoked
    input_index: i32,

    // Input data for some external ticker.
    input_data: HashMap<String, AnySeries>,
    // The output data that will be exported.
    output_data: Vec<Option<OutputData>>,

    io_info: IOInfo,
    // Check if input_info is ready
    is_input_info_ready: bool,
    // Check if input_info is ready
    is_output_info_ready: bool,

    // The symbol information
    syminfo: Option<Rc<SymbolInfo>>,

    // The range of data
    data_range: (Option<i32>, Option<i32>),

    // The output values
    callback: Option<&'a dyn Callback>,
    first_commit: bool,

    is_run: bool,
}

pub fn downcast_ctx<'a, 'b, 'c>(item: &'c mut (dyn Ctx<'a> + 'c)) -> &'c mut Context<'a, 'b, 'c> {
    unsafe {
        let raw: *mut dyn Ctx<'a> = item;
        let t = raw as *mut Context<'a, 'b, 'c>;
        t.as_mut().unwrap()
    }
}

pub fn downcast_ctx_const<'a, 'b, 'c>(item: &'c (dyn Ctx<'a> + 'c)) -> &'c Context<'a, 'b, 'c> {
    unsafe {
        let raw: *const dyn Ctx<'a> = item;
        let t = raw as *const Context<'a, 'b, 'c>;
        t.as_ref().unwrap()
    }
}

fn commit_series<'a, D>(val: PineRef<'a>) -> PineRef<'a>
where
    D: Default + PartialEq + PineStaticType + PineType<'a> + PineFrom<'a, D> + Clone + Debug + 'a,
{
    let mut series: RefData<Series<D>> = Series::implicity_from(val).unwrap();
    series.commit();
    series.into_pf()
}

pub fn commit_series_for_operator<'a>(operator: &mut dyn VarOperate<'a>) {
    let len: i32 = operator.var_len();
    // The committed set used to make sure only one instance of series commmit.
    let mut commited: HashSet<*const (dyn PineType<'a> + 'a)> = HashSet::new();
    for k in 0..len {
        let index = VarIndex::new(k, 0);
        if let Some(val) = operator.move_var(index) {
            if commited.contains(&val.as_ptr()) {
                continue;
            }
            commited.insert(val.as_ptr());
            let ret_val = match val.get_type() {
                (DataType::Float, SecondType::Series) => commit_series::<Float>(val),
                (DataType::Int, SecondType::Series) => commit_series::<Int>(val),
                (DataType::Color, SecondType::Series) => commit_series::<Color>(val),
                (DataType::Bool, SecondType::Series) => commit_series::<Bool>(val),
                (DataType::String, SecondType::Series) => commit_series::<String>(val),
                _ => val,
            };
            operator.update_var(index, ret_val);
        }
    }
}

pub fn rollback_series_for_operator<'a>(operator: &mut dyn VarOperate<'a>) {
    let len: i32 = operator.var_len();
    // The committed set used to make sure only one instance of series commmit.
    let mut commited: HashSet<*const (dyn PineType<'a> + 'a)> = HashSet::new();
    for k in 0..len {
        let index = VarIndex::new(k, 0);
        if let Some(val) = operator.move_var(index) {
            if commited.contains(&val.as_ptr()) {
                continue;
            }
            commited.insert(val.as_ptr());
            let ret_val = match val.get_type() {
                (DataType::Float, SecondType::Series) => roll_back_series::<Float>(val),
                (DataType::Int, SecondType::Series) => roll_back_series::<Int>(val),
                (DataType::Color, SecondType::Series) => roll_back_series::<Color>(val),
                (DataType::Bool, SecondType::Series) => roll_back_series::<Bool>(val),
                (DataType::String, SecondType::Series) => roll_back_series::<String>(val),
                _ => val,
            };
            operator.update_var(index, ret_val);
        }
    }
}

fn roll_back_series<'a, D>(val: PineRef<'a>) -> PineRef<'a>
where
    D: Default + PartialEq + PineStaticType + PineType<'a> + PineFrom<'a, D> + Clone + Debug + 'a,
{
    let mut series: RefData<Series<D>> = Series::implicity_from(val).unwrap();
    println!("Now series {:?}", series);
    series.roll_back();
    series.into_pf()
}

impl<'a, 'b, 'c> Context<'a, 'b, 'c> {
    pub fn new(parent: Option<&'b mut (dyn 'b + Ctx<'a>)>, t: ContextType) -> Context<'a, 'b, 'c> {
        Context {
            parent,
            context_type: t,
            sub_contexts: Vec::new(),
            vars: Vec::new(),
            varname_indexs: HashMap::new(),
            fun_instances: Vec::new(),
            runnables: vec![],
            // declare_vars: HashSet::new(),
            callback: None,
            inputs: vec![],
            input_index: -1,
            input_data: HashMap::new(),
            output_data: vec![],
            io_info: IOInfo::new(),
            is_input_info_ready: false,
            is_output_info_ready: false,
            syminfo: None,
            data_range: (Some(0), Some(0)),
            first_commit: false,
            is_run: false,
        }
    }

    pub fn new_with_callback(callback: &'a dyn Callback) -> Context<'a, 'b, 'c> {
        Context {
            parent: None,
            context_type: ContextType::Normal,
            sub_contexts: Vec::new(),
            vars: Vec::new(),
            varname_indexs: HashMap::new(),
            fun_instances: Vec::new(),
            runnables: vec![],
            // declare_vars: HashSet::new(),
            callback: Some(callback),
            inputs: vec![],
            input_index: -1,
            input_data: HashMap::new(),
            output_data: vec![],
            io_info: IOInfo::new(),
            is_input_info_ready: false,
            is_output_info_ready: false,
            syminfo: None,
            data_range: (Some(0), Some(0)),
            first_commit: false,
            is_run: false,
        }
    }

    pub fn init_vars(&mut self, vars: Vec<Option<PineRef<'a>>>) {
        self.vars = vars;
    }

    pub fn init_sub_contexts(&mut self, sub_contexts: Vec<Option<Box<dyn 'c + Ctx<'a>>>>) {
        self.sub_contexts = sub_contexts;
    }

    pub fn init_fun_instances(&mut self, fun_instances: Vec<Option<RefData<Callable<'a>>>>) {
        self.fun_instances = fun_instances;
    }

    pub fn init(&mut self, var_count: i32, subctx_count: i32, libfun_count: i32) {
        let mut vars: Vec<Option<PineRef<'a>>> = Vec::with_capacity(var_count as usize);
        vars.resize(var_count as usize, None);
        self.init_vars(vars);

        let ctx_count = subctx_count as usize;
        let mut ctxs: Vec<Option<Box<dyn 'c + Ctx<'a>>>> = Vec::with_capacity(ctx_count);
        ctxs.resize_with(ctx_count, || None);
        self.init_sub_contexts(ctxs);

        let fun_count = libfun_count as usize;
        let mut funs: Vec<Option<RefData<Callable<'a>>>> = Vec::with_capacity(fun_count);
        funs.resize_with(fun_count, || None);
        self.init_fun_instances(funs);
    }

    pub fn reset(&mut self, var_keep: i32) {
        let var_count = self.vars.len();
        self.vars.resize(var_keep as usize, None);
        self.vars.resize(var_count, None);

        let ctx_count = self.sub_contexts.len();
        self.sub_contexts.clear();
        self.sub_contexts.resize_with(ctx_count, || None);

        // function instance need not reset

        // let fun_count = self.fun_instances.len();
        // self.fun_instances.clear();
        // self.fun_instances.resize_with(fun_count, || None);
    }

    pub fn change_inputs(&mut self, inputs: Vec<Option<InputVal>>) {
        if let Some(p) = &mut self.parent {
            downcast_ctx(*p).change_inputs(inputs)
        } else {
            debug_assert!(
                self.io_info.get_inputs().is_empty()
                    || inputs.len() == self.io_info.get_inputs().len()
            );
            self.inputs = inputs;
        }
    }

    pub fn get_inputs(&self) -> &Vec<Option<InputVal>> {
        if let Some(p) = &self.parent {
            downcast_ctx_const(*p).get_inputs()
        } else {
            &self.inputs
        }
    }

    pub fn copy_next_input(&mut self) -> Option<InputVal> {
        if let Some(p) = &mut self.parent {
            downcast_ctx(*p).copy_next_input()
        } else {
            self.input_index += 1;
            if self.input_index as usize >= self.inputs.len() {
                None
            } else {
                self.inputs[self.input_index as usize].clone()
            }
        }
    }

    pub fn get_next_input_index(&mut self) -> i32 {
        if let Some(p) = &mut self.parent {
            downcast_ctx(*p).get_next_input_index()
        } else {
            self.input_index += 1;
            self.input_index
        }
    }

    pub fn reset_input_index(&mut self) {
        if let Some(p) = &mut self.parent {
            downcast_ctx(*p).reset_input_index()
        } else {
            self.input_index = -1;
        }
    }

    // io_info related methods
    pub fn set_script_type(&mut self, script_type: ScriptPurpose) {
        if let Some(p) = &mut self.parent {
            downcast_ctx(*p).set_script_type(script_type)
        } else {
            self.io_info.set_script_type(script_type);
        }
    }

    pub fn push_input_info(&mut self, input: InputInfo) {
        if let Some(p) = &mut self.parent {
            downcast_ctx(*p).push_input_info(input)
        } else {
            self.io_info.push_input(input);
        }
    }

    pub fn push_output_info(&mut self, output: OutputInfo) {
        if let Some(p) = &mut self.parent {
            downcast_ctx(*p).push_output_info(output)
        } else {
            self.io_info.push_output(output);
        }
    }

    pub fn push_output_info_retindex(&mut self, output: OutputInfo) -> i32 {
        if let Some(p) = &mut self.parent {
            downcast_ctx(*p).push_output_info_retindex(output)
        } else {
            self.io_info.push_output_retindex(output)
        }
    }

    pub fn add_input_src(&mut self, src: InputSrc) {
        if let Some(p) = &mut self.parent {
            downcast_ctx(*p).add_input_src(src)
        } else {
            self.io_info.add_input_src(src);
        }
    }

    pub fn get_io_info(&self) -> &IOInfo {
        if let Some(p) = &self.parent {
            downcast_ctx_const(*p).get_io_info()
        } else {
            &self.io_info
        }
    }

    pub fn check_is_input_info_ready(&self) -> bool {
        if let Some(p) = &self.parent {
            downcast_ctx_const(*p).check_is_input_info_ready()
        } else {
            self.is_input_info_ready
        }
    }

    pub fn let_input_info_ready(&mut self) {
        debug_assert!(!self.has_parent());
        self.is_input_info_ready = true;
    }

    pub fn check_is_output_info_ready(&self) -> bool {
        if let Some(p) = &self.parent {
            downcast_ctx_const(*p).check_is_output_info_ready()
        } else {
            self.is_output_info_ready
        }
    }

    pub fn let_output_info_ready(&mut self) {
        debug_assert!(!self.has_parent());
        self.is_output_info_ready = true;
    }

    pub fn insert_input_data(&mut self, name: String, data: AnySeries) {
        if let Some(p) = &mut self.parent {
            downcast_ctx(*p).insert_input_data(name, data)
        } else {
            self.input_data.insert(name, data);
        }
    }

    pub fn get_input_data(&self, name: &str) -> Option<&AnySeries> {
        if let Some(p) = &self.parent {
            downcast_ctx_const(*p).get_input_data(name)
        } else {
            self.input_data.get(name)
        }
    }

    pub fn push_output_data(&mut self, data: Option<OutputData>) {
        if let Some(p) = &mut self.parent {
            downcast_ctx(*p).push_output_data(data)
        } else {
            self.output_data.push(data);
        }
    }

    pub fn move_output_data(&mut self) -> Vec<Option<OutputData>> {
        debug_assert_eq!(self.output_data.len(), self.io_info.get_outputs().len());
        mem::replace(&mut self.output_data, vec![])
    }

    pub fn set_syminfo(&mut self, syminfo: Rc<SymbolInfo>) {
        if let Some(p) = &mut self.parent {
            downcast_ctx(*p).set_syminfo(syminfo)
        } else {
            self.syminfo = Some(syminfo);
        }
    }

    pub fn get_syminfo(&self) -> &Option<Rc<SymbolInfo>> {
        debug_assert!(!self.has_parent());
        &self.syminfo
    }

    pub fn get_data_range(&self) -> (Option<i32>, Option<i32>) {
        debug_assert!(!self.has_parent());
        self.data_range.clone()
    }

    pub fn update_data_range(&mut self, range: (Option<i32>, Option<i32>)) {
        debug_assert!(!self.has_parent());
        self.data_range = range;
    }

    pub fn create_sub_context(
        &'c mut self,
        index: i32,
        t: ContextType,
        var_count: i32,
        subctx_count: i32,
        libfun_count: i32,
    ) -> &mut Box<dyn Ctx<'a> + 'c>
    where
        'a: 'c,
        'b: 'c,
    {
        let mut subctx = Box::new(Context::new(None, t));
        subctx.init(var_count, subctx_count, libfun_count);
        unsafe {
            // Force the &Context to &mut Context to prevent the rust's borrow checker
            // When the sub context borrow the parent context, the parent context should not
            // use by the rust's borrow rules.

            // subctx.parent = Some(mem::transmute::<usize, &mut Context<'a, 'b, 'c>>(
            //     mem::transmute::<&Context<'a, 'b, 'c>, usize>(self),
            // ));
            // mem::transmute::<usize, &mut Context<'a, 'b, 'c>>(mem::transmute::<
            //     &Context<'a, 'b, 'c>,
            //     usize,
            // >(self))
            // .sub_contexts
            // .insert(name.clone(), subctx);
            let ptr: *mut Context<'a, 'b, 'c> = self;
            subctx.parent = Some(ptr.as_mut().unwrap());
            let context = ptr.as_mut().unwrap();
            context.sub_contexts[index as usize] = Some(subctx);
            // &mut context.sub_contexts[index as usize].unwrap()
        }
        self.get_sub_context(index).unwrap()
    }

    pub fn map_var<F>(&mut self, index: VarIndex, f: F)
    where
        F: Fn(Option<PineRef<'a>>) -> Option<PineRef<'a>>,
    {
        let context = downcast_ctx(self.get_subctx_mut(index));
        let val = mem::replace(&mut context.vars[index.varid as usize], None);
        if let Some(ret_val) = f(val) {
            context.vars[index.varid as usize] = Some(ret_val);
        }
    }

    pub fn commit(&mut self) {
        commit_series_for_operator(self);
        // Commit the Series for all of the sub context.
        for ctx in self.sub_contexts.iter_mut() {
            // If this context does not declare variables, so this context is not run,
            // we need not commit the series.
            if let Some(ctx) = ctx {
                if ctx.get_is_run() {
                    downcast_ctx(&mut **ctx).commit();
                }
            }
        }

        if !self.first_commit {
            self.first_commit = true;
        }
    }

    pub fn roll_back(&mut self) -> Result<(), PineRuntimeError> {
        rollback_series_for_operator(self);

        let mut callables = mem::replace(&mut self.runnables, vec![]);
        for callable in callables.iter_mut() {
            if let Err(code) = callable.borrow_mut().back(self) {
                return Err(PineRuntimeError::new_no_range(code));
            }
        }
        mem::replace(&mut self.runnables, callables);
        Ok(())
    }

    pub fn run_callbacks(&mut self) -> Result<(), RuntimeErr> {
        let mut callables = mem::replace(&mut self.runnables, vec![]);
        for callable in callables.iter_mut() {
            callable.borrow_mut().run(self)?;
        }
        mem::replace(&mut self.runnables, callables);
        Ok(())
    }

    pub fn contains_sub_context(&self, index: i32) -> bool {
        self.sub_contexts[index as usize].is_some()
    }

    pub fn get_sub_context(&mut self, index: i32) -> Option<&mut Box<dyn Ctx<'a> + 'c>> {
        match &mut self.sub_contexts[index as usize] {
            Some(v) => Some(v),
            None => None,
        }
    }

    pub fn set_sub_context(&mut self, index: i32, sub_context: Box<dyn Ctx<'a> + 'c>) {
        self.sub_contexts[index as usize] = Some(sub_context);
    }

    pub fn update_sub_context(&mut self, index: i32, subctx: Box<dyn Ctx<'a> + 'c>) {
        self.sub_contexts[index as usize] = Some(subctx);
    }

    pub fn get_subctx_mut(&mut self, index: VarIndex) -> &mut dyn Ctx<'a> {
        let mut dest_ctx: &mut dyn Ctx<'a> = self;
        let mut rel_ctx = index.rel_ctx;
        debug_assert!(rel_ctx >= 0);
        while rel_ctx > 0 {
            dest_ctx = *downcast_ctx(dest_ctx).parent.as_mut().unwrap();
            rel_ctx -= 1;
        }
        dest_ctx
    }

    pub fn get_subctx(&self, index: VarIndex) -> &dyn Ctx<'a> {
        let mut dest_ctx: &dyn Ctx<'a> = self;
        let mut rel_ctx = index.rel_ctx;
        debug_assert!(rel_ctx >= 0);
        while rel_ctx > 0 {
            dest_ctx = *downcast_ctx_const(dest_ctx).parent.as_ref().unwrap();
            rel_ctx -= 1;
        }
        dest_ctx
    }
}

impl<'a, 'b, 'c> VarOperate<'a> for Context<'a, 'b, 'c> {
    fn create_var(&mut self, index: i32, val: PineRef<'a>) -> Option<PineRef<'a>> {
        mem::replace(&mut self.vars[index as usize], Some(val))
    }

    fn update_var(&mut self, index: VarIndex, val: PineRef<'a>) {
        let dest_ctx = downcast_ctx(self.get_subctx_mut(index));
        downcast_ctx(dest_ctx).vars[index.varid as usize] = Some(val);
    }

    // Move the value for the specific name from this context or the parent context.
    fn move_var(&mut self, index: VarIndex) -> Option<PineRef<'a>> {
        // Insert the temporary NA into the name and move the original value out.
        let dest_ctx = downcast_ctx(self.get_subctx_mut(index));
        mem::replace(&mut dest_ctx.vars[index.varid as usize], None)
    }

    fn get_var(&self, index: VarIndex) -> &Option<PineRef<'a>> {
        let dest_ctx = downcast_ctx_const(self.get_subctx(index));
        &dest_ctx.vars[index.varid as usize]
    }

    fn var_len(&self) -> i32 {
        self.vars.len() as i32
    }
}

impl<'a, 'b, 'c> Ctx<'a> for Context<'a, 'b, 'c> {
    fn contains_var_scope(&self, index: i32) -> bool {
        self.vars[index as usize].is_some()
    }

    fn contains_var(&self, index: VarIndex) -> bool {
        let dest_ctx = self.get_subctx(index);
        downcast_ctx_const(dest_ctx).vars[index.varid as usize].is_some()
    }

    fn set_varname_index(&mut self, name: &'a str, index: i32) {
        self.varname_indexs.insert(name, index);
    }

    fn get_varname_index(&self, name: &str) -> Option<&i32> {
        self.varname_indexs.get(name)
    }

    fn get_top_varname_index(&self, name: &str) -> Option<VarIndex> {
        let mut dest_ctx: &dyn Ctx<'a> = self;
        let mut rel_count = 0;
        while dest_ctx.has_parent() {
            rel_count += 1;
            dest_ctx = *downcast_ctx_const(dest_ctx).parent.as_ref().unwrap();
        }
        match dest_ctx.get_varname_index(name) {
            None => None,
            Some(v) => Some(VarIndex::new(*v, rel_count)),
        }
    }

    fn create_runnable(&mut self, call: Rc<RefCell<dyn Runnable<'a> + 'a>>) {
        if let Some(ref mut v) = self.parent {
            v.create_runnable(call);
        } else if !self.first_commit {
            self.runnables.push(call);
        }
    }

    fn move_fun_instance(&mut self, index: i32) -> Option<RefData<Callable<'a>>> {
        mem::replace(&mut self.fun_instances[index as usize], None)
    }

    fn create_fun_instance(&mut self, index: i32, val: RefData<Callable<'a>>) {
        self.fun_instances[index as usize] = Some(val);
    }

    fn get_top_ctx(&mut self) -> &mut dyn Ctx<'a> {
        let mut dest_ctx: &mut dyn Ctx<'a> = self;
        while dest_ctx.has_parent() {
            dest_ctx = *downcast_ctx(dest_ctx).parent.as_mut().unwrap();
        }
        dest_ctx
    }

    fn get_rel_top_ctx(&mut self) -> (i32, &mut dyn Ctx<'a>) {
        let mut dest_ctx: &mut dyn Ctx<'a> = self;
        let mut rel_count = 0;
        while dest_ctx.has_parent() {
            rel_count += 1;
            dest_ctx = *downcast_ctx(dest_ctx).parent.as_mut().unwrap();
        }
        (rel_count, dest_ctx)
    }

    fn has_parent(&self) -> bool {
        self.parent.is_some()
    }

    fn set_is_run(&mut self, is_run: bool) {
        self.is_run = is_run;
    }

    fn get_is_run(&self) -> bool {
        self.is_run
    }

    fn clear_is_run(&mut self) {
        self.is_run = false;
        for subctx in self.sub_contexts.iter_mut() {
            if let Some(subctx) = subctx {
                subctx.clear_is_run();
            }
        }
    }

    fn get_type(&self) -> ContextType {
        self.context_type
    }

    fn get_callback(&self) -> Option<&'a dyn Callback> {
        self.callback
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct PineRuntimeError {
    pub code: RuntimeErr,
    pub range: StrRange,
}

impl PineRuntimeError {
    pub fn new(code: RuntimeErr, range: StrRange) -> PineRuntimeError {
        PineRuntimeError { code, range }
    }

    pub fn new_no_range(code: RuntimeErr) -> PineRuntimeError {
        PineRuntimeError {
            code,
            range: StrRange::from_start("", Position::new(0, 0)),
        }
    }
}

pub trait Runner<'a> {
    fn run(&'a self, context: &mut dyn Ctx<'a>) -> Result<PineRef<'a>, PineRuntimeError>;
}

// Evaluate  the expression with right-value.
pub trait RVRunner<'a> {
    fn rv_run(&'a self, context: &mut dyn Ctx<'a>) -> Result<PineRef<'a>, PineRuntimeError>;
}

// evaluate the expression for the function call
pub trait RunnerForFunc<'a> {
    fn run_for_func(&'a self, context: &mut dyn Ctx<'a>) -> Result<PineRef<'a>, PineRuntimeError>;
}

// evaluate the expression for the object.
pub trait RunnerForObj<'a> {
    fn run_for_obj(&'a self, context: &mut dyn Ctx<'a>) -> Result<PineRef<'a>, PineRuntimeError>;
}

pub trait StmtRunner<'a> {
    fn st_run(&'a self, context: &mut dyn Ctx<'a>) -> Result<(), PineRuntimeError>;
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{Int, PineFrom};

    #[test]
    fn context_test() {
        let mut context1 = Context::new(None, ContextType::Normal);
        context1.init_vars(vec![Some(PineRef::new(Some(1)))]);
        // context1.create_declare("hello");
        // assert!(context1.contains_declare("hello"));

        // context1.clear_declare();
        // assert!(!context1.contains_declare("hello"));

        context1.create_var(0, PineRef::new_box(Some(1)));
        assert_eq!(
            Int::implicity_from(context1.move_var(VarIndex::new(0, 0)).unwrap()),
            Ok(RefData::new_box(Some(1)))
        );

        context1.update_var(VarIndex::new(0, 0), PineRef::new_box(Some(10)));
        assert_eq!(
            Int::implicity_from(context1.move_var(VarIndex::new(0, 0)).unwrap()),
            Ok(RefData::new_box(Some(10)))
        );
        // assert!(context1.contains_var("hello"));

        context1.map_var(VarIndex::new(0, 0), |_| Some(PineRef::new_box(Some(100))));
        assert_eq!(
            Int::implicity_from(context1.move_var(VarIndex::new(0, 0)).unwrap()),
            Ok(RefData::new_box(Some(100)))
        );
    }

    #[test]
    fn callable_context_test() {
        // Parent context create callable
        let mut context1 = Context::new(None, ContextType::Normal);
        context1.create_runnable(RefData::new_rc(Callable::new(None, None)).into_rc());
        assert_eq!(context1.runnables.len(), 1);

        {
            // Child context create callable
            let mut context2 = Context::new(Some(&mut context1), ContextType::Normal);
            context2.create_runnable(RefData::new_rc(Callable::new(None, None)).into_rc());
        }
        assert_eq!(context1.runnables.len(), 2);

        context1.commit();

        // After commit, parent context and child context should not add callable by create callable
        context1.create_runnable(RefData::new_rc(Callable::new(None, None)).into_rc());
        {
            let mut context2 = Context::new(Some(&mut context1), ContextType::Normal);
            context2.create_runnable(RefData::new_rc(Callable::new(None, None)).into_rc());
        }
        assert_eq!(context1.runnables.len(), 2);

        assert_eq!(context1.roll_back(), Ok(()));
        assert_eq!(context1.run_callbacks(), Ok(()));
        assert_eq!(context1.runnables.len(), 2);
    }

    #[test]
    fn derive_context_test() {
        // hello is owned by context1, hello2 is owned by context2, hello3 is not owned by both context
        let mut context1 = Context::new(None, ContextType::Normal);
        context1.init_vars(vec![Some(PineRef::new_box(Some(1)))]);

        let mut context2 = Context::new(Some(&mut context1), ContextType::Normal);
        context2.init_vars(vec![Some(PineRef::new_box(Some(2)))]);

        assert_eq!(context2.contains_var(VarIndex::new(0, 1)), true);
        let mov_res1 = context2.move_var(VarIndex::new(0, 1)).unwrap();
        assert_eq!(mov_res1, PineRef::new(Some(1)));

        assert_eq!(context2.contains_var(VarIndex::new(0, 0)), true);
        let mov_res2 = context2.move_var(VarIndex::new(0, 0)).unwrap();
        assert_eq!(mov_res2, PineRef::new(Some(2)));

        // assert_eq!(context2.contains_var("hello3"), false);
        // assert_eq!(context2.move_var("hello3"), None);

        // context2.update_var(VarIndex::new(0, 1), mov_res1);
        // assert!(context2.vars.get(VarIndex).is_none());
        // {
        //     let parent = context2.parent.as_mut().unwrap();
        //     assert!(parent.move_var("hello").is_some());
        // }

        // context2.update_var("hello2", mov_res2);
        // assert!(context2.vars.get("hello2").is_some());

        // context2.update_var("hello3", PineRef::new(Some(10)));
        // assert!(context2.vars.get("hello3").is_some());

        // assert!(context2.contains_var("hello"));
        // assert!(context2.contains_var("hello2"));
    }
}
