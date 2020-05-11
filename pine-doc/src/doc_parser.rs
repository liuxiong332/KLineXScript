use super::doc_base::*;
use super::var_doc;
use super::vardoc_gen::*;

use pine::ast::syntax_type::*;
use pine::libs::{declare_vars, VarResult};
use pine::types::PineRef;
use std::collections::BTreeMap;
use std::rc::Rc;

#[derive(Debug, PartialEq)]
struct NameInfo {
    name: String,
    var_type: VarType,
    signatures: Vec<String>,
}

impl NameInfo {
    pub fn new(name: String, var_type: VarType, signatures: Vec<String>) -> NameInfo {
        NameInfo {
            name,
            var_type,
            signatures,
        }
    }

    pub fn simple_var(name: String, sig: &'static str) -> NameInfo {
        NameInfo {
            name: name,
            var_type: VarType::Variable,
            signatures: vec![String::from(sig)],
        }
    }

    pub fn simple_var2(name: String, sig: String) -> NameInfo {
        NameInfo {
            name: name,
            var_type: VarType::Variable,
            signatures: vec![sig],
        }
    }

    pub fn func_vars(name: String, sigs: Vec<String>) -> NameInfo {
        NameInfo {
            name: name,
            var_type: VarType::Function,
            signatures: sigs,
        }
    }
}

fn format_func_type<'a>(name: String, t: FunctionType<'a>) -> String {
    let func_sig = t
        .signature
        .0
        .iter()
        .map(|v| {
            format_var_type(String::from(v.0), v.1.clone())
                .iter()
                .map(|m| format!("{}: {}", m.name, m.signatures[0]))
                .collect::<Vec<_>>()
        })
        .flatten()
        .collect::<Vec<_>>()
        .join(", ");
    let ret = &format_var_type(String::from(""), t.signature.1)[0].signatures[0];
    format!("{}({}) -> {}", name, func_sig, ret)
}

fn format_func_types<'a>(name: String, t: Rc<FunctionTypes<'a>>) -> Vec<String> {
    t.0.iter()
        .map(|t| format_func_type(name.clone(), t.clone()))
        .collect()
}

fn format_obj_type<'a>(name: String, obj: Rc<BTreeMap<&'a str, SyntaxType<'a>>>) -> Vec<NameInfo> {
    obj.iter()
        .map(|(n, t)| format_var_type([name.clone(), String::from(*n)].join("."), t.clone()))
        .flatten()
        .collect()
}

fn format_func<'a>(name: String, func: Rc<FunctionTypes<'a>>) -> Vec<NameInfo> {
    vec![NameInfo::func_vars(
        name.clone(),
        format_func_types(name.clone(), func),
    )]
}

fn format_var_type<'a>(name: String, t: SyntaxType<'a>) -> Vec<NameInfo> {
    match t {
        SyntaxType::Void => vec![NameInfo::simple_var(name, "void")],

        SyntaxType::Simple(SimpleSyntaxType::Int) => vec![NameInfo::simple_var(name, "int")],
        SyntaxType::Simple(SimpleSyntaxType::Float) => vec![NameInfo::simple_var(name, "float")],
        SyntaxType::Simple(SimpleSyntaxType::Bool) => vec![NameInfo::simple_var(name, "bool")],
        SyntaxType::Simple(SimpleSyntaxType::Na) => vec![NameInfo::simple_var(name, "na")],
        SyntaxType::Simple(SimpleSyntaxType::Color) => vec![NameInfo::simple_var(name, "color")],
        SyntaxType::Simple(SimpleSyntaxType::String) => vec![NameInfo::simple_var(name, "string")],

        SyntaxType::Series(SimpleSyntaxType::Int) => {
            vec![NameInfo::simple_var(name, "series(int)")]
        }
        SyntaxType::Series(SimpleSyntaxType::Float) => {
            vec![NameInfo::simple_var(name, "series(float)")]
        }
        SyntaxType::Series(SimpleSyntaxType::Bool) => {
            vec![NameInfo::simple_var(name, "series(bool)")]
        }
        SyntaxType::Series(SimpleSyntaxType::Na) => vec![NameInfo::simple_var(name, "series(na)")],
        SyntaxType::Series(SimpleSyntaxType::Color) => {
            vec![NameInfo::simple_var(name, "series(color)")]
        }
        SyntaxType::Series(SimpleSyntaxType::String) => {
            vec![NameInfo::simple_var(name, "series(string)")]
        }

        SyntaxType::List(sub_t) => vec![NameInfo::simple_var2(
            name,
            format!(
                "list({})",
                format_var_type(String::from(""), SyntaxType::Simple(sub_t))
                    .into_iter()
                    .map(|d| d.signatures)
                    .flatten()
                    .collect::<Vec<_>>()
                    .join(",")
            ),
        )],

        SyntaxType::Tuple(types) => {
            let v: Vec<String> = types
                .iter()
                .map(|t| format_var_type(String::from(""), t.clone()))
                .flatten()
                .map(|d| d.signatures)
                .flatten()
                .collect();
            vec![NameInfo::simple_var2(name, format!("[{}]", v.join(", ")))]
        }

        SyntaxType::ObjectClass(t) => vec![NameInfo::simple_var2(name, String::from(t))],
        SyntaxType::Val(t) => format_var_type(name, *t),
        SyntaxType::Object(obj) => format_obj_type(name, obj),
        SyntaxType::Function(func) => format_func(name, func),
        SyntaxType::ObjectFunction(obj, func) => {
            let v1 = format_obj_type(name.clone(), obj);
            let v2 = format_func(name.clone(), func);
            vec![v1, v2].into_iter().flatten().collect::<Vec<_>>()
        }
        SyntaxType::ValFunction(val, func) => {
            let v1 = format_var_type(name.clone(), (*val).clone());
            let v2 = format_func(name.clone(), func);
            vec![v1, v2].into_iter().flatten().collect::<Vec<_>>()
        }
        SyntaxType::ValObjectFunction(val, obj, func) => {
            let v1 = format_var_type(name.clone(), (*val).clone());
            let v2 = format_obj_type(name.clone(), obj);
            let v3 = format_func(name.clone(), func);
            vec![v1, v2, v3].into_iter().flatten().collect::<Vec<_>>()
        }
        SyntaxType::UserFunction(_) => unreachable!(),
        SyntaxType::DynamicExpr(t) => format_var_type(name, (*t).clone()),
        SyntaxType::Any => unreachable!(),
    }
}

#[derive(Debug, PartialEq)]
pub struct LibVarParser {
    pub variables: BTreeMap<String, String>,
    pub functions: BTreeMap<String, String>,

    pub brief_vars: BTreeMap<String, String>,
    pub brief_funcs: BTreeMap<String, String>,
}

fn inner_vars<'a>() -> Vec<VarResult<'a>> {
    ["close", "open", "high", "low"]
        .iter()
        .map(|name| {
            VarResult::new(
                PineRef::new(Some(0f64)),
                SyntaxType::Series(SimpleSyntaxType::Float),
                name,
            )
        })
        .collect()
}

impl LibVarParser {
    pub fn new() -> LibVarParser {
        LibVarParser {
            variables: BTreeMap::new(),
            functions: BTreeMap::new(),
            brief_vars: BTreeMap::new(),
            brief_funcs: BTreeMap::new(),
        }
    }

    pub fn parse_lib_vars(&mut self) {
        let docs = var_doc::declare_vars();
        vec![declare_vars(), inner_vars()]
            .iter()
            .flatten()
            .for_each(|s| {
                let name_infos = format_var_type(String::from(s.name), s.syntax_type.clone());
                name_infos.into_iter().for_each(|s| {
                    let doc = docs
                        .iter()
                        .find(|m| m.name == s.name && m.var_type == s.var_type);
                    match s.var_type {
                        VarType::Variable => {
                            self.variables.insert(
                                s.name.clone(),
                                gen_var_doc(
                                    s.name.clone(),
                                    doc.clone(),
                                    &s.signatures,
                                    String::from("var"),
                                ),
                            );
                            self.brief_vars.insert(
                                s.name.clone(),
                                gen_brief_var_doc(s.name.clone(), doc.clone(), &s.signatures),
                            );
                        }
                        VarType::Function => {
                            self.functions.insert(
                                s.name.clone(),
                                gen_var_doc(
                                    s.name.clone(),
                                    doc.clone(),
                                    &s.signatures,
                                    String::from("fun"),
                                ),
                            );
                            self.brief_funcs.insert(
                                s.name.clone(),
                                gen_brief_var_doc(s.name.clone(), doc.clone(), &s.signatures),
                            );
                        }
                    }
                })
                // match s.syntax_type {}
            });
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::rc::Rc;
    #[test]
    fn format_type_test() {
        assert_eq!(
            format_var_type(String::from("hello"), SyntaxType::int()),
            vec![NameInfo::simple_var(String::from("hello"), "int")],
        );

        assert_eq!(
            format_var_type(String::from("hello"), SyntaxType::int_series()),
            vec![NameInfo::simple_var(String::from("hello"), "series(int)")],
        );

        assert_eq!(
            format_var_type(
                String::from("hello"),
                SyntaxType::List(SimpleSyntaxType::Int)
            ),
            vec![NameInfo::simple_var(String::from("hello"), "list(int)")]
        );

        assert_eq!(
            format_var_type(
                String::from("hello"),
                SyntaxType::Tuple(Rc::new(vec![SyntaxType::int(), SyntaxType::float()]))
            ),
            vec![NameInfo::simple_var(String::from("hello"), "[int, float]")]
        );

        let mut obj = BTreeMap::new();
        let mut obj2 = BTreeMap::new();
        obj.insert("e1", SyntaxType::int());
        obj2.insert("se", SyntaxType::float());
        obj.insert("e2", SyntaxType::Object(Rc::new(obj2)));

        assert_eq!(
            format_var_type(String::from("hello"), SyntaxType::Object(Rc::new(obj))),
            vec![
                NameInfo::simple_var(String::from("hello.e1"), "int"),
                NameInfo::simple_var(String::from("hello.e2.se"), "float")
            ]
        );
        assert_eq!(
            format_var_type(
                String::from("hello"),
                SyntaxType::Function(Rc::new(FunctionTypes(vec![
                    FunctionType::new((vec![("e1", SyntaxType::bool())], SyntaxType::int())),
                    FunctionType::new((vec![("e1", SyntaxType::int())], SyntaxType::int()))
                ])))
            ),
            vec![NameInfo::func_vars(
                String::from("hello"),
                vec![
                    String::from("hello(e1: bool) -> int"),
                    String::from("hello(e1: int) -> int")
                ]
            ),]
        );
    }
}
