use super::VarResult;
use crate::ast::syntax_type::{FunctionType, FunctionTypes, SimpleSyntaxType, SyntaxType};
use crate::helper::err_msgs::*;
use crate::helper::str_replace;
use crate::helper::{
    move_element, pine_ref_to_bool, pine_ref_to_color, pine_ref_to_f64, pine_ref_to_i64,
    pine_ref_to_string,
};
use crate::runtime::context::{downcast_ctx, Ctx};
use crate::runtime::output::{OutputData, OutputInfo, PlotInfo};
use crate::types::{
    Bool, Callable, Color, DataType, Float, Int, Object, ParamCollectCall, PineClass, PineFrom,
    PineRef, PineType, RefData, RuntimeErr, SecondType, Series, NA,
};
use regex::Regex;
use std::collections::BTreeMap;
use std::rc::Rc;

struct SymInfoProps;

impl<'a> PineClass<'a> for SymInfoProps {
    fn custom_type(&self) -> &str {
        "syminfo"
    }

    fn get(&self, ctx: &mut dyn Ctx<'a>, name: &str) -> Result<PineRef<'a>, RuntimeErr> {
        let ctx_ins = downcast_ctx(ctx);
        match name {
            "currency" => match ctx_ins.get_syminfo() {
                Some(syminfo) => Ok(PineRef::new_rc(syminfo.currency.clone())),
                _ => Ok(PineRef::new_rc(String::from(""))),
            },
            "description" => match ctx_ins.get_syminfo() {
                Some(syminfo) => Ok(PineRef::new_rc(syminfo.description.clone())),
                _ => Ok(PineRef::new_rc(String::from(""))),
            },
            "mintick" => match ctx_ins.get_syminfo() {
                Some(syminfo) => Ok(PineRef::new_box(Some(syminfo.mintick))),
                _ => Ok(PineRef::new_box(Float::from(None))),
            },
            "pointvalue" => Ok(PineRef::new_box(Some(1f64))),
            "prefix" => match ctx_ins.get_syminfo() {
                Some(syminfo) => {
                    let re = Regex::new(r"(\w+):\w+").unwrap();
                    match re.captures(&syminfo.ticker) {
                        Some(caps) => Ok(PineRef::new_rc(String::from(&caps[1]))),
                        _ => Ok(PineRef::new_rc(String::from(""))),
                    }
                }
                _ => Ok(PineRef::new_rc(String::from(""))),
            },
            "root" => match ctx_ins.get_syminfo() {
                Some(syminfo) => Ok(PineRef::new_rc(
                    syminfo.root.clone().unwrap_or(String::from("")),
                )),
                _ => Ok(PineRef::new_rc(String::from(""))),
            },
            "session" => match ctx_ins.get_syminfo() {
                Some(syminfo) => Ok(PineRef::new_rc(syminfo.session.clone())),
                _ => Ok(PineRef::new_rc(String::from(""))),
            },
            "ticker" => match ctx_ins.get_syminfo() {
                Some(syminfo) => {
                    let re = Regex::new(r"\w+:(\w+)").unwrap();
                    match re.captures(&syminfo.ticker) {
                        Some(caps) => Ok(PineRef::new_rc(String::from(&caps[1]))),
                        _ => Ok(PineRef::new_rc(String::from(""))),
                    }
                }
                _ => Ok(PineRef::new_rc(String::from(""))),
            },
            "tickerid" => match ctx_ins.get_syminfo() {
                Some(syminfo) => Ok(PineRef::new_rc(syminfo.ticker.clone())),
                _ => Ok(PineRef::new_rc(String::from(""))),
            },
            "timezone" => match ctx_ins.get_syminfo() {
                Some(syminfo) => Ok(PineRef::new_rc(syminfo.timezone.clone())),
                _ => Ok(PineRef::new_rc(String::from(""))),
            },
            "type" => match ctx_ins.get_syminfo() {
                Some(syminfo) => Ok(PineRef::new_rc(syminfo.symbol_type.clone())),
                _ => Ok(PineRef::new_rc(String::from(""))),
            },
            _ => Err(RuntimeErr::NotImplement(str_replace(
                NO_FIELD_IN_OBJECT,
                vec![String::from(name), String::from("color")],
            ))),
        }
    }

    fn copy(&self) -> Box<dyn PineClass<'a> + 'a> {
        Box::new(SymInfoProps)
    }
}

pub const VAR_NAME: &'static str = "syminfo";

pub fn declare_var<'a>() -> VarResult<'a> {
    let value = PineRef::new(Object::new(Box::new(SymInfoProps)));

    let mut obj_type = BTreeMap::new();
    obj_type.insert("currency", SyntaxType::string());
    obj_type.insert("description", SyntaxType::string());
    obj_type.insert("mintick", SyntaxType::float());
    obj_type.insert("pointvalue", SyntaxType::float());
    obj_type.insert("prefix", SyntaxType::string());
    obj_type.insert("root", SyntaxType::string());
    obj_type.insert("session", SyntaxType::string());
    obj_type.insert("ticker", SyntaxType::string());
    obj_type.insert("tickerid", SyntaxType::string());
    obj_type.insert("timezone", SyntaxType::string());
    obj_type.insert("type", SyntaxType::string());
    let syntax_type = SyntaxType::Object(Rc::new(obj_type));
    VarResult::new(value, syntax_type, VAR_NAME)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::{AnySeries, NoneCallback, SymbolInfo};
    use crate::{LibInfo, PineParser, PineRunner};

    #[test]
    fn syminfo_fields_test() {
        use crate::ast::stat_expr_types::VarIndex;
        use crate::runtime::VarOperate;
        use crate::types::{downcast_pf, Tuple};

        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![("close", SyntaxType::float_series())],
        );
        let src = r"m = [
            syminfo.currency, syminfo.description, syminfo.mintick, syminfo.pointvalue, syminfo.prefix,
            syminfo.root, syminfo.session, syminfo.ticker, syminfo.tickerid, syminfo.timezone, syminfo.type
        ]";

        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner
            .run(
                &vec![("close", AnySeries::from_float_vec(vec![Some(1f64)]))],
                Some(Rc::new(SymbolInfo {
                    symbol_type: String::from("future"),
                    timezone: String::from("GTC+8"),
                    ticker: String::from("BATS:MSFT"),
                    session: String::from("regular"),
                    trade_start: String::from(""),
                    trade_end: String::from(""),
                    root: Some(String::from("le")),
                    currency: String::from("USD"),
                    description: String::from("des"),
                    mintick: 1f64,
                })),
            )
            .unwrap();
        let tuple_res =
            downcast_pf::<Tuple>(runner.get_context().move_var(VarIndex::new(2, 0)).unwrap());
        let tuple_vec = tuple_res.unwrap().into_inner().0;
        assert_eq!(
            tuple_vec,
            vec![
                PineRef::new_rc(String::from("USD")),
                PineRef::new_rc(String::from("des")),
                PineRef::new_box(Some(1f64)),
                PineRef::new_box(Some(1f64)),
                PineRef::new_rc(String::from("BATS")),
                PineRef::new_rc(String::from("le")),
                PineRef::new_rc(String::from("regular")),
                PineRef::new_rc(String::from("MSFT")),
                PineRef::new_rc(String::from("BATS:MSFT")),
                PineRef::new_rc(String::from("GTC+8")),
                PineRef::new_rc(String::from("future")),
            ]
        );
    }
}
