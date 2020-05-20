use super::VarResult;
use crate::ast::syntax_type::{FunctionType, FunctionTypes, SyntaxType};
use crate::helper::err_msgs::*;
use crate::helper::str_replace;
use crate::helper::{move_element, pine_ref_to_bool, pine_ref_to_i64, pine_ref_to_string};
use crate::runtime::context::{downcast_ctx, Ctx};
use crate::runtime::{ScriptPurpose, StudyScript};
use crate::types::{Callable, CallableFactory, PineRef, RuntimeErr, NA};
use std::rc::Rc;

fn study<'a>(
    context: &mut dyn Ctx<'a>,
    mut param: Vec<Option<PineRef<'a>>>,
    _func_type: FunctionType<'a>,
) -> Result<PineRef<'a>, RuntimeErr> {
    move_tuplet!((title, shorttitle, overlay, format, precision) = param);
    if !downcast_ctx(context).check_is_input_info_ready() {
        if let Some(title) = pine_ref_to_string(title) {
            let study = StudyScript {
                title,
                shorttitle: pine_ref_to_string(shorttitle),
                overlay: pine_ref_to_bool(overlay),
                format: pine_ref_to_string(format),
                precision: pine_ref_to_i64(precision),
            };
            downcast_ctx(context).set_script_type(ScriptPurpose::Study(study));
        } else {
            return Err(RuntimeErr::MissingParameters(str_replace(
                REQUIRED_PARAMETERS,
                vec![String::from("title")],
            )));
        }
    }
    Ok(PineRef::new(NA))
}

pub const VAR_NAME: &'static str = "study";

pub fn declare_var<'a>() -> VarResult<'a> {
    let value = PineRef::new(CallableFactory::new(|| Callable::new(Some(study), None)));

    let func_type = FunctionTypes(vec![FunctionType::new((
        vec![
            ("title", SyntaxType::string()),
            ("shorttitle", SyntaxType::string()),
            ("overlay", SyntaxType::bool()),
            ("format", SyntaxType::string()),
            ("precision", SyntaxType::int()),
        ],
        SyntaxType::Void,
    ))]);
    let syntax_type = SyntaxType::Function(Rc::new(func_type));
    VarResult::new(value, syntax_type, VAR_NAME)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::syntax_type::SimpleSyntaxType;
    use crate::runtime::{AnySeries, NoneCallback};
    use crate::{LibInfo, PineParser, PineRunner};

    #[test]
    fn plotbar_info_test() {
        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
        );
        let src = r"study('hello', 'dd', true, 'price', 2)";
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner
            .run(
                &vec![(
                    "close",
                    AnySeries::from_float_vec(vec![Some(1f64), Some(2f64)]),
                )],
                None,
            )
            .unwrap();

        assert_eq!(
            runner.get_io_info().get_script_type(),
            &Some(ScriptPurpose::Study(StudyScript {
                title: String::from("hello"),
                shorttitle: Some(String::from("dd")),
                overlay: Some(true),
                format: Some(String::from("price")),
                precision: Some(2)
            }))
        );
    }
}
