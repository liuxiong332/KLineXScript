extern crate pine;
use pine::ast::syntax_type::{SimpleSyntaxType, SyntaxType};
use pine::libs::input;
use pine::libs::print;
use pine::runtime::data_src::{Callback, DataSrc};
use pine::runtime::AnySeries;

const INPUT_SCRIPT: &str = "
val1 = input(true, 'hello', 'bool', false)
val2 = input(true, 'hello', 'bool', false)
";

#[test]
fn datasrc_test() {
    struct MyCallback;
    impl Callback for MyCallback {
        fn print(&self, _str: String) {
            assert_eq!(_str, String::from("true"));
        }
    }

    let lib_info = pine::LibInfo::new(
        vec![print::declare_var(), input::declare_var()],
        vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
    );
    let mut parser = pine::PineScript::new_with_libinfo(lib_info, Some(&MyCallback));
    parser.parse_src(String::from(INPUT_SCRIPT)).unwrap();
    let data = vec![("close", AnySeries::from_float_vec(vec![Some(1f64)]))];
    assert!(parser.run_with_data(data, None).is_ok());
}
