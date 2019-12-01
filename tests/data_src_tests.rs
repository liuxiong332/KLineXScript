extern crate pine;
use pine::runtime::{
    context::Ctx,
    data_src::{Callback, DataSrc},
};
use pine::types::{
    Callable, DataType, Float, PineFrom, PineRef, PineType, RefData, RuntimeErr, SecondType,
    Series, SeriesToArrayCall, NA,
};
use std::collections::HashMap;

const MA_SCRIPT: &str = "
// ma = close
ma = (close + close[1] + close[2] + close[3] + close[4]) / 5
// for i = 1 to 5
//     ma := ma + close[i] 
print(ma)
";

fn pine_print<'a>(
    context: &mut dyn Ctx<'a>,
    mut param: HashMap<&'a str, PineRef<'a>>,
) -> Result<PineRef<'a>, RuntimeErr> {
    println!("pine print Series run, {:?}", param.get("item"));
    match param.remove("item") {
        None => Err(RuntimeErr::NotSupportOperator),
        Some(item_val) => {
            if item_val.get_type().1 != SecondType::Series {
                return Err(RuntimeErr::TypeMismatch(format!(
                    "Expect Series, but get {:?}",
                    item_val.get_type()
                )));
            }
            let items: RefData<Series<Float>> = Series::implicity_from(item_val).unwrap();
            println!("vec, {:?}", items);

            let s: String = items
                .get_history()
                .iter()
                .map(|v| match v {
                    None => String::from("na"),
                    Some(f) => f.to_string(),
                })
                .collect::<Vec<String>>()
                .join(",");
            context.get_callback().unwrap().print(s);
            Ok(PineRef::new(NA))
        }
    }
}

#[test]
fn datasrc_test() {
    struct MyCallback;
    impl Callback for MyCallback {
        fn print(&self, _str: String) {
            assert_eq!(_str, String::from("na,na,na,na,3"));
        }
    }

    let ma_block = pine::parse_all(MA_SCRIPT).unwrap();

    let mut inner_vars = HashMap::new();
    inner_vars.insert(
        "print",
        PineRef::new(Callable::new(
            None,
            Some(Box::new(SeriesToArrayCall::new(pine_print))),
            vec!["item"],
        )),
    );

    let mut datasrc = DataSrc::new(&ma_block, inner_vars, &MyCallback);

    let mut data = HashMap::new();
    data.insert(
        "close",
        vec![Some(1f64), Some(2f64), Some(3f64), Some(4f64), Some(5f64)],
        // vec![Some(1f64)],
    );

    assert_eq!(datasrc.run(data), Ok(()));
}
