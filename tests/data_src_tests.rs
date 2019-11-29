extern crate pine;
use pine::runtime::{
    context::Ctx,
    data_src::{Callback, DataSrc},
};
use pine::types::{
    Callable, DataType, Float, PineFrom, PineType, RuntimetErr, SecondType, Series,
    SeriesToArrayCall, NA,
};
use std::collections::HashMap;

const MA_SCRIPT: &str = "
// ma = close
ma = close + close[1] + close[2] + close[3] + close[4]
// for i = 1 to 5
//     ma := ma + close[i] 
// print(ma)
";

fn pine_print<'a>(
    context: &mut dyn Ctx<'a>,
    mut param: HashMap<&'a str, Box<dyn PineType<'a> + 'a>>,
) -> Result<Box<dyn PineType<'a> + 'a>, RuntimetErr> {
    match param.remove("item") {
        None => Err(RuntimetErr::NotSupportOperator),
        Some(item_val) => {
            if item_val.get_type().1 != SecondType::Series {
                return Err(RuntimetErr::NotSupportOperator);
            }
            let items: Box<Series<Float>> = Series::implicity_from(item_val).unwrap();
            let vec: Vec<Float> = (*items).into();
            let s: String = vec
                .into_iter()
                .map(|v| match v {
                    None => String::from("na"),
                    Some(f) => f.to_string(),
                })
                .collect::<Vec<String>>()
                .join(",");
            context.get_callback().unwrap().print(s);
            Ok(Box::new(NA))
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
        Box::new(Callable::new(
            None,
            Some(Box::new(SeriesToArrayCall::new(pine_print))),
            vec!["item"],
        )) as Box<dyn PineType>,
    );

    let mut datasrc = DataSrc::new(&ma_block, inner_vars, &MyCallback);

    let mut data = HashMap::new();
    data.insert(
        "close",
        // vec![Some(1f64), Some(2f64), Some(3f64), Some(4f64), Some(5f64)],
        vec![Some(1f64)],
    );

    assert_eq!(datasrc.run(data), Ok(()));
}
