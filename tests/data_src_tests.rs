extern crate pine;
use pine::libs;
use pine::runtime::data_src::{Callback, DataSrc};
use std::collections::HashMap;

const MA_SCRIPT: &str = "
N = 5
ma = close
// ma = (close + close[1] + close[2] + close[3] + close[4]) / 5
for i = 1 to N
    ma := ma + close[i] 
ma := ma / N
print(ma)
";

#[test]
fn datasrc_test() {
    struct MyCallback;
    impl Callback for MyCallback {
        fn print(&self, _str: String) {
            assert_eq!(_str, String::from("na,na,na,na,3"));
        }
    }

    let mut ma_block = pine::parse_all(MA_SCRIPT).unwrap();

    let inner_vars = libs::declare_vars();

    let mut datasrc = DataSrc::new(&mut ma_block, inner_vars, &MyCallback);

    let mut data = HashMap::new();
    data.insert(
        "close",
        vec![Some(1f64), Some(2f64), Some(3f64), Some(4f64), Some(5f64)],
        // vec![Some(1f64)],
    );

    assert_eq!(datasrc.run(data), Ok(()));
}

const FUNC_SCRIPT: &str = "
pine_ema(x, y) =>
    sum = 0.0
    sum := x + (y * sum[1] ? y * sum[1] : 0)
    sum
print(pine_ema(close, 2))
";

#[test]
fn func_call_test() {
    struct MyCallback;
    impl Callback for MyCallback {
        fn print(&self, _str: String) {
            assert_eq!(_str, String::from("2,8,24,64,160"));
        }
    }

    let mut func_block = pine::parse_all(FUNC_SCRIPT).unwrap();
    let inner_vars = libs::declare_vars();
    let mut datasrc = DataSrc::new(&mut func_block, inner_vars, &MyCallback);

    let mut data = HashMap::new();
    data.insert(
        "close",
        vec![Some(2f64), Some(4f64), Some(8f64), Some(16f64), Some(32f64)],
    );

    assert_eq!(datasrc.run(data), Ok(()));
}

const IF_ELSE_SCRIPT: &str = "
m = if close > open
    s = close
    s[1]
else 
    t = open
    t[1]
print(m)
";

#[test]
fn if_else_test() {
    struct MyCallback;
    impl Callback for MyCallback {
        fn print(&self, _str: String) {
            assert_eq!(_str, String::from("na,na,1,4,5"));
        }
    }

    let mut func_block = pine::parse_all(IF_ELSE_SCRIPT).unwrap();
    let inner_vars = libs::declare_vars();
    let mut datasrc = DataSrc::new(&mut func_block, inner_vars, &MyCallback);

    let mut data = HashMap::new();
    data.insert(
        "close",
        vec![Some(1f64), Some(3f64), Some(5f64), Some(7f64), Some(9f64)],
    );
    data.insert(
        "open",
        vec![Some(0f64), Some(4f64), Some(4f64), Some(8f64), Some(8f64)],
    );

    assert_eq!(datasrc.run(data), Ok(()));
}

const FOR_RANGE_SCRIPT: &str = "
float val = 0

for i = 1 to 5
    var sum = 0.0
    sum := sum + 1
    val := sum
";

#[test]
fn for_range_test() {
    struct MyCallback;
    impl Callback for MyCallback {
        fn print(&self, _str: String) {
            assert_eq!(_str, String::from("4,8"));
        }
    }

    let mut func_block = pine::parse_all(FOR_RANGE_SCRIPT).unwrap();
    let inner_vars = libs::declare_vars();
    let mut datasrc = DataSrc::new(&mut func_block, inner_vars, &MyCallback);

    let mut data = HashMap::new();
    data.insert("close", vec![Some(1f64), Some(3f64)]);

    assert_eq!(datasrc.run(data), Ok(()));
}
