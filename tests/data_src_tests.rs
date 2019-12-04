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
res = pine_ema(close, 2)
";

#[test]
fn func_call_test() {
    struct MyCallback;
    impl Callback for MyCallback {
        fn print(&self, _str: String) {
            assert_eq!(_str, String::from("2,8,16,32,64"));
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
