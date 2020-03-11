extern crate pine;
use pine::ast::syntax_type::{SimpleSyntaxType, SyntaxType};
use pine::libs::plot;
use pine::libs::print;
use pine::runtime::data_src::{Callback, DataSrc, NoneCallback};
use pine::runtime::AnySeries;

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

    let lib_info = pine::LibInfo::new(
        vec![print::declare_var()],
        vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
    );
    let mut parser = pine::PineScript::new_with_libinfo(lib_info, Some(&MyCallback));
    parser.parse_src(String::from(MA_SCRIPT)).unwrap();
    let data = vec![(
        "close",
        AnySeries::from_float_vec(vec![
            Some(1f64),
            Some(2f64),
            Some(3f64),
            Some(4f64),
            Some(5f64),
        ]),
    )];
    assert!(parser.run_with_data(data, None).is_ok());
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

    let lib_info = pine::LibInfo::new(
        vec![print::declare_var()],
        vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
    );
    let mut parser = pine::PineScript::new_with_libinfo(lib_info, Some(&MyCallback));
    parser.parse_src(String::from(FUNC_SCRIPT)).unwrap();
    let data = vec![(
        "close",
        AnySeries::from_float_vec(vec![
            Some(2f64),
            Some(4f64),
            Some(8f64),
            Some(16f64),
            Some(32f64),
        ]),
    )];
    assert!(parser.run_with_data(data, None).is_ok());
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

    let lib_info = pine::LibInfo::new(
        vec![print::declare_var()],
        vec![
            ("close", SyntaxType::Series(SimpleSyntaxType::Float)),
            ("open", SyntaxType::Series(SimpleSyntaxType::Float)),
        ],
    );
    let mut parser = pine::PineScript::new_with_libinfo(lib_info, Some(&MyCallback));
    parser.parse_src(String::from(IF_ELSE_SCRIPT)).unwrap();
    let data = vec![
        (
            "close",
            AnySeries::from_float_vec(vec![
                Some(1f64),
                Some(3f64),
                Some(5f64),
                Some(7f64),
                Some(9f64),
            ]),
        ),
        (
            "open",
            AnySeries::from_float_vec(vec![
                Some(0f64),
                Some(4f64),
                Some(4f64),
                Some(8f64),
                Some(8f64),
            ]),
        ),
    ];
    assert!(parser.run_with_data(data, None).is_ok());
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

    let lib_info = pine::LibInfo::new(
        vec![print::declare_var()],
        vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
    );
    let mut parser = pine::PineScript::new_with_libinfo(lib_info, Some(&MyCallback));
    parser.parse_src(String::from(FOR_RANGE_SCRIPT)).unwrap();
    let data = vec![(
        "close",
        AnySeries::from_float_vec(vec![Some(1f64), Some(3f64)]),
    )];
    assert!(parser.run_with_data(data, None).is_ok());
}

const EMA_SCRIPT: &str = "
pine_ema(x, y) =>
    alpha = 2 / (y + 1)
    sum = 0.0
    sum := alpha * x + (1 - alpha) * (sum[1] ? sum[1] : 0)
    sum
print(pine_ema(close, 3.0))
";

#[test]
fn ema_test() {
    struct MyCallback;
    impl Callback for MyCallback {
        fn print(&self, _str: String) {
            assert_eq!(_str, String::from("1,2.5"));
        }
    }

    let lib_info = pine::LibInfo::new(
        vec![print::declare_var()],
        vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
    );
    let mut parser = pine::PineScript::new_with_libinfo(lib_info, Some(&MyCallback));
    parser.parse_src(String::from(EMA_SCRIPT)).unwrap();
    let data = vec![(
        "close",
        AnySeries::from_float_vec(vec![Some(2f64), Some(4f64)]),
    )];

    assert!(parser.run_with_data(data, None).is_ok());
}

const MACD_SCRIPT: &str = "
pine_ema(x, y) =>
    alpha = 2 / (y + 1)
    sum = 0.0
    sum := alpha * x + (1 - alpha) * (sum[1] ? sum[1] : 0)
    sum

pine_macd() =>
    // DIF=EMA_{{(close,12)}}-EMA_{{(close,26)}}
    dif = pine_ema(close, 12.0) - pine_ema(close, 26.0)
    // DEM=EMA_{{(DIF,9)}}
    dem = pine_ema(dif, 9.0)
    //OSC=DIF-DEM=DIF-MACD
    osc = dif - dem
    osc

print(pine_macd())
";

#[test]
fn macd_test() {
    struct MyCallback;
    impl Callback for MyCallback {
        fn print(&self, _str: String) {
            assert_eq!(_str, String::from("12.763532763532766,32.82882444136006"));
        }
    }

    let lib_info = pine::LibInfo::new(
        vec![print::declare_var()],
        vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
    );
    let mut parser = pine::PineScript::new_with_libinfo(lib_info, Some(&MyCallback));
    parser.parse_src(String::from(MACD_SCRIPT)).unwrap();
    let data = vec![(
        "close",
        AnySeries::from_float_vec(vec![Some(200f64), Some(400f64)]),
    )];

    assert!(parser.run_with_data(data, None).is_ok());
}

#[test]
fn assign_test() {
    let lib_info = pine::LibInfo::new(
        vec![print::declare_var()],
        vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
    );
    let mut parser = pine::PineScript::new_with_libinfo(lib_info, Some(&NoneCallback()));
    assert!(parser
        .parse_src(String::from("m = close\nm := true"))
        .is_err());
}
#[test]
fn plot_only_test() {
    let lib_info = pine::LibInfo::new(
        vec![plot::declare_var()],
        vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
    );
    let mut parser = pine::PineScript::new_with_libinfo(lib_info, Some(&NoneCallback()));
    println!("{:?}", parser.parse_src(String::from("plot(close)")));
    assert!(parser.parse_src(String::from("plot(close)")).is_ok());
}

const ALMA_SCRIPT: &str = "
print(alma(close, 9, 0.85, 6))

// same on pine, but much less efficient
pine_alma(series, windowsize, offset, sigma) =>
    m = floor(offset * (windowsize - 1))
    s = windowsize / sigma
    norm = 0.0
    sum = 0.0
    for i = 0 to windowsize - 1
        weight = exp(-1 * pow(i - m, 2) / (2 * pow(s, 2)))
        norm := norm + weight
        sum := sum + series[windowsize - i - 1] * weight
    sum / norm
print(pine_alma(close, 9, 0.85, 6))
";

#[test]
fn alma_test() {
    struct MyCallback;
    impl Callback for MyCallback {
        fn print(&self, _str: String) {
            assert_eq!(_str, String::from("12.763532763532766,32.82882444136006"));
        }
    }

    let lib_info = pine::LibInfo::new(
        vec![print::declare_var()],
        vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
    );
    let mut parser = pine::PineScript::new_with_libinfo(lib_info, Some(&MyCallback));
    parser.parse_src(String::from(ALMA_SCRIPT)).unwrap();
    let data = vec![(
        "close",
        AnySeries::from_float_vec(vec![Some(200f64), Some(400f64)]),
    )];

    assert!(parser.run_with_data(data, None).is_ok());
}
