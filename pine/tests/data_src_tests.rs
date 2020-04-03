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
for i = 1 to N - 1
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
m1 = (alma(close, 4, 0.85, 2.0))

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
m2 = (pine_alma(close, 4, 0.85, 2.0))
";

#[test]
fn alma_test() {
    use pine::ast::stat_expr_types::VarIndex;
    use pine::helper::pine_ref_to_f64_series;
    use pine::libs::{alma, ceil, cos, pow};
    use pine::runtime::{NoneCallback, VarOperate};

    let lib_info = pine::LibInfo::new(
        vec![
            print::declare_var(),
            cos::declare_exp_var(),
            ceil::declare_floor_var(),
            pow::declare_var(),
            alma::declare_var(),
        ],
        vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
    );
    let mut parser = pine::PineScript::new_with_libinfo(lib_info, Some(&NoneCallback()));
    parser.parse_src(String::from(ALMA_SCRIPT)).unwrap();
    let data = vec![(
        "close",
        AnySeries::from_float_vec(vec![
            Some(200f64),
            Some(400f64),
            Some(400f64),
            Some(400f64),
            Some(400f64),
            Some(400f64),
        ]),
    )];

    assert!(parser.run_with_data(data, None).is_ok());

    let result1 = parser
        .get_runner()
        .get_context()
        .move_var(VarIndex::new(6, 0));
    let result2 = parser
        .get_runner()
        .get_context()
        .move_var(VarIndex::new(8, 0));
    let val1 = pine_ref_to_f64_series(result1);
    let val2 = pine_ref_to_f64_series(result2);
    assert_eq!(
        val1.unwrap().index_value(1).unwrap().unwrap().floor(),
        val2.unwrap().index_value(1).unwrap().unwrap().floor()
    );
    // println!("{:?} {:?}", result1, result2);
}

const SMA_SCRIPT: &str = "
m1 = (sma(close, 2))

// same on pine, but much less efficient
pine_sma(x, y) =>
    sum = 0.0
    for i = 0 to y - 1
        sum := sum + x[i] / y
    sum
m2 = (pine_sma(close, 2))
";

#[test]
fn sma_test() {
    use pine::ast::stat_expr_types::VarIndex;
    use pine::helper::pine_ref_to_f64_series;
    use pine::libs::sma;
    use pine::runtime::{NoneCallback, VarOperate};

    let lib_info = pine::LibInfo::new(
        vec![sma::declare_sma_var()],
        vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
    );
    let mut parser = pine::PineScript::new_with_libinfo(lib_info, Some(&NoneCallback()));
    parser.parse_src(String::from(SMA_SCRIPT)).unwrap();
    let data = vec![(
        "close",
        AnySeries::from_float_vec(vec![Some(200f64), Some(400f64)]),
    )];

    assert!(parser.run_with_data(data, None).is_ok());

    let result1 = parser
        .get_runner()
        .get_context()
        .move_var(VarIndex::new(2, 0));
    let result2 = parser
        .get_runner()
        .get_context()
        .move_var(VarIndex::new(4, 0));
    let val1 = pine_ref_to_f64_series(result1);
    let val2 = pine_ref_to_f64_series(result2);
    println!("val {:?} {:?}", val1, val2);
    assert_eq!(
        val1.unwrap().index_value(1).unwrap().unwrap().floor(),
        val2.unwrap().index_value(1).unwrap().unwrap().floor()
    );
    // println!("{:?} {:?}", result1, result2);
}

const BB_SCRIPT: &str = "
[middle, upper, lower] = bb(close, 2, 4)

// the same on pine
f_bb(src, length, mult) =>
    float basis = sma(src, length)
    float dev = mult * stdev(src, length)
    [basis, basis + dev, basis - dev]

[pineMiddle, pineUpper, pineLower] = f_bb(close, 2, 4)
";

#[test]
fn bb_test() {
    use pine::ast::stat_expr_types::VarIndex;
    use pine::helper::pine_ref_to_f64_series;
    use pine::libs::{bb, sma};
    use pine::runtime::{NoneCallback, VarOperate};

    let lib_info = pine::LibInfo::new(
        vec![
            bb::declare_var(),
            sma::declare_sma_var(),
            sma::declare_stdev_var(),
        ],
        vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
    );
    let mut parser = pine::PineScript::new_with_libinfo(lib_info, Some(&NoneCallback()));
    parser.parse_src(String::from(BB_SCRIPT)).unwrap();
    let data = vec![(
        "close",
        AnySeries::from_float_vec(vec![Some(200f64), Some(400f64)]),
    )];

    assert!(parser.run_with_data(data, None).is_ok());

    let middle = pine_ref_to_f64_series(parser.move_var(VarIndex::new(4, 0)));
    let upper = pine_ref_to_f64_series(parser.move_var(VarIndex::new(5, 0)));
    let lower = pine_ref_to_f64_series(parser.move_var(VarIndex::new(6, 0)));

    let pine_middle = pine_ref_to_f64_series(parser.move_var(VarIndex::new(8, 0)));
    let pine_upper = pine_ref_to_f64_series(parser.move_var(VarIndex::new(9, 0)));
    let pine_lower = pine_ref_to_f64_series(parser.move_var(VarIndex::new(10, 0)));
    println!("val {:?} {:?} {:?}", middle, upper, lower);
    assert_eq!(
        middle.unwrap().index_value(1).unwrap().unwrap().floor(),
        pine_middle
            .unwrap()
            .index_value(1)
            .unwrap()
            .unwrap()
            .floor()
    );
    assert_eq!(
        upper.unwrap().index_value(1).unwrap().unwrap().floor(),
        pine_upper.unwrap().index_value(1).unwrap().unwrap().floor()
    );
    assert_eq!(
        lower.unwrap().index_value(1).unwrap().unwrap().floor(),
        pine_lower.unwrap().index_value(1).unwrap().unwrap().floor()
    );
    // println!("{:?} {:?}", result1, result2);
}

const BBW_SCRIPT: &str = "
m1 = (bbw(close, 2, 4))

// the same on pine
f_bbw(src, length, mult) =>
    float basis = sma(src, length)
    float dev = mult * stdev(src, length)
    ((basis + dev) - (basis - dev)) / basis

m2 = f_bbw(close, 2, 4)
";

#[test]
fn bbw_test() {
    use pine::ast::stat_expr_types::VarIndex;
    use pine::helper::pine_ref_to_f64_series;
    use pine::libs::{bbw, sma};
    use pine::runtime::{NoneCallback, VarOperate};

    let lib_info = pine::LibInfo::new(
        vec![
            bbw::declare_var(),
            sma::declare_sma_var(),
            sma::declare_stdev_var(),
        ],
        vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
    );
    let mut parser = pine::PineScript::new_with_libinfo(lib_info, Some(&NoneCallback()));
    parser.parse_src(String::from(BBW_SCRIPT)).unwrap();
    let data = vec![(
        "close",
        AnySeries::from_float_vec(vec![Some(200f64), Some(400f64)]),
    )];

    assert!(parser.run_with_data(data, None).is_ok());

    let result1 = pine_ref_to_f64_series(parser.move_var(VarIndex::new(4, 0)));
    let result2 = pine_ref_to_f64_series(parser.move_var(VarIndex::new(6, 0)));

    assert_eq!(
        result1.unwrap().index_value(1).unwrap().unwrap().floor(),
        result2.unwrap().index_value(1).unwrap().unwrap().floor()
    );
}

const CMO_SCRIPT: &str = "
m1 = cmo(close, 2)

// the same on pine
f_cmo(src, length) =>
    float mom = change(src)
    float sm1 = sum((mom >= 0) ? mom : 0.0, length)
    float sm2 = sum((mom >= 0) ? 0.0 : -mom, length)
    100 * (sm1 - sm2) / (sm1 + sm2)

m2 = f_cmo(close, 2)
";

#[test]
fn cmo_test() {
    use pine::ast::stat_expr_types::VarIndex;
    use pine::helper::pine_ref_to_f64_series;
    use pine::libs::{change, cmo, sum};
    use pine::runtime::{NoneCallback, VarOperate};

    let lib_info = pine::LibInfo::new(
        vec![
            cmo::declare_var(),
            change::declare_change_var(),
            sum::declare_var(),
        ],
        vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
    );
    let mut parser = pine::PineScript::new_with_libinfo(lib_info, Some(&NoneCallback()));
    parser.parse_src(String::from(CMO_SCRIPT)).unwrap();
    let data = vec![(
        "close",
        AnySeries::from_float_vec(vec![Some(200f64), Some(400f64), Some(200f64)]),
    )];

    assert!(parser.run_with_data(data, None).is_ok());

    let result1 = pine_ref_to_f64_series(parser.move_var(VarIndex::new(4, 0)));
    let result2 = pine_ref_to_f64_series(parser.move_var(VarIndex::new(6, 0)));

    println!("result {:?} {:?}", result1, result2);
    assert_eq!(
        result1.unwrap().index_value(1).unwrap().unwrap().floor(),
        result2.unwrap().index_value(1).unwrap().unwrap().floor()
    );
}

const KC_SCRIPT: &str = "
[middle, upper, lower] = kc(close, 3, 4, false)

// the same on pine
f_kc(src, length, mult, useTrueRange) =>
    float basis = ema(src, length)
    float range = (useTrueRange) ? tr : (high - low)
    float rangeEma = ema(range, length)
    [basis, basis + rangeEma * mult, basis - rangeEma * mult]
    
[pineMiddle, pineUpper, pineLower] = f_kc(close, 3, 4, false)
";

#[test]
fn kc_test() {
    use pine::ast::stat_expr_types::VarIndex;
    use pine::helper::pine_ref_to_f64_series;
    use pine::libs::{ema, kc, tr};
    use pine::runtime::NoneCallback;

    let lib_info = pine::LibInfo::new(
        vec![kc::declare_var(), ema::declare_ema_var(), tr::declare_var()],
        vec![
            ("close", SyntaxType::Series(SimpleSyntaxType::Float)),
            ("high", SyntaxType::Series(SimpleSyntaxType::Float)),
            ("low", SyntaxType::Series(SimpleSyntaxType::Float)),
        ],
    );
    let mut parser = pine::PineScript::new_with_libinfo(lib_info, Some(&NoneCallback()));
    parser.parse_src(String::from(KC_SCRIPT)).unwrap();
    let data = vec![
        (
            "close",
            AnySeries::from_float_vec(vec![Some(10f64), Some(20f64)]),
        ),
        (
            "high",
            AnySeries::from_float_vec(vec![Some(10f64), Some(20f64)]),
        ),
        (
            "low",
            AnySeries::from_float_vec(vec![Some(10f64), Some(20f64)]),
        ),
    ];

    assert!(parser.run_with_data(data, None).is_ok());

    let is_equal = |parser: &mut pine::PineScript, x, y| {
        let result1 = pine_ref_to_f64_series(parser.move_var(VarIndex::new(x, 0)));
        let result2 = pine_ref_to_f64_series(parser.move_var(VarIndex::new(y, 0)));
        println!("result {:?} {:?}", result1, result2);
        assert_eq!(
            result1.unwrap().index_value(1).unwrap().unwrap().floor(),
            result2.unwrap().index_value(1).unwrap().unwrap().floor()
        );
    };

    is_equal(&mut parser, 6, 10);
    is_equal(&mut parser, 7, 11);
    is_equal(&mut parser, 8, 12);
}

const KCW_SCRIPT: &str = "
m1 = kcw(close, 5, 4, true)

// the same on pine
f_kcw(src, length, mult, useTrueRange) =>
    float basis = ema(src, length)
    float range = (useTrueRange) ? tr : (high - low)
    float rangeEma = ema(range, length)
    
    ((basis + rangeEma * mult) - (basis - rangeEma * mult)) / basis

m2 = f_kcw(close, 5, 4, true)
";

#[test]
fn kcw_test() {
    use pine::ast::stat_expr_types::VarIndex;
    use pine::helper::pine_ref_to_f64_series;
    use pine::libs::{ema, kcw, tr};
    use pine::runtime::NoneCallback;

    let lib_info = pine::LibInfo::new(
        vec![
            kcw::declare_var(),
            ema::declare_ema_var(),
            tr::declare_var(),
        ],
        vec![
            ("close", SyntaxType::Series(SimpleSyntaxType::Float)),
            ("high", SyntaxType::Series(SimpleSyntaxType::Float)),
            ("low", SyntaxType::Series(SimpleSyntaxType::Float)),
        ],
    );
    let mut parser = pine::PineScript::new_with_libinfo(lib_info, Some(&NoneCallback()));
    parser.parse_src(String::from(KCW_SCRIPT)).unwrap();
    let data = vec![
        (
            "close",
            AnySeries::from_float_vec(vec![Some(10f64), Some(20f64)]),
        ),
        (
            "high",
            AnySeries::from_float_vec(vec![Some(10f64), Some(20f64)]),
        ),
        (
            "low",
            AnySeries::from_float_vec(vec![Some(10f64), Some(20f64)]),
        ),
    ];

    assert!(parser.run_with_data(data, None).is_ok());

    let is_equal = |parser: &mut pine::PineScript, x, y| {
        let result1 = pine_ref_to_f64_series(parser.move_var(VarIndex::new(x, 0)));
        let result2 = pine_ref_to_f64_series(parser.move_var(VarIndex::new(y, 0)));
        println!("result {:?} {:?}", result1, result2);
        assert_eq!(
            result1.unwrap().index_value(1).unwrap().unwrap().floor(),
            result2.unwrap().index_value(1).unwrap().unwrap().floor()
        );
    };

    is_equal(&mut parser, 6, 8);
}

const MACD_EXAMPLE_SCRIPT: &str = "
[macdLine, signalLine, histLine] = macd(close, 12, 26, 9)
 
[_, signalLine2, _] = macd(close, 12, 26, 9)
_ = 12
";

#[test]
fn macd_example_test() {
    use pine::ast::stat_expr_types::VarIndex;
    use pine::helper::pine_ref_to_f64_series;
    use pine::libs::macd;
    use pine::runtime::NoneCallback;

    let lib_info = pine::LibInfo::new(
        vec![macd::declare_var()],
        vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
    );
    let mut parser = pine::PineScript::new_with_libinfo(lib_info, Some(&NoneCallback()));
    parser.parse_src(String::from(MACD_EXAMPLE_SCRIPT)).unwrap();
    let data = vec![(
        "close",
        AnySeries::from_float_vec(vec![Some(10f64), Some(20f64)]),
    )];

    assert!(parser.run_with_data(data, None).is_ok());

    let is_equal = |parser: &mut pine::PineScript, x, y| {
        let result1 = pine_ref_to_f64_series(parser.move_var(VarIndex::new(x, 0)));
        let result2 = pine_ref_to_f64_series(parser.move_var(VarIndex::new(y, 0)));
        println!("result {:?} {:?}", result1, result2);
        assert_eq!(
            result1.unwrap().index_value(1).unwrap().unwrap().floor(),
            result2.unwrap().index_value(1).unwrap().unwrap().floor()
        );
    };

    is_equal(&mut parser, 3, 5);
}

const RSI_SCRIPT: &str = "
m1 = rsi(close, 2)

// same on pine, but less efficient
pine_rsi(x, y) => 
    u = max(x - x[1], 0) // upward change
    d = max(x[1] - x, 0) // downward change
    rs = rma(u, y) / rma(d, y)
    res = 100 - 100 / (1 + rs)
    rs

m2 = pine_rsi(close, 2)
";

#[test]
fn rsi_test() {
    use pine::ast::stat_expr_types::VarIndex;
    use pine::helper::pine_ref_to_f64_series;
    use pine::libs::{ema, max, rsi};
    use pine::runtime::NoneCallback;

    let lib_info = pine::LibInfo::new(
        vec![
            ema::declare_rma_var(),
            max::declare_max_var(),
            rsi::declare_var(),
        ],
        vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
    );
    let mut parser = pine::PineScript::new_with_libinfo(lib_info, Some(&NoneCallback()));
    parser.parse_src(String::from(RSI_SCRIPT)).unwrap();
    let data = vec![(
        "close",
        AnySeries::from_float_vec(vec![Some(20f64), Some(10f64), Some(5f64)]),
    )];

    assert!(parser.run_with_data(data, None).is_ok());

    let is_equal = |parser: &mut pine::PineScript, x, y| {
        let result1 = pine_ref_to_f64_series(parser.move_var(VarIndex::new(x, 0)));
        let result2 = pine_ref_to_f64_series(parser.move_var(VarIndex::new(y, 0)));
        println!("result {:?} {:?}", result1, result2);
        assert_eq!(
            result1.unwrap().index_value(1).unwrap().unwrap().floor(),
            result2.unwrap().index_value(1).unwrap().unwrap().floor()
        );
    };

    is_equal(&mut parser, 4, 6);
}

const MFI_SCRIPT: &str = "
m1 = mfi(close, 2)

// the same on pine
f_mfi(src, length) =>
    float upper = sum(volume * (change(src) <= 0.0 ? 0.0 : src), length)
    float lower = sum(volume * (change(src) >= 0.0 ? 0.0 : src), length)
    
    if na(lower)
        float res = na
        res
    else
        rsi(upper, int(lower))

m2 = f_mfi(close, 2)
";

#[test]
fn mfi_test() {
    use pine::ast::stat_expr_types::VarIndex;
    use pine::helper::pine_ref_to_f64_series;
    use pine::libs::{change, mfi, na, rsi, sum};
    use pine::runtime::NoneCallback;

    let lib_info = pine::LibInfo::new(
        vec![
            change::declare_change_var(),
            mfi::declare_var(),
            na::declare_var(),
            rsi::declare_var(),
            sum::declare_var(),
        ],
        vec![
            ("close", SyntaxType::Series(SimpleSyntaxType::Float)),
            ("volume", SyntaxType::Series(SimpleSyntaxType::Int)),
        ],
    );
    let mut parser = pine::PineScript::new_with_libinfo(lib_info, Some(&NoneCallback()));
    parser.parse_src(String::from(MFI_SCRIPT)).unwrap();
    let data = vec![
        (
            "close",
            AnySeries::from_float_vec(vec![Some(20f64), Some(10f64), Some(5f64)]),
        ),
        (
            "volume",
            AnySeries::from_int_vec(vec![Some(1i64), Some(1i64), Some(1i64)]),
        ),
    ];

    assert!(parser.run_with_data(data, None).is_ok());

    let is_equal = |parser: &mut pine::PineScript, x, y| {
        let result1 = pine_ref_to_f64_series(parser.move_var(VarIndex::new(x, 0)));
        let result2 = pine_ref_to_f64_series(parser.move_var(VarIndex::new(y, 0)));
        assert_eq!(
            result1.unwrap().index_value(1).unwrap(),
            result2.unwrap().index_value(1).unwrap()
        );
    };

    is_equal(&mut parser, 7, 9);
}

const SWMA_SCRIPT: &'static str = "
m1 = vwma(close, 15)

// same on pine, but less efficient
pine_vwma(x, y) =>
    sma(x * volume, y) / sma(volume, y)
m2 = pine_vwma(close, 15)
";

#[test]
fn swma_test() {
    use pine::ast::stat_expr_types::VarIndex;
    use pine::helper::pine_ref_to_f64_series;
    use pine::libs::{sma, vwma};
    use pine::runtime::NoneCallback;

    let lib_info = pine::LibInfo::new(
        vec![vwma::declare_var(), sma::declare_sma_var()],
        vec![
            ("close", SyntaxType::Series(SimpleSyntaxType::Float)),
            ("volume", SyntaxType::Series(SimpleSyntaxType::Int)),
        ],
    );
    let mut parser = pine::PineScript::new_with_libinfo(lib_info, Some(&NoneCallback()));
    parser.parse_src(String::from(SWMA_SCRIPT)).unwrap();
    let data = vec![
        (
            "close",
            AnySeries::from_float_vec(vec![Some(20f64), Some(10f64), Some(5f64), Some(10f64)]),
        ),
        (
            "volume",
            AnySeries::from_int_vec(vec![Some(1i64), Some(1i64), Some(1i64), Some(1i64)]),
        ),
    ];

    assert!(parser.run_with_data(data, None).is_ok());

    let is_equal = |parser: &mut pine::PineScript, x, y| {
        let result1 = pine_ref_to_f64_series(parser.move_var(VarIndex::new(x, 0)));
        let result2 = pine_ref_to_f64_series(parser.move_var(VarIndex::new(y, 0)));
        assert_eq!(
            result1.unwrap().index_value(1).unwrap(),
            result2.unwrap().index_value(1).unwrap()
        );
    };

    is_equal(&mut parser, 4, 6);
}

const MYPLOT_SCRIPT: &'static str = "
// Plot colors
col_grow_above = #26A69A
col_grow_below = #FFCDD2
col_fall_above = #B2DFDB
col_fall_below = #EF5350

plot(close, title='Histogram', 
    style=plot.style_columns, 
    color=(close>=0 ? 
        (close[1] < close ? col_grow_above : col_fall_above) : 
        (close[1] < close ? col_grow_below : col_fall_below) ), 
    transp=0)
";

#[test]
fn myplot_test() {
    use pine::libs::plot;
    use pine::runtime::NoneCallback;

    let lib_info = pine::LibInfo::new(
        vec![plot::declare_var()],
        vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
    );
    let mut parser = pine::PineScript::new_with_libinfo(lib_info, Some(&NoneCallback()));
    parser.parse_src(String::from(MYPLOT_SCRIPT)).unwrap();
    let data = vec![(
        "close",
        AnySeries::from_float_vec(vec![Some(20f64), Some(10f64), Some(5f64), Some(10f64)]),
    )];

    assert!(parser.run_with_data(data, None).is_ok());
}

const INPUT_SOURCE_SCRIPT: &'static str = "
src = input(title='Source', type=input.source, defval=close)
";

#[test]
fn input_source_test() {
    use pine::libs::input;
    use pine::runtime::output::InputVal;
    use pine::runtime::NoneCallback;

    let lib_info = pine::LibInfo::new(
        vec![input::declare_var()],
        vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
    );
    let mut parser = pine::PineScript::new_with_libinfo(lib_info, Some(&NoneCallback()));
    parser.parse_src(String::from(INPUT_SOURCE_SCRIPT)).unwrap();
    let data = vec![(
        "close",
        AnySeries::from_float_vec(vec![Some(20f64), Some(10f64), Some(5f64), Some(10f64)]),
    )];

    assert!(parser
        .run(
            vec![Some(InputVal::Source(String::from("close")))],
            data,
            None
        )
        .is_ok());
}

const SRC_SCRIPT: &'static str = "
m = input(1, 'hello', 'int')
plot(close + m)
plot(close)
plot(high)
plot(high)
plot(open)
plot(low)
plot(time)
plot(volume)
plot(bar_index)
";

#[test]
fn inpur_srcs_test() {
    use pine::libs::{input, plot, time};
    use pine::runtime::output::InputSrc;
    use pine::runtime::NoneCallback;

    let lib_info = pine::LibInfo::new(
        vec![
            input::declare_var(),
            plot::declare_var(),
            time::declare_var(),
        ],
        vec![
            ("close", SyntaxType::Series(SimpleSyntaxType::Float)),
            ("high", SyntaxType::Series(SimpleSyntaxType::Float)),
            ("low", SyntaxType::Series(SimpleSyntaxType::Float)),
            ("open", SyntaxType::Series(SimpleSyntaxType::Float)),
            ("volume", SyntaxType::Series(SimpleSyntaxType::Int)),
            ("_time", SyntaxType::Series(SimpleSyntaxType::Int)),
            ("bar_index", SyntaxType::Series(SimpleSyntaxType::Int)),
        ],
    );
    let mut parser = pine::PineScript::new_with_libinfo(lib_info, Some(&NoneCallback()));
    parser.parse_src(String::from(SRC_SCRIPT)).unwrap();
    let io_info = parser.gen_io_info().unwrap();
    assert_eq!(
        io_info.get_input_srcs(),
        &vec![InputSrc::new(
            None,
            vec![
                String::from("close"),
                String::from("high"),
                String::from("open"),
                String::from("low"),
                String::from("time"),
                String::from("volume"),
            ]
        )]
    );
}

const STUDY_ONLY_SCRIPT: &'static str = r#"
study(title="CCC", shorttitle="CCC")
"#;

#[test]
fn study_only_test() {
    use pine::libs::study;
    use pine::runtime::output::{ScriptPurpose, StudyScript};
    use pine::runtime::NoneCallback;

    let lib_info = pine::LibInfo::new(
        vec![study::declare_var()],
        vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
    );
    let mut parser = pine::PineScript::new_with_libinfo(lib_info, Some(&NoneCallback()));
    parser.parse_src(String::from(STUDY_ONLY_SCRIPT)).unwrap();
    let io_info = parser.gen_io_info().unwrap();
    assert_eq!(
        io_info.get_script_type(),
        &Some(ScriptPurpose::Study(StudyScript {
            title: String::from("CCC"),
            shorttitle: Some(String::from("CCC")),
            overlay: None,
            format: None,
            precision: None
        }))
    );

    // let data = vec![("close", AnySeries::from_float_vec(vec![Some(20f64)]))];
    println!("res {:?}", parser.run(vec![], vec![], None));
    assert!(parser.run(vec![], vec![], None).is_ok());
}

const RUN1_SCRIPT: &'static str = r#"
study(title="VWAPG", shorttitle="VWAPG")
src = (high + low + open)/3
t = time("D")
start = na(t[1]) or t > t[1]

sumSrc = src * volume
sumVol = volume
sumSrc := start ? sumSrc : sumSrc + sumSrc[1]
sumVol := start ? sumVol : sumVol + sumVol[1]

// You can use built-in vwap() function instead.
plot(sumSrc / sumVol, title="VWAPG", color=color.blue)
"#;

#[test]
fn run1_test() {
    use pine::libs::{color, na, plot, study, time};
    use pine::runtime::output::{InputSrc, ScriptPurpose, StudyScript};
    use pine::runtime::NoneCallback;

    let lib_info = pine::LibInfo::new(
        vec![
            study::declare_var(),
            color::declare_var(),
            na::declare_var(),
            plot::declare_var(),
            time::declare_var(),
        ],
        vec![
            ("high", SyntaxType::Series(SimpleSyntaxType::Float)),
            ("low", SyntaxType::Series(SimpleSyntaxType::Float)),
            ("open", SyntaxType::Series(SimpleSyntaxType::Float)),
            ("volume", SyntaxType::Series(SimpleSyntaxType::Int)),
            ("_time", SyntaxType::Series(SimpleSyntaxType::Int)),
        ],
    );
    let mut parser = pine::PineScript::new_with_libinfo(lib_info, Some(&NoneCallback()));
    parser.parse_src(String::from(RUN1_SCRIPT)).unwrap();
    let io_info = parser.gen_io_info().unwrap();
    println!("io info {:?}", io_info);
    assert_eq!(
        io_info.get_input_srcs(),
        &vec![InputSrc::new(
            None,
            vec![
                String::from("high"),
                String::from("low"),
                String::from("open"),
                String::from("time"),
                String::from("volume")
            ]
        )]
    );

    let data = vec![
        ("high", AnySeries::from_float_vec(vec![Some(20f64)])),
        ("low", AnySeries::from_float_vec(vec![Some(20f64)])),
        ("open", AnySeries::from_float_vec(vec![Some(20f64)])),
        ("time", AnySeries::from_int_vec(vec![Some(20i64)])),
        ("volume", AnySeries::from_int_vec(vec![Some(20i64)])),
    ];
    // println!("res {:?}", parser.run(vec![], vec![], None));
    assert!(parser.run_with_data(data, None).is_ok());
}
