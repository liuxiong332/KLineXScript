extern crate pine;
use pine::ast::syntax_type::{SimpleSyntaxType, SyntaxType};
use pine::libs::plot;
use pine::libs::print;
use pine::runtime::data_src::{Callback, DataSrc, NoneCallback};
use pine::runtime::output::OutputData;
use pine::runtime::AnySeries;

const MA_SCRIPT: &str = "
N = 5
ma = close
// ma = (close + close[1] + close[2] + close[3] + close[4]) / 5
for i = 1 to N - 1
    ma := ma + close[i] 
ma := ma / N
plot(ma)
";

#[test]
fn datasrc_test() {
    let lib_info = pine::LibInfo::new(
        vec![plot::declare_var()],
        vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
    );
    let mut parser = pine::PineScript::new_with_libinfo(lib_info, Some(&NoneCallback()));
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
    let out_data = parser.run_with_data(data, None);
    assert_eq!(
        out_data.unwrap().data_list[0],
        Some(OutputData::new(vec![vec![
            None,
            None,
            None,
            None,
            Some(3f64)
        ]]))
    );
    // assert!(parser.run_with_data(data, None).is_ok());
}

const FUNC_SCRIPT: &str = "
pine_ema(x, y) =>
    sum = 0.0
    sum := x + (y * sum[1] ? y * sum[1] : 0)
    sum
plot(pine_ema(close, 2))
";

#[test]
fn func_call_test() {
    let lib_info = pine::LibInfo::new(
        vec![plot::declare_var()],
        vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
    );
    let mut parser = pine::PineScript::new_with_libinfo(lib_info, Some(&NoneCallback()));
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
    // assert!(parser.run_with_data(data, None).is_ok());
    let out_data = parser.run_with_data(data, None);
    assert_eq!(
        out_data.unwrap().data_list[0],
        Some(OutputData::new(vec![vec![
            Some(2f64),
            Some(8f64),
            Some(24f64),
            Some(64f64),
            Some(160f64)
        ]]))
    );
}

const IF_ELSE_SCRIPT: &str = "
m = if close > open
    s = close
    s[1]
else
    t = open
    t[1]
plot(m)
";

#[test]
fn if_else_test() {
    let lib_info = pine::LibInfo::new(
        vec![plot::declare_var()],
        vec![
            ("close", SyntaxType::Series(SimpleSyntaxType::Float)),
            ("open", SyntaxType::Series(SimpleSyntaxType::Float)),
        ],
    );
    let mut parser = pine::PineScript::new_with_libinfo(lib_info, Some(&NoneCallback()));
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
    // assert!(parser.run_with_data(data, None).is_ok());
    let out_data = parser.run_with_data(data, None);
    assert_eq!(
        out_data.unwrap().data_list[0],
        Some(OutputData::new(vec![vec![
            None,
            None,
            Some(1f64),
            Some(4f64),
            Some(5f64)
        ]]))
    );
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
    let lib_info = pine::LibInfo::new(
        vec![print::declare_var()],
        vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
    );
    let mut parser = pine::PineScript::new_with_libinfo(lib_info, Some(&NoneCallback()));
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
plot(pine_ema(close, 3.0))
";

#[test]
fn ema_test() {
    let lib_info = pine::LibInfo::new(
        vec![plot::declare_var()],
        vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
    );
    let mut parser = pine::PineScript::new_with_libinfo(lib_info, Some(&NoneCallback()));
    parser.parse_src(String::from(EMA_SCRIPT)).unwrap();
    let data = vec![(
        "close",
        AnySeries::from_float_vec(vec![Some(2f64), Some(4f64)]),
    )];

    // assert!(parser.run_with_data(data, None).is_ok());
    let out_data = parser.run_with_data(data, None);
    assert_eq!(
        out_data.unwrap().data_list[0],
        Some(OutputData::new(vec![vec![Some(1f64), Some(2.5f64),]]))
    );
}

const MACD_SCRIPT: &str = "
pine_ema(x, y) =>
    alpha = 2 / (y + 1)
    sum = 0.0
    sum := alpha * x + (1 - alpha) * (sum[1] ? sum[1] : 0)
    sum

pine_macd(fastlen, slowlen, siglen) =>
    // DIF=EMA_{{(close,12)}}-EMA_{{(close,26)}}
    dif = ema(close, fastlen) - ema(close, slowlen)
    // DEM=EMA_{{(DIF,9)}}
    dem = ema(dif, siglen)
    //OSC=DIF-DEM=DIF-MACD
    osc = dif - dem
    [dif, dem, osc]

[m1, m2, m3] = pine_macd(3, 7, 3)
plot(m1)
plot(m2)
plot(m3)

[macdLine, signalLine, histLine] = macd(close, 3, 7, 3)
plot(macdLine)
plot(signalLine)
plot(histLine)
";

#[test]
fn macd_test() {
    use pine::libs::{ema, macd};
    let lib_info = pine::LibInfo::new(
        vec![
            plot::declare_var(),
            macd::declare_var(),
            ema::declare_ema_var(),
        ],
        vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
    );
    let mut parser = pine::PineScript::new_with_libinfo(lib_info, Some(&NoneCallback()));
    parser.parse_src(String::from(MACD_SCRIPT)).unwrap();
    let data = vec![(
        "close",
        AnySeries::from_float_vec(vec![Some(2f64), Some(4f64), Some(1f64), Some(12f64)]),
    )];

    // assert!(parser.run_with_data(data, None).is_ok());
    let out_data = parser.run_with_data(data, None);
    // assert_eq!(
    //     out_data.as_ref().unwrap().data_list[0],
    //     Some(OutputData::new(vec![vec![
    //         Some(12.763532763532766f64),
    //         Some(32.82882444136006f64),
    //     ]]))
    // );
    println!(
        "get data {:?} {:?}",
        out_data.as_ref().unwrap().data_list[0],
        out_data.as_ref().unwrap().data_list[3]
    );
    assert_eq!(
        out_data.as_ref().unwrap().data_list[0],
        out_data.as_ref().unwrap().data_list[3],
    );
    assert_eq!(
        out_data.as_ref().unwrap().data_list[1],
        out_data.as_ref().unwrap().data_list[4],
    );
    assert_eq!(
        out_data.as_ref().unwrap().data_list[2],
        out_data.as_ref().unwrap().data_list[5],
    );
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
        .move_var(VarIndex::new(0, 0));
    let result2 = parser
        .get_runner()
        .get_context()
        .move_var(VarIndex::new(2, 0));
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
        .move_var(VarIndex::new(0, 0));
    let result2 = parser
        .get_runner()
        .get_context()
        .move_var(VarIndex::new(2, 0));
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

    let middle = pine_ref_to_f64_series(parser.move_var(VarIndex::new(0, 0)));
    let upper = pine_ref_to_f64_series(parser.move_var(VarIndex::new(1, 0)));
    let lower = pine_ref_to_f64_series(parser.move_var(VarIndex::new(2, 0)));

    let pine_middle = pine_ref_to_f64_series(parser.move_var(VarIndex::new(4, 0)));
    let pine_upper = pine_ref_to_f64_series(parser.move_var(VarIndex::new(5, 0)));
    let pine_lower = pine_ref_to_f64_series(parser.move_var(VarIndex::new(6, 0)));
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

    let result1 = pine_ref_to_f64_series(parser.move_var(VarIndex::new(0, 0)));
    let result2 = pine_ref_to_f64_series(parser.move_var(VarIndex::new(2, 0)));

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

    let result1 = pine_ref_to_f64_series(parser.move_var(VarIndex::new(0, 0)));
    let result2 = pine_ref_to_f64_series(parser.move_var(VarIndex::new(2, 0)));

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

    is_equal(&mut parser, 0, 4);
    is_equal(&mut parser, 1, 5);
    is_equal(&mut parser, 2, 6);
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

    is_equal(&mut parser, 0, 2);
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

    is_equal(&mut parser, 1, 3);
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

    is_equal(&mut parser, 0, 2);
}

const MY_RSI_SCRIPT: &str = r#"
src = close
len = 14
up = rma(max(change(src), 0), len)
down = rma(-min(change(src), 0), len)
rsia = down == 0 ? 100 : up == 0 ? 0 : 100 - (100 / (1 + up / down))
"#;

#[test]
fn myrsi_test() {
    use pine::ast::stat_expr_types::VarIndex;
    use pine::helper::pine_ref_to_f64_series;
    use pine::libs::{change, ema, max, rsi};
    use pine::runtime::NoneCallback;

    let lib_info = pine::LibInfo::new(
        vec![
            change::declare_change_var(),
            max::declare_max_var(),
            max::declare_min_var(),
            ema::declare_rma_var(),
        ],
        vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
    );
    let mut parser = pine::PineScript::new_with_libinfo(lib_info, Some(&NoneCallback()));
    parser.parse_src(String::from(MY_RSI_SCRIPT)).unwrap();
    let data = vec![(
        "close",
        AnySeries::from_float_vec(vec![None, None, None, None, Some(0f64), Some(0f64)]),
    )];

    assert!(parser.run_with_data(data, None).is_ok());
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
        rsi(upper, lower)

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

    is_equal(&mut parser, 0, 2);
}

const MFI2_SCRIPT: &str = r#"
study(title="Money Flow", shorttitle="MFI", format=format.price, precision=2)
length = input(title="Length", type=input.integer, defval=14, minval=1, maxval=2000)
src = hlc3
upper = sum(volume * (change(src) <= 0 ? 0 : src), length)
lower = sum(volume * (change(src) >= 0 ? 0 : src), length)

pine_rsi(x, y) => 
    // u = max(x - x[1],   0) // upward change
    // d = max( y[1] - y, 0) // downward change
    rs = x / y // rma(u, 2) / rma(d, 2)
    res = 100 - 100 / (1 + rs)
    res

mf = rsi(upper, lower)
mf2 = pine_rsi(upper, lower)
plot(mf)
plot(mf2)
"#;

#[test]
fn mfi2_test() {
    use pine::ast::stat_expr_types::VarIndex;
    use pine::helper::pine_ref_to_f64_series;
    use pine::libs::{change, format, hlc3, input, mfi, na, plot, rsi, study, sum};
    use pine::runtime::NoneCallback;

    let lib_info = pine::LibInfo::new(
        vec![
            study::declare_var(),
            format::declare_var(),
            input::declare_var(),
            hlc3::declare_var(),
            change::declare_change_var(),
            mfi::declare_var(),
            na::declare_var(),
            rsi::declare_var(),
            sum::declare_var(),
            plot::declare_var(),
        ],
        vec![
            ("high", SyntaxType::Series(SimpleSyntaxType::Float)),
            ("low", SyntaxType::Series(SimpleSyntaxType::Float)),
            ("close", SyntaxType::Series(SimpleSyntaxType::Float)),
            ("volume", SyntaxType::Series(SimpleSyntaxType::Int)),
        ],
    );
    let mut parser = pine::PineScript::new_with_libinfo(lib_info, Some(&NoneCallback()));
    parser.parse_src(String::from(MFI2_SCRIPT)).unwrap();
    let data = vec![
        (
            "high",
            AnySeries::from_float_vec(vec![Some(30f64), Some(20f64), Some(1f64)]),
        ),
        (
            "low",
            AnySeries::from_float_vec(vec![Some(10f64), Some(10f64), Some(5f64)]),
        ),
        (
            "close",
            AnySeries::from_float_vec(vec![Some(20f64), Some(10f64), Some(5f64)]),
        ),
        (
            "volume",
            AnySeries::from_int_vec(vec![Some(1i64), Some(1i64), Some(1i64)]),
        ),
    ];

    let out_data = parser.run_with_data(data, None);
    assert!(out_data.is_ok());

    let data_list = out_data.unwrap().data_list;
    assert_eq!(
        data_list[0].as_ref().unwrap().series[0],
        data_list[1].as_ref().unwrap().series[0]
    );
}

const TSI_SCRIPT: &str = r#"
pine_tsi(x, s, l) => 
    v1 = ema(ema(x - x[1], l), s) 
    v2 = ema(ema(abs(x - x[1]), l), s) 
    v1 / v2

plot(tsi(close, 2, 2))
plot(pine_tsi(close, 2, 2))
"#;

#[test]
fn tsi_test() {
    use pine::libs::{abs, ema, plot, tsi};
    use pine::runtime::NoneCallback;

    let lib_info = pine::LibInfo::new(
        vec![
            tsi::declare_var(),
            ema::declare_ema_var(),
            abs::declare_var(),
            plot::declare_var(),
        ],
        vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
    );
    let mut parser = pine::PineScript::new_with_libinfo(lib_info, Some(&NoneCallback()));
    parser.parse_src(String::from(TSI_SCRIPT)).unwrap();
    let data = vec![(
        "close",
        AnySeries::from_float_vec(vec![Some(20f64), Some(10f64), Some(5f64)]),
    )];

    let out_data = parser.run_with_data(data, None);
    assert!(out_data.is_ok());

    let data_list = out_data.unwrap().data_list;
    assert_eq!(
        data_list[0].as_ref().unwrap().series[0],
        data_list[1].as_ref().unwrap().series[0]
    );
}

const STOCH_SCRIPT: &str = r#"
pine_stoch(close, high, low, length) => 
    100 * (close - lowest(low, length)) / (highest(high, length) - lowest(low, length))

plot(stoch(close, high, low, 2))
plot(pine_stoch(close, high, low, 2))
"#;

#[test]
fn stoch_test() {
    use pine::libs::{highest, lowest, plot, stoch};
    use pine::runtime::NoneCallback;

    let lib_info = pine::LibInfo::new(
        vec![
            stoch::declare_var(),
            lowest::declare_var(),
            highest::declare_var(),
            plot::declare_var(),
        ],
        vec![
            ("close", SyntaxType::Series(SimpleSyntaxType::Float)),
            ("high", SyntaxType::Series(SimpleSyntaxType::Float)),
            ("low", SyntaxType::Series(SimpleSyntaxType::Float)),
        ],
    );
    let mut parser = pine::PineScript::new_with_libinfo(lib_info, Some(&NoneCallback()));
    parser.parse_src(String::from(STOCH_SCRIPT)).unwrap();
    let data = vec![
        (
            "close",
            AnySeries::from_float_vec(vec![Some(20f64), Some(10f64), Some(5f64)]),
        ),
        (
            "high",
            AnySeries::from_float_vec(vec![Some(30f64), Some(10f64), Some(10f64)]),
        ),
        (
            "low",
            AnySeries::from_float_vec(vec![Some(10f64), Some(5f64), Some(5f64)]),
        ),
    ];

    let out_data = parser.run_with_data(data, None);
    assert!(out_data.is_ok());

    let data_list = out_data.unwrap().data_list;
    assert_eq!(
        data_list[0].as_ref().unwrap().series[0],
        data_list[1].as_ref().unwrap().series[0]
    );
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

    is_equal(&mut parser, 0, 2);
}

const DMI_SCRIPT: &'static str = r#"
adxlen = 2
dilen = 2

dirmov(len) =>
	up = change(high)
	down = -change(low)
	truerange = rma(tr, len)
	plus = fixnan(100 * rma(up > down and up > 0 ? up : 0, len) / truerange)
	minus = fixnan(100 * rma(down > up and down > 0 ? down : 0, len) / truerange)
    [plus, minus]

myadx(dilen, adxlen) => 
	[plus, minus] = dirmov(dilen)
	sum = plus + minus
	adx = 100 * rma(abs(plus - minus) / (sum == 0 ? 1 : sum), adxlen)
	[plus, minus, adx]

[sig, up, down] = myadx(dilen, adxlen)
 
[diplus, diminus, adx] = dmi(dilen, adxlen)
"#;

#[test]
fn dmi_test() {
    use pine::ast::stat_expr_types::VarIndex;
    use pine::helper::pine_ref_to_f64_series;
    use pine::libs::{abs, change, dmi, ema, fixnan, plot, tr};
    use pine::runtime::NoneCallback;

    let lib_info = pine::LibInfo::new(
        vec![
            change::declare_change_var(),
            ema::declare_rma_var(),
            tr::declare_var(),
            fixnan::declare_var(),
            abs::declare_var(),
            dmi::declare_var(),
            plot::declare_var(),
        ],
        vec![
            ("close", SyntaxType::Series(SimpleSyntaxType::Float)),
            ("high", SyntaxType::Series(SimpleSyntaxType::Float)),
            ("low", SyntaxType::Series(SimpleSyntaxType::Float)),
        ],
    );
    let mut parser = pine::PineScript::new_with_libinfo(lib_info, Some(&NoneCallback()));
    parser.parse_src(String::from(DMI_SCRIPT)).unwrap();
    let data = vec![
        (
            "close",
            AnySeries::from_float_vec(vec![Some(20f64), Some(10f64), Some(5f64), Some(10f64)]),
        ),
        (
            "high",
            AnySeries::from_float_vec(vec![Some(30f64), Some(10f64), Some(10f64), Some(20f64)]),
        ),
        (
            "low",
            AnySeries::from_float_vec(vec![Some(10f64), Some(10f64), Some(5f64), Some(8f64)]),
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

    is_equal(&mut parser, 4, 7);
    is_equal(&mut parser, 5, 8);
    is_equal(&mut parser, 6, 9);
}

const HMA_SCRIPT: &'static str = r#"
src = close
length = 2
hmaBuildIn = hma(src, length)

// X=2*WMA(C,ROUND(N/2))-WMA(C,N);
// HULLMA=WMA(X,ROUND(SQRT(N)));
x = 2 * wma(src, floor(length / 2)) - wma(src, length)
hullma = wma(x, round(sqrt(length)))
"#;

#[test]
fn hma_test() {
    use pine::ast::stat_expr_types::VarIndex;
    use pine::helper::pine_ref_to_f64_series;
    use pine::libs::{ceil, cos, hma, sma};
    use pine::runtime::NoneCallback;

    let lib_info = pine::LibInfo::new(
        vec![
            hma::declare_var(),
            sma::declare_wma_var(),
            ceil::declare_round_var(),
            ceil::declare_floor_var(),
            cos::declare_sqrt_var(),
        ],
        vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
    );
    let mut parser = pine::PineScript::new_with_libinfo(lib_info, Some(&NoneCallback()));
    parser.parse_src(String::from(HMA_SCRIPT)).unwrap();
    let data = vec![(
        "close",
        AnySeries::from_float_vec(vec![Some(20f64), Some(10f64), Some(5f64), Some(10f64)]),
    )];

    assert!(parser.run_with_data(data, None).is_ok());

    let is_equal = |parser: &mut pine::PineScript, x, y| {
        let result1 = pine_ref_to_f64_series(parser.move_var(VarIndex::new(x, 0)));
        let result2 = pine_ref_to_f64_series(parser.move_var(VarIndex::new(y, 0)));
        assert_eq!(
            result1.unwrap().index_value(1).unwrap(),
            result2.unwrap().index_value(1).unwrap()
        );
    };
    is_equal(&mut parser, 2, 4);
}

const STDEV_SCRIPT: &str = "
plot(stdev(close, 2)) 
pstdev(Series, Period) =>
    mean = sum(Series, Period) / Period
    summation = 0.0
    for i=0 to Period-1
        sampleMinusMean = nz(Series[i]) - mean
        summation := summation + sampleMinusMean * sampleMinusMean
    sqrt(summation / Period)
plot(pstdev(close, 2)) 
";

#[test]
fn stdev_test() {
    use pine::libs::{abs, cos, ema, nz, plot, sma, sum};
    use pine::runtime::NoneCallback;

    let lib_info = pine::LibInfo::new(
        vec![
            sma::declare_stdev_var(),
            sum::declare_var(),
            nz::declare_var(),
            cos::declare_sqrt_var(),
            plot::declare_var(),
        ],
        vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
    );
    let mut parser = pine::PineScript::new_with_libinfo(lib_info, Some(&NoneCallback()));
    parser.parse_src(String::from(STDEV_SCRIPT)).unwrap();
    let data = vec![(
        "close",
        AnySeries::from_float_vec(vec![Some(20f64), Some(10f64), Some(5f64)]),
    )];

    let out_data = parser.run_with_data(data, None);
    assert!(out_data.is_ok());

    let data_list = out_data.unwrap().data_list;
    assert_eq!(
        data_list[0].as_ref().unwrap().series[0].last(),
        data_list[1].as_ref().unwrap().series[0].last()
    );
}

const CCI_SCRIPT: &str = "
length = 2
src = close
cci1 = (src - sma(src, length)) / (0.015 * dev(src, length))
plot(cci1)
plot(cci(src, length)) 
";

#[test]
fn cci_test() {
    use pine::libs::{cci, plot, sma};
    use pine::runtime::NoneCallback;

    let lib_info = pine::LibInfo::new(
        vec![
            sma::declare_dev_var(),
            sma::declare_sma_var(),
            cci::declare_var(),
            plot::declare_var(),
        ],
        vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
    );
    let mut parser = pine::PineScript::new_with_libinfo(lib_info, Some(&NoneCallback()));
    parser.parse_src(String::from(CCI_SCRIPT)).unwrap();
    let data = vec![(
        "close",
        AnySeries::from_float_vec(vec![Some(20f64), Some(10f64), Some(5f64)]),
    )];

    let out_data = parser.run_with_data(data, None);
    assert!(out_data.is_ok());

    let data_list = out_data.unwrap().data_list;
    assert_eq!(
        data_list[0].as_ref().unwrap().series[0].last(),
        data_list[1].as_ref().unwrap().series[0].last()
    );
}

const CORRELATION_SCRIPT: &str = "
plot(correlation(close,open,2))

conv = sma(close * open, 2) - sma(close, 2) * sma(open, 2)
s = conv / (stdev(close, 2) * stdev(open, 2))
plot(s)
";

#[test]
fn correlation_test() {
    use pine::libs::{correlation, plot, sma};
    use pine::runtime::NoneCallback;

    let lib_info = pine::LibInfo::new(
        vec![
            sma::declare_stdev_var(),
            sma::declare_sma_var(),
            correlation::declare_var(),
            plot::declare_var(),
        ],
        vec![
            ("close", SyntaxType::Series(SimpleSyntaxType::Float)),
            ("open", SyntaxType::Series(SimpleSyntaxType::Float)),
        ],
    );
    let mut parser = pine::PineScript::new_with_libinfo(lib_info, Some(&NoneCallback()));
    parser.parse_src(String::from(CORRELATION_SCRIPT)).unwrap();
    let data = vec![
        (
            "close",
            AnySeries::from_float_vec(vec![Some(20f64), Some(10f64), Some(5f64)]),
        ),
        (
            "open",
            AnySeries::from_float_vec(vec![Some(10f64), Some(20f64), Some(5f64)]),
        ),
    ];

    let out_data = parser.run_with_data(data, None);
    assert!(out_data.is_ok());

    let data_list = out_data.unwrap().data_list;
    assert_eq!(
        data_list[0].as_ref().unwrap().series[0].last(),
        data_list[1].as_ref().unwrap().series[0].last()
    );
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
    opacity=0)
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

const VOLUME_SCRIPT: &'static str = r#"
plot(volume)
"#;

#[test]
fn volume_test() {
    use pine::libs::plot;
    use pine::runtime::output::{OutputData, ScriptPurpose, StudyScript};
    use pine::runtime::NoneCallback;

    let lib_info = pine::LibInfo::new(
        vec![plot::declare_var()],
        vec![("volume", SyntaxType::Series(SimpleSyntaxType::Int))],
    );
    let mut parser = pine::PineScript::new_with_libinfo(lib_info, Some(&NoneCallback()));
    parser.parse_src(String::from(VOLUME_SCRIPT)).unwrap();
    assert!(parser.gen_io_info().is_ok());
    // let data = vec![("close", AnySeries::from_float_vec(vec![Some(20f64)]))];
    println!("res {:?}", parser.run(vec![], vec![], None));
    let result = parser.run_with_data(
        vec![("volume", AnySeries::from_int_vec(vec![Some(20i64)]))],
        None,
    );
    assert!(result.is_ok());
    assert_eq!(
        result.unwrap().data_list,
        vec![Some(OutputData::new(vec![vec![Some(20.0f64)]]))]
    );
}

const TIME_SCRIPT: &'static str = r#"
int a1 = dayofmonth
int a2 = dayofweek
float a3 = dayofweek==dayofweek.monday ? 1.0 :2.0
float a4 = dayofweek==dayofweek.tuesday ? 1.0 :2.0
plot(a1)
plot(a2)
plot(a3)
plot(a4)
"#;

#[test]
fn dayofweek_test() {
    use pine::libs::{plot, year};
    use pine::runtime::output::{OutputData, ScriptPurpose, StudyScript, SymbolInfo};
    use pine::runtime::NoneCallback;
    use std::rc::Rc;

    let lib_info = pine::LibInfo::new(
        vec![
            plot::declare_var(),
            year::declare_dayofmonth_var(),
            year::declare_dayofweek_var(),
        ],
        vec![("_time", SyntaxType::Series(SimpleSyntaxType::Int))],
    );
    let mut parser = pine::PineScript::new_with_libinfo(lib_info, Some(&NoneCallback()));
    parser.parse_src(String::from(TIME_SCRIPT)).unwrap();
    assert!(parser.gen_io_info().is_ok());
    // let data = vec![("close", AnySeries::from_float_vec(vec![Some(20f64)]))];
    println!("res {:?}", parser.run(vec![], vec![], None));
    let result = parser.run_with_data(
        vec![(
            "_time",
            AnySeries::from_int_vec(vec![Some(1587978379382i64), Some(1588003200000)]),
        )],
        Some(Rc::new(SymbolInfo {
            symbol_type: String::from("future"),
            timezone: String::from("Asia/Shanghai"),
            ticker: String::from("BATS:MSFT"),
            session: String::from("regular"),
            trade_start: String::from(""),
            trade_end: String::from(""),
            root: Some(String::from("le")),
            currency: String::from("USD"),
            description: String::from("des"),
            mintick: 1f64,
        })),
    );
    assert!(result.is_ok());
    assert_eq!(
        result.unwrap().data_list,
        vec![
            Some(OutputData::new(vec![vec![Some(27f64), Some(28f64)]])),
            Some(OutputData::new(vec![vec![Some(2f64), Some(3f64)]])),
            Some(OutputData::new(vec![vec![Some(1f64), Some(2f64)]])),
            Some(OutputData::new(vec![vec![Some(2f64), Some(1f64)]]))
        ]
    );
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

const EMA2_SCRIPT: &str = r#"
out = ema(log(close), 2) 
plot(out, color=color.maroon, title="TRIX")
"#;

#[test]
fn ema2_test() {
    use pine::libs::{color, cos, ema};
    use pine::runtime::NoneCallback;

    let lib_info = pine::LibInfo::new(
        vec![
            cos::declare_log_var(),
            color::declare_var(),
            ema::declare_ema_var(),
            plot::declare_var(),
        ],
        vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
    );
    let mut parser = pine::PineScript::new_with_libinfo(lib_info, Some(&NoneCallback()));
    parser.parse_src(String::from(EMA2_SCRIPT)).unwrap();
    let data = vec![(
        "close",
        AnySeries::from_float_vec(vec![Some(100f64), Some(101f64), Some(102f64)]),
    )];

    let out_data = parser.run_with_data(
        vec![("close", AnySeries::from_float_vec(vec![Some(0f64)]))],
        None,
    );
    assert!(out_data.is_ok());

    let out_data = parser.run_with_data(data, None);
    println!("Now data {:?}", out_data);
    // assert!(out_data.is_err());
}

const OBV_SCRIPT: &'static str = r#"
src = close
obv = cum(sign(change(src)) * volume)
plot(obv, color=color.blue, title="OBV")
"#;

#[test]
fn obv_test() {
    use pine::libs::{change, color, cos, cum};
    use pine::runtime::NoneCallback;

    let lib_info = pine::LibInfo::new(
        vec![
            cos::declare_sign_var(),
            color::declare_var(),
            change::declare_change_var(),
            cum::declare_var(),
            plot::declare_var(),
        ],
        vec![
            ("close", SyntaxType::Series(SimpleSyntaxType::Float)),
            ("volume", SyntaxType::Series(SimpleSyntaxType::Int)),
        ],
    );
    let mut parser = pine::PineScript::new_with_libinfo(lib_info, Some(&NoneCallback()));
    parser.parse_src(String::from(OBV_SCRIPT)).unwrap();
    let data = vec![
        (
            "close",
            AnySeries::from_float_vec(vec![Some(100f64), Some(101f64), Some(100f64)]),
        ),
        (
            "volume",
            AnySeries::from_int_vec(vec![Some(1i64), Some(2i64), Some(3i64)]),
        ),
    ];
    // na 1 -1 -> na 1 -1 -> na 2 -3 -> na 2 -1

    let out_data = parser.run_with_data(data, None);
    assert!(out_data.is_ok());
    assert_eq!(
        out_data.unwrap().data_list,
        vec![Some(OutputData::new(vec![vec![
            Some(0f64),
            Some(2f64),
            Some(-1f64)
        ]]))]
    );
}

const WR_SCRIPT: &'static str = r#"
_pr(length) =>
    max = highest(length)
    min = lowest(length)
    100 * (close - max) / (max - min)
percentR = _pr(2)
plot(percentR, title="%R", color=#ff6d00, opacity=0)
"#;

#[test]
fn wr_test() {
    use pine::libs::{highest, lowest};
    use pine::runtime::NoneCallback;

    let lib_info = pine::LibInfo::new(
        vec![
            highest::declare_var(),
            lowest::declare_var(),
            plot::declare_var(),
        ],
        vec![
            ("close", SyntaxType::Series(SimpleSyntaxType::Float)),
            ("high", SyntaxType::Series(SimpleSyntaxType::Float)),
            ("low", SyntaxType::Series(SimpleSyntaxType::Float)),
        ],
    );
    let mut parser = pine::PineScript::new_with_libinfo(lib_info, Some(&NoneCallback()));
    parser.parse_src(String::from(WR_SCRIPT)).unwrap();
    let data = vec![
        (
            "close",
            AnySeries::from_float_vec(vec![Some(2f64), Some(4f64), Some(4f64)]),
        ),
        (
            "high",
            AnySeries::from_float_vec(vec![Some(4f64), Some(6f64), Some(8f64)]),
        ),
        (
            "low",
            AnySeries::from_float_vec(vec![Some(2f64), Some(4f64), Some(2f64)]),
        ),
    ];
    // 4 6 8, 2 2 2, -2 -2 -4, 2 4 6
    let out_data = parser.run_with_data(data, None);
    assert!(out_data.is_ok());
    assert_eq!(
        out_data.unwrap().data_list,
        vec![Some(OutputData::new(vec![vec![
            Some(-100f64),
            Some(-50f64),
            Some(-200f64 / 3f64)
        ]]))]
    );
}

const Chaikin_SCRIPT: &'static str = r#"
short = 2
long = 2
osc = ema(accdist, short) - ema(accdist, long)
plot(osc)
"#;

#[test]
fn chaikin_test() {
    use pine::libs::{accdist, ema};
    use pine::runtime::NoneCallback;

    let lib_info = pine::LibInfo::new(
        vec![
            ema::declare_ema_var(),
            accdist::declare_var(),
            plot::declare_var(),
        ],
        vec![
            ("close", SyntaxType::Series(SimpleSyntaxType::Float)),
            ("high", SyntaxType::Series(SimpleSyntaxType::Float)),
            ("low", SyntaxType::Series(SimpleSyntaxType::Float)),
            ("volume", SyntaxType::Series(SimpleSyntaxType::Int)),
        ],
    );
    let mut parser = pine::PineScript::new_with_libinfo(lib_info, Some(&NoneCallback()));
    parser.parse_src(String::from(Chaikin_SCRIPT)).unwrap();
    let data = vec![
        (
            "close",
            AnySeries::from_float_vec(vec![Some(2f64), Some(4f64), Some(4f64)]),
        ),
        (
            "high",
            AnySeries::from_float_vec(vec![Some(4f64), Some(6f64), Some(8f64)]),
        ),
        (
            "low",
            AnySeries::from_float_vec(vec![Some(2f64), Some(4f64), Some(2f64)]),
        ),
        (
            "volume",
            AnySeries::from_int_vec(vec![Some(2i64), Some(4i64), Some(2i64)]),
        ),
    ];
    // 4 6 8, 2 2 2, -2 -2 -4, 2 4 6
    assert!(parser.gen_io_info().is_ok());
    let out_data = parser.run_with_data(data, None);
    assert!(out_data.is_ok());
    println!("Out data {:?}", out_data.as_ref().unwrap().data_list);
    assert!(out_data.as_ref().unwrap().data_list[0]
        .as_ref()
        .unwrap()
        .series[0][0]
        .is_some());
}

const MYPLOT2_SCRIPT: &'static str = r#"
plot(open)
plot(close)
plot(close)
plot(close)
plot(close)
plot(close)
plot(close)
plot(close)
plot(close)
plot(close)
"#;

#[test]
fn myplot2_test() {
    use pine::runtime::NoneCallback;

    let lib_info = pine::LibInfo::new(
        vec![plot::declare_var()],
        vec![
            ("close", SyntaxType::Series(SimpleSyntaxType::Float)),
            ("open", SyntaxType::Series(SimpleSyntaxType::Float)),
        ],
    );
    let mut parser = pine::PineScript::new_with_libinfo(lib_info, Some(&NoneCallback()));
    parser.parse_src(String::from(MYPLOT2_SCRIPT)).unwrap();

    let mut closes: Vec<Option<f64>> = vec![];
    let mut openes: Vec<Option<f64>> = vec![];
    for m in 0..400 {
        closes.push(Some(100f64 + m as f64));
    }
    for m in 0..400 {
        openes.push(Some(100f64 + m as f64));
    }
    let data = vec![
        ("close", AnySeries::from_float_vec(closes)),
        ("open", AnySeries::from_float_vec(openes)),
    ];
    // 4 6 8, 2 2 2, -2 -2 -4, 2 4 6
    assert!(parser.gen_io_info().is_ok());

    for _ in 0..3 {
        let out_data = parser.run_with_data(data.clone(), None);
        assert!(out_data.is_ok());
        println!("Out data {:?}", out_data.as_ref().unwrap().data_list);
        for i in 0..10 {
            assert_eq!(
                out_data.as_ref().unwrap().data_list[i]
                    .as_ref()
                    .unwrap()
                    .series[0]
                    .len(),
                400
            );
        }
    }
}

const ALMA_TIME_SCRIPT: &'static str = r#"
plot(alma(dayofweek, 4, 0.85, 2.0))
plot(highest(dayofweek, 2))
"#;

#[test]
fn alma_time_test() {
    use pine::libs::{alma, highest, year};
    use pine::runtime::NoneCallback;

    let lib_info = pine::LibInfo::new(
        vec![
            plot::declare_var(),
            alma::declare_var(),
            year::declare_dayofweek_var(),
            highest::declare_var(),
        ],
        vec![("_time", SyntaxType::Series(SimpleSyntaxType::Int))],
    );
    let mut parser = pine::PineScript::new_with_libinfo(lib_info, Some(&NoneCallback()));
    parser.parse_src(String::from(ALMA_TIME_SCRIPT)).unwrap();

    let mut times: Vec<Option<i64>> = vec![];
    for m in 0..16 {
        times.push(Some(100i64 + m * 1000 * 3600 * 24 as i64));
    }
    let data = vec![("_time", AnySeries::from_int_vec(times))];
    assert!(parser.gen_io_info().is_ok());

    let out_data = parser.run_with_data(data.clone(), None);
    assert!(out_data.is_ok());
    println!("Out data {:?}", out_data.as_ref().unwrap().data_list);
    for i in 0..2 {
        assert!(out_data.as_ref().unwrap().data_list[i]
            .as_ref()
            .unwrap()
            .series[0][15]
            .is_some());
    }
}
