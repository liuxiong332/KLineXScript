extern crate pine;
use pine::ast::syntax_type::{SimpleSyntaxType, SyntaxType};
use pine::libs::declare_vars;
use pine::libs::plot;
use pine::libs::print;
use pine::runtime::data_src::{Callback, DataSrc, NoneCallback};
use pine::runtime::AnySeries;

const MACD_SCRIPT: &str = r#"
//@version=4
study(title="MACD", shorttitle="MACD")

// Getting inputs
fast_length = input(title="Fast Length", type=input.integer, defval=12)
slow_length = input(title="Slow Length", type=input.integer, defval=26)
src = input(title="Source", type=input.source, defval=close)
signal_length = input(title="Signal Smoothing", type=input.integer, minval = 1, maxval = 50, defval = 9)
sma_source = input(title="Simple MA(Oscillator)", type=input.bool, defval=false)
sma_signal = input(title="Simple MA(Signal Line)", type=input.bool, defval=false)

// Plot colors
col_grow_above = #26A69A
col_grow_below = #FFCDD2
col_fall_above = #B2DFDB
col_fall_below = #EF5350
col_macd = #0094ff
col_signal = #ff6a00

// Calculating
fast_ma = sma_source ? sma(src, fast_length) : ema(src, fast_length)
slow_ma = sma_source ? sma(src, slow_length) : ema(src, slow_length)
mymacd = fast_ma - slow_ma
signal = sma_signal ? sma(mymacd, signal_length) : ema(mymacd, signal_length)
hist = mymacd - signal

plot(hist, title="Histogram", style=plot.style_columns, color=(hist>=0 ? (hist[1] < hist ? col_grow_above : col_fall_above) : (hist[1] < hist ? col_grow_below : col_fall_below) ), transp=0 )
plot(mymacd, title="MACD", color=col_macd, transp=0)
plot(signal, title="Signal", color=col_signal, transp=0)
"#;

const AD_SCRIPTS: &'static str = r#"
//@version=4
study(title="Accumulation/Distribution", shorttitle="Accum/Dist", overlay=false)
ad = cum(close==high and close==low or high==low ? 0 : ((2*close-low-high)/(high-low))*volume)
plot(ad, title = "Accumulation/Distribution", color=color.olive)
"#;

const ADL_SCRIPTS: &'static str = r#"
//@version=4
study(title = "Advance Decline Line", shorttitle="ADL", precision=2)
sym(s) => security(s, timeframe.period, close)
difference = (sym("USI:ADVN.NY") - sym("USI:DECL.NY"))/(sym("USI:UNCH.NY") + 1)
adline = cum(difference > 0 ? sqrt(difference) : -sqrt(-difference))
plot(adline)
"#;

const ALMA_SCRIPTS: &'static str = r#"
//@version=4
study(title = "Arnaud Legoux Moving Average", shorttitle="ALMA", overlay=true)

source = close

windowsize = input(title="Window Size", type=input.integer, defval=9)
offset = input(title="Offset", type=input.float, defval=0.85)
sigma = input(title="Sigma", type=input.float, defval=6)

plot(alma(source, windowsize, offset, sigma))
"#;

#[test]
fn datasrc_test() {
    let lib_info = pine::LibInfo::new(
        declare_vars(),
        vec![
            ("close", SyntaxType::Series(SimpleSyntaxType::Float)),
            ("open", SyntaxType::Series(SimpleSyntaxType::Float)),
            ("high", SyntaxType::Series(SimpleSyntaxType::Float)),
            ("low", SyntaxType::Series(SimpleSyntaxType::Float)),
            ("volume", SyntaxType::Series(SimpleSyntaxType::Int)),
            ("_time", SyntaxType::Series(SimpleSyntaxType::Int)),
            ("bar_index", SyntaxType::Series(SimpleSyntaxType::Int)),
        ],
    );
    let mut parser = pine::PineScript::new_with_libinfo(lib_info, Some(&NoneCallback()));

    let data = vec![
        (
            "close",
            AnySeries::from_float_vec(vec![Some(1f64), Some(2f64)]),
        ),
        (
            "open",
            AnySeries::from_float_vec(vec![Some(1f64), Some(2f64)]),
        ),
        (
            "high",
            AnySeries::from_float_vec(vec![Some(1f64), Some(2f64)]),
        ),
        (
            "low",
            AnySeries::from_float_vec(vec![Some(1f64), Some(2f64)]),
        ),
        (
            "volume",
            AnySeries::from_int_vec(vec![Some(1i64), Some(2i64)]),
        ),
        (
            "_time",
            AnySeries::from_int_vec(vec![Some(100i64), Some(200i64)]),
        ),
    ];

    parser.parse_src(String::from(MACD_SCRIPT)).unwrap();
    assert!(parser.run_with_data(data.clone(), None).is_ok());

    parser.parse_src(String::from(AD_SCRIPTS)).unwrap();
    assert!(parser.run_with_data(data.clone(), None).is_ok());

    // parser.parse_src(String::from(ADL_SCRIPTS)).unwrap();
    // assert!(parser.run_with_data(data.clone(), None).is_ok());

    parser.parse_src(String::from(ALMA_SCRIPTS)).unwrap();
    println!("{:?}", parser.run_with_data(data.clone(), None));
    assert!(parser.run_with_data(data.clone(), None).is_ok());
}
