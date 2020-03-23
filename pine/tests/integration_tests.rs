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

#[test]
fn datasrc_test() {
    let lib_info = pine::LibInfo::new(
        declare_vars(),
        vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
    );
    let mut parser = pine::PineScript::new_with_libinfo(lib_info, Some(&NoneCallback()));
    parser.parse_src(String::from(MACD_SCRIPT)).unwrap();
    let data = vec![(
        "close",
        AnySeries::from_float_vec(vec![Some(1f64), Some(2f64)]),
    )];
    assert!(parser.run_with_data(data, None).is_ok());
}
