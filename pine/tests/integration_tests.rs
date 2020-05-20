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

plot(hist, title="Histogram", style=plot.style_columns, color=(hist>=0 ? (hist[1] < hist ? col_grow_above : col_fall_above) : (hist[1] < hist ? col_grow_below : col_fall_below) ), opacity=0 )
plot(mymacd, title="MACD", color=col_macd, opacity=0)
plot(signal, title="Signal", color=col_signal, opacity=0)
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

const VAR_SCRIPTS: &'static str = r#"
m1 = hl2
m2 = hlc3
m3 = ohlc4
"#;

const ADI_SCRIPTS: &'static str = r#"
//@version=4
study("Average Directional Index", shorttitle="ADX", format=format.price, precision=2)
adxlen = input(14, title="ADX Smoothing")
dilen = input(14, title="DI Length")
dirmov(len) =>
    up = change(high)
    down = -change(low)
    plusDM = na(up) ? na : (up > down and up > 0 ? up : 0)
    minusDM = na(down) ? na : (down > up and down > 0 ? down : 0)
    truerange = rma(tr, len)
    plus = 100 * rma(plusDM, len) / truerange
    minus = fixnan(100 * rma(minusDM, len) / truerange)
    [plus, minus]

adx(dilen, adxlen) =>
    [plus, minus] = dirmov(dilen)
    sum = plus + minus
    adx = 100 * rma(abs(plus - minus) / (sum == 0 ? 1 : sum), adxlen)

sig = adx(dilen, adxlen)

plot(sig, color=color.red, title="ADX")
"#;

const VI_SCRIPTS: &'static str = r#"
//@version=4
study(title = "Vortex Indicator", shorttitle="VI", format=format.price, precision=4)
period_ = input(14, title="Period", minval=2)

VMP = sum( abs( high - low[1]), period_ )
VMM = sum( abs( low - high[1]), period_ )
STR = sum( atr(1), period_ )
VIP = VMP / STR
VIM = VMM / STR

plot(VIP, title="VI +", color=#3BB3E4)
plot(VIM, title="VI -", color=#FF006E)
"#;

const SUPER_TREND_SCRIPTS: &'static str = r#"
study("Supertrend V1.0 - Buy or Sell Signal", overlay = true)

Factor = input(3, minval=1, maxval = 100)
Pd = input(7, minval=1, maxval = 100)


Up = hl2 - (Factor * atr(Pd))
Dn = hl2 + (Factor * atr(Pd))

TrendUp = 0.0
TrendDown = 0.0

TrendUp := close[1] > TrendUp[1]? max(Up, TrendUp[1]) : Up
TrendDown := close[1] < TrendDown[1]? min(Dn, TrendDown[1]) : Dn

Trend = 0
Trend := close > TrendDown[1] ? 1: close< TrendUp[1]? -1: nz(Trend[1],1)
Tsl = Trend == 1? TrendUp: TrendDown

linecolor = close == 1 ? color.green : color.red

plot(Tsl, color = linecolor , style = plot.style_line , linewidth = 2,title = "SuperTrend")

// plotshape(cross(close,Tsl) and close>Tsl , "Up Arrow", "triangleup", "belowbar", color.green,0,0)
// plotshape(cross(Tsl,close) and close<Tsl , "Down Arrow", "triangledown", "abovebar", color.red,0,0)
// //plot(Trend==1 and Trend[1]==-1,color = linecolor, style = circles, linewidth = 3,title="Trend")

// plotarrow(Trend == 1 and Trend[1] == -1 ? Trend : na, title="Up Entry Arrow", colorup=color.lime, maxheight=60, minheight=50, opacity=0)
// plotarrow(Trend == -1 and Trend[1] == 1 ? Trend : na, title="Down Entry Arrow", colordown=color.red, maxheight=60, minheight=50, opacity=0)
"#;

const VWAP_SCRIPTS: &'static str = r#"
//@version=4
study(title="VWAP", shorttitle="VWAP", overlay=true)

src = input(close)
t = time
start = na(t[1]) or t > t[1]

sumSrc = src * volume
sumVol = volume
sumSrc := start ? sumSrc : sumSrc + sumSrc[1]
sumVol := start ? sumVol : sumVol + sumVol[1]

// You can use built-in vwap() function instead.

plot(sumSrc / sumVol, title="VWAP", color=color.blue)
"#;

const ALMA_ERR_SCRIPTS: &'static str = r#"
study("alma test ") 
plot(alma(close, -9, 0.85, 6))
plot(close,color = color.red)
"#;

const VWAP1_SCRIPTS: &'static str = r#"
length = input(14, "Time Period Length")
band = input(1.0, "Multipiler")
vp = 0.0
for i = 0 to length - 1
	vp := vp + volume[i]
    
xv = 0.0
for i = 0 to length - 1
	xv := xv + close[i] * (volume[i])

vwap1 = xv / vp
plot(vwap1)
"#;

const INVALID_INPUT: &'static str = r#"
length = input(hl2)
"#;

const LONG_SCRIPT: &'static str = r#"
study("Zig Zag", overlay=true)

dev_threshold = input(title="Deviation (%)", type=input.float, defval=5, minval=1, maxval=100)
depth = input(title="Depth", type=input.integer, defval=10, minval=1)

pivots(src, length, isHigh) =>
    p = nz(src[length])

    if length == 0
        [bar_index, p]
    else
        isFound = true
        for i = 0 to length - 1
            if isHigh and src[i] > p
                isFound := false
            if not isHigh and src[i] < p
                isFound := false
        
        for i = length + 1 to 2 * length
            if isHigh and src[i] >= p
                isFound := false
            if not isHigh and src[i] <= p
                isFound := false
    
        // [int(na), float(na)]
        // [bar_index[length], p]
        if isFound and length * 2 <= bar_index
            [bar_index[length], p]
        else
            [int(na), float(na)]
        

[iH, pH] = pivots(high, floor(depth / 2), true)
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

    parser.parse_src(String::from(VAR_SCRIPTS)).unwrap();
    assert!(parser.run_with_data(data.clone(), None).is_ok());

    parser.parse_src(String::from(ADI_SCRIPTS)).unwrap();
    assert!(parser.run_with_data(data.clone(), None).is_ok());

    parser.parse_src(String::from(VI_SCRIPTS)).unwrap();
    assert!(parser.run_with_data(data.clone(), None).is_ok());

    parser.parse_src(String::from(SUPER_TREND_SCRIPTS)).unwrap();
    assert!(parser.run_with_data(data.clone(), None).is_ok());

    parser.parse_src(String::from(VWAP_SCRIPTS)).unwrap();
    assert!(parser.run_with_data(data.clone(), None).is_ok());

    parser.parse_src(String::from(ALMA_ERR_SCRIPTS)).unwrap();
    assert!(parser.run_with_data(data.clone(), None).is_err());

    parser.parse_src(String::from(VWAP1_SCRIPTS)).unwrap();
    assert!(parser.run_with_data(data.clone(), None).is_ok());

    parser.parse_src(String::from(INVALID_INPUT)).unwrap();
    assert!(parser.run_with_data(data.clone(), None).is_err());

    parser.parse_src(String::from(LONG_SCRIPT)).unwrap();
    assert!(parser.run_with_data(data.clone(), None).is_ok());
}
