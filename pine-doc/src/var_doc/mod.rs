mod abs;
mod accdist;
mod acos;
mod alma;
mod asin;
mod atan;
mod atr;
mod avg;
mod barstate;
mod bb;
mod bbw;
mod cci;
mod ceil;
mod change;
mod close;
mod cmo;
mod cog;
mod color;
mod correlation;
mod cos;
mod cum;
mod dayofmonth;
mod dayofweek;
mod dev;
mod dmi;
mod ema;
mod exp;
mod falling;
mod fill;
mod fixnan;
mod floor;
mod high;
mod highest;
mod highestbars;
mod hl2;
mod hlc3;
mod hline;
mod hma;
mod hour;
mod iff;
mod input;
mod kc;
mod kcw;
mod log;
mod log10;
mod low;
mod lowest;
mod lowestbars;
mod macd;
mod max;
mod mfi;
mod min;
mod minute;
mod mom;
mod month;
mod na;
mod nz;
mod ohlc4;
mod open;
mod plot;
mod pow;
mod rising;
mod rma;
mod round;
mod rsi;
mod sign;
mod sin;
mod sma;
mod sqrt;
mod stdev;
mod stoch;
mod study;
mod sum;
mod swma;
mod tan;
mod time;
mod timenow;
mod timestamp;
mod tr;
mod tsi;
mod variance;
mod vwma;
mod weekofyear;
mod wma;
mod year;
use super::DocBase;

pub fn declare_vars() -> Vec<DocBase> {
    vec![
        plot::gen_doc(),
        input::gen_doc(),
        accdist::gen_doc(),
        abs::gen_doc(),
        acos::gen_doc(),
        alma::gen_doc(),
        asin::gen_doc(),
        atan::gen_doc(),
        atr::gen_doc(),
        avg::gen_doc(),
        barstate::gen_doc(),
        color::gen_doc(),
        dayofmonth::gen_doc(),
        dayofweek::gen_doc(),
        hour::gen_doc(),
        minute::gen_doc(),
        month::gen_doc(),
        time::gen_doc(),
        timenow::gen_doc(),
        weekofyear::gen_doc(),
        year::gen_doc(),
        hl2::gen_doc(),
        hlc3::gen_doc(),
        ohlc4::gen_doc(),
        tr::gen_doc(),
        bb::gen_doc(),
        bbw::gen_doc(),
        cci::gen_doc(),
        ceil::gen_doc(),
        change::gen_doc(),
        cmo::gen_doc(),
        cog::gen_doc(),
        correlation::gen_doc(),
        cos::gen_doc(),
        cum::gen_doc(),
        dev::gen_doc(),
        dmi::gen_doc(),
        ema::gen_doc(),
        exp::gen_doc(),
        falling::gen_doc(),
        fill::gen_doc(),
        fixnan::gen_doc(),
        floor::gen_doc(),
        highest::gen_doc(),
        highestbars::gen_doc(),
        hline::gen_doc(),
        hma::gen_doc(),
        iff::gen_doc(),
        kc::gen_doc(),
        kcw::gen_doc(),
        log::gen_doc(),
        log10::gen_doc(),
        lowest::gen_doc(),
        lowestbars::gen_doc(),
        macd::gen_doc(),
        max::gen_doc(),
        mfi::gen_doc(),
        min::gen_doc(),
        mom::gen_doc(),
        na::gen_doc(),
        nz::gen_doc(),
        pow::gen_doc(),
        rising::gen_doc(),
        rma::gen_doc(),
        round::gen_doc(),
        rsi::gen_doc(),
        sign::gen_doc(),
        sin::gen_doc(),
        sma::gen_doc(),
        sqrt::gen_doc(),
        stdev::gen_doc(),
        stoch::gen_doc(),
        study::gen_doc(),
        sum::gen_doc(),
        swma::gen_doc(),
        tan::gen_doc(),
        timestamp::gen_doc(),
        tsi::gen_doc(),
        variance::gen_doc(),
        vwma::gen_doc(),
        wma::gen_doc(),
        close::gen_doc(),
        open::gen_doc(),
        high::gen_doc(),
        low::gen_doc(),
    ]
    .into_iter()
    .flatten()
    .collect()
}
