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
mod hl2;
mod hlc3;
mod hour;
mod input;
mod minute;
mod month;
mod ohlc4;
mod plot;
mod time;
mod timenow;
mod tr;
mod weekofyear;
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
    ]
    .into_iter()
    .flatten()
    .collect()
}
