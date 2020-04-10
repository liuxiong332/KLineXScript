pub mod abs;
pub mod accdist;
pub mod alma;
pub mod atr;
pub mod avg;
pub mod barstate;
pub mod bb;
pub mod bbw;
pub mod cci;
pub mod ceil;
pub mod change;
pub mod cmo;
pub mod cog;
pub mod color;
pub mod correlation;
pub mod cos;
pub mod cum;
pub mod dmi;
pub mod ema;
pub mod falling;
pub mod fill;
pub mod fixnan;
pub mod format;
pub mod highest;
pub mod highestbars;
pub mod hl2;
pub mod hlc3;
pub mod hline;
pub mod hma;
pub mod iff;
pub mod input;
pub mod kc;
pub mod kcw;
pub mod lowest;
pub mod lowestbars;
pub mod macd;
pub mod max;
pub mod mfi;
pub mod na;
pub mod nz;
pub mod ohlc4;
pub mod plot;
pub mod plotarrow;
pub mod plotbar;
pub mod plotcandle;
pub mod plotchar;
pub mod plotshape;
pub mod pow;
pub mod print;
pub mod rising;
pub mod rsi;
pub mod security;
pub mod sma;
pub mod study;
pub mod sum;
pub mod swma;
pub mod syminfo;
pub mod time;
pub mod timenow;
pub mod timestamp;
pub mod tr;
pub mod tsi;
pub mod vwma;
pub mod year;

use crate::ast::syntax_type::SyntaxType;
use crate::types::PineRef;
use std::collections::HashSet;

pub struct VarResult<'a> {
    pub value: PineRef<'a>,
    pub syntax_type: SyntaxType<'a>,
    pub name: &'static str,
}

impl<'a> VarResult<'a> {
    pub fn new(
        value: PineRef<'a>,
        syntax_type: SyntaxType<'a>,
        name: &'static str,
    ) -> VarResult<'a> {
        VarResult {
            value,
            syntax_type,
            name,
        }
    }
}

fn check_names<'a>(vars: &Vec<VarResult<'a>>) -> Vec<&'static str> {
    let mut set = HashSet::new();
    let mut dup_list = vec![];
    for v in vars {
        if set.contains(v.name) {
            dup_list.push(v.name);
        }
        set.insert(v.name);
    }
    return dup_list;
}

pub fn declare_vars<'a>() -> Vec<VarResult<'a>> {
    let list = vec![
        plot::declare_var(),
        print::declare_var(),
        input::declare_var(),
        plotarrow::declare_var(),
        plotbar::declare_var(),
        plotcandle::declare_var(),
        plotchar::declare_var(),
        plotshape::declare_var(),
        color::declare_var(),
        study::declare_var(),
        syminfo::declare_var(),
        barstate::declare_var(),
        accdist::declare_var(),
        time::declare_var(),
        timenow::declare_var(),
        timestamp::declare_var(),
        security::declare_var(),
        year::declare_year_var(),
        year::declare_month_var(),
        year::declare_weekofyear_var(),
        year::declare_dayofweek_var(),
        year::declare_dayofmonth_var(),
        year::declare_hour_var(),
        year::declare_minute_var(),
        abs::declare_var(),
        cos::declare_cos_var(),
        cos::declare_acos_var(),
        cos::declare_sin_var(),
        cos::declare_asin_var(),
        cos::declare_tan_var(),
        cos::declare_atan_var(),
        cos::declare_exp_var(),
        cos::declare_sqrt_var(),
        cos::declare_log_var(),
        cos::declare_log10_var(),
        cos::declare_sign_var(),
        na::declare_var(),
        nz::declare_var(),
        max::declare_max_var(),
        max::declare_min_var(),
        avg::declare_var(),
        ceil::declare_ceil_var(),
        ceil::declare_floor_var(),
        ceil::declare_round_var(),
        alma::declare_var(),
        pow::declare_var(),
        sma::declare_sma_var(),
        sma::declare_wma_var(),
        sma::declare_dev_var(),
        sma::declare_stdev_var(),
        sma::declare_variance_var(),
        ema::declare_ema_var(),
        ema::declare_rma_var(),
        atr::declare_var(),
        bb::declare_var(),
        bbw::declare_var(),
        cci::declare_var(),
        change::declare_mom_var(),
        change::declare_change_var(),
        cmo::declare_var(),
        sum::declare_var(),
        cum::declare_var(),
        cog::declare_var(),
        correlation::declare_var(),
        dmi::declare_var(),
        falling::declare_var(),
        rising::declare_var(),
        fixnan::declare_var(),
        highest::declare_var(),
        lowest::declare_var(),
        highestbars::declare_var(),
        lowestbars::declare_var(),
        hma::declare_var(),
        iff::declare_var(),
        tr::declare_var(),
        kc::declare_var(),
        kcw::declare_var(),
        macd::declare_var(),
        rsi::declare_var(),
        mfi::declare_var(),
        swma::declare_var(),
        vwma::declare_var(),
        hl2::declare_var(),
        hlc3::declare_var(),
        ohlc4::declare_var(),
        fill::declare_var(),
        format::declare_var(),
        hline::declare_var(),
        tsi::declare_var(),
    ];
    debug_assert!(
        check_names(&list).len() == 0,
        format!("Duplicate function names {:?}", check_names(&list))
    );
    // map.insert(print::VAR_NAME, print::declare_var());
    // map.insert(plot::VAR_NAME, plot::declare_var());
    list
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn libname_test() {
        declare_vars();
    }
}
