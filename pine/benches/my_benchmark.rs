extern crate pine;
use pine::ast::syntax_type::{SimpleSyntaxType, SyntaxType};
use pine::libs::declare_vars;
use pine::libs::plot;
use pine::libs::print;
use pine::runtime::data_src::{Callback, DataSrc, NoneCallback};
use pine::runtime::AnySeries;

use criterion::{black_box, criterion_group, criterion_main, Criterion};

const VI_SCRIPTS: &'static str = r#"
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

fn run_data() {
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

    let mut high_data: Vec<Option<f64>> = vec![];
    high_data.resize(800, Some(10f64));

    let mut low_data: Vec<Option<f64>> = vec![];
    low_data.resize(800, Some(1f64));

    let data = vec![
        ("high", AnySeries::from_float_vec(high_data)),
        ("low", AnySeries::from_float_vec(low_data)),
    ];
    parser.parse_src(String::from(VI_SCRIPTS)).unwrap();
    assert!(parser.run_with_data(data, None).is_ok());
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("pine runner", |b| b.iter(|| run_data()));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
