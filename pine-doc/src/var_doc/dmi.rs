use crate::{DocBase, VarType};

const DESCRIPTION: &'static str = r#"
The dmi function returns the directional movement index.
"#;

const EXAMPLE: &'static str = r#"
```pine
study(title="Directional Movement Index", shorttitle="DMI", format=format.price, precision=4)
len = input(17, minval=1, title="DI Length")
lensig = input(14, title="ADX Smoothing", minval=1, maxval=50)
[diplus, diminus, adx] = dmi(len, lensig)
plot(adx, color=color.red, title="ADX")
plot(diplus, color=color.blue, title="+DI")
plot(diminus, color=color.orange, title="-DI")
```
"#;

const ARGUMENTS: &'static str = r#"
**diLength (int)** DI Period.
**adxSmoothing (int)** ADX Smoothing Period.
"#;

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "dmi",
        signatures: vec![],
        description: DESCRIPTION,
        example: EXAMPLE,
        returns: "Tuple of three DMI series: Positive Directional Movement (+DI), Negative Directional Movement (-DI) and Average Directional Movement Index (ADX).",
        arguments: ARGUMENTS,
        remarks: "",
        links: "[rsi](#fun-rsi) [tsi](#fun-tsi) [mfi](#fun-mfi)",
    };
    vec![fn_doc]
}
