use crate::{DocBase, VarType};

const DESCRIPTION: &'static str = r#"
Chande Momentum Oscillator. Calculates the difference between the sum of recent gains and the sum of recent losses and then divides the result by the sum of all price movement over the same period.
"#;

const EXAMPLE: &'static str = r#"
```pine
study('My Script')
plot(cmo(close, 5), color=color.yellow)

// the same on pine
f_cmo(src, length) =>
    float mom = change(src)
    float sm1 = sum((mom >= 0) ? mom : 0.0, length)
    float sm2 = sum((mom >= 0) ? 0.0 : -mom, length)
    return = 100 * (sm1 - sm2) / (sm1 + sm2)

plot(f_cmo(close, 5))
```
"#;

const PINE_FN_ARGUMENTS: &'static str = "
**series (series)** Series of values to process.
**length (int)** Number of bars (length).
";

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "cmo",
        signatures: vec![],
        description: DESCRIPTION,
        example: EXAMPLE,
        returns: "Chande Momentum Oscillator.",
        arguments: PINE_FN_ARGUMENTS,
        remarks: "",
        links: "",
    };
    vec![fn_doc]
}
