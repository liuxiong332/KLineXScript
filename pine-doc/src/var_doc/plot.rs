use crate::{DocBase, VarType};

const PINE_FN_EXAMPLE: &'static str = r#"
```pine
plot(high+low, title='Title', color=#00ffaa, linewidth=2, style=plot.style_area, opacity=70, offset=15, trackprice=true)

// You may fill the background between any two plots with a fill() function:
p1 = plot(open)
p2 = plot(close)
fill(p1, p2, color=color.green)
```
"#;

const PINE_FN_ARGUMENTS: &'static str = "
**series (series(float))** Series of data to be plotted. Required argument.
**title (string)** Title of the plot.
";

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "plot",
        signatures: vec![],
        description: "Plots a series of data on the chart.",
        example: PINE_FN_EXAMPLE,
        returns: "A plot object, that can be used in [fill](#fun_fill)",
        arguments: PINE_FN_ARGUMENTS,
        remarks: "",
        links: "[plotshape](#fun_plotshape)",
    };
    vec![fn_doc]
}
