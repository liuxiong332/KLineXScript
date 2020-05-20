use crate::{DocBase, VarType};

const DESCRIPTION: &'static str = r#"
Fills background between two plots or hlines with a given color.
"#;

const EXAMPLES: &'static str = r#"
```pine
h1 = hline(20)
h2 = hline(10)
fill(h1, h2)

p1 = plot(open)
p2 = plot(close)
fill(p1, p2, color=color.green)
```
"#;

const ARGUMENTS: &'static str = r#"
**hline1 (hline)** The first hline object. Required argument.
**hline2 (hline)** The second hline object. Required argument.
**plot1 (plot)** The first plot object. Required argument.
**plot2 (plot)** The second plot object. Required argument.
**color (color)** Color of the plot. You can use constants like `color=color.red` or `color=#ff001a` as well as complex expressions like `color = close >= open ? color.green : color.red`. Optional argument.
**opacity (int)** Transparency of the filled background. Possible values are from 0 (not transparent) to 100 (invisible). Optional argument.
**title (string)** Title of the created fill object. Optional argument.
"#;

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "fill",
        signatures: vec![],
        description: DESCRIPTION,
        example: EXAMPLES,
        returns: "",
        arguments: ARGUMENTS,
        remarks: "",
        links: "[plot](#fun-plot) [hline](#fun-hline)",
    };
    vec![fn_doc]
}
