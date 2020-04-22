use crate::{DocBase, VarType};

const DESCRIPTION: &'static str = r#"
Renders a horizontal line at a given fixed price level.
"#;

const EXAMPLES: &'static str = r#"
```pine
hline(3.14, title='Pi', color=color.blue, linestyle=hline.style_dotted, linewidth=2)

// You may fill the background between any two hlines with a fill() function:
h1 = hline(20)
h2 = hline(10)
fill(h1, h2)
```
"#;

const ARGUMENTS: &'static str = r#"
price (float) Price value at which the object will be rendered. Required argument.
title (string) Title of the object.
color (color) Color of the rendered line. Must be a constant value (not an expression). Optional argument.
linestyle (int) Style of the rendered line. Possible values are: [hline.style_solid](#var-hline-style_solid), [hline.style_dotted](#var-hline-style_dotted), [hline.style_dashed](#var-hline-style_dashed). Optional argument.
linewidth (int) Width of the rendered line, use values from 1 to 4. Default value is 1.
editable (bool) If true then hline style will be editable in Format dialog. Default is true.
"#;

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "hline",
        signatures: vec![],
        description: DESCRIPTION,
        example: EXAMPLES,
        returns: "An hline object, that can be used in [fill](#fun-fill)",
        arguments: ARGUMENTS,
        remarks: "",
        links: "[fill](#fun-fill)",
    };
    vec![fn_doc]
}
