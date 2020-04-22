use crate::{DocBase, VarType};

const DESCRIPTION: &'static str = r#"
If ... then ... else ...
"#;

const EXAMPLES: &'static str = r#"
```pine
// Draw circles at the bars where open crosses close
s1 = iff(cross(open, close), avg(open,close), na)
plot(s1, style=plot.style_circles, linewidth=4, color=color.green)
```
"#;

const ARGUMENTS: &'static str = r#"
**condition (series)** Series with condition values. Zero value (0 and also NaN, +Infinity, -Infinity) is considered to be false, any other value is true.
**then (series)** Series with values to return if condition is true.
**_else (series)** Series with values to return if condition is false. Use na for `_else` argument if you do not need 'else' branch.
"#;

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "iff",
        signatures: vec![],
        description: DESCRIPTION,
        example: EXAMPLES,
        returns: "y or z series.",
        arguments: ARGUMENTS,
        remarks: "iff does exactly the same thing as ternary conditional operator ?: but in a functional style. Also iff is slightly less efficient than operator ?:",
        links: "[na](#fun-na)",
    };
    vec![fn_doc]
}
