use crate::{DocBase, VarType};

const DESCRIPTION: &'static str = r#"
Adds an input to your script indicator. 
User can see and edit inputs on the chart setting dialog of the script study. 
Script inputs look and behave exactly the same as inputs of built-in Technical Analysis indicators.
"#;

const PINE_FN_EXAMPLE: &'static str = r#"
```pine
b = input(title="On/Off", type=input.bool, defval=true)
plot(b ? open : na)

i = input(title="Offset", type=input.integer, defval=7, minval=-10, maxval=10)
plot(offset(close, i))

f = input(title="Angle", type=input.float, defval=-0.5, minval=-3.14, maxval=3.14, step=0.02)
plot(sin(f) > 0 ? close : open)

```
"#;

const PINE_FN_ARGUMENTS: &'static str = "
**defval (Depends on 'type' argument)** Default value of the input variable. Note, that input value that will be ACTUALLY USED by the script is set by user on the Chart Setting dialog.
**title (string)** Title of the input
**type (string)** Input type. Possible values are [input.bool](#var-input-bool), [input.integer](#var-input-integer), [input.float](#var-input-float), [input.string](#var-input-string), [input.symbol](#var_input-symbol), [input.resolution](#var-input-resolution), [input.session](#var-input-session), [input.source](#var-input-source).
**minval (integer, float)** Minimal possible value of the input variable. This argument is used only when input type is [input.integer](#var-input-integer) or [input.float](#var-input-float).
**maxval (integer, float)** Maximum possible value of the input variable. This argument is used only when input type is [input.integer](#var-input-integer) or [input.float](#var-input-float).
**confirm (bool)** If true, then user will be asked to confirm input value before indicator is added to chart. Default value is false. This argument not used when input type is [input.source](#var-input-source).
**step (integer, float)** Step value to use for incrementing/decrementing input from format dialog. Default value is 1. This argument is used only for input types [input.integer](#var-input-integer) and [input.float](#var-input-float).
**options (List of constants: [<type>...])** A list of options to choose from. This argument is used only for input types [input.integer](#var-input-integer), [input.float](#var-input-float) and [input.string](#var-input-string).
";

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "input",
        signatures: vec![],
        description: DESCRIPTION,
        example: PINE_FN_EXAMPLE,
        returns: "Value of input variable.",
        arguments: PINE_FN_ARGUMENTS,
        remarks: "",
        links: "",
    };
    vec![fn_doc]
}
