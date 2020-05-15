use crate::{DocBase, VarType};

const DESCRIPTION: &'static str = r#"
The function sets a number of study properties.
"#;

const EXAMPLE: &'static str = r#"
```pine
study(title='MyScriptStudy')
study(title="MyScriptStudy", shorttitle="MSS", overlay=true)
```
"#;

const ARGUMENT: &'static str = r#"
**title (string)** study title that would be seen in Indicators widget. Argument IS REQUIRED.
**shorttitle (string)** study short title that would be seen in the chart legend. Argument is optional.
**overlay (bool)** if true the study will be added as an overlay for the main series. If false - it would be added on a separate chart pane. Default is false.
**format (string)** type of formatting study values on the price axis. Possible values are: format.inherit, format.price, format.volume. Default is format.inherit.
"#;

// **precision (int)** number of digits after the floating point for study values on the price axis. Must be a non negative integer and not greater than 16. If omitted, using formatting from parent series. If format is format.inherit and this argument is set, then format becomes format.price.
// **scale (int)** price scale that the indicator should be attached to. Possible values are: scale.right, scale.left, scale.none. Value scale.none can be applied only in combination with 'overlay=true' setting. If omitted, using scale from main series.
// **max_bars_back (int)** Maximum number of bars available for a study for historical reference. This parameter is applied to every built-in or user variable in the script if there is a reference to historical data of a variable in the script code (‘[]’ operator is used). Variable buffer sizes in the Pine Script are typically autodetected. This however is not possible in certain cases which is why the parameter allows a user to manually set the lower bound of this value. NOTE: using of the max_bars_back function instead of the parameter is optimal because it applies to only one variable.
// **linktoseries (bool)** if true then the study will be always on the same pane and same price scale as the main series. Should be used only in combination with 'overlay=true'. Default is false.

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "study",
        signatures: vec![],
        description: DESCRIPTION,
        example: EXAMPLE,
        returns: "",
        arguments: ARGUMENT,
        remarks: "",
        links: "",
    };
    vec![fn_doc]
}
