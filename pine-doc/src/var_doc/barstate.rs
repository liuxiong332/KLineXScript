use crate::{DocBase, VarType};

const ISCONFIRMED_REMARK: &'static str = r#"
PineScript code that uses this variable could calculate differently on history and real-time data.
It is NOT recommended to use [barstate.isconfirmed](#var-barstate-isconfirmed) in [security](#fun-security) expression. Its value requested from security is unpredictable.
"#;

const ISCONFIRMED_LINKS: &'static str = r#"
[barstate.isfirst](#var-barstate-isfirst) [barstate.islast](#var-barstate-islast) [barstate.ishistory](#var-barstate-ishistory) 
[barstate.isrealtime](#var-barstate-isrealtime) [barstate.isnew](#var-barstate-isnew)
"#;

const ISFIRST_REMARK: &'static str = r#"
PineScript code that uses this variable could calculate differently on history and real-time data.
"#;

const ISFIRST_LINKS: &'static str = r#"
[barstate.islast](#var-barstate-islast) [barstate.ishistory](#var-barstate-ishistory) [barstate.isrealtime](#var-barstate-isrealtime) 
[barstate.isnew](#var-barstate-isnew) [barstate.isconfirmed](#var-barstate-isconfirmed)
"#;

const ISHISTORY_REMARK: &'static str = r#"
PineScript code that uses this variable could calculate differently on history and real-time data.
"#;

const ISHISTORY_LINKS: &'static str = r#"
[barstate.isfirst](#var-barstate-isfirst) [barstate.islast](#var-barstate-islast) [barstate.isrealtime](#var-barstate-isrealtime) 
[barstate.isnew](#var-barstate-isnew) [barstate.isconfirmed](#var-barstate-isconfirmed)
"#;

const ISLAST_REMARK: &'static str = r#"
PineScript code that uses this variable could calculate differently on history and real-time data.
"#;

const ISLAST_LINKS: &'static str = r#"
[barstate.isfirst](#var-barstate-isfirst) [barstate.ishistory](#var-barstate-ishistory) [barstate.isrealtime](#var-barstate-isrealtime) 
[barstate.isnew](#var-barstate-isnew) [barstate.isconfirmed](#var-barstate-isconfirmed)
"#;

const ISNEW_DESC: &'static str = r#"
Returns true if script is currently calculating on new bar, false otherwise. This variable is true when calculating on historical bars or on first update of a newly generated real-time bar.
"#;

const ISNEW_REMARK: &'static str = r#"
PineScript code that uses this variable could calculate differently on history and real-time data.
"#;

const ISNEW_LINKS: &'static str = r#"
[barstate.isfirst](#var-barstate-isfirst) [barstate.islast](#var-barstate-islast) [barstate.ishistory](#var-barstate-ishistory) 
[barstate.isrealtime](#var-barstate-isrealtime) [barstate.isconfirmed](#var-barstate-isconfirmed)
"#;

const ISREALTIME_REMARK: &'static str = r#"
PineScript code that uses this variable could calculate differently on history and real-time data.
"#;

const ISREALTIME_LINKS: &'static str = r#"
[barstate.isfirst](#var-barstate-isfirst) [barstate.islast](#var-barstate-islast) [barstate.ishistory](#var-barstate-ishistory) 
[barstate.isnew](#var-barstate-isnew) [barstate.isconfirmed](#var-barstate-isconfirmed)"#;

pub fn gen_doc() -> Vec<DocBase> {
    vec![DocBase {
        var_type: VarType::Variable,
        name: "barstate.isconfirmed",
        signatures: vec![],
        description: "Returns true if the script is calculating the last (closing) update of the current bar. The next script calculation will be on the new bar data.",
        example: "",
        returns: "",
        arguments: "",
        remarks: ISCONFIRMED_REMARK,
        links: ISCONFIRMED_LINKS,
    }, DocBase {
        var_type: VarType::Variable,
        name: "barstate.isfirst",
        signatures: vec![],
        description: "Returns true if current bar is first bar in barset, false otherwise.",
        example: "",
        returns: "",
        arguments: "",
        remarks: ISFIRST_REMARK,
        links: ISFIRST_LINKS,
    }, DocBase {
        var_type: VarType::Variable,
        name: "barstate.ishistory",
        signatures: vec![],
        description: "Returns true if current bar is a historical bar, false otherwise.",
        example: "",
        returns: "",
        arguments: "",
        remarks: ISHISTORY_REMARK,
        links: ISHISTORY_LINKS,
    }, DocBase {
        var_type: VarType::Variable,
        name: "barstate.islast",
        signatures: vec![],
        description: "Returns true if current bar is the last bar in barset, false otherwise. This condition is true for all real-time bars in barset.",
        example: "",
        returns: "",
        arguments: "",
        remarks: ISLAST_REMARK,
        links: ISLAST_LINKS,
    }, DocBase {
        var_type: VarType::Variable,
        name: "barstate.isnew",
        signatures: vec![],
        description: ISNEW_DESC,
        example: "",
        returns: "",
        arguments: "",
        remarks: ISNEW_REMARK,
        links: ISNEW_LINKS,
    }, DocBase {
        var_type: VarType::Variable,
        name: "barstate.isrealtime",
        signatures: vec![],
        description: "Returns true if current bar is a real-time bar, false otherwise.",
        example: "",
        returns: "",
        arguments: "",
        remarks: ISREALTIME_REMARK,
        links: ISREALTIME_LINKS,
    }]
}
