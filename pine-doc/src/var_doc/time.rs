use crate::{DocBase, VarType};

const TIME_DESC: &'static str = r#"
Function time returns UNIX time of current bar for the specified resolution and session or NaN if time point is out-of-session.
"#;

const TIME_EXAMPLE: &'static str = r#"
```pine
study("Time", overlay=true)
// Try this on chart AAPL,1
timeinrange(res, sess) => not na(time(res, sess)) ? 1 : 0
plot(timeinrange("1", "1300-1400"), color=color.red)

// This plots 1.0 at every start of 10 minute bar on a 1 minute chart:
newbar(res) => change(time(res)) == 0 ? 0 : 1
plot(newbar("10"))
```
While setting up a session you can specify not just the hours and minutes but also the days of the week that will be included in that session.
If the days aren't specified, the session is considered to have been set from Monday to Friday (Saturday and Sunday are excluded as the weekend days), i.e. "1100-2000" is the same as "1100-1200:23456".
For example, on a symbol that is traded seven days a week with the 24-hour trading session the following script will not color Saturdays and Sundays.
"#;

const TIME_ARGUMENTS: &'static str = r#"
**resolution (string)** Resolution.
**session (string)** Session specification. Optional argument, session of the symbol used by default.
"#;

pub fn gen_doc() -> Vec<DocBase> {
    vec![
        DocBase {
            var_type: VarType::Variable,
            name: "time",
            signatures: vec![],
            description: "Current bar time in UNIX format. It is the number of milliseconds that have elapsed since 00:00:00 UTC, 1 January 1970.",
            example: "",
            returns: "",
            arguments: "",
            remarks: "",
            links: "",
        },
        DocBase {
            var_type: VarType::Function,
            name: "time",
            signatures: vec![],
            description: TIME_DESC,
            example: "",
            returns: "UNIX time.",
            arguments: TIME_ARGUMENTS,
            remarks: "UNIX time is the number of milliseconds that have elapsed since 00:00:00 UTC, 1 January 1970.",
            links: "",
        },
    ]
}
