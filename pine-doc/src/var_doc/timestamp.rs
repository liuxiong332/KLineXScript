use crate::{DocBase, VarType};

const DESCRIPTION: &'static str = r#"
Function timestamp returns UNIX time of specified date and time.
"#;

const EXAMPLE: &'static str = r#"
```pine
//@version=4
study("My Script")
plot(timestamp(2016, 01, 19, 09, 30), linewidth=3, color=color.green)
plot(timestamp(syminfo.timezone, 2016, 01, 19, 09, 30), color=color.blue)
plot(timestamp(2016, 01, 19, 09, 30), color=color.yellow)
plot(timestamp("GMT+6", 2016, 01, 19, 09, 30))
plot(timestamp(2019, 06, 19, 09, 30, 15), color=color.lime)
plot(timestamp("GMT+3", 2019, 06, 19, 09, 30, 15), color=color.fuchsia)
```
"#;

const ARGUMENT: &'static str = r#"
**timezone (string)** (Optional argument) Timezone
**year (int)** Year
**month (int)** Month
**day (int)** Day
**hour (int)** Hour
**minute (int)** Minute
**second (int)** (Optional argument) Second. Default is 0.
"#;

const REMARKS: &'static str = r#"
UNIX time is the number of milliseconds that have elapsed since 00:00:00 UTC, 1 January 1970. By default Timezone is [syminfo.timezone](#var-syminfo-timezone), but it can be specified by GMT-notation (e.g "GMT-5", "GMT+5", "GMT+5:30", etc), or one of following values:
"America/New_York"
"America/Los_Angeles"
"America/Chicago"
"America/Phoenix"
"America/Toronto"
"America/Vancouver"
"America/Argentina/Buenos_Aires"
"America/El_Salvador"
"America/Sao_Paulo"
"America/Bogota"
"Europe/Moscow"
"Europe/Athens"
"Europe/Berlin"
"Europe/London"
"Europe/Madrid"
"Europe/Paris"
"Europe/Warsaw"
"Australia/Sydney"
"Australia/Brisbane"
"Australia/Adelaide"
"Australia/ACT"
"Asia/Almaty"
"Asia/Ashkhabad"
"Asia/Tokyo"
"Asia/Taipei"
"Asia/Singapore"
"Asia/Shanghai"
"Asia/Seoul"
"Asia/Tehran"
"Asia/Dubai"
"Asia/Kolkata"
"Asia/Hong_Kong"
"Asia/Bangkok"
"Pacific/Auckland"
"Pacific/Chatham"
"Pacific/Fakaofo"
"Pacific/Honolulu"
"#;

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "timestamp",
        signatures: vec![],
        description: DESCRIPTION,
        example: EXAMPLE,
        returns: "UNIX time.",
        arguments: ARGUMENT,
        remarks: REMARKS,
        links: "[time](#fun-time)",
    };
    vec![fn_doc]
}
