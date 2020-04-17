extern crate regex;
extern crate serde;

mod doc_base;
mod doc_parser;
mod var_doc;
mod vardoc_gen;

pub use doc_base::*;
pub use vardoc_gen::*;

use doc_parser::*;

use regex::Captures;
use regex::Regex;

use std::collections::HashMap;
use std::error::Error;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

use serde::{Deserialize, Serialize};

const ALL_DOC_HTML: &'static str = r#"
<!DOCTYPE html>
<html>
<head>
<meta charset="utf-8">
<title>Pine Language API</title>
<link rel="stylesheet" type="text/css" href="../theme.css">
<link rel="stylesheet" href="perfect-scrollbar.min.css">
<link rel="stylesheet" href="default.css">

</head>
<body>
{}
<script src="highlight.min.js"></script>
<script>hljs.initHighlightingOnLoad();</script>
<script src="jquery-2.1.1.min.js"></script>
<script src="perfect-scrollbar.min.js"></script>
</body>
</html>
"#;

#[macro_use]
extern crate lazy_static;

fn process_name(name: String) -> String {
    name.replace(".", "-")
}

fn gen_doc(doc_parser: &LibVarParser) -> String {
    [
        doc_parser
            .variables
            .iter()
            .map(|s| s.1.clone())
            .collect::<Vec<_>>()
            .join("\n"),
        doc_parser
            .functions
            .iter()
            .map(|s| s.1.clone())
            .collect::<Vec<_>>()
            .join("\n"),
    ]
    .join("\n")
}

fn gen_detail_doc(doc_parser: &LibVarParser) -> String {
    let doc_content = gen_doc(doc_parser);
    let re = Regex::new(r"\{\}").unwrap();
    String::from(re.replace(ALL_DOC_HTML, |_caps: &Captures| doc_content.clone()))
}

fn gen_json_doc(doc_parser: &LibVarParser) -> String {
    let mut vars: HashMap<_, _> = doc_parser
        .variables
        .iter()
        .map(|s| (process_name(format!("{}-{}", "var", s.0)), s.1))
        .collect::<HashMap<_, _>>();

    let funs: HashMap<_, _> = doc_parser
        .functions
        .iter()
        .map(|s| (process_name(format!("{}-{}", "fun", s.0)), s.1))
        .collect::<HashMap<_, _>>();
    vars.extend(funs.into_iter());
    serde_json::to_string(&vars).unwrap()
}

fn write_doc(doc_parser: &LibVarParser) {
    let path = Path::new("pine-doc/static/output/all_doc.html");
    let display = path.display();

    let mut file = match File::create(&path) {
        Err(why) => panic!("couldn't create {}: {}", display, why.description()),
        Ok(file) => file,
    };

    // println!("{:?}", self.gen_detail_doc());
    match file.write_all(gen_detail_doc(doc_parser).as_bytes()) {
        Err(why) => panic!("couldn't write to {}: {}", display, why.description()),
        Ok(_) => println!("successfully wrote to {}", path.display()),
    }

    let doc_path = Path::new("pine-doc/static/output/doc.html");
    let doc_display = doc_path.display();

    let mut doc_file = match File::create(&doc_path) {
        Err(why) => panic!("couldn't create {}: {}", doc_display, why.description()),
        Ok(file) => file,
    };

    match doc_file.write_all(gen_doc(doc_parser).as_bytes()) {
        Err(why) => panic!("couldn't write to {}: {}", doc_display, why.description()),
        Ok(_) => println!("successfully wrote to {}", doc_path.display()),
    }

    let doc_path = Path::new("pine-doc/static/output/doc.json");
    let doc_display = doc_path.display();

    let mut doc_file = match File::create(&doc_path) {
        Err(why) => panic!("couldn't create {}: {}", doc_display, why.description()),
        Ok(file) => file,
    };

    match doc_file.write_all(gen_json_doc(doc_parser).as_bytes()) {
        Err(why) => panic!("couldn't write to {}: {}", doc_display, why.description()),
        Ok(_) => println!("successfully wrote to {}", doc_path.display()),
    }
}

#[derive(Serialize, Deserialize)]
struct OutputRes {
    variables: Vec<String>,
    functions: Vec<String>,
}

fn write_vars(doc_parser: &LibVarParser) {
    let output = OutputRes {
        variables: doc_parser.variables.iter().map(|s| s.0).cloned().collect(),
        functions: doc_parser.functions.iter().map(|s| s.0).cloned().collect(),
    };
    let path = Path::new("pine-doc/static/output/output.json");
    let display = path.display();

    let mut file = match File::create(&path) {
        Err(why) => panic!("couldn't create file: {}", why.description()),
        Ok(file) => file,
    };

    match file.write_all(serde_json::to_string(&output).unwrap().as_bytes()) {
        Err(why) => panic!("couldn't write to file: {}", why.description()),
        Ok(_) => println!("successfully wrote to {}", display),
    }
}

fn main() {
    let mut parser = LibVarParser::new();
    parser.parse_lib_vars();
    write_doc(&parser);
    write_vars(&parser);
}
