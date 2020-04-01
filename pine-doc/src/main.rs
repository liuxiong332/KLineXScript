mod doc_base;
mod doc_parser;
mod var_doc;
mod vardoc_gen;

pub use doc_base::*;
pub use vardoc_gen::*;

use doc_parser::*;

use std::error::Error;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

#[macro_use]
extern crate lazy_static;
fn gen_detail_doc(doc_parser: &LibVarParser) -> String {
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
}

fn main() {
    println!("Hello");
    let mut parser = LibVarParser::new();
    parser.parse_lib_vars();
    write_doc(&parser);
}
