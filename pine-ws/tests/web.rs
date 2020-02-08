//! Test suite for the Web and headless browsers.

#![cfg(target_arch = "wasm32")]

extern crate wasm_bindgen_test;
use pine_ws::*;
use wasm_bindgen_test::*;

// wasm_bindgen_test_configure!(run_in_browser);

#[wasm_bindgen_test]
fn runner_test() {
    // assert_eq!(1 + 1, 2);
    let mut runner = new_runner();
    assert_eq!(parse_src(&mut runner, "m=1"), Ok(()));
}
