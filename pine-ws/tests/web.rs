//! Test suite for the Web and headless browsers.

#![cfg(target_arch = "wasm32")]

extern crate wasm_bindgen_test;

use pine::runtime::InputVal;
use pine_ws::*;
use wasm_bindgen::prelude::*;
use wasm_bindgen_test::*;

// wasm_bindgen_test_configure!(run_in_browser);

#[wasm_bindgen_test]
fn runner_test() {
    init_panic_hook();
    // assert_eq!(1 + 1, 2);
    let mut runner = new_runner();
    assert_eq!(
        parse_src(&mut runner, "m = input(1, 'hello', 'int')\nplot(close + m)"),
        Ok(())
    );
    assert!(gen_io_info(&mut runner).is_ok());

    let input_data: Vec<f64> = vec![0f64, 0f64, 0f64, 0f64];
    assert!(run_with_data(
        &mut runner,
        JsValue::from_serde(&vec!["close", "open", "high", "low"]).unwrap(),
        1,
        input_data.into_boxed_slice().as_mut()
    )
    .is_ok());

    let input_vals: Vec<Option<InputVal>> = vec![Some(InputVal::Int(1))];
    assert!(run_with_input(&mut runner, JsValue::from_serde(&input_vals).unwrap()).is_ok());
}
