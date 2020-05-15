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
        parse_src(
            &mut runner,
            String::from("m = input(1, 'hello', 'int')\nplot(close + m)")
        ),
        Ok(())
    );
    assert!(gen_io_info(&mut runner).is_ok());

    let input_data: Vec<f64> = vec![0f64, 0f64, 0f64, 0f64];
    assert!(run_with_data(
        &mut runner,
        JsValue::from_serde(&vec!["close", "open", "high", "low"]).unwrap(),
        1,
        input_data.clone().into_boxed_slice().as_mut(),
        JsValue::NULL,
    )
    .is_ok());

    let input_vals: Vec<Option<InputVal>> = vec![Some(InputVal::Int(1))];
    assert!(run_with_input(&mut runner, JsValue::from_serde(&input_vals).unwrap()).is_ok());

    assert!(run(
        &mut runner,
        JsValue::from_serde(&input_vals).unwrap(),
        JsValue::from_serde(&vec!["close", "open", "high", "low"]).unwrap(),
        1,
        input_data.clone().into_boxed_slice().as_mut(),
        JsValue::NULL,
    )
    .is_ok());

    assert!(update(
        &mut runner,
        JsValue::from_serde(&vec!["close", "open", "high", "low"]).unwrap(),
        1,
        input_data.clone().into_boxed_slice().as_mut()
    )
    .is_ok());

    assert!(update_from(
        &mut runner,
        JsValue::from_serde(&vec!["close", "open", "high", "low"]).unwrap(),
        0,
        1,
        input_data.clone().into_boxed_slice().as_mut()
    )
    .is_ok());
}

#[wasm_bindgen_test]
fn runner_simple_test() {
    init_panic_hook();
    // assert_eq!(1 + 1, 2);
    let mut runner = new_runner();
    assert_eq!(parse_src(&mut runner, String::from("plot(close)")), Ok(()));
    assert!(gen_io_info(&mut runner).is_ok());
}

#[wasm_bindgen_test]
fn volume_test() {
    init_panic_hook();
    // assert_eq!(1 + 1, 2);
    let mut runner = new_runner();
    assert_eq!(parse_src(&mut runner, String::from("plot(volume)")), Ok(()));
    assert!(gen_io_info(&mut runner).is_ok());
    let input_data: Vec<f64> = vec![10f64];

    let result = run_with_data(
        &mut runner,
        JsValue::from_serde(&vec!["volume"]).unwrap(),
        1,
        input_data.into_boxed_slice().as_mut(),
        JsValue::NULL,
    );
    assert!(result.is_ok());
    if let Ok(output) = result {
        let mut out_data = output_array_get(&output, 0);
        let vec = unsafe { Vec::from_raw_parts(output_series(&mut out_data), 3, 3) };
        assert_eq!(vec, vec![1f64, 1f64, 10f64]);
    }
}

#[wasm_bindgen_test]
fn timenow_test() {
    init_panic_hook();
    // assert_eq!(1 + 1, 2);
    let mut runner = new_runner();
    assert_eq!(
        parse_src(&mut runner, String::from("int a = timenow\nplot(a)")),
        Ok(())
    );
    assert!(gen_io_info(&mut runner).is_ok());
}
