mod utils;

use wasm_bindgen::prelude::*;

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

// #[wasm_bindgen]
// extern {
//     fn alert(s: &str);
// }

// #[wasm_bindgen]
// pub fn greet() {
//     alert("Hello, pine-ws!");
// }

// #[wasm_bindgen]
// pub fn send_example_to_js() -> JsValue {
//     let mut field1 = HashMap::new();
//     field1.insert(0, String::from("ex"));
//     let example = Example {
//         field1,
//         field2: vec![vec![1., 2.], vec![3., 4.]],
//         field3: [1., 2., 3., 4.]
//     };

//     JsValue::from_serde(&example).unwrap()
// }

use pine::runtime::{InputVal, NoneCallback, OutputData, OutputInfo, PineFormatError, PlotInfo};
use pine::PineScript;
use std::convert::TryInto;
use std::f64;
use std::mem::transmute;

#[wasm_bindgen]
pub struct ExportPineRunner {
    script: *mut (),
}

#[wasm_bindgen]
pub fn new_runner() -> ExportPineRunner {
    let box_script = Box::new(PineScript::new(Some(&NoneCallback())));
    ExportPineRunner {
        script: Box::into_raw(box_script) as *mut (),
    }
}

#[wasm_bindgen]
pub fn parse_src(runner: &mut ExportPineRunner, src: &str) -> Result<(), JsValue> {
    let runner_ins = unsafe {
        let script = transmute::<*mut (), *mut PineScript>(runner.script);
        script.as_mut().unwrap()
    };
    match runner_ins.parse_src(src) {
        Ok(_) => Ok(()),
        Err(errs) => Err(JsValue::from_serde(&errs).unwrap()),
    }
}

#[wasm_bindgen]
pub fn gen_io_info(runner: &mut ExportPineRunner) -> Result<JsValue, JsValue> {
    let runner_ins = unsafe {
        let script = transmute::<*mut (), *mut PineScript>(runner.script);
        script.as_mut().unwrap()
    };
    match runner_ins.gen_io_info() {
        Ok(io_info) => Ok(JsValue::from_serde(&io_info).unwrap()),
        Err(errs) => Err(JsValue::from_serde(&errs).unwrap()),
    }
}

fn output_data_to_slice(output: Vec<Option<OutputData>>) -> Box<[f64]> {
    let mut res: Vec<f64> = Vec::new();
    for data in output {
        match data {
            Some(v) => {
                let from = v.from.unwrap();
                let to = v.to.unwrap();
                let bytes = [from.to_le_bytes(), to.to_le_bytes()].concat();
                let dest_bytes: [u8; 8] = bytes.as_slice().try_into().unwrap();
                res.push(f64::from_le_bytes(dest_bytes));
                for element in v.series {
                    match element {
                        Some(fv) => res.push(fv),
                        None => res.push(f64::NAN),
                    }
                }
            }
            None => res.push(f64::from_le_bytes([0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0])),
        }
    }
    res.into_boxed_slice()
}

#[wasm_bindgen]
pub fn run_with_input(
    runner: &mut ExportPineRunner,
    input_val: JsValue,
) -> Result<JsValue, JsValue> {
    let runner_ins = unsafe {
        let script = transmute::<*mut (), *mut PineScript>(runner.script);
        script.as_mut().unwrap()
    };
    let input: Vec<Option<InputVal>> = input_val.into_serde().unwrap();
    match runner_ins.run_with_input(input) {
        Ok(output) => Ok(JsValue::from_serde(&output).unwrap()),
        Err(err) => Err(JsValue::from_serde(&err).unwrap()),
    }
}

#[wasm_bindgen]
pub fn run_with_data(runner: &mut ExportPineRunner, data: JsValue) -> Result<JsValue, JsValue> {
    let runner_ins = unsafe {
        let script = transmute::<*mut (), *mut PineScript>(runner.script);
        script.as_mut().unwrap()
    };
    let input: Vec<(String, Vec<Option<f64>>)> = data.into_serde().unwrap();
    let input_data: Vec<_> = input
        .into_iter()
        .map(|s| {
            if s.0 == "close" {
                ("close", s.1)
            } else if s.0 == "open" {
                ("open", s.1)
            } else if s.0 == "high" {
                ("high", s.1)
            } else if s.0 == "low" {
                ("low", s.1)
            } else {
                unreachable!();
            }
        })
        .collect();
    match runner_ins.run_with_data(input_data) {
        Ok(output) => Ok(JsValue::from_serde(&output).unwrap()),
        Err(err) => Err(JsValue::from_serde(&err).unwrap()),
    }
}

impl Drop for ExportPineRunner {
    fn drop(&mut self) {
        unsafe {
            let script = transmute::<*mut (), *mut PineScript>(self.script);
            Box::from_raw(script);
        };
    }
}
