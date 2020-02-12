mod utils;

use std::mem;
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

use pine::runtime::{
    InputVal, NoneCallback, OutputData, OutputDataCollect, OutputInfo, PineFormatError, PlotInfo,
};
use pine::PineScript;
use std::convert::TryInto;
use std::f64;
use std::mem::transmute;

#[wasm_bindgen]
pub fn init_panic_hook() {
    utils::set_panic_hook();
}

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

fn output_data_to_slice(output: OutputDataCollect) -> *mut f64 {
    let mut res: Vec<f64> = Vec::new();
    // push [from, to] as float64 to result array.
    let bytes = [output.from.to_le_bytes(), output.to.to_le_bytes()].concat();
    let dest_bytes: [u8; 8] = bytes.as_slice().try_into().unwrap();
    res.push(f64::from_le_bytes(dest_bytes));

    for data in output.data_list {
        match data {
            Some(v) => {
                // For every output data, first push the length of data vec
                res.push(v.series.len() as f64);
                for ones in v.series {
                    for element in ones {
                        match element {
                            Some(fv) => res.push(fv),
                            None => res.push(f64::NAN),
                        }
                    }
                }
            }
            None => res.push(f64::from_le_bytes([0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0])),
        }
    }
    res.shrink_to_fit();
    let ptr = res.as_mut_ptr();
    mem::forget(res); // prevent deallocation in Rust
    ptr
}

#[wasm_bindgen]
pub fn run_with_input(
    runner: &mut ExportPineRunner,
    input_val: JsValue,
) -> Result<*mut f64, JsValue> {
    let runner_ins = unsafe {
        let script = transmute::<*mut (), *mut PineScript>(runner.script);
        script.as_mut().unwrap()
    };
    let input: Vec<Option<InputVal>> = input_val.into_serde().unwrap();
    match runner_ins.run_with_input(input) {
        Ok(output) => Ok(output_data_to_slice(output)),
        Err(err) => Err(JsValue::from_serde(&err).unwrap()),
    }
}

fn slice_input_data(origin_data: &[f64], index: usize, count: usize) -> Vec<Option<f64>> {
    origin_data[index * count..(index + 1) * count]
        .iter()
        .map(|s| if *s == f64::NAN { None } else { Some(*s) })
        .collect()
}

fn transfer_input_data(
    src_strs: Vec<String>,
    count: usize,
    data: &[f64],
) -> Vec<(&'static str, Vec<Option<f64>>)> {
    src_strs
        .into_iter()
        .enumerate()
        .map(|(i, s)| {
            if s == "close" {
                ("close", slice_input_data(data, i, count))
            } else if s == "open" {
                ("open", slice_input_data(data, i, count))
            } else if s == "high" {
                ("high", slice_input_data(data, i, count))
            } else if s == "low" {
                ("low", slice_input_data(data, i, count))
            } else {
                unreachable!();
            }
        })
        .collect()
}

#[wasm_bindgen]
pub fn run_with_data(
    runner: &mut ExportPineRunner,
    srcs: JsValue,
    count: usize,
    data: &[f64],
) -> Result<*mut f64, JsValue> {
    let runner_ins = unsafe {
        let script = transmute::<*mut (), *mut PineScript>(runner.script);
        script.as_mut().unwrap()
    };
    let src_strs: Vec<String> = srcs.into_serde().unwrap();
    debug_assert_eq!(data.len(), src_strs.len() * count);
    let input_data = transfer_input_data(src_strs, count, data);
    match runner_ins.run_with_data(input_data) {
        Ok(output) => Ok(output_data_to_slice(output)),
        Err(err) => Err(JsValue::from_serde(&err).unwrap()),
    }
}

#[wasm_bindgen]
pub fn run(
    runner: &mut ExportPineRunner,
    input_val: JsValue,
    srcs: JsValue,
    count: usize,
    data: &[f64],
) -> Result<*mut f64, JsValue> {
    let runner_ins = unsafe {
        let script = transmute::<*mut (), *mut PineScript>(runner.script);
        script.as_mut().unwrap()
    };
    let src_strs: Vec<String> = srcs.into_serde().unwrap();
    debug_assert_eq!(data.len(), src_strs.len() * count);

    let input: Vec<Option<InputVal>> = input_val.into_serde().unwrap();
    let input_data = transfer_input_data(src_strs, count, data);
    match runner_ins.run(input, input_data) {
        Ok(output) => Ok(output_data_to_slice(output)),
        Err(err) => Err(JsValue::from_serde(&err).unwrap()),
    }
}

#[wasm_bindgen]
pub fn update(
    runner: &mut ExportPineRunner,
    srcs: JsValue,
    count: usize,
    data: &[f64],
) -> Result<*mut f64, JsValue> {
    let runner_ins = unsafe {
        let script = transmute::<*mut (), *mut PineScript>(runner.script);
        script.as_mut().unwrap()
    };
    let src_strs: Vec<String> = srcs.into_serde().unwrap();
    debug_assert_eq!(data.len(), src_strs.len() * count);
    let input_data = transfer_input_data(src_strs, count, data);
    match runner_ins.update(input_data) {
        Ok(output) => Ok(output_data_to_slice(output)),
        Err(err) => Err(JsValue::from_serde(&err).unwrap()),
    }
}

#[wasm_bindgen]
pub fn update_from(
    runner: &mut ExportPineRunner,
    srcs: JsValue,
    from: i32,
    count: usize,
    data: &[f64],
) -> Result<*mut f64, JsValue> {
    let runner_ins = unsafe {
        let script = transmute::<*mut (), *mut PineScript>(runner.script);
        script.as_mut().unwrap()
    };
    let src_strs: Vec<String> = srcs.into_serde().unwrap();
    debug_assert_eq!(data.len(), src_strs.len() * count);
    let input_data = transfer_input_data(src_strs, count, data);
    match runner_ins.update_from(input_data, from) {
        Ok(output) => Ok(output_data_to_slice(output)),
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
