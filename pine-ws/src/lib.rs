mod utils;

use std::mem;
use wasm_bindgen::prelude::*;

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

// If want to print the message to console, use log!
extern crate web_sys;

macro_rules! log {
    ( $( $t:tt )* ) => {
        web_sys::console::log_1(&format!( $( $t )* ).into());
    }
}

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
    AnySeries, InputVal, NoneCallback, OutputData, OutputDataCollect, OutputInfo, PineFormatError,
    PlotInfo, StrOptionsData, SymbolInfo,
};
use pine::PineScript;
use std::convert::TryInto;
use std::f64;
use std::mem::transmute;
use std::rc::Rc;

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
pub fn parse_src(runner: &mut ExportPineRunner, src: String) -> Result<(), JsValue> {
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

#[wasm_bindgen]
pub struct ExportOutputData {
    output_data: *mut Option<OutputData>,
}

#[wasm_bindgen]
pub fn output_series(output: &mut ExportOutputData) -> *mut f64 {
    let mut res: Vec<f64> = Vec::new();

    let output_d = unsafe { transmute::<_, &mut Option<OutputData>>(output.output_data) };
    match output_d {
        None => res.push(0f64),
        Some(output_d) => {
            res.push(output_d.series.len() as f64);
            for data in output_d.series.iter() {
                // For every output data, first push the length of data vec
                res.push(data.len() as f64);
                for ones in data.iter() {
                    match ones {
                        Some(fv) => res.push(*fv),
                        None => res.push(f64::NAN),
                    }
                }
            }
        }
    }
    res.shrink_to_fit();
    let ptr = res.as_mut_ptr();
    mem::forget(res); // prevent deallocation in Rust
    ptr
}

#[wasm_bindgen]
pub fn output_options(output: &mut ExportOutputData) -> JsValue {
    let output_d = unsafe { transmute::<_, &mut Option<OutputData>>(output.output_data) };
    match output_d {
        None => {
            let opts: Vec<String> = vec![];
            JsValue::from_serde(&opts).unwrap()
        }
        Some(output_d) => {
            let opts: Vec<_> = output_d
                .colors
                .iter()
                .map(|d| d.options.join("|"))
                .collect();
            JsValue::from_serde(&opts).unwrap()
        }
    }
}

#[wasm_bindgen]
pub fn output_colors(output: &mut ExportOutputData) -> *mut i32 {
    let mut res: Vec<i32> = Vec::new();

    let output_d = unsafe { transmute::<_, &mut Option<OutputData>>(output.output_data) };
    match output_d {
        None => res.push(0i32),
        Some(output_d) => {
            res.push(output_d.colors.len() as i32);
            for data in output_d.colors.iter() {
                // For every output data, first push the length of data vec
                res.push(data.values.len() as i32);
                for ones in data.values.iter() {
                    match ones {
                        Some(fv) => res.push(*fv),
                        None => res.push(0),
                    }
                }
            }
        }
    }
    res.shrink_to_fit();
    let ptr = res.as_mut_ptr();
    mem::forget(res); // prevent deallocation in Rust
    ptr
}

#[wasm_bindgen]
pub struct ExportOutputArray {
    outputs: *mut OutputDataCollect,
}

impl Drop for ExportOutputArray {
    fn drop(&mut self) {
        unsafe {
            Box::from_raw(self.outputs);
        }
    }
}

#[wasm_bindgen]
pub fn output_array(array: &ExportOutputArray) -> JsValue {
    let output = unsafe { transmute::<_, &mut OutputDataCollect>(array.outputs) };
    JsValue::from_serde(&vec![output.from, output.to, output.data_list.len() as i32]).unwrap()
}

#[wasm_bindgen]
pub fn output_array_get(array: &ExportOutputArray, i: usize) -> ExportOutputData {
    let output = unsafe { transmute::<_, &mut OutputDataCollect>(array.outputs) };

    ExportOutputData {
        output_data: &mut output.data_list[i],
    }
}

fn output_data_to_slice(output: OutputDataCollect) -> ExportOutputArray {
    ExportOutputArray {
        outputs: Box::into_raw(Box::new(output)),
    }
}

#[wasm_bindgen]
pub fn run_with_input(
    runner: &mut ExportPineRunner,
    input_val: JsValue,
) -> Result<ExportOutputArray, JsValue> {
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

fn slice_input_data_i64(origin_data: &[f64], index: usize, count: usize) -> Vec<Option<i64>> {
    origin_data[index * count..(index + 1) * count]
        .iter()
        .map(|s| Some(*s as i64))
        .collect()
}

fn transfer_input_data(
    src_strs: Vec<String>,
    count: usize,
    data: &[f64],
) -> Vec<(&'static str, AnySeries)> {
    src_strs
        .into_iter()
        .enumerate()
        .map(|(i, s)| match s.as_str() {
            "close" => (
                "close",
                AnySeries::from_float_vec(slice_input_data(data, i, count)),
            ),
            "open" => (
                "open",
                AnySeries::from_float_vec(slice_input_data(data, i, count)),
            ),
            "high" => (
                "high",
                AnySeries::from_float_vec(slice_input_data(data, i, count)),
            ),
            "low" => (
                "low",
                AnySeries::from_float_vec(slice_input_data(data, i, count)),
            ),
            "time" => (
                "_time",
                AnySeries::from_int_vec(slice_input_data_i64(data, i, count)),
            ),
            "volume" => (
                "volume",
                AnySeries::from_int_vec(slice_input_data_i64(data, i, count)),
            ),
            _ => unreachable!(),
        })
        .collect()
}

#[wasm_bindgen]
pub fn run_with_data(
    runner: &mut ExportPineRunner,
    srcs: JsValue,
    count: usize,
    data: &[f64],
    syminfo: JsValue,
) -> Result<ExportOutputArray, JsValue> {
    let runner_ins = unsafe {
        let script = transmute::<*mut (), *mut PineScript>(runner.script);
        script.as_mut().unwrap()
    };
    let src_strs: Vec<String> = srcs.into_serde().unwrap();
    debug_assert_eq!(data.len(), src_strs.len() * count);
    let input_data = transfer_input_data(src_strs, count, data);

    let info: Option<Rc<SymbolInfo>> = match syminfo.into_serde() {
        Ok(info) => Some(Rc::new(info)),
        Err(_) => None,
    };
    log!("Get sym info {:?}", info);
    match runner_ins.run_with_datal(input_data, count, info) {
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
    syminfo: JsValue,
) -> Result<ExportOutputArray, JsValue> {
    let runner_ins = unsafe {
        let script = transmute::<*mut (), *mut PineScript>(runner.script);
        script.as_mut().unwrap()
    };
    let src_strs: Vec<String> = srcs.into_serde().unwrap();
    debug_assert_eq!(data.len(), src_strs.len() * count);

    let input: Vec<Option<InputVal>> = input_val.into_serde().unwrap();
    let input_data = transfer_input_data(src_strs, count, data);

    let info: Option<Rc<SymbolInfo>> = match syminfo.into_serde() {
        Ok(info) => Some(Rc::new(info)),
        Err(_) => None,
    };
    match runner_ins.runl(input, input_data, count, info) {
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
) -> Result<ExportOutputArray, JsValue> {
    let runner_ins = unsafe {
        let script = transmute::<*mut (), *mut PineScript>(runner.script);
        script.as_mut().unwrap()
    };
    let src_strs: Vec<String> = srcs.into_serde().unwrap();
    debug_assert_eq!(data.len(), src_strs.len() * count);
    let input_data = transfer_input_data(src_strs, count, data);
    match runner_ins.updatel(input_data, count) {
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
) -> Result<ExportOutputArray, JsValue> {
    let runner_ins = unsafe {
        let script = transmute::<*mut (), *mut PineScript>(runner.script);
        script.as_mut().unwrap()
    };
    let src_strs: Vec<String> = srcs.into_serde().unwrap();
    debug_assert_eq!(data.len(), src_strs.len() * count);
    let input_data = transfer_input_data(src_strs, count, data);
    match runner_ins.update_froml(input_data, from, count) {
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
