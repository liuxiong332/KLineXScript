use pine::libs;
use pine::runtime::data_src::{Callback, DataSrc};
use pine::types::Float;
use pyo3::exceptions;
use pyo3::prelude::*;
use pyo3::types::PyList;
use pyo3::wrap_pyfunction;
use std::collections::HashMap;

struct PyCallbackObj<'p> {
    pyobj: PyObject,
    py: Python<'p>,
}

impl<'p> Callback for PyCallbackObj<'p> {
    fn print(&self, _str: String) {
        let print_method = self.pyobj.getattr(self.py, "print");
        let result = print_method.unwrap().call(self.py, (_str,), None).unwrap();
    }
}

#[pyfunction]
/// Formats the sum of two numbers as string
fn load_script(py: Python, script: String, close: Vec<f64>, callbacks: PyObject) -> PyResult<()> {
    let mut script_block = pine::parse_all(&script).unwrap();

    let inner_vars = libs::declare_vars();

    let callback = PyCallbackObj {
        pyobj: callbacks,
        py,
    };
    let mut datasrc = DataSrc::new(&mut script_block, inner_vars, &callback);

    let mut data = HashMap::new();

    let close_val: Vec<Float> = close.into_iter().map(|s| Some(s)).collect();
    data.insert("close", close_val);

    match datasrc.run(data) {
        Ok(_) => Ok(()),
        Err(err) => Err(PyErr::new::<exceptions::TypeError, _>(format!(
            "Err {:?}",
            err
        ))),
    }
}

/// This module is a python module implemented in Rust.
#[pymodule]
fn pine_py(py: Python, m: &PyModule) -> PyResult<()> {
    m.add_wrapped(wrap_pyfunction!(load_script))?;

    Ok(())
}
