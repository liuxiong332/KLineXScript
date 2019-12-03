use super::context::{Context, ContextType, Ctx, Runner};
use super::ctxid_parser::CtxIdParser;
use crate::ast::stat_expr_types::Block;
use crate::types::{Float, PineFrom, PineRef, PineType, RefData, RuntimeErr, Series};
use std::collections::HashMap;

pub trait Callback {
    fn print(&self, _str: String) {}
}

pub struct DataSrc<'a, 'b, 'c> {
    context: Context<'a, 'b, 'c>,
    blk: &'a Block<'a>,
    pub callback: &'a dyn Callback,
}

fn get_len(data: &HashMap<&'static str, Vec<Float>>) -> Result<usize, RuntimeErr> {
    let lens: Vec<usize> = data.iter().map(|(_, v)| v.len()).collect();
    if lens.len() == 0 {
        return Err(RuntimeErr::NotValidParam);
    }
    for l in &lens[1..] {
        if *l != lens[0] {
            return Err(RuntimeErr::NotValidParam);
        }
    }
    Ok(lens[0])
}

impl<'a, 'b, 'c> DataSrc<'a, 'b, 'c> {
    pub fn new(
        blk: &'a mut Block<'a>,
        vars: HashMap<&'a str, PineRef<'a>>,
        callback: &'a dyn Callback,
    ) -> DataSrc<'a, 'b, 'c> {
        CtxIdParser::new().parse_blk(blk);
        let mut context = Context::new_with_callback(callback);
        for (k, v) in vars.into_iter() {
            context.create_var(k, v);
        }
        DataSrc {
            blk,
            context,
            callback,
        }
    }

    fn run_data(
        &mut self,
        data: HashMap<&'static str, Vec<Float>>,
        len: usize,
    ) -> Result<(), RuntimeErr> {
        for i in 0..len {
            // Extract data into context
            for (k, v) in data.iter() {
                let series = self.context.move_var(k).unwrap();
                let mut float_s: RefData<Series<Float>> = Series::implicity_from(series).unwrap();
                float_s.update(v[i]);
                self.context.update_var(k, float_s.into_pf());
            }
            self.blk.run(&mut self.context)?;
            self.context.commit();
            self.context.clear_declare();
        }
        self.context.run_callbacks()?;
        Ok(())
    }

    pub fn run(&mut self, data: HashMap<&'static str, Vec<Float>>) -> Result<(), RuntimeErr> {
        let len = get_len(&data)?;

        // Create variable from the hash map
        for (k, _) in data.iter() {
            let s: Series<Float> = Series::new();
            self.context.create_var(k, PineRef::new_rc(s));
        }
        self.run_data(data, len)
    }

    pub fn update(&mut self, data: HashMap<&'static str, Vec<Float>>) -> Result<(), RuntimeErr> {
        let len = get_len(&data)?;
        self.context.roll_back()?;
        self.run_data(data, len)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::name::VarName;
    use crate::ast::stat_expr_types::{Assignment, Exp, Statement};

    struct MyCallback;
    impl Callback for MyCallback {}

    #[test]
    fn datasrc_test() {
        let mut blk = Block::new(
            vec![Statement::Assignment(Box::new(Assignment::new(
                vec![VarName("hello")],
                Exp::VarName(VarName("close")),
                false,
                None,
            )))],
            None,
        );
        let mut datasrc = DataSrc::new(&mut blk, HashMap::new(), &MyCallback);

        let mut data = HashMap::new();
        data.insert("close", vec![Some(10f64), Some(100f64)]);

        assert_eq!(datasrc.run(data), Ok(()));
        datasrc.context.map_var("hello", |hv| match hv {
            None => None,
            Some(v) => {
                let ser: RefData<Series<Float>> = Series::implicity_from(v).unwrap();
                let expect_ser = RefData::new_rc(Series::from_vec(vec![Some(10f64), Some(100f64)]));
                assert_eq!(ser, expect_ser);
                Some(ser.into_pf())
            }
        });
    }
}
