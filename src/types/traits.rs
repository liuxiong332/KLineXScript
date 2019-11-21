use crate::ast::stat_expr_types::DataType;

pub trait PineType<'a> {
    fn into(self, data_type: DataType) -> Box<dyn PineType<'a>>;
}

pub trait PineClass {
    fn get<'a, D: PineType<'a>, E>(&self, name: &str) -> Result<D, E>;
}
