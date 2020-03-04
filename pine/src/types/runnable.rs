use super::RuntimeErr;
use crate::runtime::Ctx;

pub trait Runnable<'a> {
    fn back(&mut self, _ctx: &mut dyn Ctx<'a>) -> Result<(), RuntimeErr> {
        Ok(())
    }

    fn run(&mut self, _ctx: &mut dyn Ctx<'a>) -> Result<(), RuntimeErr> {
        Ok(())
    }
}
