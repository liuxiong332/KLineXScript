use crate::ast::stat_expr_types::VarIndex;
use crate::runtime::context::{downcast_ctx, Ctx};
use crate::runtime::InputSrc;

pub fn ensure_srcs<'a>(
    ctx: &mut dyn Ctx<'a>,
    srcs: Vec<&'static str>,
    mut index_updater: impl FnMut(Vec<VarIndex>),
) {
    if !downcast_ctx(ctx).check_is_input_info_ready() {
        downcast_ctx(ctx).add_input_src(InputSrc::new(
            None,
            srcs.iter().map(|&s| String::from(s)).collect(),
        ));

        index_updater(
            srcs.iter()
                .map(|s| ctx.get_top_varname_index(s).unwrap())
                .collect(),
        )
    }
}
