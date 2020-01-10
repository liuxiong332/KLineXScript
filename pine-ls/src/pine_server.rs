use lsp_types::*;

struct PineServer {
    init_params: InitializeParams,
}

fn client_supports_related_information(init_params: &InitializeParams) -> bool {
    let try_fun = || {
        init_params
            .capabilities
            .text_document
            .as_ref()?
            .publish_diagnostics
            .as_ref()?
            .related_information
    };
    try_fun().unwrap_or(false)
}
