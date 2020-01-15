use self::jsonrpc_core::{IoHandler, Params};
use super::pine_server::PineServer;
use jsonrpc_core;
use lsp_types::*;
use serde;
use serde_json;
use serde_json::json;
use std::io::{self, BufRead, Read, Write};
use std::sync::mpsc::channel;
use std::sync::{Arc, Mutex};
use std::thread::spawn;

pub fn start() {
    let (request_sender, request_receiver) = channel();
    let (response_sender, response_receiver) = channel::<String>();

    let mut io = IoHandler::new();

    let pine_server = Arc::new(Mutex::new(PineServer::new(response_sender.clone())));

    let server = Arc::clone(&pine_server);
    io.add_method("initialize", move |params: Params| {
        // info!("Initialize with params {:?}", params);
        server.lock().unwrap().init_params(params.parse()?);
        let mut capabilities = ServerCapabilities::default();
        // Make the text document sync mode as incremental
        capabilities.text_document_sync = Some(TextDocumentSyncCapability::Kind(
            TextDocumentSyncKind::Incremental,
        ));
        // capabilities.declaration_provider = Some(true);
        // capabilities.definition_provider = Some(true);
        // capabilities.references_provider = Some(true);
        capabilities.hover_provider = Some(true);
        capabilities.signature_help_provider = Some(SignatureHelpOptions {
            trigger_characters: Some(vec![String::from("("), String::from(".")]),
            retrigger_characters: None,
            work_done_progress_options: WorkDoneProgressOptions {
                work_done_progress: None,
            },
        });
        let result = InitializeResult {
            capabilities,
            server_info: None,
        };
        serde_json::to_value(result).map_err(|_| jsonrpc_core::Error::internal_error())
    });

    let server = Arc::clone(&pine_server);
    io.add_notification("initialized", move |params: Params| {
        info!("Initialized {:?}", params);
        server.lock().unwrap().send_notification(
            "window/showMessage",
            ShowMessageParams {
                typ: MessageType::Log,
                message: String::from("After initialized"),
            },
        );
    });

    // io.add_notification("workspace/didChangeConfiguration", move |params: Params| {
    //     info!("Did Change configuration {:?}", params);
    // });

    // io.add_notification(
    //     "workspace/didChangeWorkspaceFolders",
    //     move |params: Params| {
    //         info!("Did Change Workspace folders {:?}", params);
    //     },
    // );

    let server = Arc::clone(&pine_server);
    io.add_notification("textDocument/didOpen", move |params: Params| {
        // info!("Open text document {:?}", params);
        server.lock().unwrap().send_notification(
            "window/showMessage",
            ShowMessageParams {
                typ: MessageType::Log,
                message: String::from("Open text document"),
            },
        );
        server.lock().unwrap().add_doc(params.parse().unwrap());
    });

    let server = Arc::clone(&pine_server);
    io.add_notification("textDocument/didChange", move |params: Params| {
        info!("Change text document {:?}", params);
        server.lock().unwrap().change_doc(params.parse().unwrap());
    });

    io.add_notification("textDocument/didClose", move |params: Params| {
        info!("Close text document {:?}", params);
    });

    io.add_method("textDocument/hover", move |params: Params| {
        info!("hover text document {:?}", params);
        Ok(json!(null))
    });

    // Spawn thread to read requests from stdin
    spawn(move || {
        let stdin = io::stdin();
        loop {
            let request = read_request(&mut stdin.lock());
            match request_sender.send(request) {
                Ok(_) => continue,
                Err(_) => {
                    info!("Channel hung up. Unlocking stdin handle.");
                    break;
                }
            }
        }
    });

    // Spawn thread to write notifications to stdout
    spawn(move || {
        let mut stdout = io::stdout();
        loop {
            match response_receiver.recv() {
                Ok(response) => {
                    send_response(&mut stdout, &response);
                }
                Err(_) => {
                    info!("Channel hung up.");
                    break;
                }
            }
        }
    });

    loop {
        match request_receiver.recv() {
            Ok(request) => {
                let response = io.handle_request_sync(&request);
                if let Some(response) = response {
                    response_sender.send(response).unwrap();
                }
            }
            Err(_) => {
                info!("Channel hung up.");
                break;
            }
        }
    }
}

fn read_request(reader: &mut dyn BufRead) -> String {
    let content_length = read_header(reader);

    let mut request = String::new();
    reader
        .take(content_length)
        .read_to_string(&mut request)
        .unwrap();
    trace!("GOT REQUEST: {:?}", request);
    request
}

fn read_header(reader: &mut dyn BufRead) -> u64 {
    let mut buffer = String::new();
    reader.read_line(&mut buffer).unwrap();
    let fields = buffer.trim_end().split(": ").collect::<Vec<&str>>();
    if fields.get(0) != Some(&"Content-Length") {
        trace!("{:?}", fields);
        panic!();
    }
    let content_length = fields[1].parse::<u64>().unwrap();

    let mut buffer = String::new();
    reader.read_line(&mut buffer).unwrap();
    if buffer == "\r\n" {
        return content_length;
    }

    let fields = buffer.trim_end().split(": ").collect::<Vec<&str>>();
    if fields.get(0) != Some(&"Content-Type") {
        trace!("{:?}", fields);
        panic!();
    } else {
        trace!("got Content-Type: {}", &fields[1]);
    }

    let mut buffer = String::new();
    reader.read_line(&mut buffer).unwrap();
    if buffer != "\r\n" {
        trace!("{:?}", buffer);
        panic!();
    }

    content_length
}

fn send_response(writer: &mut dyn Write, response: &str) {
    trace!("SEND RESPONSE: {:?} {}", response, response.len());
    write!(
        writer,
        "Content-Length: {}\r\n\r\n{}",
        response.len(),
        response
    )
    .unwrap();
    writer.flush().expect("Could not flush stdout");
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::BufReader;

    #[test]
    fn response_test() {
        let mut buf = vec![];
        send_response(&mut buf, "hello world");

        let mut read_buf = BufReader::new(buf.as_slice());
        assert_eq!("hello world", read_request(&mut read_buf));
    }
}
