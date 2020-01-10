use self::jsonrpc_core::{IoHandler, Params};
use jsonrpc_core;
use lsp_types::*;
use serde;
use serde_json;
use serde_json::json;
use std::io::{self, BufRead, Read, Write};
use std::sync::mpsc::{channel, Sender};
use std::thread::spawn;

pub fn start() {
    let (request_sender, request_receiver) = channel();
    let (response_sender, response_receiver) = channel::<String>();

    let mut io = IoHandler::new();

    io.add_method("initialize", move |params: Params| {
        // let value = server.lock().unwrap().initialize_request(params.parse()?)?;
        // serde_json::to_value(value).map_err(|_| jsonrpc_core::Error::internal_error())
        info!("Initialize with params {:?}", params);
        let mut capabilities = ServerCapabilities::default();
        // Make the text document sync mode as incremental
        capabilities.text_document_sync = Some(TextDocumentSyncCapability::Kind(
            TextDocumentSyncKind::Incremental,
        ));
        // capabilities.declaration_provider = Some(true);
        // capabilities.definition_provider = Some(true);
        // capabilities.references_provider = Some(true);
        let result = InitializeResult {
            capabilities,
            server_info: None,
        };
        serde_json::to_value(result).map_err(|_| jsonrpc_core::Error::internal_error())
    });

    // let server = lang_server.clone();
    io.add_notification("textDocument/didOpen", move |params: Params| {
        // server
        //     .lock()
        //     .unwrap()
        //     .text_document_did_open_notification(&params.parse().unwrap())
        info!("Open text document {:?}", params);
    });

    io.add_notification("textDocument/didChange", move |params: Params| {
        // server
        //     .lock()
        //     .unwrap()
        //     .text_document_did_open_notification(&params.parse().unwrap())
        info!("Change text document {:?}", params);
    });

    io.add_notification("textDocument/didClose", move |params: Params| {
        // server
        //     .lock()
        //     .unwrap()
        //     .text_document_did_open_notification(&params.parse().unwrap())
        info!("Close text document {:?}", params);
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
    trace!("SEND RESPONSE: {:?}", response);
    writeln!(writer, "Content-Length: {}\r", response.len()).unwrap();
    writeln!(writer, "\r").unwrap();
    write!(writer, "{}", response).unwrap();
    writer.flush().expect("Could not flush stdout");
}
