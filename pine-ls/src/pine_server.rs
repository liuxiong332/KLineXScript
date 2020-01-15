use super::text_doc::TextDoc;
use jsonrpc_core::request::Notification;
use jsonrpc_core::Params;
use lsp_types::*;
use pine::ast::input::StrRange;
use std::collections::HashMap;
use std::sync::mpsc::Sender;

pub struct PineServer<'a> {
    init_params: Option<InitializeParams>,
    text_docs: HashMap<Url, TextDoc<'a>>,
    sender: Sender<String>,
}

fn from_str_range(range: StrRange) -> Range {
    Range::new(
        Position::new(
            range.start.get_line() as u64,
            range.start.get_character() as u64,
        ),
        Position::new(
            range.end.get_line() as u64,
            range.end.get_character() as u64,
        ),
    )
}

// fn to_str_range(range: Range) -> StrRange {
//     StrRange::new(
//         StrPos::new(range.start.line as u32, range.start.character as u32),
//         StrPos::new(range.end.line as u32, range.end.character as u32),
//     )
// }

impl<'a> PineServer<'a> {
    pub fn new(sender: Sender<String>) -> PineServer<'a> {
        PineServer {
            sender,
            init_params: None,
            text_docs: HashMap::new(),
        }
    }

    pub fn init_params(&mut self, init_params: InitializeParams) {
        self.init_params = Some(init_params);
    }

    pub fn add_doc(&mut self, params: DidOpenTextDocumentParams) {
        let text = params.text_document.text;
        let uri = params.text_document.uri;
        let mut new_doc = TextDoc::new(text, uri.clone());
        self.parse_doc(&mut new_doc);
        self.text_docs.insert(uri, new_doc);

        self.send_notification(
            "window/showMessage",
            ShowMessageParams {
                typ: MessageType::Log,
                message: String::from("After open text document"),
            },
        );
    }

    pub fn change_doc(&mut self, params: DidChangeTextDocumentParams) {
        match self.text_docs.get_mut(&params.text_document.uri) {
            Some(doc) => {
                params.content_changes.into_iter().for_each(|item| {
                    if let Some(range) = item.range {
                        // let StrRange { start, end } = to_str_range(range);
                        doc.change(
                            range.start,
                            range.end,
                            item.range_length.unwrap() as usize,
                            item.text,
                        );
                    } else {
                        doc.reset(item.text);
                    }
                });
            }
            None => return,
        };
        let mut text_doc = self.text_docs.remove(&params.text_document.uri).unwrap();
        self.parse_doc(&mut text_doc);
        self.text_docs.insert(params.text_document.uri, text_doc);

        self.send_notification(
            "window/showMessage",
            ShowMessageParams {
                typ: MessageType::Log,
                message: String::from("Change text document"),
            },
        );
    }

    pub fn parse_doc(&mut self, doc: &mut TextDoc) {
        // The below is the test code for publishing diagnostics.
        // let range = Range {
        //     start: lsp_types::Position {
        //         line: 3,
        //         character: 0,
        //     },
        //     end: lsp_types::Position {
        //         line: 3,
        //         character: 2,
        //     },
        // };
        // let diagnostics = vec![Diagnostic {
        //     range: range.clone(),
        //     code: None,
        //     severity: Some(DiagnosticSeverity::Error),
        //     source: Some("pine ls".to_owned()),
        //     message: "No entity \'ent2\' within library \'lib\'".to_owned(),
        //     related_information: Some(vec![DiagnosticRelatedInformation {
        //         location: Location::new(doc.get_uri().clone(), range.clone()),
        //         message: String::from("Spelling matters"),
        //     }]),
        //     tags: None,
        // }];

        // let publish_diagnostics = PublishDiagnosticsParams {
        //     uri: doc.get_uri().clone(),
        //     diagnostics: diagnostics,
        //     version: None,
        // };
        // info!("publish errors {:?}", publish_diagnostics);
        // self.send_notification("textDocument/publishDiagnostics", publish_diagnostics);
        if let Err(errs) = doc.parse_src() {
            let diagnostics: Vec<_> = errs
                .into_iter()
                .map(|err| {
                    Diagnostic::new(
                        from_str_range(err.range),
                        Some(DiagnosticSeverity::Error),
                        None,
                        Some(String::from("pine ls")),
                        err.message,
                        None,
                        None,
                    )
                })
                .collect();

            let publish_diagnostics = PublishDiagnosticsParams {
                uri: doc.get_uri().clone(),
                diagnostics: diagnostics,
                version: None,
            };
            // info!("publish errors {:?}", publish_diagnostics);
            self.send_notification("textDocument/publishDiagnostics", publish_diagnostics);
        } else {
            let publish_diagnostics = PublishDiagnosticsParams {
                uri: doc.get_uri().clone(),
                diagnostics: vec![],
                version: None,
            };
            self.send_notification("textDocument/publishDiagnostics", publish_diagnostics);
        }
    }

    pub fn send_notification(
        &self,
        method: impl Into<String>,
        notification: impl serde::ser::Serialize,
    ) {
        let params_json = match serde_json::to_value(notification).unwrap() {
            serde_json::Value::Object(map) => map,
            map => panic!("{:?}", map),
        };

        let notification_json = Notification {
            jsonrpc: Some(jsonrpc_core::Version::V2),
            method: method.into(),
            params: Params::Map(params_json),
        };

        self.sender
            .send(serde_json::to_string(&notification_json).unwrap())
            .unwrap();
    }
}

// fn client_supports_related_information(init_params: &InitializeParams) -> bool {
//     let try_fun = || {
//         init_params
//             .capabilities
//             .text_document
//             .as_ref()?
//             .publish_diagnostics
//             .as_ref()?
//             .related_information
//     };
//     try_fun().unwrap_or(false)
// }

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::mpsc::channel;

    #[test]
    fn pine_server_test() {
        let (sender, receiver) = channel::<String>();
        let mut server = PineServer::new(sender);
        server.add_doc(DidOpenTextDocumentParams {
            text_document: TextDocumentItem::new(
                Url::parse("http://a.b").unwrap(),
                String::from("pine"),
                2,
                String::from("a = "),
            ),
        });
        // println!("receive msg {:?}", receiver.recv().unwrap());

        // assert_eq!(1, 2);
    }
}
