use async_trait::async_trait;
use parking_lot::RwLock;
use rbbardiche::parse;
use tokio::task::spawn_blocking;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};

#[derive(Debug)]
pub struct Backend {
    client: Client,
    client_capabilities: RwLock<ClientCapabilities>,
}

impl Backend {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            client_capabilities: RwLock::new(ClientCapabilities::default()),
        }
    }

    fn has_publish_diagnostics(&self) -> bool {
        let lock = self.client_capabilities.read();
        if let Some(text_document) = &lock.text_document {
            text_document.publish_diagnostics.is_some()
        } else {
            false
        }
    }
}

#[async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        {
            let mut lock = self.client_capabilities.write();
            *lock = params.capabilities.clone();
        }
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncKind::FULL.into()),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await;
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        if !self.has_publish_diagnostics() {
            return;
        }
        parse_and_report(
            self.client.clone(),
            params.text_document.text.clone(),
            VersionedTextDocumentIdentifier {
                uri: params.text_document.uri.clone(),
                version: params.text_document.version,
            },
        )
        .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        if !self.has_publish_diagnostics() {
            return;
        }
        for change in &params.content_changes {
            if change.range.is_none() {
                parse_and_report(
                    self.client.clone(),
                    change.text.clone(),
                    params.text_document.clone(),
                )
                .await;
            }
        }
    }

    async fn hover(&self, _: HoverParams) -> Result<Option<Hover>> {
        Ok(Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: String::from("Hello!"),
            }),
            range: None,
        }))
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
}

async fn parse_and_report(
    client: Client,
    text: String,
    text_document: VersionedTextDocumentIdentifier,
) {
    let text2 = text.clone();
    let task = spawn_blocking(move || parse(text2.as_bytes()));
    let (_, errors) = match task.await {
        Ok(x) => x,
        Err(e) => {
            let e = if e.is_panic() {
                let e = e.into_panic();
                if let Some(&e) = e.downcast_ref::<&'static str>() {
                    e.to_owned()
                } else if let Some(e) = e.downcast_ref::<String>() {
                    e.clone()
                } else {
                    "Box<Any>".to_string()
                }
            } else {
                e.to_string()
            };

            client
                .publish_diagnostics(
                    text_document.uri.clone(),
                    vec![Diagnostic {
                        range: Range {
                            start: Position {
                                line: 0,
                                character: 0,
                            },
                            end: Position {
                                line: 0,
                                character: 1,
                            },
                        },
                        severity: None,
                        code: None,
                        code_description: None,
                        source: None,
                        message: e,
                        related_information: None,
                        tags: None,
                        data: None,
                    }],
                    Some(text_document.version),
                )
                .await;
            return;
        }
    };
    let diagnostics = errors
        .iter()
        .map(|error| Diagnostic {
            range: Range {
                start: pos(&text, error.range().0),
                end: pos(&text, error.range().1),
            },
            severity: Some(if error.is_error() {
                DiagnosticSeverity::ERROR
            } else {
                DiagnosticSeverity::WARNING
            }),
            code: None,
            code_description: None,
            source: None,
            message: error.to_string(),
            related_information: None,
            tags: None,
            data: None,
        })
        .collect::<Vec<_>>();
    client
        .publish_diagnostics(
            text_document.uri.clone(),
            diagnostics,
            Some(text_document.version),
        )
        .await;
}

// TODO: switch to more efficient implementation
fn pos(s: &str, at: usize) -> Position {
    let mut pos = Position {
        line: 0,
        character: 0,
    };
    let at = {
        let mut at = at;
        while !s.is_char_boundary(at) && at < s.len() {
            at += 1;
        }
        at
    };
    for ch in s[..at].chars() {
        if ch == '\n' {
            pos.line += 1;
            pos.character = 0;
        } else if (ch as u32) < 0x10000 {
            pos.character += 1;
        } else {
            pos.character += 2;
        }
    }

    pos
}
