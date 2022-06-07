use async_trait::async_trait;
use parking_lot::RwLock;
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
        if self.has_publish_diagnostics() {
            self.client
                .publish_diagnostics(
                    params.text_document.uri.clone(),
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
                        message: "Something something something".to_string(),
                        related_information: None,
                        tags: None,
                        data: None,
                    }],
                    Some(params.text_document.version),
                )
                .await;
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
