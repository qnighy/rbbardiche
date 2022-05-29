import path from "node:path";
import { ExtensionContext } from "vscode";
import { LanguageClient, LanguageClientOptions, ServerOptions } from "vscode-languageclient/node";

let client: LanguageClient | undefined = undefined;

export function activate(context: ExtensionContext) {
  const lspDir = context.asAbsolutePath(path.join("..", "lsp", "Cargo.toml"));

  const serverOptions: ServerOptions = {
    command: "cargo",
    args: ["run", "--manifest-path", lspDir],
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: 'file', language: 'ruby' }],
  };

  client = new LanguageClient(
    'rbbardiche',
    'Ruby Bardiche',
    serverOptions,
    clientOptions
  );

  client.start();
}

export async function deactivate(): Promise<void> {
  if (!client) return;
  const c = client;
  client = undefined;
  return await c.stop();
}
