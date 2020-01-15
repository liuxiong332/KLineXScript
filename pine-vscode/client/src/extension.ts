/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import * as path from 'path';
import { workspace, ExtensionContext } from 'vscode';

import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	TransportKind
} from 'vscode-languageclient';

let client: LanguageClient;

const languageServerBinaryName = 'pine-ls';

function getServerOptionsEmbedded(context: ExtensionContext) {
	let serverCommand = context.asAbsolutePath(
		path.join('dist', languageServerBinaryName)
	);
	let serverOptions: ServerOptions = {
		run: {
			command: serverCommand,
			options: {
				env: { RUST_LOG: "info" }
			}
		},
		debug: {
			command: serverCommand,
			options: {
				env: { RUST_LOG: "trace", RUST_BACKTRACE: "1" }
			}
		},
	};
	return serverOptions;
}

export function activate(context: ExtensionContext) {
	// The server is implemented in node
	// let serverModule = context.asAbsolutePath(
	// 	path.join('server', 'out', 'server.js')
	// );
	// // The debug options for the server
	// // --inspect=6009: runs the server in Node's Inspector mode so VS Code can attach to the server for debugging
	// let debugOptions = { execArgv: ['--nolazy', '--inspect=6009'] };

	// // If the extension is launched in debug mode then the debug server options are used
	// // Otherwise the run options are used
	// let serverOptions: ServerOptions = {
	// 	run: { module: serverModule, transport: TransportKind.ipc },
	// 	debug: {
	// 		module: serverModule,
	// 		transport: TransportKind.ipc,
	// 		options: debugOptions
	// 	}
	// };

	let serverOptions = getServerOptionsEmbedded(context);

	// Options to control the language client
	let clientOptions: LanguageClientOptions = {
		// Register the server for plain text documents
		documentSelector: [{ scheme: 'file', language: 'pine' }],
		synchronize: {
			// Notify the server about file changes to '.clientrc files contained in the workspace
			fileEvents: workspace.createFileSystemWatcher('**/.clientrc')
		}
	};

	// Create the language client and start the client.
	client = new LanguageClient(
		'pine_ls',
		'PINE LS',
		serverOptions,
		clientOptions
	);

	// Start the client. This will also launch the server
	client.start();
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}
