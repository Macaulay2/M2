-- LanguageServer package for Macaulay2
-- Copyright (C) 2025-2026 Doug Torrance <dtorrance9@gatech.edu>

-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License along
-- with this program; if not, see <https://www.gnu.org/licenses/>.

newPackage("LanguageServer",
    Headline => "language server",
    Version => "0.1",
    Date => "May 30, 2026",
    Authors => {{
            Name => "Doug Torrance",
            Email => "dtorrance9@gatech.edu",
            HomePage => "https://d-torrance.github.io"}},
    AuxiliaryFiles => true,
    Keywords => {"System"},
    PackageImports => {"JSONRPC", "Parsing"})

export {
    -- classes
    "LSPServer",

    -- methods
    "start",
    }

exportFrom(JSONRPC, "setLogger")

importFrom(Core, {
        "markdown",
        "nonnull",
        "toURL",
        })

------------------------------------
-- add header to outgoing message --
------------------------------------
addHeader = msg -> concatenate(
    "Content-Length: ", toString length msg, "\r\n",
    "Content-Type: application/vscode-jsonrpc; charset=utf-8\r\n\r\n",
    msg)

----------------------------------------------------------------------
-- remove (and extract content length) header from incoming message --
----------------------------------------------------------------------

-- parse using http grammar from
-- https://www.rfc-editor.org/rfc/rfc7230#section-3.2
owsP = *orP(" ", "\t")
fieldVCharP = Parser(c -> if c === null then null else (
        x := first ascii c;
        if x < 0x21 or x == 0x7f then null
        else terminalParser c))
fieldValueP = *andP(fieldVCharP, optP(+orP(" ", "\t") @ fieldVCharP))

headerP = (-* extract content length *- x -> x#2) % andP(
    "Content-Length:", owsP, NNParser, owsP, "\r\n",
    optP(andP("Content-Type:", owsP, fieldValueP, owsP)),"\r\n")

contentP = concatenate % *Parser(c ->
    if c === null then null else terminalParser c)

parseLSP = (headerP @ contentP) : charAnalyzer

-----------------
-- LSP methods --
-----------------

-- mutable hash table containing text of open files
-- keys: uri, values: list of strings (lines of text in file)
openDocuments = new MutableHashTable

getWord = (txtdoc, pos) -> (
    line := openDocuments#(txtdoc#"uri")#(pos#"line");
    start := stop := pos#"character";
    while start > 0 and match("\\w", line#(start - 1)) do start -= 1;
    while stop < #line - 1 and match("\\w", line#(stop + 1)) do stop += 1;
    substring(line, start, stop - start + 1))

toLocation = method()
toLocation FilePosition := pos -> hashTable {
    "uri" => toURL toAbsolutePath pos#0,
    "range" => hashTable {
        "start" => hashTable {
            "line" => pos#1 - 1, -- LSP uses 0-based line numbers
            "character" => pos#2},
        "end" => hashTable {
            "line" => pos#3 - 1,
            "character" => pos#4}}}
toLocation VisibleList := L -> apply(L, toLocation)

getLocation = method()
getLocation Thing := x -> (
    loc := lookup(locate, class x);
    if loc =!= null then toLocation loc x)

getLocation MethodFunction            :=
getLocation MethodFunctionBinary      :=
getLocation MethodFunctionSingle      :=
getLocation MethodFunctionWithOptions := f -> (
    toLocation unique nonnull locate methods f)

LSPmethods = hashTable {
    "textDocument/didOpen" => (
        {"textDocument"},
        txtdoc -> (
            openDocuments#(txtdoc#"uri") = lines txtdoc#"text";)),
    "textDocument/didClose" => (
        {"textDocument"},
        txtdoc -> (remove(openDocuments, txtdoc#"uri");)),
    -- TODO: use incremental changes
    "textDocument/didChange" => (
        {"textDocument", "contentChanges"},
        (txtdoc, changes) -> scan(changes, change ->
            openDocuments#(txtdoc#"uri") = lines change#"text")) ,
    "textDocument/hover" => (
        {"textDocument", "position"},
        (txtdoc, pos) -> (
            word := getWord(txtdoc, pos);
            hoverdoc := ??(? value word);
            if hoverdoc =!= null then hashTable {
                "contents" => hashTable {
                    "kind" => "markdown",
                    "value" => markdown hoverdoc}})),
    "textDocument/definition" => (
        {"textDocument", "position"},
        (txtdoc, pos) -> (
            word := getWord(txtdoc, pos);
            ??(getLocation value word))),
    "textDocument/completion" => (
        {"textDocument", "position"},
        (txtdoc, pos) -> (
            word := getWord(txtdoc, pos);
            completions := apropos("^" | regexQuote word);
            if #completions > 0 then apply(completions,
                completion -> hashTable {"label" => completion}))),
    }

---------------------
-- LSPServer class --
---------------------

LSPServer = new Type of MutableHashTable
LSPServer.synonym = "LSP server"
globalAssignment LSPServer

exitCode = 1
new LSPServer := T -> (
    jserver := new JSONRPCServer;
    -- install dummy methods
    scanKeys(LSPmethods, key ->
        registerMethod(jserver, key,
            x -> JSONRPCError(-32002, "Server Not Initialized")));
    registerMethod(jserver, "initialize", () -> (
            -- now install actual methods
            scanPairs(LSPmethods, (key, val) ->
                registerMethod(jserver, key, val#0, val#1));
            hashTable {
                "capabilities" => hashTable {
                    "textDocumentSync" => 1,
                    "hoverProvider" => true,
                    "definitionProvider" => true,
                    "completionProvider" => hashTable {},
                    },
                "serverInfo" => hashTable {
                    "name" => "Macaulay2 Language Server",
                    "version" => LanguageServer.Options.Version}}));
    registerMethod(jserver, "shutdown", () -> (
            invalidRequest := x -> JSONRPCError(-32600, "Invalid Request");
            scanKeys(LSPmethods,
                key -> registerMethod(jserver, key, invalidRequest));
            registerMethod(jserver, "shutdown", invalidRequest);
            exitCode = 0;));
    registerMethod(jserver, "exit", () -> exit exitCode);
    new T from {
        "JSON-RPC server" => jserver,
        "logger" => x -> null})

setLogger(LSPServer, Function) := (server, logger) -> (
    setLogger(server#"JSON-RPC server", logger);
    server#"logger" = logger)

start = method()
start LSPServer := server -> (
    server#"logger" "starting server";
    (len, stream) := (0, "");
    while true do (
        if #stream == 0 then (
            wait stdio;
            stream = read stdio);
        (len, stream) = try parseLSP stream else (
            server#"logger" "Content-Length missing; exiting";
            exit 1);
        while #stream < len do (
            wait stdio;
            stream |= read stdio);
        request := substring(stream, 0, len);
        stream = substring(stream, len);
        server#"logger"("client request:" | request);
        response := handleRequest(server#"JSON-RPC server", request);
        if response =!= null then (
            response = addHeader response;
            server#"logger"("server response: " | response);
            stdio << response << flush)))

beginDocumentation()

load "./LanguageServer/doc.m2"

end

-- to get working in Emacs:
(require 'eglot)
(add-to-list 'eglot-server-programs '(M2-mode "M2-language-server"))

-- or lsp-mode:
(require 'lsp-mode)
(add-to-list 'lsp-language-id-configuration '(M2-mode . "M2"))
(lsp-register-client (make-lsp-client
        :new-connection (lsp-stdio-connection "M2-language-server")
        :activation-fn (lsp-activate-on "M2")
        :server-id 'M2))
