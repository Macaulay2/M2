newPackage("LanguageServer",
    Headline => "language server",
    Version => "0.1",
    Date => "May 2025",
    Authors => {{
	    Name     => "Doug Torrance",
	    Email    => "dtorrance@piedmont.edu",
	    HomePage => "https://webwork.piedmont.edu/~dtorrance"}},
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
    "textDocument/signatureHelp" => (
	{"textDocument", "position"},
	(txtdoc, pos) -> (
	    word := getWord(txtdoc, pos);
	    hashTable {"signatures" => {
		    hashTable {"label" => net(? value word ?? "")}}}))}

---------------------
-- LSPServer class --
---------------------

LSPServer = new Type of MutableHashTable
LSPServer.synonym = "LSP server"
globalAssignment LSPServer

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
		    "signatureHelpProvider" => hashTable {}},
		"serverInfo" => hashTable {
		    "name" => "Macaulay2 Language Server",
		    "version" => LanguageServer.Options.Version}}));
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
	    stdio << response << endl << flush)))

end

restart

-- to get working in Emacs:
(require 'eglot)
(add-to-list 'eglot-server-programs '(M2-mode "M2-language-server"))
