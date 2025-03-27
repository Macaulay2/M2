newPackage("LanguageServer",
    Headline => "language server",
    Version => "0.1",
    Date => "March 2025",
    Authors => {{
	    Name     => "Doug Torrance",
	    Email    => "dtorrance@piedmont.edu",
	    HomePage => "https://webwork.piedmont.edu/~dtorrance"}},
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

removeHeader = (((n, s) -> substring(s, 0, n)) % (headerP @ contentP) :
    charAnalyzer)

-----------------
-- LSP methods --
-----------------

-- TODO: what if user hasn't saved? can we get this from the client?
getLine = (txtdoc, pos) -> (
    (lines get replace("^file://", "", txtdoc#"uri"))#(pos#"line"))

getWord = (txtdoc, pos) -> (
    line := getLine(txtdoc, pos);
    start := stop := pos#"character";
    while start > 0 and match("\\w", line#(start - 1)) do start -= 1;
    while stop < #line - 1 and match("\\w", line#(stop + 1)) do stop += 1;
    substring(line, start, stop - start + 1))

LSPmethods = hashTable {
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
    while true do (
	wait stdio;
	request := read stdio;
	if #request > 0 then (
	    server#"logger"("client request:" | request);
	    response := handleRequest(server#"JSON-RPC server",
		try removeHeader request else request);
	    if response =!= null then (
		response = addHeader response;
		server#"logger"("server response: " | response);
		stdio << response << endl << flush))))

end

restart

-- to get working in Emacs:
(require 'eglot)

(add-to-list 'eglot-server-programs
    '(M2-mode "/home/profzoom/src/macaulay2/M2/M2/Macaulay2/packages/LanguageServer/M2-language-server"))
