newPackage(
    "LanguageServer",
    Version => "0.1",
--    Date => "",
    Headline => "implementation of the Language Server Protocol for Macaulay2",
    Authors => {{
	    Name => "Doug Torrance",
	    Email => "dtorrance@piedmont.edu",
	    HomePage => "https://webwork.piedmont.edu/~dtorrance"}},
    Keywords => {"System"},
    PackageImports => {"JSON", "Parsing"}
    )

export {
    "runLanguageServer"
    }

load "./LanguageServer/json-rpc.m2"

LSPServer = new Type of MutableHashTable
protect Initialized
protect Shutdown

load "./LanguageServer/lsp-lifecycle.m2"

createLanguageServer = file -> (
    server := new LSPServer from {
	symbol JSONRPCServer => new JSONRPCServer,
	Initialized => false,
	Shutdown => false,
	symbol File => file};
    addLifecycleMethods server;
    server)

------------------------------------
-- add header to outgoing message --
------------------------------------
addHeader = msg -> concatenate(
    "Content-Length: ", toString length msg, "\r\n",
    "application/vscode-jsonrpc; charset=utf-8\r\n",
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

-------------------------
-- run language server --
-------------------------

runLanguageServer = method()
runLanguageServer LSPServer := server -> (
    g := if isListener server.File then openInOut server.File else server.File;
    while true do (
	wait g;
	-- we send the string "error" to trigger a JSON parsing error
	request := try removeHeader read g else "error";
	g << addHeader handleRequest(server.JSONRPCServer, request) << flush))

runLanguageServer ZZ := port -> runLanguageServer(
    createLanguageServer openListener("$:" | toString port))
installMethod(runLanguageServer, () ->
    runLanguageServer createLanguageServer stdio)

load "./LanguageServer/tests.m2"

end

restart

loadPackage("LanguageServer", Reload => true)
runLanguageServer()

-*
(add-to-list 'eglot-server-programs
    '(M2-mode "M2"  "--srcdir" "/home/profzoom/src/macaulay2/M2/M2"
	"-e" "needsPackage(\"LanguageServer\"); runLanguageServer()"))
*-
