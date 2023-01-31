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

load "./LanguageServer/json-rpc.m2"

LSPServer = new Type of MutableHashTable
protect Initialized

load "./LanguageServer/lsp-lifecycle.m2"

createLanguageServer = () -> (
    server := new LSPServer from {
	symbol JSONRPCServer => new JSONRPCServer,
	Initialized => false};
    addLifecycleMethods server;
    server)

-- TODO: figure out how to plug this code into everything else

addHeader = msg -> concatenate(
    "Content-Length: ",
    toString length msg,
    "\r\n\r\n",
    msg)
addHeader "foo"

-- http grammar from https://www.rfc-editor.org/rfc/rfc7230#section-3.2

owsP = *orP(" ", "\t")
fieldVCharP = Parser(c -> if c === null then null else (
	x := first ascii c;
	if x < 0x21 or x == 0x7f then null
	else terminalParser c))
fieldValueP = *andP(fieldVCharP, optP(+orP(" ", "\t") @ fieldVCharP))

-- extract content length from header
headerP = (x -> x#2) % andP(
    "Content-Length:", owsP, NNParser, owsP, "\r\n",
    optP(andP("Content-Type:", owsP, fieldValueP, owsP)),"\r\n")

contentP = concatenate % *Parser(c ->
    if c === null then null else terminalParser c)

removeHeader = (((n, s) -> substring(s, 0, n)) % (headerP @ contentP) :
    charAnalyzer)

end
restart
debug loadPackage("LanguageServer", Reload => true)
s = "Content-Length: 5\r\nContent-Type:foo\r\nthe quick brown fox jumps over the lazy dog"
removeHeader s

f = headerP : charAnalyzer
f "Content-Length: 3\r\nContent-Type:	  	application/vscode-jsonrpc; charset=utf-8\r\n"

class oo
headerP = (x -> x#1) % andP(
    "Content-Length: ",
    NNParser,
    "\r\n",
    optP("Content-Type: " @ fieldValueP),
    "\r\n")

fromLSP = method()
fromLSP String := x -> (
    -- TODO: deal w/ Content-Length
    msg := (((fromJSON @@ last) % headerP @ contentP) : charAnalyzer) x;
    if msg#?"id" then ( -- TODO: check to make sure we have the correct members
	if msg#?"method" then RequestMessage msg
	else ResponseMessage msg)
    else NotificationMessage msg)

end

-- turn on lsp-mode
///
(add-to-list 'lsp-language-id-configuration
    '(M2-mode . "macaulay2"))
(lsp-register-client
    (make-lsp-client
    	:new-connection (lsp-stdio-connection
	    "~/src/macaulay2/M2/M2/Macaulay2/packages/LanguageServer/macaulay2-lsp")
    	:activation-fn (lsp-activate-on "macaulay2")
    	:server-id 'macaulay2))
///

debug loadPackage("LanguageServer", Reload => true)

errorDepth = 2
RequestMessage {}

e
msg = requestMessage(1, "initialize")

print responses#(msg#"method") msg
print oo
print (responses#(msg#"method") msg)

respond msg

responses#(msg#"method") msg

responses#?(msg#"method")

fromLSP toLSP responseMessage(3, responseError(4, "foo"))
fromLSP toLSP notificationMessage("foo")


infoHelp "currentPackage"
currentPackage.Options.Version

#///{
    "jsonrpc": "2.0",
    "result": {
        "serverInfo": {
            "version": "0.0",
            "name": "Macaulay2 Language Server"
        },
        "capabilities": {
            
        }
    },
    "id": 42
}///

#demark("xxxxx", {})
