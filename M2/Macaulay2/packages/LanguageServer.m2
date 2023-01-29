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

server = new MutableHashTable from {
    "json-rpc server" => new JSONRPCServer,
    "initialized" => false
    }

load "./LanguageServer/lsp-lifecycle.m2"

end


--------------------------
--------------------------
-- LSPMessage -> string --
--------------------------
--------------------------

toLSP = method()
toLSP LSPMessage := msg -> (
    cont := net msg;
    concatenate(
	"Content-Length: ",
	toString length cont,
	"\r\n\r\n",
	cont))

--------------------------
--------------------------
-- string -> LSPMessage --
--------------------------
--------------------------

fieldVCharP = Parser(c -> if c === null then null else (
	x := first ascii c;
	if x < 0x21 or x == 0x7f then null
	else terminalParser c))
fieldValueP = *andP(fieldVCharP, optP(+orP(" ", "\t") @ fieldVCharP))

headerP = (x -> x#1) % andP(
    "Content-Length: ",
    NNParser,
    "\r\n",
    optP("Content-Type: " @ fieldValueP),
    "\r\n")

contentP = concatenate % *Parser(
    c -> if c === null then null else terminalParser c)

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
