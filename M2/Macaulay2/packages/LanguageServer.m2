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

-------------------------
-- JSON-RPC structures --
-------------------------

-- abstract message
LSPMessage = new SelfInitializingType of HashTable
net LSPMessage := x -> toJSON(x, Indent => 4)

new LSPMessage from List := (
    msg, L) -> LSPMessage hashTable append(L, "jsonrpc" => "2.0")

-- request message
RequestMessage = new SelfInitializingType of LSPMessage

requestMessage = method()
requestMessage(ZZ, String, HashTable) :=
requestMessage(ZZ, String, VisibleList) :=
requestMessage(String, String, HashTable) :=
requestMessage(String, String, VisibleList) := (
    i, meth, params) -> RequestMessage {
    "id"     => i,
    "method" => meth,
    "params" => params}
requestMessage(ZZ, String) :=
requestMessage(String, String) := (
    i, meth) -> RequestMessage {
    "id"     => i,
    "method" => meth}

-- response error
ResponseError = new SelfInitializingType of HashTable
net ResponseError := x -> toJSON(x, Indent => 4)

responseError = method()
responseError(ZZ, String) := (c, msg) -> ResponseError {
    "code"    => c,
    "message" => msg}
responseError(ZZ, String, String) :=
responseError(ZZ, String, ZZ) :=
responseError(ZZ, String, Boolean) :=
responseError(ZZ, String, VisibleList) :=
responseError(ZZ, String, HashTable) :=
responseError(ZZ, String, Nothing) := (c, msg, data) -> ResponseError {
    "code"    => c,
    "message" => msg,
    "data"    => data}

-- TODO: deal w/ specific error codes?

-- response message
ResponseMessage = new SelfInitializingType of LSPMessage

responseMessage = method()
responseMessage(ZZ, String) :=
responseMessage(ZZ, ZZ) :=
responseMessage(ZZ, Boolean) :=
responseMessage(ZZ, HashTable) :=
responseMessage(ZZ, Nothing) :=
responseMessage(String, String) :=
responseMessage(String, ZZ) :=
responseMessage(String, Boolean) :=
responseMessage(String, HashTable) :=
responseMessage(String, Nothing) :=
responseMessage(Nothing, String) :=
responseMessage(Nothing, ZZ) :=
responseMessage(Nothing, Boolean) :=
responseMessage(Nothing, HashTable) :=
responseMessage(Nothing, Nothing) := (i, result) -> ResponseMessage {
    "id" => i,
    "result" => result}
responseMessage(ZZ, ResponseError) :=
responseMessage(String, ResponseError) :=
responseMessage(Nothing, ResponseError) := (i, err) -> ResponseMessage {
    "id" => i,
    "error" => err}

-- notification message
NotificationMessage = new SelfInitializingType of LSPMessage

notificationMessage = method()
notificationMessage String := meth -> NotificationMessage {"method" => meth}
notificationMessage(String, VisibleList) :=
notificationMessage(String, HashTable) := (
    meth, params) -> NotificationMessage {
    "method" => meth,
    "params" => params}

responses = hashTable{
    "initialize" => request -> (
	responseMessage(request#"id", hashTable{
		"capabilities" => hashTable {},
		"serverInfo" => hashTable {
		    "name" => "Macaulay2 Language Server",
		    "version" => currentPackage.Options.Version}}))}

respond = method()
respond RequestMessage := msg -> (
    if responses#?(msg#"method") then toLSP responses#(msg#"method") msg)
respond ResponseMessage := msg -> ""

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
