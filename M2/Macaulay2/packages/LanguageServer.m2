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

----------------------------------
----------------------------------
------------ JSON-RPC ------------
---- https://www.jsonrpc.org/ ----
----------------------------------
----------------------------------

toJSONRPC = method()
toJSONRPC VisibleList := x -> toJSON(
    hashTable append(x, "jsonrpc" => "2.0"), Indent => 4)

-- TODO: maybe create classes for these things?

--------------------
-- request object --
--------------------
requestObject = method()
requestObject(String, HashTable, ZZ) :=
requestObject(String, VisibleList, ZZ) :=
requestObject(String, HashTable, String) :=
requestObject(String, VisibleList, String) :=
requestObject(String, HashTable, Nothing) :=
requestObject(String, VisibleList, Nothing) := (
    method', params, id') -> toJSONRPC {
    "method" => method',
    "params" => params,
    "id" => id'}

-- params is optional
requestObject(String, ZZ) :=
requestObject(String, String) :=
requestObject(String, Nothing) := (
    method', id') -> toJSONRPC {
    "method" => method',
    "id" => id'}

-- notification object (no id)
notificationObject = method()
notificationObject(String, HashTable) :=
notificationObject(String, VisibleList) := (
    method', params) -> toJSONRPC {
    "method" => method',
    "params" => params}

notificationObject String := method' -> toJSONRPC {"method" => method'}

---------------------
-- response object --
---------------------

responseObject = method()
responseObject(Thing, ZZ) :=
responseObject(Thing, String) :=
responseObject(Thing, Nothing) := (
    result, id') -> toJSONRPC {
    "result" => result,
    "id" => id'}

errorObject = method()
errorObject(ZZ, String, Thing, ZZ) :=
errorObject(ZZ, String, Thing, String) :=
errorObject(ZZ, String, Thing, Nothing) := (
    code', message, data, id') -> toJSONRPC {
    "error" => hashTable {
	"code" => code',
	"message" => message,
	"data" => data},
    "id" => id'}

-- data is optional
errorObject(ZZ, String, ZZ) :=
errorObject(ZZ, String, String) :=
errorObject(ZZ, String, Nothing) := (
    code', message, id') -> toJSONRPC {
    "error" => hashTable {
	"code" => code',
	"message" => message},
    "id" => id'}


----------------------------------
----------------------------------
---- Language Server Protocol ----
----------------------------------
----------------------------------

toLSP = method()
toLSP String := content' -> concatenate(
    "Content-Length: ",
    toString length content',
    "\r\n\r\n",
    content')

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

-- TODO: how do we handle getting data one piece at a time?
fromLSP = method()
fromLSP String := (
    ((len, content') -> (
	    if #content' != len then error(
		"Content-Length header does not match length of content")
	    else fromJSON content')) % headerP @ contentP) : charAnalyzer
