-- https://www.jsonrpc.org/specification

export {
    "JSONRPCServer",
    "handleRequest",
    "addMethod",
    "raiseError"
    }

-- response message
Response = new SelfInitializingType of HashTable
new Response from List := (
    response, L) -> Response hashTable append(L, "jsonrpc" => "2.0")

responseSuccess = method()
responseSuccess(Thing, ZZ) :=
responseSuccess(Thing, String) := (result, i) -> Response {
    "result" => result, "id" => i}

responseError = method(Options => {"id" => null})
responseError(ZZ, String) := o -> (kode, msg) -> Response {
    "error" => hashTable {"code" => kode, "message" => msg}, "id" => o#"id"}
responseError(ZZ, String, Thing) := o -> (kode, msg, data) -> Response {
    "error" => hashTable {"code" => kode, "message" => msg, "data" => data},
    "id" => o#"id"}

-- for raising errors inside methods
JSONRPCError = new SelfInitializingType of HashTable

raiseError = method()
raiseError(ZZ, String) := (kode, msg) -> JSONRPCError {
    "code" => kode,
    "message" => msg}
raiseError(ZZ, String, Thing) := (kode, msg, data) -> JSONRPCError {
    "code" => kode,
    "message" => msg,
    "data" => data}
responseError JSONRPCError := o -> err -> Response {
    "error" => err, "id" => o#"id"}

-- JSON-RPC server
JSONRPCServer = new Type of MutableHashTable

handleRequest = method()
handleRequest(JSONRPCServer, String) := (server, json) -> (
    response := (
	try request := fromJSON json
	then handleRequest(server, request)
	else responseError(-32700, "Parse error"));
    if response =!= null then toJSON response)
handleRequest(JSONRPCServer, List) := (server, requests) -> (
    if #requests == 0 then handleRequest(server, null)
    else (
	result := select(apply(requests, request -> handleRequest(server,
		    request)), x -> x =!= null);
	if #result > 0 then result))
handleRequest(JSONRPCServer, HashTable) := (server, request) -> (
    if not (
	(request#?"jsonrpc" and request#"jsonrpc" === "2.0") and
	(request#?"method" and instance(request#"method", String)))
    then return handleRequest(server, null); -- invalid request
    if request#?"id" and not (
	instance(request#"id", String) or
	instance(request#"id", ZZ) or
	request#"id" === nil)
    then return handleRequest(server, null); -- invalid request
    if server#?(request#"method") then (
	result := (
	    try callMethod(server#(request#"method"),
		if request#?"params" then request#"params" else {})
	    else raiseError(-32602, "Invalid params"));
	if request#?"id" then (
	    if instance(result, JSONRPCError)
	    then responseError(result, "id" => request#"id")
	    else responseSuccess(result, request#"id")))
    else if request#?"id"
    then responseError(-32601, "Method not found", "id" => request#"id"))
handleRequest(JSONRPCServer, Thing) := (server, badrequest) -> (
    responseError(-32600, "Invalid Request"))

JSONRPCMethod = new SelfInitializingType of HashTable

addMethod = method()
addMethod(JSONRPCServer, String, Function) := (server, name, f) -> (
    server#name = JSONRPCMethod {"function" => f})
addMethod(JSONRPCServer, String, List, Function) := (
    server, name, params, f) -> (
    server#name = JSONRPCMethod {"params" => params, "function" => f})

callMethod = method()
callMethod(JSONRPCMethod, List) := (
    mthd, params) -> mthd#"function" toSequence (
    if mthd#?"params" then apply(#mthd#"params", i ->
	if i >= #params then null
	else params#i)
    else params)
callMethod(JSONRPCMethod, HashTable) := (
    mthd, params) -> (
    mthd#"function" (
	if mthd#?"params"
	then toSequence(apply(mthd#"params", param ->
		if params#?param then params#param else null))
	else ()))
