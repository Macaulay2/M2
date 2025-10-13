-- JSONRPC package for Macaulay2
-- Copyright (C) 2025 Doug Torrance <dtorrance@piedmont.edu>

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

newPackage("JSONRPC",
    Headline => "JSON-RPC server",
    Version => "0.1",
    Date => "May 19, 2025",
    Authors => {{
	    Name => "Doug Torrance",
	    Email => "dtorrance@piedmont.edu",
	    HomePage => "https://webwork.piedmont.edu/~dtorrance"}},
    Keywords => {"System"},
    PackageImports => {"JSON"})

export {
    -- classes
    "JSONRPCError",
    "JSONRPCServer",

    -- methods
    "handleRequest",
    "makeRequest",
    "registerMethod",
    "setLogger",
    }

JSONRPCServer = new Type of MutableHashTable
new JSONRPCServer := T -> new T from {"logger" => x -> null}
JSONRPCServer.synonym = "JSON-RPC server"
globalAssignment JSONRPCServer

setLogger = method()
setLogger(JSONRPCServer, Function) := (server, f) -> (server#"logger" = f;)

JSONRPCError = new SelfInitializingType of HashTable
JSONRPCError.synonym = "JSON-RPC error"
new JSONRPCError from (ZZ, String) := (T, errCode, msg) -> (
    JSONRPCError {
	"code" => errCode,
	"message" => msg})
new JSONRPCError from (ZZ, String, Thing) := (T, errCode, msg, data) -> (
    JSONRPCError {
	"code" => errCode,
	"message" => msg,
	"data" => data})

-- unexported helper method
makeResponse = method()
makeResponse(JSONRPCServer, JSONRPCError, Thing) := (server, err, ID) -> (
    r := hashTable {
	"jsonrpc" => "2.0",
	"error" => err,
	"id" => ID};
    server#"logger"("sending response: " | toJSON r);
    r)
makeResponse(JSONRPCServer, Thing, Thing) := (server, result, ID) -> (
    r := hashTable {
	"jsonrpc" => "2.0",
	"result" => result,
	"id" => ID};
    server#"logger"("sending response: " | toJSON r);
    r)

handleRequest = method()
handleRequest(JSONRPCServer, String) := (server, str) -> (
    server#"logger"("received request: " | str);
    r := (
	try request := fromJSON str
	then handleRequestHelper(server, request)
	else makeResponse(server, JSONRPCError(-32700, "Parse error"), null));
    if r =!= null then toJSON r)

-- unexported helper method so we can handle batches
handleRequestHelper = method()
handleRequestHelper(JSONRPCServer, List) := (server, requests) -> (
    if #requests == 0 then handleRequestHelper(server, null)
    else (
	result := select(apply(requests,
		request -> handleRequestHelper(server, request)),
	    x -> x =!= null);
	if #result > 0 then result))
handleRequestHelper(JSONRPCServer, HashTable) := (server, request) -> (
    if (
	not isSubset(keys request, {"id", "jsonrpc", "method", "params"}) or
	not request#?"jsonrpc" or
	request#"jsonrpc" != "2.0" or
	not request#?"method" or
	not instance(request#"method", String) or
	request#?"id" and not (
	    instance(request#"id", String) or
	    instance(request#"id", ZZ) or
	    request#"id" === nil))
    then handleRequestHelper(server, null) -- invalid request
    else if not server#?(request#"method")
    then (
	if request#?"id"
	then makeResponse(
	    server,
	    JSONRPCError(-32601, "Method not found"),
	    request#"id"))
    else (
	r := callMethod(server#(request#"method"), request#"params" ?? {},
	    request#"id" ?? null);
	if request#?"id" then r))
handleRequestHelper(JSONRPCServer, Thing) := (server, badrequest) -> (
    makeResponse(server, JSONRPCError(-32600, "Invalid Request"), null))

-- unexported, but user can kind of interact w/ it using "methods"
-- and then code(ZZ) and locate(ZZ)
JSONRPCMethod = new SelfInitializingType of HashTable
net JSONRPCMethod := m -> toString prepend(
    m#"name",
    toSequence m#"params" ?? ())
locate JSONRPCMethod := m -> locate m#"function"
code JSONRPCMethod := code @@ locate
importFrom(Core, "precedence")
precedence JSONRPCMethod := lookup(precedence, Function)

callMethod = method()
callMethod(JSONRPCMethod, List, Thing) := (m, params, ID) -> (
    m#"server"#"logger" concatenate(
	"calling method \"", m#"name", "\" with params ", toJSON params);
    inp := toSequence(
	if m#?"params"
	then apply(#m#"params",
	    i -> if i >= #params then null else params#i)
	else params);
    if #inp == 1 then inp = inp#0;
    r := (try m#"function" inp
	-- TODO: use lastError here once it's available
	-- afterwards, validate params and only throw this
	-- error when they're bad
	-- also update JSONRPCError doc node
	else JSONRPCError(-32602, "Invalid params"));
    if instance(r, JSONRPCError)
    then m#"server"#"logger" concatenate(
	"method \"", m#"name", "\" failed with error: ", toJSON r)
    else m#"server"#"logger" concatenate(
	"method \"", m#"name", "\" returned: ", toJSON r);
    makeResponse(m#"server", r, ID))

callMethod(JSONRPCMethod, HashTable, Thing) := (m, params, ID) -> (
    callMethod(m, apply(m#"params" ?? {}, param -> ?? params#param), ID))

registerMethod = method()
registerMethod(JSONRPCServer, String, List, Function) := (s, n, p, f) -> (
    s#n = JSONRPCMethod {
	"server" => s,
	"name" => n,
	"function" => f,
	"params" => p};)
registerMethod(JSONRPCServer, String, Function) := (s, n, f) -> (
    s#n = JSONRPCMethod {
	"server" => s,
	"name" => n,
	"function" => f};)

importFrom(Core, "previousMethodsFound")
methods JSONRPCServer := server -> previousMethodsFound = (
    new NumberedVerticalList from select(values server, JSONRPCMethod))

makeRequest = method()
makeRequest(String, HashTable, String) :=
makeRequest(String, HashTable, ZZ)     :=
makeRequest(String, List,      String) :=
makeRequest(String, List,      ZZ)     := (m, params, ID) -> toJSON hashTable {
    "jsonrpc" => "2.0",
    "method" => m,
    "params" => params,
    "id" => ID}
-- no params
makeRequest(String, String) :=
makeRequest(String, ZZ)     := (m, ID) -> toJSON hashTable {
    "jsonrpc" => "2.0",
    "method" => m,
    "id" => ID}
-- notifications (no id)
makeRequest(String, HashTable) :=
makeRequest(String, List)      := (m, params) -> toJSON hashTable {
    "jsonrpc" => "2.0",
    "method" => m,
    "params" => params}
makeRequest String := m -> toJSON hashTable {
    "jsonrpc" => "2.0",
    "method" => m}
-- batching
makeRequest List := x -> concatenate("[", demark(", ", makeRequest \ x), "]")

beginDocumentation()

doc ///
  Key
    JSONRPC
  Headline
    JSONRPC 2.0 server
  Description
    Text
      This package provides a lightweight and flexible implementation of a
      @HREF("https://jsonrpc.org", "JSON-RPC 2.0")@ server, providing an easy
      way to register and handle remote procedure calls over JSON. It supports
      method registration, batch processing, and error handling while ensuring
      compliance with the JSON-RPC specification.  The package includes
      utilities for constructing valid JSON-RPC requests and notifications,
      making it simple to integrate into existing applications. Designed for
      reliability and ease of use, it offers built-in logging and a clean,
      intuitive API for managing JSON-RPC methods.
    Example
      server = new JSONRPCServer
      registerMethod(server, "sum", plus)
      setLogger(server, printerr)
      handleRequest(server, makeRequest("sum", {2, 3}, 1))
  Subnodes
    JSONRPCServer
    makeRequest
///

doc ///
  Key
     JSONRPCServer
    (NewMethod, JSONRPCServer)
  Headline
    JSONRPC server class
  Usage
    new JSONRPCServer
  Description
    Text
      @CODE "JSONRPCSever"@ is the core class of the package, responsible for
      handling JSON-RPC requests. It allows you to
      @TO2(registerMethod, "register methods")@,
      @TO2(handleRequest, "process incoming requests")@ (including batches),
      and send appropriate responses or error messages, all
      while following the @HREF("https://www.jsonrpc.org/specification",
	  "JSON-RPC 2.0 specification")@.  The class also includes basic
      @TO2(setLogger, "logging functionality")@ to track incoming requests and
      responses.
    Example
      server = new JSONRPCServer
  Subnodes
    registerMethod
    handleRequest
    setLogger
///

doc ///
  Key
     registerMethod
    (registerMethod, JSONRPCServer, String, List, Function)
    (registerMethod, JSONRPCServer, String, Function)
  Headline
    register a method for a JSON-RPC server
  Usage
    registerMethod(server, name, params, f)
  Inputs
    server:JSONRPCServer
    name:String
    params:List -- optional
    f:Function
  Description
    Text
      This method registers a method with @CODE "server"@.  The @CODE "name"@
      parameter is the name of the method that will be exposed in JSON-RPC
      requests, and @CODE "f"@ is the function that will be called when the
      method is invoked.  The optional @CODE "params"@ argument is a list of
      expected parameter names for the method. If provided, it helps validate
      the parameters in incoming requests. The function should accept the
      parameters passed in the request and return the result to be sent back in
      the response.
    Example
      server = new JSONRPCServer
      registerMethod(server, "sum", plus)
      registerMethod(server, "subtract", {"minuend", "subtrahend"}, difference)
      handleRequest(server, makeRequest("sum", {1, 2, 4}, 1))
      handleRequest(server, makeRequest("subtract", {42, 23}, 2))
    Text
      To see a list of registered methods for a given server, run
      @TO (methods, JSONRPCServer)@.
    Example
      methods server
    Text
      If you would like to deregister a method, then you may use
      @TO (remove, HashTable, Thing)@.
    Example
      remove(server, "sum")
      methods server
  Subnodes
    (methods, JSONRPCServer)
///

doc ///
  Key
    (methods, JSONRPCServer)
  Headline
    list the methods registered to a JSON-RPC server
  Usage
    methods server
  Inputs
    server:JSONRPCServer
  Outputs
    :NumberedVerticalList -- the methods registered to @CODE "server"@
  Description
    Text
      This method behaves much like the usual @TO methods@ method.  It lists
      each of the methods that have been registered to the given server.
    Example
      server = new JSONRPCServer
      -- sum and difference don't play nice w/ locate since they're compiled
      registerMethod(server, "add", (x, y) -> x + y)
      registerMethod(server, "subtract", {"x", "y"}, (x, y) -> x - y)
      methods server
    Text
      After running @CODE "methods"@, both @TO (locate, ZZ)@ and @TO (code, ZZ)@
      work as expected.
    Example
      locate 0
      -- TODO: code 0 (error: line number 1 not found in file stdio)
///

doc ///
  Key
     handleRequest
    (handleRequest, JSONRPCServer, String)
  Headline
    handle a JSON-RPC request
  Usage
    handleRequest(server, str)
  Inputs
    server:JSONRPCServer
    str:String -- a JSON-RPC 2.0 request object
  Outputs
    :{String,Nothing}
      the response to the request or null if the request was a notification
  Description
    Text
      This method processes an incoming JSON-RPC request. It takes a JSON-encoded
      string representing a request (or batch of requests) and returns the
      corresponding JSON-RPC response. It handles method invocation, error
      handling, and batching automatically.  Note that @TO makeRequest@ is
      useful for constructing requests.
    Example
      server = new JSONRPCServer
      registerMethod(server, "sum", plus)
      handleRequest(server, makeRequest("sum", {2, 3}, 1))
      handleRequest(server, makeRequest({("sum", {4, 5}, 2), ("sum", {6, 7}, 3)}))
    Text
      If you need to do any error handling in a method, then the
      @TO JSONRPCError@ class may be useful.
  Subnodes
    JSONRPCError
///

doc ///
  Key
     JSONRPCError
    (NewFromMethod, JSONRPCError, ZZ, String)
    (NewFromMethod, JSONRPCError, ZZ, String, Thing)
  Headline
    class for JSON-RPC errors
  Usage
    JSONRPCError(errCode, msg, data)
    JSONRPCError(errCode, msg)
  Inputs
    errCode:ZZ
    msg:String
    data:Thing
  Description
    Text
      This class is used to represent errors that occur during the processing
      of JSON-RPC requests. It provides a structured way to format error
      responses, including an error code, message, and optional additional
      data. This class ensures that errors are properly formatted according to
      the JSON-RPC 2.0 specification and can be easily included in responses to
      clients.

      Consider the following example.  The default response doesn't include a
      very useful error message.
    Example
      server = new JSONRPCServer
      registerMethod(server, "divide", (x, y) -> x/y)
      handleRequest(server, makeRequest("divide", {1, 0}, 1))
    Text
      Let's replace it with a more useful one.
    Example
      registerMethod(server, "divide", (x, y) -> (
	      if zero y then JSONRPCError(-32001, "division by zero")
	      else x/y))
      handleRequest(server, makeRequest("divide", {1, 0}, 1))
      handleRequest(server, makeRequest("divide", {22, 7}, 1))
    Text
      Note that the error codes -32000 to -32099 are reserved for use by
      JSON-RPC servers, so @CODE "errCode"@ should lie in this interval
      (although this isn't checked).
///

doc ///
  Key
     makeRequest
    (makeRequest, List)
    (makeRequest, String)
    (makeRequest, String, HashTable)
    (makeRequest, String, HashTable, String)
    (makeRequest, String, HashTable, ZZ)
    (makeRequest, String, List)
    (makeRequest, String, List, String)
    (makeRequest, String, List, ZZ)
    (makeRequest, String, String)
    (makeRequest, String, ZZ)
  Headline
    construct a JSON-RPC request
  Usage
    makeRequest(m, params, ID)
  Inputs
    m:String -- method name
    params:{HashTable,List} -- method parameters (optional)
    ID:{String,ZZ} -- id (if omitted, then the request is a notification)
  Outputs
    :String -- JSON-RPC request object
  Description
    Text
      This method constructs a JSON-RPC request. The @CODE "m"@ parameter
      specifies the name of the method being called, while @CODE "params"@
      contains the arguments for the method (if any). The optional @CODE "ID"@
      parameter is used to correlate requests and responses. This method
      generates a JSON-encoded string representing the request, ready to be
      sent to the server. If no @CODE "ID"@ is provided, then the request is a
      notification and the server will return nothing.
    Example
      makeRequest("foo", hashTable{"x" => 2, "y" => 3}, 1)
      makeRequest("baz", {4, 5, 6}, 2)
    Text
      When a list is given, its elements are expected to be sequences that
      can be passed to @TO makeRequest@.  The result is an array containing
      a batch request.
    Example
      makeRequest({
	      ("foo", hashTable{"x" => 7, "y" => 8}, 3),
	      ("bar", {9, 10, 11}, 4)})
///

doc ///
  Key
     setLogger
    (setLogger, JSONRPCServer, Function)
  Headline
    set up logging for a JSON-RPC server
  Usage
    setLogger(server, f)
  Inputs
    server:JSONRPCServer
    f:Function
  Description
    Text
      This method sets a custom logging function for the server. The provided
      logger function will be used to log relevant server activities, such as
      incoming requests and responses. This allows for flexible logging and
      monitoring, enabling users to integrate with existing logging systems or
      implement custom logging behavior.  The function @CODE "f"@ should accept
      a single string.

      Let's set it up so that our server logs both to @TO stderr@ and to
      a file.
    Example
      server = new JSONRPCServer
      registerMethod(server, "sum", plus)
      logfile = temporaryFileName();
      setLogger(server, x -> (printerr x; logfile << x << endl;));
      handleRequest(server, makeRequest("sum", {2, 3}, 1))
      handleRequest(server, makeRequest("sum", {4, 5}, 2))
      logfile << close;
      get logfile
///

---------------------------------------------------------
-- examples from https://www.jsonrpc.org/specification --
---------------------------------------------------------

TEST ///
-- positional parameters
needsPackage "JSON"
server = new JSONRPCServer
assertJSONRPC = (request, expected) -> assert BinaryOperation(symbol ===,
    fromJSON handleRequest(server, request), fromJSON expected)
registerMethod(server, "subtract", {"minuend", "subtrahend"}, (x, y) -> x - y)
assertJSONRPC("{\"jsonrpc\": \"2.0\", \"method\": \"subtract\", \"params\": [42, 23], \"id\": 1}",
    "{\"jsonrpc\": \"2.0\", \"result\": 19, \"id\": 1}")
assertJSONRPC("{\"jsonrpc\": \"2.0\", \"method\": \"subtract\", \"params\": [23, 42], \"id\": 2}",
    "{\"jsonrpc\": \"2.0\", \"result\": -19, \"id\": 2}")
///

TEST ///
-- named parameters
needsPackage "JSON"
server = new JSONRPCServer
assertJSONRPC = (request, expected) -> assert BinaryOperation(symbol ===,
    fromJSON handleRequest(server, request), fromJSON expected)
registerMethod(server, "subtract", {"minuend", "subtrahend"}, (x, y) -> x - y)
assertJSONRPC("{\"jsonrpc\": \"2.0\", \"method\": \"subtract\", \"params\": {\"subtrahend\": 23, \"minuend\": 42}, \"id\": 3}",
    "{\"jsonrpc\": \"2.0\", \"result\": 19, \"id\": 3}")
assertJSONRPC("{\"jsonrpc\": \"2.0\", \"method\": \"subtract\", \"params\": {\"minuend\": 42, \"subtrahend\": 23}, \"id\": 4}",
    "{\"jsonrpc\": \"2.0\", \"result\": 19, \"id\": 4}")
///

TEST ///
-- notifications
server = new JSONRPCServer
assertNull = request -> assert BinaryOperation(symbol ===,
    handleRequest(server, request), null)
assertNull "{\"jsonrpc\": \"2.0\",\"method\": \"update\", \"params\": [1,2,3,4,5]}"
assertNull "{\"jsonrpc\": \"2.0\", \"method\": \"foobar\"}"
///

TEST ///
-- non-existent method
needsPackage "JSON"
server = new JSONRPCServer
assertJSONRPC = (request, expected) -> assert BinaryOperation(symbol ===,
    fromJSON handleRequest(server, request), fromJSON expected)
assertJSONRPC("{\"jsonrpc\": \"2.0\", \"method\": \"foobar\", \"id\": \"1\"}",
    "{\"jsonrpc\": \"2.0\", \"error\": {\"code\": -32601, \"message\": \"Method not found\"}, \"id\": \"1\"}")
///

TEST ///
-- invalid json
needsPackage "JSON"
server = new JSONRPCServer
assertJSONRPC = (request, expected) -> assert BinaryOperation(symbol ===,
    fromJSON handleRequest(server, request), fromJSON expected)
assertJSONRPC("{\"jsonrpc\": \"2.0\", \"method\": \"foobar, \"params\": \"bar\", \"baz]",
    "{\"jsonrpc\": \"2.0\", \"error\": {\"code\": -32700, \"message\": \"Parse error\"}, \"id\": null}")
///

TEST ///
-- invalid request
needsPackage "JSON"
server = new JSONRPCServer
assertJSONRPC = (request, expected) -> assert BinaryOperation(symbol ===,
    fromJSON handleRequest(server, request), fromJSON expected)
assertJSONRPC("{\"jsonrpc\": \"2.0\", \"method\": 1, \"params\": \"bar\"}",
	"{\"jsonrpc\": \"2.0\", \"error\": {\"code\": -32600, \"message\": \"Invalid Request\"}, \"id\": null}")
///

TEST ///
-- batch, invalid json
needsPackage "JSON"
server = new JSONRPCServer
assertJSONRPC = (request, expected) -> assert BinaryOperation(symbol ===,
    fromJSON handleRequest(server, request), fromJSON expected)
assertJSONRPC("[
  {\"jsonrpc\": \"2.0\", \"method\": \"sum\", \"params\": [1,2,4], \"id\": \"1\"},
  {\"jsonrpc\": \"2.0\", \"method\"
]",
    "{\"jsonrpc\": \"2.0\", \"error\": {\"code\": -32700, \"message\": \"Parse error\"}, \"id\": null}")
///

TEST ///
-- empty array
needsPackage "JSON"
server = new JSONRPCServer
assertJSONRPC = (request, expected) -> assert BinaryOperation(symbol ===,
    fromJSON handleRequest(server, request), fromJSON expected)
assertJSONRPC("[]",
    "{\"jsonrpc\": \"2.0\", \"error\": {\"code\": -32600, \"message\": \"Invalid Request\"}, \"id\": null}")
///

TEST ///
-- invalid batch, not empty
needsPackage "JSON"
server = new JSONRPCServer
assertJSONRPC = (request, expected) -> assert BinaryOperation(symbol ===,
    fromJSON handleRequest(server, request), fromJSON expected)
assertJSONRPC("[1]",
    "[
  {\"jsonrpc\": \"2.0\", \"error\": {\"code\": -32600, \"message\": \"Invalid Request\"}, \"id\": null}
]")
///

TEST ///
-- invalid batch
needsPackage "JSON"
server = new JSONRPCServer
assertJSONRPC = (request, expected) -> assert BinaryOperation(symbol ===,
    fromJSON handleRequest(server, request), fromJSON expected)
assertJSONRPC("[1,2,3]",
    "[
  {\"jsonrpc\": \"2.0\", \"error\": {\"code\": -32600, \"message\": \"Invalid Request\"}, \"id\": null},
  {\"jsonrpc\": \"2.0\", \"error\": {\"code\": -32600, \"message\": \"Invalid Request\"}, \"id\": null},
  {\"jsonrpc\": \"2.0\", \"error\": {\"code\": -32600, \"message\": \"Invalid Request\"}, \"id\": null}
]")
///

TEST ///
-- batch
needsPackage "JSON"
server = new JSONRPCServer
assertJSONRPC = (request, expected) -> assert BinaryOperation(symbol ===,
    fromJSON handleRequest(server, request), fromJSON expected)
registerMethod(server, "sum", plus)
registerMethod(server, "subtract", {"minuend", "subtrahend"}, (x, y) -> x - y)
registerMethod(server, "get_data", () -> {"hello", 5})
assertJSONRPC("[
  {\"jsonrpc\": \"2.0\", \"method\": \"sum\", \"params\": [1,2,4], \"id\": \"1\"},
  {\"jsonrpc\": \"2.0\", \"method\": \"hello\", \"params\": [7]},
  {\"jsonrpc\": \"2.0\", \"method\": \"subtract\", \"params\": [42,23], \"id\": \"2\"},
  {\"foo\": \"boo\"},
  {\"jsonrpc\": \"2.0\", \"method\": \"foo.get\", \"params\": {\"name\": \"myself\"}, \"id\": \"5\"},
  {\"jsonrpc\": \"2.0\", \"method\": \"get_data\", \"id\": \"9\"}
]",
    "[
  {\"jsonrpc\": \"2.0\", \"result\": 7, \"id\": \"1\"},
  {\"jsonrpc\": \"2.0\", \"result\": 19, \"id\": \"2\"},
  {\"jsonrpc\": \"2.0\", \"error\": {\"code\": -32600, \"message\": \"Invalid Request\"}, \"id\": null},
  {\"jsonrpc\": \"2.0\", \"error\": {\"code\": -32601, \"message\": \"Method not found\"}, \"id\": \"5\"},
  {\"jsonrpc\": \"2.0\", \"result\": [\"hello\", 5], \"id\": \"9\"}
]")
///

TEST ///
-- batch, all notifications
server = new JSONRPCServer
assertNull = request -> assert BinaryOperation(symbol ===,
    handleRequest(server, request), null)
assertNull "[
  {\"jsonrpc\": \"2.0\", \"method\": \"sum\", \"params\": [1,2,4]},
  {\"jsonrpc\": \"2.0\", \"method\": \"hello\", \"params\": [7]}
]"
///

----------------------------------------------------------------
-- end of examples from https://www.jsonrpc.org/specification --
----------------------------------------------------------------

TEST ///
-- JSONRPCError
needsPackage "JSON"
server = new JSONRPCServer
assertJSONRPC = (request, expected) -> assert BinaryOperation(symbol ===,
    fromJSON handleRequest(server, request), fromJSON expected)
registerMethod(server, "foo", () -> JSONRPCError(1234, "bar"))
assertJSONRPC("{\"jsonrpc\": \"2.0\", \"method\": \"foo\", \"id\": 1}",
    "{\"error\": {\"code\": 1234, \"message\": \"bar\"}, \"jsonrpc\": \"2.0\", \"id\": 1}")
///

TEST ///
-- methods
server = new JSONRPCServer
nullf = () -> null
registerMethod(server, "foo", nullf)
registerMethod(server, "bar", nullf)
assert Equation(#methods server, 2)
assert BinaryOperation(symbol ===, locate 0, locate nullf)
///

TEST ///
-- logging
server = new JSONRPCServer
logstr = ""
registerMethod(server, "sum", plus)
setLogger(server, x -> logstr |= x | newline)
handleRequest(server, "{\"jsonrpc\": \"2.0\", \"method\": \"sum\", \"params\": [1,2,4], \"id\": \"1\"}")
assert match("received request: ", logstr)
assert match("calling method \"sum\" with params \\[1, 2, 4\\]", logstr)
assert match("method \"sum\" returned: 7", logstr)
assert match("sending response: ", logstr)
///

TEST ///
-- makeRequest
needsPackage "JSON"
server = new JSONRPCServer
assertJSONRPC = (request, expected) -> assert BinaryOperation(symbol ===,
    fromJSON handleRequest(server, request), fromJSON expected)
assertNull = request -> assert BinaryOperation(symbol ===,
    handleRequest(server, request), null)
registerMethod(server, "subtract", {"minuend", "subtrahend"}, (x, y) -> x - y)
registerMethod(server, "get_data", () -> {"hello", 5})
assertJSONRPC(
    makeRequest("subtract", hashTable {("minuend", 42), ("subtrahend", 23)}, "1"),
    "{\"result\": 19, \"jsonrpc\": \"2.0\", \"id\": \"1\"}")
assertJSONRPC(
    makeRequest("subtract", hashTable {("minuend", 42), ("subtrahend", 23)}, 2),
    "{\"result\": 19, \"jsonrpc\": \"2.0\", \"id\": 2}")
assertJSONRPC(makeRequest("subtract", {42, 23}, "3"),
    "{\"result\": 19, \"jsonrpc\": \"2.0\", \"id\": \"3\"}")
assertJSONRPC(makeRequest("subtract", {42, 23}, 4),
    "{\"result\": 19, \"jsonrpc\": \"2.0\", \"id\": 4}")
assertJSONRPC(makeRequest("get_data", "5"),
    "{\"result\": [\"hello\", 5], \"jsonrpc\": \"2.0\", \"id\": \"5\"}")
assertJSONRPC(makeRequest("get_data", 6),
    "{\"result\": [\"hello\", 5], \"jsonrpc\": \"2.0\", \"id\": 6}")
assertJSONRPC(makeRequest({("get_data", 7), ("get_data", 8)}),
    "[{\"result\": [\"hello\", 5], \"jsonrpc\": \"2.0\", \"id\": 7},
      {\"result\": [\"hello\", 5], \"jsonrpc\": \"2.0\", \"id\": 8}]")
assertNull makeRequest("hello", new HashTable)
assertNull makeRequest("hello", {})
assertNull makeRequest "hello"
///

TEST ///
-- one-argument methods
needsPackage "JSON"
server = new JSONRPCServer
assertJSONRPC = (request, expected) -> assert BinaryOperation(symbol ===,
    fromJSON handleRequest(server, request), fromJSON expected)
registerMethod(server, "addTwo", x -> x + 2)
registerMethod(server, "addThree", {"x"}, x -> x + 3)
assertJSONRPC(
    makeRequest("addTwo", {4}, 1),
    "{\"result\": 6, \"jsonrpc\": \"2.0\", \"id\": 1}")
assertJSONRPC(
    makeRequest("addThree", {5}, 2),
    "{\"result\": 8, \"jsonrpc\": \"2.0\", \"id\": 2}")
assertJSONRPC(
    makeRequest("addThree", hashTable {"x" => 6}, 2),
    "{\"result\": 9, \"jsonrpc\": \"2.0\", \"id\": 2}")
///
