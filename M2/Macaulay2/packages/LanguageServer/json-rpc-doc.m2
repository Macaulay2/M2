doc ///
  Key
    JSONRPCServer
  Headline
    JSON-RPC server
  Usage
    new JSONRPCserver
  Description
    Text
      A @HREF("https://www.jsonrpc.org/specification", "JSON-RPC")@ server.
      Add methods to the server using @TO addMethod@ and handle
      requests to the server using @TO handleRequest@.
    Example
      server = new JSONRPCServer
      addMethod(server, "hello", () -> "Hello, world!")
      handleRequest(server, "{\"jsonrpc\": \"2.0\", \"method\": \"hello\", \"id\": 1}")
///

doc ///
  Key
    addMethod
    (addMethod, JSONRPCServer, String, Function)
    (addMethod, JSONRPCServer, String, List, Function)
  Headline
    add a method to a JSON-RPC server
  Usage
    addMethod(server, name, f)
    addMethod(server, name, params, f)
  Inputs
    server:JSONRPCServer
    name:String -- the name of the method
    params:List -- containing strings, the parameters of the method (optional)
    f:Function -- the implementation of the method
  Description
    Text
      Add a method to the given @TO JSONRPCServer@. The method should have a
      name (as a string), (optionally) a list of parameters (each strings),
      and a function to run when called.
    Example
      server = new JSONRPCServer
      addMethod(server, "hello", () -> "Hello, world!")
      addMethod(server, "add", {"x", "y"}, (x, y) -> x + y)
///

-- low-level, not intended for user
undocumented {
    (handleRequest, JSONRPCServer, HashTable),
    (handleRequest, JSONRPCServer, List),
    (handleRequest, JSONRPCServer, Thing)}

doc ///
  Key
    handleRequest
    (handleRequest, JSONRPCServer, String)
  Headline
    handle a request to a JSON-RPC server
  Usage
    handleRequest(server, request)
  Inputs
    server:JSONRPCServer
    request:String
  Description
    Text
      Handle a request to a JSON-RPC server.  Requests should be in
      the form of a valid JSON-RPC string and call methods that have
      been installed with @TO addMethod@.  The return value is a
      JSON-RPC string.
    Example
      server = new JSONRPCServer
      handleRequest(server, "{\"jsonrpc\": \"2.0\", \"method\": \"hello\", \"id\": 1}")
      addMethod(server, "hello", () -> "Hello, world!")
      handleRequest(server, "{\"jsonrpc\": \"2.0\", \"method\": \"hello\", \"id\": 2}")
    Text
      If there is no @SAMP "id"@, then the request is treated as a
      notification and nothing is returned.
    Example
      handleRequest(server, "{\"jsonrpc\": \"2.0\", \"method\": \"hello\"}")
    Text
      If a method has parameters, then a @SAMP "\"params\""@ key
      should be included in the request.  Its value may either be an
      array of values or an object with key-value pairs, the keys
      being the names of the parameters.
    Example
      addMethod(server, "add", {"x", "y"}, (x, y) -> x + y)
      handleRequest(server, "{\"jsonrpc\": \"2.0\", \"method\": \"add\", \"params\": [2, 3], \"id\": 3}")
      handleRequest(server, "{\"jsonrpc\": \"2.0\", \"method\": \"add\", \"params\": {\"x\": 2, \"y\": 3}, \"id\": 4}")
///
