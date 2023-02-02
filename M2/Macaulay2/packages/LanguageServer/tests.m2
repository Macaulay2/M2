TEST ///
-- examples from https://www.jsonrpc.org/specification
needsPackage "JSON"
 server = new JSONRPCServer
assertNull = request -> assert BinaryOperation(symbol ===,
    handleRequest(server, request), null)
assertJSONRPC = (request, expected) -> assert BinaryOperation(symbol ===,
    fromJSON handleRequest(server, request), fromJSON expected)
addMethod(server, "subtract", {"minuend", "subtrahend"}, (x, y) -> x - y)
addMethod(server, "sum", plus)
addMethod(server, "get_data", () -> {"hello", 5})
-- positional parameters
assertJSONRPC("{\"jsonrpc\": \"2.0\", \"method\": \"subtract\", \"params\": [42, 23], \"id\": 1}",
    "{\"jsonrpc\": \"2.0\", \"result\": 19, \"id\": 1}")
assertJSONRPC("{\"jsonrpc\": \"2.0\", \"method\": \"subtract\", \"params\": [23, 42], \"id\": 2}",
    "{\"jsonrpc\": \"2.0\", \"result\": -19, \"id\": 2}")
-- named parameters
assertJSONRPC("{\"jsonrpc\": \"2.0\", \"method\": \"subtract\", \"params\": {\"subtrahend\": 23, \"minuend\": 42}, \"id\": 3}",
    "{\"jsonrpc\": \"2.0\", \"result\": 19, \"id\": 3}")
assertJSONRPC("{\"jsonrpc\": \"2.0\", \"method\": \"subtract\", \"params\": {\"minuend\": 42, \"subtrahend\": 23}, \"id\": 4}",
    "{\"jsonrpc\": \"2.0\", \"result\": 19, \"id\": 4}")
-- notifications
assertNull "{\"jsonrpc\": \"2.0\",\"method\": \"update\", \"params\": [1,2,3,4,5]}"
assertNull "{\"jsonrpc\": \"2.0\", \"method\": \"foobar\"}"
-- non-existent method
assertJSONRPC("{\"jsonrpc\": \"2.0\", \"method\": \"foobar\", \"id\": \"1\"}",
    "{\"jsonrpc\": \"2.0\", \"error\": {\"code\": -32601, \"message\": \"Method not found\"}, \"id\": \"1\"}")
-- invalid json
assertJSONRPC("{\"jsonrpc\": \"2.0\", \"method\": \"foobar, \"params\": \"bar\", \"baz]",
    "{\"jsonrpc\": \"2.0\", \"error\": {\"code\": -32700, \"message\": \"Parse error\"}, \"id\": null}")
-- invalid request
assertJSONRPC("{\"jsonrpc\": \"2.0\", \"method\": 1, \"params\": \"bar\"}",
	"{\"jsonrpc\": \"2.0\", \"error\": {\"code\": -32600, \"message\": \"Invalid Request\"}, \"id\": null}")
-- batch, invalid json
assertJSONRPC("[
  {\"jsonrpc\": \"2.0\", \"method\": \"sum\", \"params\": [1,2,4], \"id\": \"1\"},
  {\"jsonrpc\": \"2.0\", \"method\"
]",
    "{\"jsonrpc\": \"2.0\", \"error\": {\"code\": -32700, \"message\": \"Parse error\"}, \"id\": null}")
-- empty array
assertJSONRPC("[]",
    "{\"jsonrpc\": \"2.0\", \"error\": {\"code\": -32600, \"message\": \"Invalid Request\"}, \"id\": null}")
-- invalid batch, not empty
assertJSONRPC("[1]",
    "[
  {\"jsonrpc\": \"2.0\", \"error\": {\"code\": -32600, \"message\": \"Invalid Request\"}, \"id\": null}
]")
-- invalid batch
assertJSONRPC("[1,2,3]",
    "[
  {\"jsonrpc\": \"2.0\", \"error\": {\"code\": -32600, \"message\": \"Invalid Request\"}, \"id\": null},
  {\"jsonrpc\": \"2.0\", \"error\": {\"code\": -32600, \"message\": \"Invalid Request\"}, \"id\": null},
  {\"jsonrpc\": \"2.0\", \"error\": {\"code\": -32600, \"message\": \"Invalid Request\"}, \"id\": null}
]")
-- batch
assertJSONRPC("[
  {\"jsonrpc\": \"2.0\", \"method\": \"sum\", \"params\": [1,2,4], \"id\": \"1\"},
  {\"jsonrpc\": \"2.0\", \"method\": \"notify_hello\", \"params\": [7]},
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
-- batch, all notifications
assertNull "[
  {\"jsonrpc\": \"2.0\", \"method\": \"notify_sum\", \"params\": [1,2,4]},
  {\"jsonrpc\": \"2.0\", \"method\": \"notify_hello\", \"params\": [7]}
]"
///

TEST ///
runLanguageServer(); stdio << "foo" << endl; read stdio
 
