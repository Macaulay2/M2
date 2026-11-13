needsPackage "JSONRPC"
needsPackage "JSON"

server = (new LSPServer)#"JSON-RPC server"
handleRequest(server, makeRequest("initialize", {}, 1))
uri = "test.m2"
handleRequest(server, makeRequest("textDocument/didOpen",
        hashTable {"textDocument" => hashTable {
                "uri" => uri, "text" => "ring\n"}}))
response = fromJSON handleRequest(server, makeRequest("textDocument/definition",
        hashTable {
            "textDocument" => hashTable {"uri" => uri},
            "position" => hashTable {"line" => 0, "character" => 0}}, 1))
locs = response#"result"
assert BinaryOperation(symbol >, #locs, 0)
assert all(locs, loc -> loc#?"uri" and loc#?"range")
