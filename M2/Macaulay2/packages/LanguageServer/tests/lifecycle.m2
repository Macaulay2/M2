needsPackage "JSON"
needsPackage "JSONRPC"

server = (new LSPServer)#"JSON-RPC server"

-- before initialize, LSP methods return "Server Not Initialized" (-32002)
response = fromJSON handleRequest(server, makeRequest("textDocument/hover",
        hashTable {
            "textDocument" => hashTable {"uri" => "test.m2"},
            "position" => hashTable {"line" => 0, "character" => 0}}, 1))
assert Equation(response#"error"#"code", -32002)

-- initialize
response = fromJSON handleRequest(server, makeRequest("initialize", {}, 2))
capabilities = response#"result"#"capabilities"
assert capabilities#?"hoverProvider"
assert capabilities#?"definitionProvider"
assert capabilities#?"completionProvider"
assert Equation(capabilities#"textDocumentSync", 1)
assert Equation(response#"result"#"serverInfo"#"name",
    "Macaulay2 Language Server")

-- shutdown disables LSP methods (-32600)
handleRequest(server, makeRequest("shutdown", 3))
response = fromJSON handleRequest(server, makeRequest("textDocument/hover",
        hashTable {
            "textDocument" => hashTable {"uri" => "test.m2"},
            "position" => hashTable {"line" => 0, "character" => 0}}, 4))
assert Equation(response#"error"#"code", -32600)
