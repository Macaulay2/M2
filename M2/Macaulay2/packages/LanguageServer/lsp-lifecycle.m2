callIfInitialized = (server, f) -> x -> (
    if server.Initialized then f x
    else raiseError(-32002, "Server Not Initialized"))

addLSPMethod = method()
addLSPMethod(LSPServer, String, Function) := (server, name, f) -> (
    addMethod(server.JSONRPCServer, name, callIfInitialized(server, f)))
addLSPMethod(LSPServer, String, List, Function) := (
    server, name, params, f) -> (
    addMethod(server.JSONRPCServer, name, params, callIfInitialized(server, f)))

addLifecycleMethods = server -> (
    addMethod(server.JSONRPCServer, "initialize", () -> (
	    server.Initialized = true;
	    hashTable {
		"capabilities" => hashTable {},
		"serverInfo" => hashTable {
		    "name" => "Macaulay2 Language Server",
		    "version" => LanguageServer.Options.Version}}));
    )
