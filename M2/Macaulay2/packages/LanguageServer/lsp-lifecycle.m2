callIfInitialized = (server, f) -> x -> (
    if not server.Initialized then raiseError(-32002, "Server Not Initialized")
    else if server.Shutdown then raiseError(-32600, "Invalid Request")
    else f x)

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
    addLSPMethod(server, "shutdown", () -> server.Shutdown = true);
    addMethod(server.JSONRPCServer, "exit", () -> (
	    close server.File;
	    exit if server.Shutdown then 0 else 1));
    )
