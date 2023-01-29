callIfInitialized = f -> x -> (
    if server#"initialized" then f x
    else raiseError(-32002, "Server Not Initialized"))

addLSPMethod = method()
addLSPMethod(String, Function) := (name, f) -> (
    addMethod(server#"json-rpc server", name, callIfInitialized f))
addLSPMethod(String, List, Function) := (name, params, f) -> (
    addMethod(server#"json-rpc server", name, params, callIfInitialized f))

addMethod(server#"json-rpc server", "initialize", () -> (
	server#"initialized" = true;
	hashTable {
	    "capabilities" => hashTable {},
	    "serverInfo" => hashTable {
		"name" => "Macaulay2 Language Server",
		"version" => LanguageServer.Options.Version}}))
