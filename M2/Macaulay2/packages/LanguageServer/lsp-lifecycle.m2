addMethod(server#"json-rpc server", "initialize", () -> (
	server#"initialized" = true;
	hashTable {
	    "capabilities" => hashTable {},
	    "serverInfo" => hashTable {
		"name" => "Macaulay2 Language Server",
		"version" => LanguageServer.Options.Version}}))
