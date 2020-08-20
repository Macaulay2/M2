Program = new Type of HashTable
ProgramRun = new Type of HashTable
programPaths = new MutableHashTable

-- we expect a trailing slash in the path, but the paths given in the
-- PATH environment variable likely will not have one, so we add one
-- if needed
addSlash = programPath -> (
    if last programPath != "/" then return programPath | "/"
    else return programPath
)

checkProgramPath = (name, cmds, opts) -> (
    if all(cmds, cmd -> run(cmd | " >/dev/null 2>&1") == 0) then (
	if opts.Verbose == true then print("    found");
	return true;
    ) else (
	if opts.Verbose == true then print("    not found");
	return false;
    )
)

addPrefix = (cmd, prefix) ->
    if match(prefix_0, first separate(" ", cmd)) then prefix_1 | cmd else cmd

getProgramPath = (name, cmds, opts) -> (
    pathsToTry := {};
    -- try user-configured path first
    if programPaths#?name then
	pathsToTry = append(pathsToTry, programPaths#name);
    -- now try M2-installed path
    pathsToTry = append(pathsToTry, prefixDirectory | currentLayout#"programs");
    -- any additional paths specified by the caller
    pathsToTry = pathsToTry | opts.AdditionalPaths;
    -- finally, try PATH
    if getenv "PATH" != "" then
	pathsToTry = join(pathsToTry, separate(":", getenv "PATH"));
    pathsToTry = apply(pathsToTry, addSlash);
    prefixes := {(".*", "")} | opts.Prefix;
    scan(pathsToTry, pathToTry -> (
	if opts.Verbose == true then
	    print("checking for " | name | " in " | pathToTry | "...");
	prefix := scan(prefixes, prefix -> (
	    if opts.Verbose == true and #prefixes > 1 then
		print("  trying prefix \"" | prefix_1 |
		    "\" for executables matching \"" | prefix_0 | "\"...");
	    if checkProgramPath(name, apply(cmds, cmd ->
		pathToTry | addPrefix(cmd, prefix)), opts) then break prefix)
	);
	if prefix =!= null then break (pathToTry, prefix)
    ))
)

findProgram = method(TypicalValue => Program,
    Options => {
	RaiseError => true,
	Verbose => false,
	Prefix => {},
	AdditionalPaths => {}
    })
findProgram(String, String) := opts -> (name, cmd) ->
    findProgram(name, {cmd}, opts)
findProgram(String, List) := opts -> (name, cmds) -> (
    programPathAndPrefix := getProgramPath(name, cmds, opts);
    if programPathAndPrefix === null then
	if opts.RaiseError then error("could not find " | name)
	else return null;
    new Program from {"name" => name, "path" => programPathAndPrefix_0,
	"prefix" => programPathAndPrefix_1}
)

runProgram = method(TypicalValue => ProgramRun,
    Options => {
	RaiseError => true,
	KeepFiles => false,
	Verbose => false,
	RunDirectory => null
	})
runProgram(Program, String) := opts -> (program, args) ->
    runProgram(program, program#"name", args, opts)
runProgram(Program, String, String) := opts -> (program, name, args) -> (
    tmpFile := temporaryFileName();
    outFile := tmpFile | ".out";
    errFile := tmpFile | ".err";
    cmd := if opts.RunDirectory =!= null then (
	if not isDirectory opts.RunDirectory then
	    makeDirectory opts.RunDirectory;
	"cd " | opts.RunDirectory | " && " ) else "";
    cmd = cmd | program#"path" | addPrefix(name, program#"prefix") | " " | args;
    returnValue := run (cmd | " > " | outFile | " 2> " | errFile);
    message := "running: " | cmd | "\n";
    output := get outFile;
    if output != "" then message = message | output;
    err := get errFile;
    if err != "" then message = message | err;
    if opts.Verbose then print(message);
    result := {
	"command" => cmd,
	"output" => output,
	"error" => err,
	"return value" => returnValue};
    if opts.KeepFiles then result = result | {
	"output file" => outFile,
	"error file" => errFile}
    else (
	removeFile outFile;
	removeFile errFile;
    );
    if opts.RaiseError and returnValue != 0 then error(
	program#"name" | " returned an error" |
	if opts.Verbose then "" else "\n" | message);
    new ProgramRun from result
)

net Program := program -> program#"name"
net ProgramRun := pr -> net pr#"return value"
