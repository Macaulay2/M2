Program = new Type of HashTable
ProgramRun = new Type of HashTable
programPaths = new MutableHashTable

fixPath = programPath -> (
    -- escape any unescaped spaces or parentheses
    programPath = replace(///(?<!\\)([ ()])///, ///\\\1///, programPath);
    -- we expect a trailing slash in the path, but the paths given in the
    -- PATH environment variable likely will not have one, so we add one
    -- if needed
    if last programPath != "/" then programPath | "/"
    else programPath
)

-- returns (found, thisVersion)
-- found is an integer:
--   0 = successfully found program
--   1 = could not find program
--   2 = found program, but too version number too low
--   3 = found program, but could not determine version number
-- thisVersion is a string containing the version number found, or null
--   if MinimumVersion option is not given or the version number can't be
--   be determined.
checkProgramPath = (name, cmds, pathToTry, prefix, opts) -> (
    -- unescape spaces/parentheses and resolve HOME for fileExists and fileMode
    unescapedPathToTry := replace(///^\$\{?HOME\}?///, getenv "HOME",
	replace(///\\([ ()])///, ///\1///, pathToTry));
    found := if all(apply(cmds, cmd -> addPrefix(cmd, prefix)), cmd -> (
	exe := unescapedPathToTry | first separate(" ", cmd);
	if not fileExists exe then false else
	-- check executable bit (0111)
	if fileMode exe & 73 == 0 then false else
	run(pathToTry | cmd | " > /dev/null 2>&1") == 0)) then 0 else 1;
    msg := "";
    thisVersion := null;
    if found == 0 then (
	if opts.MinimumVersion === null then msg = "    found"
	else (
	    thisVersion = replace("(^\\s+)|(\\s+$)", "", get("!" | pathToTry |
		addPrefix(opts.MinimumVersion_1, prefix)));
	    if not match(///^\d[\-+\.:\~\da-zA-Z]*$///, thisVersion) then (
		msg = "    found version \"" | thisVersion |
		    "\", but this does not appear to be a valid version number";
		found = 3;
		thisVersion = null;
	    ) else (
		if thisVersion >= opts.MinimumVersion_0 then
		    msg = "    found version " | thisVersion |
			" >= " | opts.MinimumVersion_0
		else (
		    msg = "    found, but version " | thisVersion | " < " |
			opts.MinimumVersion_0;
		    found = 2;
		)
	    )
	)
    ) else msg = "    not found";
    if opts.Verbose == true then print(msg);
    (found, thisVersion)
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
	pathsToTry = join(pathsToTry,
	    apply(separate(":", getenv "PATH"), dir ->
		if dir == "" then "." else dir));
    pathsToTry = fixPath \ pathsToTry;
    prefixes := {(".*", "")} | opts.Prefix;
    errorCode := 1;
    versionFound := "0.0";
    result := scan(pathsToTry, pathToTry -> (
	if opts.Verbose == true then
	    print("checking for " | name | " in " | pathToTry | "...");
	prefix := scan(prefixes, prefix -> (
	    if opts.Verbose == true and #prefixes > 1 then
		print("  trying prefix \"" | prefix_1 |
		    "\" for executables matching \"" | prefix_0 | "\"...");
	    result := checkProgramPath(name, cmds, pathToTry, prefix, opts);
	    if result_0 == 0 then (
		errorCode = 0;
		versionFound = result_1;
		break prefix
	    ) else if result_0 == 2 and result_1 > versionFound then (
		errorCode = 2;
		versionFound = result_1;
	    ) else if result_0 == 3 then errorCode = 3;
	));
	if errorCode == 0 then
	    break (errorCode, pathToTry, prefix, versionFound)
    ));
    if result =!= null then result else (errorCode, null, null, versionFound)
)

findProgram = method(TypicalValue => Program,
    Options => {
	RaiseError => true,
	Verbose => false,
	Prefix => {},
	AdditionalPaths => {},
	MinimumVersion => null
    })
findProgram(String, String) := opts -> (name, cmd) ->
    findProgram(name, {cmd}, opts)
findProgram(String, List) := opts -> (name, cmds) -> (
    programInfo := getProgramPath(name, cmds, opts);
    if programInfo_0 != 0 then
	if opts.RaiseError then (
	    msg := "";
	    if programInfo_0 == 1 then
		msg = "could not find " | name
	    else if programInfo_0 == 2 then
		msg = "found " | name | ", but version (" | programInfo_3 |
		    ") is too low"
	    else if programInfo_0 == 3 then
		msg = "found " | name | ", but could not determine version";
	    error(msg)
	) else return null;
    new Program from {"name" => name, "path" => programInfo_1,
	"prefix" => programInfo_2} |
	if opts.MinimumVersion =!= null then {"version" => programInfo_3}
	else {}
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
