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
foundProgram              := 0
didNotFindProgram         := 1
foundProgramButOldVersion := 2
foundProgramButBadVersion := 3
-- thisVersion is a string containing the version number found, or null
--   if MinimumVersion option is not given or the version number can't be
--   be determined.
checkProgramPath = (cmds, pathToTry, prefix, opts) -> (
    verboseLog := if opts.Verbose then printerr else identity;
    -- unescape spaces/parentheses and resolve HOME for fileExists and fileMode
    unescapedPathToTry := replace(///^\$\{?HOME\}?///, getenv "HOME",
	replace(///\\([ ()])///, ///\1///, pathToTry));
    found := if all(apply(cmds, cmd -> addPrefix(cmd, prefix)), cmd -> (
	exe := unescapedPathToTry | first separate(" ", cmd);
	if not fileExists exe then (
	    verboseLog(exe, " does not exist"); false) else
	-- check executable bit
	if fileMode exe & 0o111 == 0 then (
	    verboseLog(exe, " exists but is not executable"); false) else (
	    verboseLog(exe, " exists and is executable");
	    verboseLog("running ", format(pathToTry | cmd), ":");
	    ret := run(pathToTry | cmd |
		if opts.Verbose then "" else " > /dev/null 2>&1" );
	    verboseLog("return value: " | ret);
	    ret == 0))) then foundProgram else didNotFindProgram;
    thisVersion := null;
    if found == foundProgram and opts.MinimumVersion =!= null then (
	thisVersion = replace("(^\\s+)|(\\s+$)", "", get("!" | pathToTry |
		addPrefix(opts.MinimumVersion_1, prefix)));
	if not match(///^\d[\-+\.:\~\da-zA-Z]*$///, thisVersion) then (
	    verboseLog("found version ", format thisVersion,
		" but this does not appear to be a valid version number");
	    found = foundProgramButBadVersion;
	    thisVersion = null;
	) else if thisVersion >= opts.MinimumVersion_0 then
	    verboseLog("found version ", thisVersion, " >= ",
		opts.MinimumVersion_0)
	else (
	    verboseLog("found, but version ", thisVersion, " < ",
		opts.MinimumVersion_0);
	    found = foundProgramButOldVersion;
	)
    );
    (found, thisVersion)
)

addPrefix = (cmd, prefix) ->
    if match(prefix_0, first separate(" ", cmd)) then prefix_1 | cmd else cmd

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
    if not (instance(opts.Prefix, List) and
	all(opts.Prefix, x -> instance(x, Sequence)) and
	all(opts.Prefix, x -> class \ x === (String, String))) then
	error "expected Prefix to be a list of sequences of two strings";
    if not (instance(opts.AdditionalPaths, List) and
	all(opts.AdditionalPaths, x -> instance(x, String))) then
	error "expected AdditionalPaths to be a list of strings";
    if opts.MinimumVersion =!= null and not(
	instance(opts.MinimumVersion, Sequence) and
	class \ opts.MinimumVersion === (String, String)) then
	error "expected MinimumVersion to be a sequence of two strings";
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
    errorCode := didNotFindProgram;
    versionFound := "0.0";
    for pathToTry in pathsToTry do for prefix in prefixes do (
	(found, thisVersion) := checkProgramPath(cmds, pathToTry, prefix, opts);
	if found == foundProgram then return new Program from {
	    "name" => name,
	    "path" => pathToTry,
	    "prefix" => prefix } | if opts.MinimumVersion =!= null then {
	    "version" => thisVersion} else {} else
	if found != didNotFindProgram then (
	    errorCode = found;
	    if found == foundProgramButOldVersion and
		thisVersion > versionFound then versionFound = thisVersion
	    )
	);
    if opts.RaiseError then error(
	if errorCode == didNotFindProgram then "could not find " | name else
	if errorCode == foundProgramButOldVersion then "found " | name |
	    ", but version (" | versionFound | ") is too low" else
	if errorCode == foundProgramButBadVersion then "found " | name |
	    ", but could not determine version" else "unknown error");
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

net Program := toString Program := program -> program#"name"
html Program := html @@ toString
net ProgramRun := pr -> net pr#"return value"
