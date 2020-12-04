-----------------------------------------------------------------------------
-- Methods for processing and accessing examples from the documentation
-----------------------------------------------------------------------------
-* Exported:
 * capture
 * examples
 *-

processExamplesStrict = true

-----------------------------------------------------------------------------
-- local utilities
-----------------------------------------------------------------------------

M2outputRE      = "\n+(?=i+[1-9][0-9]* : )"
separateM2output = str -> drop(drop(separate(M2outputRE, str),1),-1)

-----------------------------------------------------------------------------
-- capture
-----------------------------------------------------------------------------

-- TODO: the output format is provisional
-- TODO: does't capture stderr
capture' := capture
capture = method(Options => { UserMode => true })
capture Net    := opts -> s -> capture(toString s,       opts)
capture List   := opts -> s -> capture(demark_newline s, opts)
-- TODO: do this in interp.dd instead
-- TODO: alternatively, change the setup to do this in a clean thread
capture String := opts -> s -> if opts.UserMode then capture' s else (
    -- output is (Boolean, String) => (Err?, Output)
    -- TODO: this should eventually be unnecessary
    oldThreadLocalVars := (gbTrace, debugLevel, errorDepth, interpreterDepth, debuggingMode, stopIfError, notify);
    interpreterDepth = 1;

    oldPrivateDictionary := User#"private dictionary";
    oldDictionaryPath := dictionaryPath;
    oldLoadedPackages := loadedPackages;
    oldCurrentPackage := currentPackage;
    pushvar(symbol OutputDictionary, new GlobalDictionary);

    User#"private dictionary" = new Dictionary;
    OutputDictionary = new GlobalDictionary;
    dictionaryPath = {
	User#"private dictionary", -- this is necessary mainly due to indeterminates.m2
	oldPrivateDictionary,
	Core.Dictionary,
	OutputDictionary,
	PackageDictionary};
    scan(Core#"pre-installed packages", needsPackage);
    needsPackage toString currentPackage;
    currentPackage = User;

    ret := capture' s;

    User#"private dictionary" = oldPrivateDictionary;
    dictionaryPath = oldDictionaryPath;
    loadedPackages = oldLoadedPackages;
    currentPackage = oldCurrentPackage;
    popvar symbol OutputDictionary;

    (gbTrace, debugLevel, errorDepth, interpreterDepth, debuggingMode, stopIfError, notify) = oldThreadLocalVars;
    ret)
protect symbol capture

-----------------------------------------------------------------------------
-- extract examples
-----------------------------------------------------------------------------

extractExamplesLoop := method(Dispatch => Thing)
extractExamplesLoop Thing       := x -> ()
extractExamplesLoop Sequence    :=
extractExamplesLoop Hypertext   := x -> deepSplice apply(toSequence x, extractExamplesLoop)
extractExamplesLoop ExampleItem := toSequence

extractExamples = docBody -> (
    ex := toList extractExamplesLoop docBody;
    -- don't convert "ex" on the next line to a sequence,
    -- because the hash code for caching example outputs will change
    if #ex > 0 then currentPackage#"example inputs"#(format currentDocumentTag) = ex;
    docBody)

-----------------------------------------------------------------------------
-- examples: get a list of examples in a documentation node
-----------------------------------------------------------------------------

examples = method(Dispatch => Thing)
examples Hypertext := dom -> raise(stack extractExamplesLoop dom, -1)
examples Thing     := key -> (
    rawdoc := fetchAnyRawDocumentation makeDocumentTag key;
    if rawdoc =!= null and rawdoc.?Description then examples DIV{rawdoc.Description})

-----------------------------------------------------------------------------
-- storeExampleOutput
-----------------------------------------------------------------------------

getExampleOutputFilename := (pkg, fkey) -> (
    if pkg#?"package prefix" and pkg#"package prefix" =!= null then (
	packageLayout := detectCurrentLayout pkg#"package prefix";
	if packageLayout === null then error "internal error: package layout not detected";
	pkg#"package prefix" | replace("PKG", pkg#"pkgname", Layout#packageLayout#"packageexampleoutput") | toFilename fkey | ".out")
    else error "internal error: package prefix is undefined")

getExampleOutput := (pkg, fkey) -> (
    -- TODO: only get from cache if the hash hasn't changed
    if pkg#"example results"#?fkey then return pkg#"example results"#fkey;
    verboseLog := if debugLevel > 1 then printerr else identity;
    filename := getExampleOutputFilename(pkg, fkey);
    output := if fileExists filename
    then ( verboseLog("info: reading cached example results from ", filename); get filename )
    else if width (ex := examples fkey) =!= 0
    then ( verboseLog("info: capturing example results on-demand"); last capture(ex, UserMode => false) );
    pkg#"example results"#fkey = if output === null then {} else separateM2output output)

-- used in installPackage.m2
-- TODO: store in a database instead
storeExampleOutput = (pkg, fkey, outf, verboseLog) -> (
    verboseLog("storing example results from output file", minimizeFilename outf);
    if fileExists outf then (
	outstr := reproduciblePaths get outf;
	outf << outstr << close;
	pkg#"example results"#fkey = separateM2output outstr)
    else verboseLog("warning: missing file ", outf));

-- used in installPackage.m2
captureExampleOutput = (pkg, fkey, inputs, cacheFunc, inf, outf, errf, inputhash, changeFunc, usermode, verboseLog) -> (
    stdio << flush; -- just in case previous timing information hasn't been flushed yet
    desc := "example results for " | format fkey;
    -- try capturing in the same process
    if  not match("no-capture-flag", inputs) -- this flag is really necessary, but only sometimes
    -- FIXME: these are workarounds to prevent bugs, in order of priority for being fixed:
    and not match("(gbTrace|read|run|stderr|stdio|print|<<)", inputs) -- stderr and prints are not handled correctly
    and not match("(notify|stopIfError|debuggingMode)", inputs) -- stopIfError and debuggingMode may be fixable
    and not match("([Cc]ommand|schedule|thread|Task)", inputs) -- remove when threads work more predictably
    and not match("(temporaryFileName)", inputs) -- this is sometimes bug prone
    and not match("(installMethod|load|export|newPackage)", inputs) -- exports may land in the package User
    and not match("(GlobalAssignHook|GlobalReleaseHook)", inputs) -- same as above
    and not match({"ThreadedGB", "RunExternalM2"}, pkg#"pkgname") -- TODO: eventually remove
    and false -- TODO: this is temporarily here, to be removed after v1.17 is released
    then (
	desc = concatenate(desc, 62 - #desc);
	stderr << commentize pad("capturing " | desc, 72) << flush; -- the timing info will appear at the end
	(err, output) := evaluateWithPackage(pkg, inputs, capture_(UserMode => false));
	if not err then return outf << "-- -*- M2-comint -*- hash: " << inputhash << endl << output << close);
    -- fallback to using an external process
    stderr << commentize pad("making " | desc, 72) << flush;
    data := if pkg#"example data files"#?fkey then pkg#"example data files"#fkey else {};
    inf << replace("-\\* no-capture-flag \\*-", "", inputs) << endl << close;
    if runFile(inf, inputhash, outf, errf, pkg, changeFunc fkey, usermode, data)
    then ( removeFile inf; cacheFunc fkey ))

-----------------------------------------------------------------------------
-- process examples
-----------------------------------------------------------------------------
-- TODO: make this reentrant

local currentExampleKey
local currentExampleCounter
local currentExampleResults

processExamplesLoop = method(Dispatch => Thing)
processExamplesLoop Thing       := identity
processExamplesLoop Sequence    :=
processExamplesLoop Hypertext   := x -> apply(x, processExamplesLoop)
processExamplesLoop ExampleItem := x -> (
    result := if currentExampleResults#?currentExampleCounter then PRE currentExampleResults#currentExampleCounter else (
	if #currentExampleResults === currentExampleCounter then (
	    if processExamplesStrict
	    then error("example results terminate prematurely: ", toString currentExampleKey)
	    else printerr("warning: example results terminate prematurely: ", toString currentExampleKey));
	PRE concatenate("i", toString (currentExampleCounter + 1), " : -- example results terminated prematurely"));
    currentExampleCounter = currentExampleCounter + 1;
    result)

processExamples = (pkgname, fkey, docBody) -> (
    pkg := getpkg pkgname;
    currentExampleKey = fkey;
    currentExampleCounter = 0;
    currentExampleResults = getExampleOutput(pkg, fkey);
    if #currentExampleResults > 0 then processExamplesLoop docBody else docBody)
