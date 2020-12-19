-----------------------------------------------------------------------------
-- Methods for processing and accessing examples from the documentation
-----------------------------------------------------------------------------
-* Exported:
 * EXAMPLE
 * capture
 * examples
 *-

processExamplesStrict = true

-----------------------------------------------------------------------------
-- local utilities
-----------------------------------------------------------------------------

M2outputRE       = "\n+(?=i+[1-9][0-9]* : )"
M2outputHash     = "-- -*- M2-comint -*- hash: "
separateM2output = str -> (
    L := separate(M2outputRE, replace("(\\A\n+|\n+\\Z)", "", str));
    if match(regexQuote M2outputHash, str) then drop(drop(L, -1), 1) else L)

trimlines := L -> apply(L, x ->
    if instance(x, String) then (
	s := lines x;
	r := if s#?0 then demark_newline prepend(replace("^[[:space:]]+", "", s#0), drop(s, 1)) else x;
	if #r > 0 then r)
    else x)

-----------------------------------------------------------------------------
-- EXAMPLE
-----------------------------------------------------------------------------

makeExampleItem = method()
-- TODO: can this be handled with a NewFromMethod?
makeExampleItem PRE    := p -> flatten apply(toList p, s -> PRE \ separateM2output s)
makeExampleItem String := s -> ExampleItem s

-- allows canned examples with EXAMPLE PRE "..."
EXAMPLE = method(Dispatch => Thing)
EXAMPLE PRE         :=
EXAMPLE String      := x -> EXAMPLE {x}
EXAMPLE VisibleList := x -> (
    L := flatten \\ makeExampleItem \ nonnull trimlines toList x;
    if #L == 0 then error "EXAMPLE: empty list of examples encountered";
    TABLE flatten {"class" => "examples", apply(L, item -> TR TD item)})

-----------------------------------------------------------------------------
-- capture
-----------------------------------------------------------------------------

-- TODO: the output format is provisional
-- TODO: does't capture stderr
capture' := capture
capture = method(Options => { UserMode => true, Package => null })
capture Net    := opts -> s -> capture(toString s,       opts)
capture List   := opts -> s -> capture(demark_newline s, opts)
-- TODO: do this in interp.dd instead
-- TODO: alternatively, change the setup to do this in a clean thread
capture String := opts -> s -> if opts.UserMode then capture' s else (
    -- output is (Boolean, String) => (Err?, Output)
    -- TODO: this should eventually be unnecessary
    oldMutableVars := new MutableHashTable;
    scan(flatten apply(loadedPackages, pkg -> pkg#"exported mutable symbols"), symb -> oldMutableVars#symb = value symb);
    -* see run.m2 for details of defaultMode, argumentMode, etc. *-
    -- TODO: somehow use SetUlimit, GCMAXHEAP, GCSTATS, GCVERBOSE,
    --       ArgInt, ArgQ, ArgNoReadline, ArgNoSetup, and ArgNoThreads
    argmode := if 0 < argumentMode & InvertArgs then xor(defaultMode, argumentMode) else argumentMode;
    hasmode := m -> argmode & m == m;
    pushvar(symbol randomSeed, if hasmode ArgNoRandomize then 0 else randomSeed);
    -- FIXME: https://github.com/Macaulay2/M2/issues/1536#issuecomment-721826413
    -- if hasmode ArgStop        then (stopIfError, debuggingMode) = (true, false);
    if hasmode ArgNoDebug     then debuggingMode = false;
    if hasmode ArgNoBacktrace then backtrace = false;
    if hasmode ArgNotify      then notify = true;
    interpreterDepth = 1;

    oldPrivateDictionary := User#"private dictionary";
    User#"private dictionary" = new Dictionary;
    OutputDictionary = new GlobalDictionary;
    dictionaryPath = {
	Core.Dictionary,
	OutputDictionary,
	PackageDictionary};
    if not hasmode ArgNoPreload then
    scan(Core#"pre-installed packages", needsPackage);
    needsPackage toString if opts#Package === null then currentPackage else opts#Package;
    dictionaryPath = prepend(oldPrivateDictionary,      dictionaryPath); -- this is necessary mainly due to T from degreesMonoid
    dictionaryPath = prepend(User#"private dictionary", dictionaryPath); -- this is necessary mainly due to indeterminates.m2
    currentPackage = User;

    ret := capture' s;
    collectGarbage();

    User#"private dictionary" = oldPrivateDictionary;
    -- TODO: this should eventually be unnecessary
    scan(keys oldMutableVars, symb -> symb <- oldMutableVars#symb);
    popvar symbol randomSeed;
    ret)
protect symbol capture

-- returns false if the inputs or the package are not known to behave well with capture
-- this is also used in testing.m2, where isTest is set to true.
isCapturable = (inputs, pkg, isTest) -> (
    -- argumentMode is mainly used by ctest to select M2 subprocess arguments,
    -- or whether capture should be avoided; see packages/CMakeLists.txt
    -- alternatively, no-capture-flag can be used with an example or test
    if argumentMode & NoCapture =!= 0 or match("no-capture-flag", inputs) then return false;
    -- strip commented segments first
    inputs = replace("--.*$", "",       inputs);
    inputs = replace("-\\*.*?\\*-", "", inputs);
    -- TODO: remove this when the effects of capture on other packages is reviewed
    (isTest or match({"FirstPackage", "Macaulay2Doc"},            pkg#"pkgname"))
    and not match({"EngineTests", "ThreadedGB", "RunExternalM2"}, pkg#"pkgname")
    -- FIXME: these are workarounds to prevent bugs, in order of priority for being fixed:
    and not match("(gbTrace|stderr|stdio|(P|p)rint|<<)",      inputs) -- stderr and prints are not handled correctly
    and not match("(notify|stopIfError|debuggingMode)",       inputs) -- stopIfError and debuggingMode may be fixable
    and not match("(alarm|exec|exit|quit|restart|run)\\b",    inputs) -- these commands interrupt the interpreter
    and not match("(read|input|load|needs)\\b",               inputs) -- these commands hide undesirable functions
    and not match("([Cc]ommand|fork|schedule|thread|Task)",   inputs) -- remove when threads work more predictably
    and not match("(temporaryFileName)",                      inputs) -- this is sometimes bug prone
    and not match("(addHook|export|newPackage)",              inputs) -- these commands have permanent effects
    and not match("(installMethod|installAssignmentMethod)",  inputs) -- same as above
    and not match("(Global.{6,7}Hook|StartFunction|Echo)",    inputs) -- same as above
    )

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
    then ( verboseLog("info: capturing example results on-demand"); last capture(ex, UserMode => false, Package => pkg) );
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
    if isCapturable(inputs, pkg, false) then (
	desc = concatenate(desc, 62 - #desc);
	stderr << commentize pad("capturing " | desc, 72) << flush; -- the timing info will appear at the end
	(err, output) := capture(inputs, UserMode => false, Package => pkg);
	if not err then return outf << M2outputHash << inputhash << endl << output << close);
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
-- TODO: avoid the issue of extra indented lines being skipped in SimpleDoc
-- a hacky fix is dumping any remaining example results along with the last example
-- a better fix probably requires rethinking the ExampleItem mechanism

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

processExamples = (pkg, fkey, docBody) -> (
    currentExampleKey = fkey;
    currentExampleCounter = 0;
    currentExampleResults = getExampleOutput(pkg, fkey);
    if #currentExampleResults > 0 then processExamplesLoop docBody else docBody)
