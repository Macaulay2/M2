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

M2outputRE      := "(\n+)i+[1-9][0-9]* : "
M2outputREindex := 1

separateM2output := str -> (
    m := regex("^i1 : ", str);
    if m#?0 then str = substring(m#0#0, str);
    -- TODO: do this with regex instead
    while str#?-1 and str#-1 == "\n" do str = substring(0, #str - 1, str);
    separate(M2outputRE, M2outputREindex, str))

-----------------------------------------------------------------------------
-- capture
-----------------------------------------------------------------------------

-- TODO: the output format is provisional
-- TODO: does't capture stderr
capture' := capture
capture = method()
capture Net    := s -> capture' toString s
capture List   := s -> capture' demark_newline s
capture String := s -> capture' s -- output is (Boolean, String) => (Err?, Output)

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
examples Hypertext := dom -> stack extractExamplesLoop dom
examples Thing     := key -> (
    rawdoc := fetchAnyRawDocumentation makeDocumentTag key;
    if rawdoc =!= null and rawdoc.?Description then (stack extractExamplesLoop rawdoc.Description)^-1)

-----------------------------------------------------------------------------
-- storeExampleOutput
-----------------------------------------------------------------------------

getExampleOutputFilename := (pkg, fkey) -> (
    if pkg#?"package prefix" and pkg#"package prefix" =!= null then (
	packageLayout := detectCurrentLayout pkg#"package prefix";
	if packageLayout === null then error "internal error: package layout not detected";
	pkg#"package prefix" | replace("PKG", pkg#"pkgname", Layout#packageLayout#"packageexampleoutput") | toFilename fkey | ".out")
    else error "internal error: package prefix is undefined")

readCachedExampleResults := (pkg, fkey) -> (
    verboseLog := if debugLevel > 1 then printerr else identity;
    fn := getExampleOutputFilename(pkg, fkey);
    pkg#"example results"#fkey = if fileExists fn
    then ( verboseLog("info: reading cached example results from ", fn); drop(separateM2output get fn, -1) )
    else ( verboseLog("info: example output file not present: ", fn); {} ))

storeExampleOutput = (pkg, fkey, outf, verboseLog) -> (
    if fileExists outf then (
	outstr := reproduciblePaths get outf;
	outf << outstr << close;
	pkg#"example results"#fkey = drop(separateM2output outstr, -1))
    else verboseLog("warning: missing file ", outf));

captureExampleOutput = (pkg, fkey, inputs, cacheFunc, inf, outf, errf, inputhash, changeFunc, usermode, verboseLog) -> (
--    verboseLog("info: capturing example output in the same process");
--    (err, output) := evaluateWithPackage(pkg, inputs, capture);
--    outf << "-- -*- M2-comint -*- hash: " << inputhash << endl << output << close)
    (
	verboseLog("info: computing example results in separate process");
	data := if pkg#"example data files"#?fkey then pkg#"example data files"#fkey else {};
	desc := "example results for " | fkey;
	inf << inputs << endl << close;
	if runFile(inf, inputhash, outf, errf, desc, pkg, changeFunc fkey, usermode, data)
	then ( removeFile inf; cacheFunc fkey )))

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
    currentExampleResults = if pkg#"example results"#?fkey then pkg#"example results"#fkey else readCachedExampleResults(pkg, fkey);
    processExamplesLoop docBody)
