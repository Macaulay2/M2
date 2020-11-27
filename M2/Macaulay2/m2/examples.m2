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
capture = method()
capture Net    := s -> capture toString s
capture List   := s -> capture demark_newline s
--capture String := s -> capture' s -- output is (Boolean, String) => (Err?, Output)
-- TODO: do this in interp.dd instead
capture String := s -> (
    pushvar(symbol interpreterDepth, 1);
    ret := capture' s;
    popvar symbol interpreterDepth;
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
    then ( verboseLog("info: capturing example results on-demand"); last capture ex );
    pkg#"example results"#fkey = if output === null then {} else separateM2output output)

-- used in installPackage.m2
-- TODO: store in a database instead
storeExampleOutput = (pkg, fkey, outf, verboseLog) -> (
    if fileExists outf then (
	outstr := reproduciblePaths get outf;
	outf << outstr << close;
	pkg#"example results"#fkey = separateM2output outstr)
    else verboseLog("warning: missing file ", outf));

-- used in installPackage.m2
captureExampleOutput = (pkg, fkey, inputs, cacheFunc, inf, outf, errf, inputhash, changeFunc, usermode, verboseLog) -> (
    desc := "example results for " | fkey;
--    printerr("capturing ", desc);
--    (err, output) := evaluateWithPackage(pkg, inputs, capture);
--    if not err then ( outf << "-- -*- M2-comint -*- hash: " << inputhash << endl << output << close ) else
    (
	data := if pkg#"example data files"#?fkey then pkg#"example data files"#fkey else {};
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
    currentExampleResults = getExampleOutput(pkg, fkey);
    if #currentExampleResults > 0 then processExamplesLoop docBody else docBody)
