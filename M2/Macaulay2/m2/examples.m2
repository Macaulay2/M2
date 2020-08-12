-----------------------------------------------------------------------------
-- process examples
-----------------------------------------------------------------------------

processExamplesStrict = true

exampleOutputFilename = null

exampleCounter := 0
exampleResults := {}

-----------------------------------------------------------------------------
-- separateM2output
-----------------------------------------------------------------------------

M2outputRE      := "(\n+)i+[1-9][0-9]* : "
M2outputREindex := 1

-- TODO: needed in installPackage.m2
separateM2output = method()
separateM2output String := str -> (
    m := regex("^i1 : ", str);
    if m#?0 then str = substring(m#0#0, str);
    -- TODO: do this with regex instead
    while str#?-1 and str#-1 == "\n" do str = substring(0, #str - 1, str);
    separate(M2outputRE, M2outputREindex, str))

-- TODO: where should this be used?
capture = method()
capture String := s -> (
     (err, out) := internalCapture s;
     (err, out, separateM2output out))

-----------------------------------------------------------------------------
-- extract examples
-----------------------------------------------------------------------------

extractExamplesLoop := method(Dispatch => Thing)
extractExamplesLoop Thing       := x -> {}
extractExamplesLoop Sequence    :=
extractExamplesLoop Hypertext   := x -> join apply(toSequence x, extractExamplesLoop)
extractExamplesLoop ExampleItem := toList

extractExamples = docBody -> (
    ex := extractExamplesLoop docBody;
    -- don't convert "ex" on the next line to a sequence,
    -- because the hash code for caching example outputs will change
    if #ex > 0 then currentPackage#"example inputs"#currentNodeName = ex;
    docBody)

-----------------------------------------------------------------------------
-- extract examples
-----------------------------------------------------------------------------

-- may return 'null'
makeExampleOutputFileName = (fkey, pkg) -> (
    if pkg#?"package prefix" and pkg#"package prefix" =!= null then (
	packageLayout := detectCurrentLayout pkg#"package prefix";
	if packageLayout === null then error "internal error: package layout not detected";
	pkg#"package prefix" | replace("PKG", pkg#"pkgname", Layout#packageLayout#"packageexampleoutput") | toFilename fkey | ".out"))

checkForExampleOutputFile := (node,pkg) -> (
    exampleCounter = 0;
    exampleResults = {};
    if pkg#"example results"#?node then (
	exampleResults = pkg#"example results"#node;
	true)
    else if exampleOutputFilename =!= null then (
	if fileExists exampleOutputFilename then (
	    if debugLevel > 1 then stderr << "--reading example results from " << exampleOutputFilename << endl;
	    exampleResults = pkg#"example results"#node = drop(separateM2output get exampleOutputFilename, -1);
	    true)
	else (
	    if debugLevel > 0 then stderr << "--example output file not present: " << exampleOutputFilename << endl;
	    false))
    else false)

-----------------------------------------------------------------------------
-- process examples
-----------------------------------------------------------------------------

currentExampleKey := ""

processExamplesLoop = method(Dispatch => Thing)
processExamplesLoop TO       :=
processExamplesLoop TO2      :=
processExamplesLoop TOH      :=
processExamplesLoop Option   :=
processExamplesLoop String   := identity

processExamplesLoop Sequence    :=
processExamplesLoop Hypertext   := x -> apply(x,processExamplesLoop)
processExamplesLoop ExampleItem := x -> (
    ret := (
	if exampleResults#?exampleCounter then PRE exampleResults#exampleCounter
	else (
	    if #exampleResults === exampleCounter then (
		if processExamplesStrict
		then error("example results terminate prematurely: ", toString currentExampleKey)
		else stderr << "--warning: example results terminate prematurely: " << currentExampleKey << endl);
	    PRE concatenate("i", toString (exampleCounter + 1), " : -- example results terminated prematurely")));
    exampleCounter = exampleCounter + 1;
    ret)

processExamples = (pkg, fkey, docBody) -> (
    pkg = getpkg pkg;
    exampleOutputFilename = makeExampleOutputFileName(fkey, pkg);
    if checkForExampleOutputFile(fkey, pkg) then (
	currentExampleKey = fkey;
	docBody = processExamplesLoop docBody;
	currentExampleKey = "");
    docBody)
