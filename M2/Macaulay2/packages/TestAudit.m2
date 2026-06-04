newPackage(
    "TestAudit",
    Version => "0.1",
    Date => "May 19, 2026",
    Headline => "test audit functionality",
    Authors => {
        { Name => "Taylor Brysiewicz", Email => "tbrysiew@uwo.ca", HomePage => "https://sites.google.com/view/taylorbrysiewicz/home" },
        { Name => "Ashley Dean", Email => "amd069@uark.edu" },
        { Name => "Connor Haynes" },
        { Name => "David K. Johnson", Email => "djohn225@uwo.ca", HomePage => "https://sites.google.com/view/davidkjohnson" },
        { Name => "Guanyu Li", Email => "gl479@cornell.edu", HomePage => "https://sites.google.com/view/guanyu-li-math/home" },
        { Name => "Keller VandeBogert", Email => "keller.v@uky.edu", HomePage => "https://sites.google.com/view/kellervandebogert/home" }
        },
    Keywords => {"Miscellaneous"},
    AuxiliaryFiles => false,
    DebuggingMode => false
    )

export {
    "testAudit",
    "testScore",
    "CommentReport",
    "SpeedReport",
    "ScoreReport",
    "Experimental"
    }

testAudit = method(Options => {CommentReport => false, SpeedReport => false, ScoreReport => false, Experimental => false})
testScore = method()

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- The coverage checks below are textual heuristics, not dispatch tracing.
testCodeString = method()
testCodeStringFromInputs := inputs -> (
    demark(newline, apply(toList(0..#inputs-1), i -> inputs#i#"code"))
)

testCodeString Package := pkg -> testCodeStringFromInputs tests pkg
testCodeString String := pkgname -> testCodeString needsPackage(pkgname, LoadDocumentation => true)


packageSourceString = method()
packageSourceString Package := pkg -> try get pkg#"source file" else ""
packageSourceString String := pkgname -> packageSourceString needsPackage(pkgname, LoadDocumentation => true)


exportedSymbols = method()
exportedSymbols Package := pkg -> sort toList pkg#"exported symbols"
exportedSymbols String := pkgname -> exportedSymbols needsPackage(pkgname, LoadDocumentation => true)


-- These helpers classify exported symbols by the value attached to the symbol.
-- Anything that is not a Function or a Type is reported as "other".
isExportedFunction := s -> instance(value s, Function)

isExportedType := s -> instance(value s, Type)

optionNamesForSymbol := s -> (
    try sort apply(toList keys options value s, toString) else {}
)

-- Like optionNamesForSymbol but takes a Function value directly.
optionNamesForFunction := f -> (
    try sort apply(toList keys options f, toString) else {}
)

-- Function values to which this package has added methods. Includes both
-- exported and pre-existing (e.g. built-in) function symbols: a package can
-- add `(pullback, RingMap, RingMap)` without exporting `pullback`, and the
-- audit should still cover `pullback`.
methodHeadFunctions := pkg -> unique apply(toList methods pkg, m -> m#0)

wordMatch := (name, code) -> match("\\b" | toString name | "\\b", code)

countLinesMatching := (pat, code) -> #select(lines code, line -> match(pat, line))

shortList := (L, n) -> (
    if #L === 0 then ""
    else if #L <= n then demark(", ", L)
    else demark(", ", take(L, n)) | ", ..."
)

-- Avoid constructing the invalid range 0..-1 when a list is empty.
lineNumbers := n -> if n === 0 then {} else toList(0..n-1)

snippet := (s, n) -> (
    t := replace("^ *| *$", "", s);
    if length t <= n then t else substring(0, n, t) | "..."
)

auditListLines := (label, L) -> (
    {"- " | label | " (" | toString(#L) | "):"} |
    (if #L === 0 then {"    none"} else apply(L, item -> "    - " | item)) |
    {""}
)

printAuditList := (label, L) -> (
    scan(auditListLines(label, L), print);
)

-- Ignore development scratch code after a literal "end" line in package files.
sourceLinesBeforeEnd := filename -> (
    srcLines := lines get filename;
    endLines := select(lineNumbers(#srcLines), i -> match("^ *end *$", srcLines#i));
    if #endLines === 0 then srcLines else take(srcLines, first endLines)
)

sourceLineMatches := (pkg, pat) -> (
    filename := try pkg#"source file" else "";
    if filename === "" or not fileExists filename then {}
    else (
        srcLines := sourceLinesBeforeEnd filename;
        apply(select(lineNumbers(#srcLines), i -> match(pat, srcLines#i)), i ->
            snippet(srcLines#i, 72) | " (source: " | filename | ":" | toString(i + 1) | ")")
    )
)

blockCommentLineMatches := (pkg, pat) -> (
    filename := try pkg#"source file" else "";
    if filename === "" or not fileExists filename then {}
    else (
        srcLines := sourceLinesBeforeEnd filename;
        inBlock := false;
        matches := {};
        scan(lineNumbers(#srcLines), i -> (
            line := srcLines#i;
            startsBlock := match("-\\*", line);
            endsBlock := match("\\*-", line);
            wasInBlock := inBlock;

            if startsBlock then inBlock = true;
            if (wasInBlock or startsBlock) and match(pat, line) then (
                matches = append(matches, snippet(line, 72) | " (source: " | filename | ":" | toString(i + 1) | ")")
            );
            if endsBlock then inBlock = false;
        ));
        matches
    )
)

silencedTestMatches := pkg -> unique(sourceLineMatches(pkg, "^ *--+ *TEST *///") | blockCommentLineMatches(pkg, "TEST *///"))

actualCodeLine := line -> not match("^\\s*$", line) and not match("^\\s*--", line)

actualCodeBetweenTests := (filename, firstLoc, secondLoc) -> (
    if not fileExists filename then false
    else (
        srcLines := lines get filename;
        firstLine := firstLoc#3 + 1;
        lastLine := secondLoc#1 - 1;
        codeLines := if firstLine <= lastLine then select(toList(firstLine..lastLine), n ->
            n >= 1 and n <= #srcLines and actualCodeLine srcLines#(n - 1)) else {};
        #codeLines > 0
    )
)

-- Sort FilePositions indirectly; FilePosition itself has no useful ordering.
interspersedTests := locs -> (
    files := unique apply(locs, loc -> loc#0);
    any(files, filename -> (
        fileLocs := last \ sort apply(select(toList(0..#locs-1), i -> (locs#i)#0 === filename),
            i -> ((locs#i)#1, i, locs#i));
        #fileLocs > 1 and any(toList(0..#fileLocs-2), i ->
            actualCodeBetweenTests(filename, fileLocs#i, fileLocs#(i + 1)))
    ))
)

-- "auxiliary" means tests come from more than one file; "together" means tests
-- exist and are not interspersed with code.
styleOfTests := inputs -> (
    if #inputs === 0 then {"no tests"}
    else (
        locs := apply(toList(0..#inputs-1), i -> locate inputs#i);
        styles := {};
        interspersed := interspersedTests locs;
        if #(unique apply(locs, loc -> loc#0)) > 1 then styles = append(styles, "auxiliary");
        styles = append(styles, if interspersed then "interspersed" else "together");
        styles
    )
)

testSourceLines := inputs -> (
    locs := apply(lineNumbers(#inputs), i -> locate inputs#i);
    files := unique apply(locs, loc -> loc#0);
    {"test sources:"} |
    (if #files === 0
     then {"    none"}
     else apply(files, file -> (
         firstLine := first sort apply(select(locs, loc -> loc#0 === file), loc -> loc#1);
         "    - " | file | ":" | toString firstLine
     )))
)

--------------------------------------------------------------------------------
-- Comment report
--------------------------------------------------------------------------------

-- Exclude the synthetic "-- test source:" line Macaulay2 adds to TestInput code.
commentLinesIn := code -> select(lines code, line -> match("^ *--", line) and not match("^ *-- test source:", line))

-- Look immediately above a TEST block for a contiguous run of -- comments.
headerCommentLinesBefore := testInput -> (
    loc := locate testInput;
    filename := loc#0;
    startLine := loc#1;
    if not fileExists filename then {}
    else (
        srcLines := lines get filename;
        i := startLine - 2;
        comments := {};
        while i >= 0 and match("^ *--", srcLines#i) do (
            comments = prepend(srcLines#i, comments);
            i = i - 1;
        );
        comments
    )
)

testCommentLineIndices := testInput -> (
    loc := locate testInput;
    filename := loc#0;
    startLine := loc#1;
    endLine := loc#3;
    if not fileExists filename then {filename, {}}
    else (
        srcLines := lines get filename;
        headerIndices := {};
        i := startLine - 2;
        while i >= 0 and match("^ *--", srcLines#i) do (
            headerIndices = prepend(i, headerIndices);
            i = i - 1;
        );
        testIndices := if startLine <= endLine then select(toList(startLine-1..endLine-1), j ->
            j >= 0 and j < #srcLines and match("^ *--", srcLines#j)) else {};
        {filename, unique(headerIndices | testIndices)}
    )
)

taskMarkerMatches := inputs -> (
    pat := "^ *--+ *([Ff][Ii][Xx][Mm][Ee]|[Tt][Oo][Dd][Oo])\\b";
    unique flatten apply(lineNumbers(#inputs), i -> (
        indexedLines := testCommentLineIndices inputs#i;
        filename := indexedLines#0;
        if filename === "" or not fileExists filename then {}
        else (
            srcLines := lines get filename;
            apply(select(indexedLines#1, j -> match(pat, srcLines#j)), j ->
                snippet(srcLines#j, 72) | " (source: " | filename | ":" | toString(j + 1) | ")")
        )
    ))
)

commentSectionLines := (label, comments) -> (
    {label | ":"} | apply(comments, c -> "    " | c)
)

-- Tests with no comments are skipped; a package with none gets "(no comments)".
commentReportLines := inputs -> (
    blocks := flatten apply(toList(0..#inputs-1), i -> (
        code := inputs#i#"code";
        headerComments := headerCommentLinesBefore inputs#i;
        inTestComments := commentLinesIn code;

        if #headerComments > 0 or #inTestComments > 0
        then {"", "TEST " | toString i | " -- source: " | toString locate inputs#i, "------------------------------------------------------------------------"} |
            commentSectionLines("Header comments", headerComments) |
            commentSectionLines("In-test comments", inTestComments)
        else {}
    ));
    {"", "Comments:"} | if #blocks === 0 then {"(no comments)"} else blocks
)

printCommentReport := inputs -> (
    scan(commentReportLines inputs, print);
)

--------------------------------------------------------------------------------
-- Speed report
--------------------------------------------------------------------------------

timeString := seconds -> toString seconds | "s"

-- Timing failures are reported per test rather than aborting the audit.
speedReportLines := (pkg, inputs) -> (
    timings := apply(toList(0..#inputs-1), i -> (
        result := try (
            t := elapsedTiming check(i, pkg);
            {i, t#0, toString locate inputs#i, true}
        ) else {i, null, toString locate inputs#i, false};
        result
    ));
    successful := select(timings, row -> row#3);
    totalTime := sum apply(successful, row -> row#1);

    {"", "Speed Report:", ""} |
    apply(timings, row -> (
        if row#3
        then "    - TEST " | toString(row#0) | ": " | timeString(row#1) | " (" | row#2 | ")"
        else "    - TEST " | toString(row#0) | ": failed while timing (" | row#2 | ")"
    )) |
    {"", "    total timed tests: " | toString(#successful) | "/" | toString(#timings),
     "    total time: " | timeString totalTime}
)

--------------------------------------------------------------------------------
-- ScoreReport report
--------------------------------------------------------------------------------

-- Empty categories count as satisfied rather than penalizing small packages.
scoreFraction := (covered, total) -> if total === 0 then 1 else covered / total

scoreValues := (syms, funcs, types, others, extraMethodFuncNames, untestedExports, untestedFunctions, optionPairs, untestedOptions, silencedTests, fixmeTodos) -> (
    testedExports := #syms - #untestedExports;
    totalFunctions := #funcs + #extraMethodFuncNames;
    testedFunctions := totalFunctions - #untestedFunctions;
    extraMethodFuncsTested := #extraMethodFuncNames - #(select(extraMethodFuncNames, n -> member(n, untestedFunctions)));
    testedTypes := #types - #(select(apply(types, toString), name -> member(name, untestedExports)));
    testedOthers := #others - #(select(apply(others, toString), name -> member(name, untestedExports)));
    testedOptions := #optionPairs - #untestedOptions;
    testedTotal := #syms + #extraMethodFuncNames + #optionPairs;
    testedCovered := testedExports + extraMethodFuncsTested + testedOptions;
    testedScore := toRR(80 * scoreFraction(testedCovered, testedTotal));
    silencedScore := max(0, 10 - #silencedTests);
    todoScore := max(0, 10 - #fixmeTodos);
    totalScore := toRR(testedScore + silencedScore + todoScore);
    scoreLines := {
        "ScoreReport: " | toString totalScore | " out of 100",
        "    tested: " | toString(testedScore) | " out of 80",
        "    exports covered: " | toString(toRR(100 * scoreFraction(testedExports, #syms))) | "% (" | toString(testedExports) | "/" | toString(#syms) | ")",
        "    functions covered: " | toString(toRR(100 * scoreFraction(testedFunctions, totalFunctions))) | "% (" | toString(testedFunctions) | "/" | toString(totalFunctions) | ")",
        "    types covered: " | toString(toRR(100 * scoreFraction(testedTypes, #types))) | "% (" | toString(testedTypes) | "/" | toString(#types) | ")",
        "    other exports covered: " | toString(toRR(100 * scoreFraction(testedOthers, #others))) | "% (" | toString(testedOthers) | "/" | toString(#others) | ")",
        "    options covered: " | toString(toRR(100 * scoreFraction(testedOptions, #optionPairs))) | "% (" | toString(testedOptions) | "/" | toString(#optionPairs) | ")",
        "    silenced tests: " | toString(silencedScore) | " out of 10 (" | toString(#silencedTests) | " found)",
        "    FIXME/TODO markers: " | toString(todoScore) | " out of 10 (" | toString(#fixmeTodos) | " found)"};
    {totalScore, scoreLines}
)

-- Compute a deliberately simple heuristic score.  It is meant to guide human
-- review, not certify test quality.
scoreReportLines := (inputs, syms, funcs, types, others, extraMethodFuncNames, untestedExports, untestedFunctions, optionPairs, untestedOptions, silencedTests, fixmeTodos) -> (
    {""} | (scoreValues(syms, funcs, types, others, extraMethodFuncNames, untestedExports, untestedFunctions, optionPairs, untestedOptions, silencedTests, fixmeTodos))#1
)

--------------------------------------------------------------------------------
-- (EXPERIMENTAL) testAudit hashtable rewrite (EXPERIMENTAL)
--------------------------------------------------------------------------------

-- i should come back and think about the pros/cons of having the (some of) keys/values below be their actual
-- Things as opposed to the strings
checkTestedExports := (exportsList, testsTable) -> (
    -- i wish i wasn't constructing this hashtable just to construct the other, but i'm not sure how to check
    -- testedness otherwise at the moment
    testedExportsTable := new HashTable from apply(exportsList, k -> (k => select(testsTable, t -> wordMatch(k, t#"code"))));
    exportsTable := new HashTable from apply(
	exportsList, e -> (
	    e => new HashTable from {
		"exportClass" => class value e,
		"isTested" => (#(testedExportsTable#e) =!= 0),
		"testsContaining" => testedExportsTable#e
		}
	    )
	);
    return exportsTable;
    )

testAuditHashTable := pkg -> (
    testsTable := tests pkg; -- tests of the form TEST /// ... ///
    -- brokenTable = brokenTests pkg; -- tests of the form BROKENTEST /// ... /// (not implemented)
    exportsList := pkg#"exported symbols"; -- do we also want the "exported mutable symbols?"

    -- returns a hashtable that reports the type of an export, whether it is tested, and what tests contain it
    exportsTable := checkTestedExports(exportsList,testsTable);

    -- returns a hashtable that reports how long a test took to run
    -- if opts.SpeedReport then speedReportTable := speedReport(testsTable) else null;

    -- returns a hashtable that reports on comments "attached" to tests
    -- if opts.CommentReport then commentReportTable := commentReport(testsTable) else null;

    -- returns a hashtable that reports on the score of the test collection
    -- if opts.ScoreReport then score := scoreReport(testsTable) else null;

    -- testAuditTable = new HashTable from {
    --	    "testsTable" => testsTable,
    --	    "exportsTable" => exportsTable,
    --	    "speedReport" => speedReportTable,
    --	    "commentReport" => commentReportTable,
    --	    "scoreReport" => scoreReportTable
    --	};

    -- if opts.returnHashTable then return testAuditTable;

    -- printing content (should we package all of this information in the testAuditTable? most of it is easy to compute)
    exportedFuncs := select(exportsTable, e -> member(toString (e#"exportClass"), {"MethodFunction", "MethodFunctionWithOptions", "FunctionClosure", "Function"}));
    numExportedFuncs := #exportedFuncs; -- "warning: local declaration of numExportedFuncs shields variable with same name" ????
    numExportedTypes := #select(exportsTable, e -> toString (e#"exportClass") === "Type");
    numExportedOther := #exportsTable - numExportedFuncs - numExportedTypes;

    untestedExports := select(exportsTable, e -> e#"isTested" == false);
    untestedFuncs := select(exportedFuncs, e -> e#"isTested" == false);

    silencedTests := silencedTestMatches pkg;
    fixmeTodos := taskMarkerMatches testsTable;

    reportLines := {
	"exported: " | toString(numExportedFuncs) | " functions, " | toString(numExportedTypes) | " types, " | toString(numExportedOther) | "other symbols",
	"n_tests: " | toString(#testsTable),
	"style: " | demark(", ", styleOfTests testsTable),
	""} |
        testSourceLines testsTable |
	{"",
        "Report:",
	""
	} |
	auditListLines("untested functions", apply(keys untestedFuncs, f -> toString f)) |
--        auditListLines("untested options", untestedOptionLabels) |
        auditListLines("silenced tests", silencedTests) |
        auditListLines("FIXME/TODO markers", fixmeTodos);

    report := demark(newline, reportLines);
    report
    )
--------------------------------------------------------------------------------
-- End of experimental hashtables section
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Main audit
--------------------------------------------------------------------------------

testAudit Package := opts -> pkg -> (
    if opts.Experimental then return testAuditHashTable(pkg);
    -- Capture method-key info BEFORE calling `tests pkg`: `tests` reloads the
    -- package with documentation, which silently replaces the global Package
    -- and detaches methods registered on the original object.
    methodHeads := methodHeadFunctions pkg;
    inputs := tests pkg; -- returns the tests as a hashtable
    code := testCodeStringFromInputs inputs; -- concats all test code with newlines between
    sourceCode := packageSourceString pkg; -- returns string of all source code
    syms := exportedSymbols pkg; -- returns exported symbols using pkg#"exported symbols"

    funcs := select(syms, isExportedFunction); -- returns symbols with class "MethodFunc", "MethodFuncWithOptions", "FuncClosure", or "Func" (I've abbreviated the types)
    types := select(syms, isExportedType); -- returns symbols that satisfy 'class s === "Type"'
    others := select(syms, s -> not isExportedFunction s and not isExportedType s); -- is there a smarter way to do this?

    -- Methods added by this package to functions whose symbols this package
    -- does not export (e.g., a package can add (pullback, RingMap, RingMap)
    -- to the built-in `pullback` without exporting `pullback`). Restrict to
    -- identifier-style names so operator-valued method heads (++, **, _, ...)
    -- don't trip up wordMatch's `\b` regex.
    exportedFuncNames := apply(funcs, toString);
    isIdentifierName := name -> match("^[A-Za-z][A-Za-z0-9]*$", name);
    extraMethodFuncs := select(methodHeads, f -> (
        n := toString f;
        isIdentifierName n and not member(n, exportedFuncNames)
    ));
    extraMethodFuncNames := apply(extraMethodFuncs, toString);

    untestedExports := select(apply(syms, toString), name -> not wordMatch(name, code));
    untestedFunctions := select(exportedFuncNames | extraMethodFuncNames, name -> not wordMatch(name, code));

    optionPairs := flatten apply(funcs, f -> apply(optionNamesForSymbol f, opt -> {toString f, opt})) |
        flatten apply(extraMethodFuncs, f -> apply(optionNamesForFunction f, opt -> {toString f, opt}));
    untestedOptions := select(optionPairs, pair -> not wordMatch(pair#1, code));
    untestedOptionLabels := apply(untestedOptions, pair -> pair#0 | ": " | pair#1);

    silencedTests := silencedTestMatches pkg;
    fixmeTodos := taskMarkerMatches inputs;

    exportHeader := "exported: " | toString(#funcs) | " functions, " | toString(#types) | " types, " | toString(#others) | " other symbols";
    if #extraMethodFuncNames > 0 then
        exportHeader = exportHeader | "; " | toString(#extraMethodFuncNames) | " methods on external functions";

    reportLines := {
        exportHeader,
        "n_tests: " | toString(#inputs),
        "style: " | demark(", ", styleOfTests inputs),
        ""} |
        testSourceLines inputs |
        {"",
        "Report:",
        ""
        } |
        auditListLines("untested functions", untestedFunctions) |
        auditListLines("untested options", untestedOptionLabels) |
        auditListLines("silenced tests", silencedTests) |
        auditListLines("FIXME/TODO markers", fixmeTodos) |
        (if opts.ScoreReport then scoreReportLines(inputs, syms, funcs, types, others, extraMethodFuncNames, untestedExports, untestedFunctions, optionPairs, untestedOptions, silencedTests, fixmeTodos) else {}) |
        (if opts.SpeedReport then speedReportLines(pkg, inputs) else {}) |
        (if opts.CommentReport then commentReportLines inputs else {});

    report := demark(newline, reportLines);
    report
)

-- Load documentation eagerly: the lazy reload that `tests` would otherwise
-- trigger silently re-registers methods against a new Package object and
-- detaches them from this one (so methods pkg drops method-key info after
-- the first call to tests pkg).
testAudit String := opts -> pkgname -> testAudit(needsPackage(pkgname, LoadDocumentation => true), opts)

testScore Package := pkg -> (
    -- Capture method-keys before tests pkg reloads the package; see testAudit.
    methodHeads := methodHeadFunctions pkg;
    inputs := tests pkg;
    code := testCodeStringFromInputs inputs;
    syms := exportedSymbols pkg;
    funcs := select(syms, isExportedFunction);
    types := select(syms, isExportedType);
    others := select(syms, s -> not isExportedFunction s and not isExportedType s);

    exportedFuncNames := apply(funcs, toString);
    isIdentifierName := name -> match("^[A-Za-z][A-Za-z0-9]*$", name);
    extraMethodFuncs := select(methodHeads, f -> (
        n := toString f;
        isIdentifierName n and not member(n, exportedFuncNames)
    ));
    extraMethodFuncNames := apply(extraMethodFuncs, toString);

    untestedExports := select(apply(syms, toString), name -> not wordMatch(name, code));
    untestedFunctions := select(exportedFuncNames | extraMethodFuncNames, name -> not wordMatch(name, code));
    optionPairs := flatten apply(funcs, f -> apply(optionNamesForSymbol f, opt -> {toString f, opt})) |
        flatten apply(extraMethodFuncs, f -> apply(optionNamesForFunction f, opt -> {toString f, opt}));
    untestedOptions := select(optionPairs, pair -> not wordMatch(pair#1, code));

    silencedTests := silencedTestMatches pkg;
    fixmeTodos := taskMarkerMatches inputs;

    (scoreValues(syms, funcs, types, others, extraMethodFuncNames, untestedExports, untestedFunctions, optionPairs, untestedOptions, silencedTests, fixmeTodos))#0
)

-- See note on testAudit String for why LoadDocumentation => true is explicit.
testScore String := pkgname -> testScore needsPackage(pkgname, LoadDocumentation => true)



-* Documentation section *-
beginDocumentation()
doc ///
Key
  TestAudit
Headline
  test audit functionality
Description
  Text
    The @TT "TestAudit"@ package provides a small report about the tests of a package.
    The report lists the number and location of tests, classifies how tests are organized, and gives simple textual checks for exported functions, options, silenced tests, and FIXME or TODO comments.
///
doc ///
Key
  testAudit
  (testAudit, Package)
  (testAudit, String)
  CommentReport
  [testAudit, CommentReport]
  SpeedReport
  [testAudit, SpeedReport]
  ScoreReport
  [testAudit, ScoreReport]
Headline
  produce a test audit report for a package
Usage
  testAudit pkg
  testAudit(pkg, CommentReport => true, SpeedReport => true, ScoreReport => true)
Inputs
  pkg:{Package,String}
  CommentReport => Boolean
  SpeedReport => Boolean
  ScoreReport => Boolean
Outputs
  :String
    the audit report
Description
  Text
    Returns a string report about the tests of @TT "pkg"@.
    The default report summarizes test sources, test organization, untested exported functions and options, silenced tests, and FIXME or TODO markers around test blocks.
  Text
    Optional Boolean arguments add sections: @TT "CommentReport"@ includes comments attached to tests, @TT "SpeedReport"@ times tests with @TO check@, and @TT "ScoreReport"@ includes the heuristic score returned by @TO testScore@.
  Example
    testAudit "TestAudit"
SeeAlso
  testAudit
///

doc ///
Key
  testScore
  (testScore, Package)
  (testScore, String)
Headline
  compute the heuristic test score for a package
Usage
  testScore pkg
Inputs
  pkg:{Package,String}
Outputs
  :RR
    the heuristic test score
Description
  Text
    This function returns only the numerical score that appears in the @TT "ScoreReport"@ section of @TO testAudit@.
    The score is out of 100.
  Example
    testScore "TestAudit"
SeeAlso
  testAudit
///

--testAudit test
TEST /// 
  testAudit("TestAudit")
///

--CommentReport option test
TEST /// 
  testAudit("TestAudit", CommentReport=>true)
///

--SpeedReport option test (cannot call on TestAudit because)
--infinite recursion...
TEST /// 
  testAudit("ConwayPolynomials", SpeedReport=>true)
///

--ScoreReport option test
TEST ///
  report = testAudit("TestAudit", ScoreReport=>true);
///

--testScore test
TEST ///
  score = testScore "TestAudit";
  assert(score >= 0 and score <= 100)
///

end

restart

-- thoughts about rewrite with hashtables

-- each test already has an object associated with it that captures:
--     - location
--     - code
--     - index
--     - package

-- what does it mean for a function to be tested? it means that, within
-- a test block, the function is called at least once.
--
-- a test may be more, however. maybe the most common type of test is a test
-- that takes the output of a file and checks it using some assert call.
--
-- it would be nice if we could distinguish between these, but this is an issue for later.

-- a run of testAudit(pkg) should first call this `tests pkg` function to grab the existing
-- test hash table, and then do our postprocessing. we want the return type to also be a
-- hashtable for parsing reasons.

-- what is happening when we run testAudit("blah", SpeedReport=>true) and we get all
-- those package reloads and warnings?
