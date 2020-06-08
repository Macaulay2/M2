-- -*- coding: utf-8 -*-
-- TODO: add linter
newPackage(
    "SimpleDoc",
    Version => "1.2",
    Date => "May 18, 2020",
    Headline => "a simple documentation function",
    Authors => {
	{ Name => "Dan Grayson", Email => "dan@math.uiuc.edu", HomePage => "https://faculty.math.illinois.edu/~dan/" },
	{ Name => "Mike Stillman", Email => "mike@math.cornell.edu", HomePage => "https://pi.math.cornell.edu/~mike/" },
	{ Name => "Mahrud Sayrafi", Email => "mahrud@umn.edu", HomePage => "https://math.umn.edu/~mahrud/" }
	},
    PackageImports => { "Text" },
    DebuggingMode => false,
    AuxiliaryFiles => true
    )

export {"doc", "multidoc", "packageTemplate", -- functions
    "arXiv", "stacksProject", "wikipedia", -- helper functions
    "docTemplate", "docExample", "testExample", "simpleDocFrob", -- templates and examples
    "Node", "Item", "CannedExample", "Pre", "Code", "Acknowledgement", "Contributors", "References" -- temporary nodes
    }

-- Primary functions
doc = method()
doc String := docstring -> document toDoc(KeyFunctions, docstring)

multidoc = method()
multidoc String := multidocstring -> document \ toDoc(NodeFunctions, multidocstring)

packageTemplate = method()
packageTemplate String := (packagename) -> replace("%%NAME%%", packagename, packagetemplate)

-- Helper functions
toDoc = (functionTable, text) -> (
    linenum := 0;
    textlines := for line in lines text list (
	linenum = linenum + 1;
	if match("^[[:space:]]*--", line) then continue -- skip comment lines
	else makeTextline(line, linenum));
    deepSplice applySplit(functionTable, textlines))

applySplit = (functionTable, textlines) -> apply(splitByIndent(textlines, false), (s, e) -> (
	key := getText textlines#s;
	if not functionTable#?key then error splice(
	    "unrecognized keyword, line ", toString getLinenum textlines#s, " of string: ", format key, "; ",
	    "expected: ", toSequence between(" ", sort keys functionTable));
	functionTable#key(textlines_{s+1..e}, getLinenum textlines#s)))

-- Mapping tables for evaluating docstring keywords
NodeFunctions = new HashTable from {
    "Node" => (textlines, keylinenum) -> (
	parsed := deepSplice applySplit(KeyFunctions, textlines);
	if any(parsed, i -> member(first i, {Inputs, Outputs})) and not any(parsed, i -> first i === Usage)
	then error("multidoc node, line ", toString keylinenum, " of string: Inputs or Outputs specified, but Usage not provided");
	parsed)
     }

KeyFunctions = new HashTable from {
    "Key"             => (textlines, keylinenum) -> Key             => getKeys(textlines, keylinenum),
    "Headline"        => (textlines, keylinenum) -> Headline        => singleString(Headline, textlines, keylinenum),
    "Usage"           => (textlines, keylinenum) -> Usage           => multiString(Usage, textlines, keylinenum),
    "Inputs"          => (textlines, keylinenum) -> Inputs          => items(textlines, keylinenum),
    "Outputs"         => (textlines, keylinenum) -> Outputs         => items(textlines, keylinenum),
    "Consequences"    => (textlines, keylinenum) -> Consequences    => applySplit(ConsequencesFuntions, textlines),
    "Description"     => (textlines, keylinenum) -> toSequence applySplit(DescriptionFunctions, textlines),
    "Acknowledgement" => (textlines, keylinenum) -> -* Acknowledgement => *- {markup(textlines, keylinenum)},
    "Contributors"    => (textlines, keylinenum) -> -* Contributors    => *- {markup(textlines, keylinenum)},
    "References"      => (textlines, keylinenum) -> -* References      => *- {markup(textlines, keylinenum)},
    "ExampleFiles"    => (textlines, keylinenum) -> ExampleFiles    => getText \ textlines,
    "Caveat"          => (textlines, keylinenum) -> Caveat          => {markup(textlines, keylinenum)},
    "SeeAlso"         => (textlines, keylinenum) -> SeeAlso         => apply(select(getText \ textlines, p -> #p > 0), value),
    "Subnodes"        => (textlines, keylinenum) -> Subnodes        => apply(getText \ textlines, p -> if match("^:", p) then substring(1, p) else TO value p),
 }

DescriptionFunctions = new HashTable from {
    "Example"       => (textlines, keylinenum) -> getExample(textlines, keylinenum, false),
    "CannedExample" => (textlines, keylinenum) -> getExample(textlines, keylinenum, true),
    "Text"          => (textlines, keylinenum) -> markup(textlines, keylinenum),
    "Pre"           => (textlines, keylinenum) -> PRE reassemble(min\\getIndent\textlines, textlines),
    "Code"          => (textlines, keylinenum) -> getCode(textlines, keylinenum),
    }

ConsequencesFuntions = new HashTable from {
    "Item" => (textlines, keylinenum) -> markup(textlines, keylinenum)
    }

-- Processing functions

-- We represent a line of text by a triple (text, indent, linenum) where
--   text : String	the content of the line, with indentation removed, or null if the line was empty (?)
--   indent : ZZ	the number of spaces of indentation removed, or infinity if the line was empty
--   linenum : ZZ	the source line number
-- We use these access functions uniformly:
getText = textline -> textline#0
getIndent = textline -> textline#1
getLinenum = textline -> textline#2
-- We use this creation function:
makeTextline = (line, linenum) -> (
    text := replace("(^[[:space:]]+|[[:space:]]+$)", "", line);
    indent := getIndentLevel line;
    (text, indent, linenum))

-- return of number of leading spaces + leading tabs * 8 before text, or infinity for empty line
getIndentLevel = str -> (
    level := 0;
    for c in characters str do (
	if c === " " then level = level + 1
	else if c === "\t" then level = 8 * ((level + 8) // 8)
	else if c === "\r" then level = 0
	else return level);
    infinity)

-- return list of intervals such that the start of all intervals has the same, minimum intentation
-- if empties is true then empty lines split intervals
splitByIndent = (textlines, empties) -> (
    indents := for n in getIndent \ textlines list (if empties and n === infinity then -1 else n);
    m := infinity;
    r := for i to #indents - 1 list if m + 1 <= indents#i then continue else (m = indents#i; i);
    r = append(r, #indents);
    apply(#r - 1, i -> (r#i, r#(i + 1) - 1)))

safevalue = t -> try value t else ( stderr << "in the evaluation of: " << stack lines t << endl; value t )

-- render @...@ blocks
-- TODO: use negative lookaheads to simplify this
render = (textlines, keylinenum) -> (
    if #textlines == 0 then return "";
    text := concatenate between(" ", getText \ textlines);
    segments := separateRegexp(///(^|[^\\])(@)///, 2, text);
    segments = apply(segments, segment -> replace(///\\@///, "@", segment));
    if not odd(#segments) then error("unmatched @ near line ", toString keylinenum);
    parsed := for i to #segments - 1 list (
	if segments#i == "" then continue;
	if even i then TEX segments#i else safevalue concatenate("(", segments#i, ")"));
    if instance(parsed, List) and #parsed == 1 then first parsed else parsed)

markup = (textlines, keylinenum) -> (
    textline := makeTextline("", if #textlines == 0 then "unknown" else getLinenum textlines#0 - 1);
    textlines = prepend(textline, textlines);
    intervals := splitByIndent(textlines, true);
    DIV apply(intervals, (s, e) -> (
	    result := render(textlines_{s+1..e}, getLinenum textlines#s);
	    if instance(result, HypertextContainer) or instance(result, HypertextParagraph) then result else PARA result)))

singleString = (key, textlines, keylinenum) -> (
     if #textlines == 0 then
       error("line ", toString keylinenum, " of string: expected single indented line after ", toString key)
     else if #textlines > 1 then
       error("line ", toString getLinenum textlines#1, " of string: expected single indented line after ", toString key);
     getText textlines#0)

-- originally written by Andrew Hoefel
multiString = (key, textlines, keylinenum) -> (
     if #textlines == 0 then
       error("line ", toString keylinenum, " of string: expected at least one indented line after ", toString key);
     concatenate between(newline, getText \ textlines))

-- used for inputs, outputs, and options
items = (textlines, keylinenum) -> apply(splitByIndent(textlines, false), (s, e) -> (
	line := getText textlines#s;
	ps := separateRegexp("[[:space:]]*(:|=>)[[:space:]]*", line); -- split by ":" or "=>"
	if #ps =!= 2 then error("line ", toString getLinenum textlines#s, " of string: expected line containing a colon or a double arrow");
	result := if s === e then "" else render(textlines_{s+1..e}, getLinenum textlines#s);
	if ps#1 != "" then result = value ps#1 => result;
	if ps#0 != "" then result = (if match("=>", line) then value else identity) ps#0 => result;
	result))

-- reassemble textlines into a docstring
reassemble = (indent, textlines) -> concatenate between(newline,
    for line in textlines list ( if getIndent line =!= infinity then getIndent line - indent : " ", getText line ))

getKeys = (textlines, keylinenum) -> (
    keyList := select(apply(getText \ textlines, value), key -> key =!= null);
    if #keyList == 0 then error("Key (line ", toString keylinenum, " of string): expected at least one key");
    keyList)

getCode = (textlines, keylinenum) -> (
    m := min\\getIndent\textlines;
    snippet := apply(textlines, x -> (getIndent x - m, getText x, newline));
    value concatenate ("(", newline, snippet, ")"))

getExample = (textlines, keylinenum, canned) -> (
    EXAMPLE if canned then { PRE reassemble(getIndent textlines#0, textlines) }
    else apply(splitByIndent(textlines, false), (i, j) -> reassemble(getIndent textlines#0, take(textlines, {i,j}))))

-- helper functions for writing documentation
load("./SimpleDoc/helpers.m2")

-- docstring and package templetes
load("./SimpleDoc/templates.m2")

-- an example that can also be used as a test
load("./SimpleDoc/example.m2")

-* Documentation section *-
beginDocumentation()

-- load the multidocstring
multidoc get (currentFileDirectory | "SimpleDoc/doc.txt")
multidoc get (currentFileDirectory | "SimpleDoc/helpers-doc.txt")

-- load the documentation and tests for the example
value docExample
value testExample

end--

uninstallPackage "SimpleDoc"
restart
installPackage "SimpleDoc"
viewHelp (simpleDocFrob, ZZ, Matrix)

restart
debug Core
debug SimpleDoc
text = substring(8, length docExample - 4 - 8 +1, docExample)
toDoc(KeyFunctions, text)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PACKAGES=SimpleDoc RemakePackages=true RemakeAllDocumentation=true IgnoreExampleErrors=false RerunExamples=true"
-- End:
