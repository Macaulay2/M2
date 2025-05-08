newPackage(
    "JSON",
    Headline => "JSON encoding and decoding",
    Version => "0.4",
    Date => "February 24, 2025",
    Authors => {{
	    Name => "Doug Torrance",
	    Email => "dtorrance@piedmont.edu",
	    HomePage => "https://webwork.piedmont.edu/~dtorrance"}},
    Keywords => {"System"},
    PackageImports => {"Parsing"},
    PackageExports => {"Text"},
    AuxiliaryFiles => true)

---------------
-- ChangeLog --
---------------

-*
0.4 (2025-02-24, M2 1.25.05)
* add "json" synonym for "toJSON"
* remove JSONEncoder class
* add several new toJSON methods (Thing, MutableHashTable, Hypertext)
* set toJSON to Dispatch => Thing so it will work for short sequences
* properly deal w/ escaped characters in fromJSON

0.3 (2024-09-14, M2 1.24.11)
* remove redundant Constant methods now that we can use inheritance
* remove use of now-unsupported FileName option to TEST
* update test after "format" bug fix

0.2 (2024-01-24, M2 1.23)
* use single-string version of exportFrom
* use null coalescing operator in toJSON

0.1 (2022-08-31, M2 1.21)
* initial release

*-

export {
    -- methods
    "toJSON",
    "fromJSON",
    "json" => "toJSON",

    -- symbols
    "Indent",
    "IndentLevel",
    "NameSeparator",
    "ValueSeparator",
    }

exportFrom_Parsing "nil"

----------------------------------------------------------------
-- parser based on https://datatracker.ietf.org/doc/html/rfc8259
----------------------------------------------------------------

-- whitespace
wsP   = *orP(" ", "\t", "\n", "\r")
strip = p -> first % (p @ wsP)

-- structural characters
beginArrayP     = strip "["
beginObjectP    = strip "{"
endArrayP       = strip "]"
endObjectP      = strip "}"
nameSeparatorP  = strip ":"
valueSeparatorP = strip ","

-- literals
falseP = (x -> false) % constParser "false"
nullP  = (x -> nil)   % constParser "null" -- using null would break parsing
trueP  = (x -> true)  % constParser "true"

-- numbers
digit19P = orP("1", "2", "3", "4", "5", "6", "7", "8", "9")
digitP   = "0" | digit19P
expP     = andP(orP("e", "E"), optP(orP("-", "+")), +digitP)
fracP    = andP(".", +digitP)
intP     = orP("0", digit19P @ *digitP)
numberP  = (x -> value concatenate delete(nil, deepSplice x)
    ) % andP(optP("-"), intP, optP(fracP), optP(expP))

-- strings
hexDigitP = digitP | orP("a", "b", "c", "d", "e", "f",
    "A", "B", "C", "D", "E", "F");
unescapedP = Parser(c -> if c === null then null else (
	x := first utf8 c;
	if x < 0x20 or x == 0x22 or x == 0x5c or x > 0x10ffff
	then null
	else terminalParser c))
deformat = x -> (
    if last x === "/" then "/"
    else value concatenate("\"", x, "\""))
escapedP = deformat % ("\\" @
    orP("\"", "\\", "/", "b", "f", "n", "r", "t",
	andP("u", hexDigitP, hexDigitP, hexDigitP, hexDigitP)))
charP = unescapedP | escapedP
stringP = ((l, x, r) -> concatenate x) % andP("\"", *charP, "\"")

-- objects
memberP = ((k, gets, v) -> k => v) % andP(
    stringP, nameSeparatorP, futureParser valueP)
objectP = ((l, x, r) ->  hashTable (
	if x === nil then {}
	else toList deepSplice x)) % andP(
    beginObjectP,
    optP(memberP @ * (last % valueSeparatorP @ memberP)),
    endObjectP)

-- arrays
arrayP = ((l, x, r) -> (
	if x === nil then {}
	else toList deepSplice x)) % andP(
    beginArrayP,
    optP(futureParser valueP @
	*(last % valueSeparatorP @ futureParser valueP)),
    endArrayP)

-- values
valueP = strip orP(falseP, nullP, trueP, objectP, arrayP, numberP, stringP)
jsonTextP = (last @@ last) % (*wsP @ valueP)

utf8Analyzer = Analyzer(s -> (
	if not instance(s, String) then error "analyzer expected a string";
	chars := characters s;
	i := 0;
	() -> if chars#?i then (
	    r := (i, chars#i);
	    i = i + 1;
	    r)))

fromJSON = method()
fromJSON String := jsonTextP : utf8Analyzer
fromJSON File   := fromJSON @@ get

--------------
-- encoding --
--------------

toJSON = method(
    Dispatch => Thing,
    Options => {
	Indent         => null,
	IndentLevel    => 0,
	NameSeparator  => ": ",
	ValueSeparator => null,
	Sort           => false})

toJSON Thing   := toJSON MutableHashTable := o -> format @@ toString
toJSON String  := o -> format
toJSON RR      := o -> format_0
toJSON Number  := o -> format_0 @@ numeric
toJSON ZZ      :=
toJSON Boolean :=
toJSON Nothing := o -> toString
toJSON Symbol  := o -> x -> (
    if x === nil then "null" else format toString x)
toJSON Hypertext := o -> format @@ html

maybeNewline = o -> if o.Indent === null then "" else newline

valuesep = o -> o.ValueSeparator ?? if o.Indent === null then ", " else ","

indent = o -> (
    if o.Indent === null then ""
    else if instance(o.Indent, ZZ) then concatenate(o.IndentLevel * o.Indent)
    else if instance(o.Indent, String) then concatenate(
	o.IndentLevel : o.Indent)
    else error("expected indent level to be null, an integer, or a string"))

demarkValues = (o, f) -> concatenate(
    maybeNewline o,
    (o ++= {IndentLevel => o.IndentLevel + 1}; indent o),
    demark(valuesep o | maybeNewline o | indent o, f o),
    maybeNewline o,
    (o ++= {IndentLevel => o.IndentLevel - 1}; indent o))

toJSON VisibleList := o -> L -> (
    if #L > 0 then concatenate(
	"[",
	demarkValues(o, o' -> apply(L, x -> toJSON(x, o'))),
	"]")
    else "[]")

toJSON HashTable := o -> H -> (
    if #H > 0 then concatenate(
	"{",
	demarkValues(o, o' -> (if o.Sort then sort else identity) apply(
		keys H, k -> concatenate(
		    toJSON toString k,
		    o.NameSeparator,
		    toJSON(H#k, o')))),
	"}")
    else "{}")

beginDocumentation()

doc ///
  Key
    JSON
  Headline
    JSON encoding and decoding
  Description
    Text
      @HREF{"https://www.json.org/", "JSON"}@ (JavaScript Object Notation)
      is a common data interchange format.  This package provides two methods,
      @TO toJSON@ and @TO fromJSON@, for converting Macaulay2 things to
      valid JSON data and vice versa.
    Example
      toJSON {hashTable{"foo" => "bar"}, 1, 3.14159, true, false, nil}
      fromJSON oo
///

doc ///
  Key
    toJSON
    (toJSON,Boolean)
    (toJSON,HashTable)
    (toJSON,Hypertext)
    (toJSON,MutableHashTable)
    (toJSON,Nothing)
    (toJSON,Number)
    (toJSON,RR)
    (toJSON,String)
    (toJSON,Symbol)
    (toJSON,Thing)
    (toJSON,VisibleList)
    (toJSON,ZZ)
    [toJSON,Indent]
    [toJSON,IndentLevel]
    [toJSON,ValueSeparator]
    [toJSON,NameSeparator]
    [toJSON,Sort]
    Indent
    ValueSeparator
    NameSeparator
  Headline
    encode Macaulay2 things as JSON data
  Usage
    toJSON x
  Inputs
    x:Thing
    Indent => {Nothing, ZZ, String} -- how much to indent
    IndentLevel => ZZ -- current indentation level (intended for internal use)
    ValueSeparator => {Nothing, String} -- the string between values
    NameSeparator => String -- the string after names in object members
    Sort => Boolean -- whether to sort object members
  Outputs
    :String -- containing JSON data
  Description
    Text
      This method (which is also available using its synonym @CODE "json"@)
      returns a string containing JSON data corresponding to the
      given Macaulay2 thing.  If the @TT "Indent"@ option is @TT "null"@
      (the default), then there are no newlines or indentation.
    Example
      x = hashTable {"foo" => {1, 2, {pi, true, false, nil}}}
      toJSON x
    Text
      If the @TT "Indent"@ option is an integer, then newlines are added between
      values of arrays and members of lists and the given integer determines the
      number of spaces to indent for each level of indentation.
    Example
      toJSON(x, Indent => 2)
    Text
      Alternatively, the @TT "Indent"@ option can be a string corresponding to
      the indentation used for each level.
    Example
      toJSON(x, Indent => "\t")
    Text
      The @TT "ValueSeparator"@ option determines the string to use between
      values in an array and members in an object.  If it is @TT "null"@ (the
      default), then the string will either be @TT "\", \""@ or @TT "\",\""@,
      depending on whether @TT "Indent"@ is @TT "null"@ or not, respectively.
      Otherwise, the given string is used.
    Example
      toJSON(x, ValueSeparator => " , ")
    Text
      The @TT "NameSeparator"@ option determines the string to use after
      the name of an object member.  By default, it is @TT "\": \""@.
    Example
      toJSON(x, NameSeparator => " : ")
    Text
      By default, the members of objects are given in arbitrary order.  To
      sort them by name in lexicographic order as strings, use the @TT "Sort"@
      option.
    Example
      toJSON(hashTable{"foo" => 1, "bar" => 2, "baz" => 3}, Sort => true)
///

doc ///
  Key
    fromJSON
    (fromJSON, String)
    (fromJSON, File)
  Headline
    decode JSON data into Macaulay2 things
  Usage
    fromJSON s
  Inputs
    s:{String,File}
  Outputs
    :Thing -- the Macaulay2 equivalent of the given data
  Description
    Text
      The JSON data provided in the given string or file is parsed using the
      @TO "Parsing"@ package with the context-free grammar specified by
      @HREF{"https://datatracker.ietf.org/doc/html/rfc8259", "RFC 8259"}@.
      The type of the return value will vary depending on the data.

      Numbers will result in @TT "ZZ"@ or @TT "RR"@ objects, as appropriate.
    Example
      fromJSON "2"
      fromJSON "2.71828"
    Text
      Strings will result in strings.
    Example
      fromJSON "\"Hello, world!\""
    Text
      JSON's @TT "true"@ and @TT "false"@ will result in the corresponding
      Macaulay2 booleans.
    Example
      fromJSON "true"
      fromJSON "false"
    Text
      Due to the implementation of the @TT "Parsing"@ package, @TO "null"@
      cannot be a return value, and so the symbol @TO "nil"@ is returned
      when JSON's @TT "null"@ is given.
    Example
      fromJSON "null"
    Text
      Objects will result in hash tables.
    Example
      fromJSON "{\"foo\": 1, \"bar\": 2}"
    Text
      Arrays will result in lists.
    Example
      fromJSON "[1, 2, 3]"
    Text
      The input may also be a file containing JSON data.
    Example
      jsonFile = temporaryFileName() | ".json"
      jsonFile << "[1, 2, 3]" << endl << close
      fromJSON openIn jsonFile
///

-- generate parsing test file
///
tmpdir = temporaryFileName()
makeDirectory tmpdir
run("cd " | tmpdir |" && git clone https://github.com/nst/JSONTestSuite")
testdir = tmpdir | "/JSONTestSuite/test_parsing"
tsts = select(readDirectory(testdir), f ->
    match("\\.json$", f))

needsPackage "Parsing" -- for nil
debug Core -- for commentize

outdir = (needsPackage "JSON")#"source directory" | "JSON"
makeDirectory outdir

copyrightBanner = "tests from JSON Parsing Test Suite
Copyright 2016 Nicolas Seriot
MIT License
https://github.com/nst/JSONTestSuite"

outfile = openOut(outdir | "/test-parse.m2")
outfile << commentize  copyrightBanner << endl
for tst in sort select(tsts, f -> match("^y_", f)) do (
    outfile << endl << commentize tst << endl;
    testjson = get(testdir | "/" | tst);
    outfile << "assert BinaryOperation(symbol ===, fromJSON " <<
    format testjson << ", " << toExternalString fromJSON testjson << ")" << endl)
close outfile
///

TEST get(currentPackage#"auxiliary files" | "test-parse.m2")
TEST get(currentPackage#"auxiliary files" | "test-encode.m2")
