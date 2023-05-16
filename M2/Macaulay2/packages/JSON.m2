newPackage(
    "JSON",
    Headline => "JSON encoding and decoding",
    Version => "0.1",
    Date => "August 31, 2022",
    Authors => {{
	    Name => "Doug Torrance",
	    Email => "dtorrance@piedmont.edu",
	    HomePage => "https://webwork.piedmont.edu/~dtorrance"}},
    Keywords => {"System"},
    PackageImports => {"Parsing"},
    AuxiliaryFiles => true)

export {
    "toJSON",
    "fromJSON",
    "Indent",
    "NameSeparator",
    "ValueSeparator"
    }

exportFrom_Parsing {"nil"}

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
charP = unescapedP | ("\\" @
    orP("\"", "\\", "/", "b", "f", "n", "r", "t",
	andP("u", hexDigitP, hexDigitP, hexDigitP, hexDigitP)))
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

JSONEncoder = new SelfInitializingType of MutableHashTable

protect IndentLevel

jsonEncoder = method()
jsonEncoder OptionTable := o -> (
    e := JSONEncoder o;
    e.IndentLevel = 0;
    if e.ValueSeparator === null then e.ValueSeparator = (
	if e.Indent === null then ", " else ",");
    e)

toJSON = method(Options => {
	Indent         => null,
	NameSeparator  => ": ",
	ValueSeparator => null,
	Sort           => false
	})

toJSON Thing := o -> x -> toJSON(jsonEncoder o, x)

toJSON(JSONEncoder, String)  := o -> (e, x) -> format x
toJSON(JSONEncoder, RR)      := o -> (e, x) -> format(0, x)
toJSON(JSONEncoder, ZZ)      :=
toJSON(JSONEncoder, Boolean) :=
toJSON(JSONEncoder, Nothing) := o -> (e, x) -> toString x
toJSON(JSONEncoder, QQ)       :=
toJSON(JSONEncoder, Constant) :=  o -> (e, x) -> toJSON(e, numeric x)
toJSON(JSONEncoder, Symbol) := o -> (e, x) -> (
    if x === nil then "null" else error("unknown symbol"))

maybeNewline = method()
maybeNewline JSONEncoder := e -> if e.Indent === null then "" else newline

indent = method()
indent JSONEncoder := e -> (
    if e.Indent === null then ""
    else if instance(e.Indent, ZZ) then concatenate(e.IndentLevel * e.Indent)
    else if instance(e.Indent, String) then concatenate(
	e.IndentLevel : e.Indent)
    else error("expected indent level to be null, an integer, or a string"))

incIndentLevel = method()
incIndentLevel JSONEncoder := e -> e.IndentLevel = e.IndentLevel + 1

decIndentLevel = method()
decIndentLevel JSONEncoder := e -> e.IndentLevel = e.IndentLevel - 1

demarkValues = method()
demarkValues(JSONEncoder, Function) := (e, f) -> concatenate(
    maybeNewline e,
    (incIndentLevel e; indent e),
    demark(e.ValueSeparator | maybeNewline e | indent e, f()),
    maybeNewline e,
    (decIndentLevel e; indent e))

toJSON(JSONEncoder, VisibleList):= o -> (e, L) -> (
    if #L > 0 then concatenate(
	"[",
	demarkValues(e, () -> apply(L, x -> toJSON(e, x))),
	"]")
    else "[]")

toJSON(JSONEncoder, HashTable) := o -> (e, H) -> (
    if #H > 0 then concatenate(
	"{",
	demarkValues(e, () -> (if e.Sort then sort else identity) apply(
		keys H, k -> concatenate(
		    toJSON(e, toString k),
		    e.NameSeparator,
		    toJSON(e, H#k)))),
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
    (toJSON,Thing)
    [toJSON,Indent]
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
    ValueSeparator => {Nothing, String} -- the string between values
    NameSeparator => String -- the string after names in object members
    Sort => Boolean -- whether to sort object members
  Outputs
    :String -- containing JSON data
  Description
    Text
      This method returns a string containing JSON data corresponding to the
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
    json = get(testdir | "/" | tst);
    outfile << "assert BinaryOperation(symbol ===, fromJSON " <<
    format json << ", " << toExternalString fromJSON json << ")" << endl)
close outfile
///

TEST(currentPackage#"source directory" | "JSON/test-parse.m2",
    FileName => true)

TEST(currentPackage#"source directory" | "JSON/test-encode.m2",
    FileName => true)
