--		Copyright 1994 by Daniel R. Grayson

endFunctions := {}
addEndFunction = g -> (
     endFunctions = append(endFunctions,g);
     g)
exit1 := ret -> (
     scan(endFunctions, f -> f());
     exit ret)
erase quote exit
exit = exit1
protect quote exit

between = (m,v) -> mingle(v,#v-1:m)

-----------------------------------------------------------------------------

ExampleHashTable := new MutableHashTable
DocDatabase := null
docExtension := () -> (
     if phase === 2 then ".tmp"		  -- writing, to be renamed .pre
     else if phase === 3 then ".pre"	  -- reading temporary one renamed
     else if phase == 4 then ".tmp"	  -- writing, to be renamed .doc
     else ".doc"			  -- reading
     )
docFilename := () -> (
     v := lines(commandLine#0,"/");
     v = apply(#v-2, i -> v#i);		  -- drop isn't defined yet
     concatenate(between("/",v),"/cache/Macaulay2", docExtension()))

if phase === 1 then addStartFunction( 
     () -> DocDatabase = (
	  try (
	       g := openDatabase docFilename();
	       -- << "--using help file " << docFilename() << endl;
	       g)
	  else (
	       stderr << "--warning: couldn't open help file " << docFilename() << endl;
	       new MutableHashTable)))

if phase === 2 or phase === 4 then DocDatabase = openDatabaseOut docFilename()

GlobalAssignHook Function := (X,x) -> if not Documentation#?x then Documentation#x = X

--GlobalReleaseHook Function := (F,f) -> (
--     stderr << "warning: " << string F << " redefined" << endl;
--     )

name Function := f -> (
     if Documentation#?f then (
	  s := Documentation#f;
	  if class s === String then s
	  else if class s === Symbol then string s
	  else "--function--"
	  )
     else "--function--"
     )

repl := z -> (
     if class z === List or class z === Sequence 
     then SEQ apply(toList z, repl)
     else if instance(z,BasicList)
     then apply(z,repl)
     else z
     )

doc = x -> (
     if class x === Symbol then x = string x;
     if Documentation#?x then (
	  d := Documentation#x;
	  if class d === Symbol and value d === x then d = string d;
	  if Documentation#?d then d = Documentation#d;
	  if class d === SEQ then d
	  else if class d === String and DocDatabase#?d then evaluate DocDatabase#d
	  else if class d === List then d
	  else null)
     else if DocDatabase#?x then evaluate DocDatabase#x else null)

err := nodeName -> error("warning: documentation already provided for '", nodeName, "'")

keysDoc := () -> (
	if DocDatabase === null
	then keys Documentation
	else join(keys DocDatabase, keys Documentation)
	)

-----------------------------------------------------------------------------

topicList = () -> sort(select( keysDoc(), i -> basictype i === String))

examples = x -> (
     if Documentation#?x then x = Documentation#x;
     x = string x;
     if ExampleHashTable#?x then ExampleHashTable#x else {}
     )

fm1 := s -> concatenate (
     if class s === Sequence then (
	  if #s === 4 then (
	       if class s#0 === ScriptedFunctor
	       then (
		    if s#0 .? subscript
		    then (name s#0, "_", name s#1, "(", name s#2, ",", name s#3, ")")
		    else (name s#0, "^", name s#1, "(", name s#2, ",", name s#3, ")")
		    )
	       else if s#0 === NewOfFromMethod
	       then ("new ", name s#1, " of ", name s#2, " from ", name s#3)
	       else if s#0 === cohomology
	       then ("HH_", name s#1, "^", name s#2, " ", name s#3)
	       else if s#0 === homology
	       then ("HH^", name s#1, "_", name s#2, " ", name s#3)
	       else (s#0, "(", name s#1, ",", name s#2, ",", name s#3, ")")
	       )
	  else if #s === 3 then (
	       if class s#0 === Symbol then (
		    if s#0 === NewFromMethod
		    then ("new ", name s#1, " from ", name s#2)
		    else if s#0 === NewOfMethod
		    then ("new ", name s#1, " of ", name s#2)
		    else if string s#0 === " "
		    then (name s#1, " ", name s#2)
		    else (name s#1, " ", name s#0, " ", name s#2)
		    )
	       else if s#0 === homology
	       then ("HH_", name s#1, " ", name s#2)
	       else if s#0 === cohomology
	       then ("HH^", name s#1, " ", name s#2)
     	       else (name s#0, "(", name s#1, ",", name s#2, ")")
	       )
	  else if #s === 2 then (
	       if class s#0 === ScriptedFunction then (
		    hh := s#0;
		    if hh.?subscript and hh.?superscript
		    then (
			 (name s#0, " _ ", name s#1, "  or  ", name s#0, " ^ ", name s#1)
			 )
		    else if hh.?subscript then (name s#0, " _ ", name s#1)
		    else (name s#0, " ^ ", name s#1)
		    )
	       else if s#0 === NewMethod then ("new ", name s#1)
	       else (name s#0, " ", name s#1)
	       )
	  else (name s)
	  )
     else (name s)
     )

fm := (o,s) -> o << fm1 s

hr := (o) -> o << "-----------------------------------------------------------------------------" << endl

briefHelp := (o,s) -> (
     d := doc s;
     if d === null 
     then (
	  o << fm1 s << " --> Thing" << endl;		  -- no documentation available
	  false
	  )
     else if class d === List then (
	  o << fm1 s << " --> " << d#0 << endl;
	  d = drop(d,2);
     	  i := 0;
     	  while i < #d and d#i =!= PARA do i = i+1;
	  thereWasMore := i < #d;
	  d = take(d,i);
	  if #d > 0 then o << endl << text repl d << endl;
	  thereWasMore)
     else (
	  o << text d << endl;
	  false))

previousMethods := new MutableHashTable
saveMethod := meth -> (
     if previousMethods#?meth 
     then previousMethods#meth
     else (
     	  i := #previousMethods//2;
     	  previousMethods#i = meth;
     	  previousMethods#meth = i;
     	  i)
     )

help2 := (o,s) -> (
     d := doc s;
     if d === null 
     then (
	  o << fm1 s << " --> Thing" << endl;		  -- no documentation available
	  )
     else if class d === List then (
	  o << fm1 s << " --> " << d#0 << endl;
	  if #d > 2 then o << endl << text repl drop(d,2) << endl;
	  )
     else o << text d << endl;
--     if class s =!= Sequence then (
--	  m := methods s;
--	  if #m > 0 then (
--	       scan(m, meth -> (
--			 hr o; 
--			 if briefHelp(o,meth) then (
--			      o << endl
--			      << "Type 'help "
--			      << saveMethod meth 
--			      << "' for more help." << endl;
--			      );
--			 ));
--	       hr o;
--	       );
--	  );
     if # options value s > 0 then (
	  o << "Options and default values:" << endl;
	  hr o;
	  scan(rsort pairs options value s, (option,default) -> (
		    o << s << "( ... , " << option << " => " << default << ") :" << endl;
		    if Documentation#?(value s,option)
		    then (
			 o << endl << text repl Documentation#(value s,option) << endl;
			 )
		    else (
			 o << endl << "No documentation available." << endl;
			 );
		    hr o;
		    )
	       );
	  );
     )

help = s -> (
     pager := getenv "PAGER";
     if pager === "" then pager = "more";
     if getenv "TERM" === "emacs" 
     or version#"OS" === "Windows NT" 
     then pager = null;
     o := if pager === null then stdout else openOut concatenate("!", pager );
     if class s === List
     then (
	  scan(s, i -> ( hr o; help2 (o,i); ));
	  )
     else if class s === ZZ then (
	  if 0 <= s and s < #previousMethods then (
	       o << endl;
	       help2 (o,previousMethods#s);
	       )
	  else (
	       stderr << "No documentation on " << s << endl;
	       )
	  )
     else (
	  o << endl;
	  help2 (o,s);
	  );
     if o =!= stdout then close o;
     )

topics = Command (
     () -> (
	  << columnate(
	       topicList(),
	       if width stdout == 0 then 79 else width stdout - 1) << endl;
	  )
     )

apropos = (pattern) -> (
     mat := "*" | string pattern | "*";
     sort select( keys symbolTable(), i -> match(i,mat)))

writableGlobals := new MutableHashTable
scan((
	  quote oooo,
	  quote ooo,
	  quote oo,
	  quote path,
	  quote phase,
	  -- quote writeExamples,
	  -- quote readExamples,
	  quote hypertex
	  ), x -> writableGlobals#x = true)

testFileCounter := 0
exprCounter := 0
file := null

fourDigits := i -> (
     s := string i;
     concatenate(4-#s:"0", s)
     )

TEST = (e) -> if phase === 2 then (
     testFileCounter = testFileCounter + 1;
     openOut concatenate("../tmp/Tests/", fourDigits testFileCounter, ".m2") 
     << e << endl << close;
     )
-- writeExamples = false
-- readExamples = false

--someExamples := z -> (
--     class z === EXAMPLE or (
--     	  (basictype z === BasicList or basictype z === Sequence)
--     	  and 1 === # select(1, z, someExamples)))

extractExamples := x -> (
     if class x === EXAMPLE
     then {x#0}
     else if basictype x === BasicList or basictype x === Sequence
     then join apply(toSequence x, extractExamples)
     else {})

NameFile := "../tmp/Examples/FileNames"

saveNameHashTable := null
NameHashTable := () -> (
     NameHashTable = () -> saveNameHashTable;
     saveNameHashTable = (
     	  if phase === 4 then hashTable evaluate get NameFile
     	  else (
      	       try new MutableHashTable from hashTable evaluate get NameFile
     	       else new MutableHashTable
	       )
	  )
     )
if phase === 2 then (
     addEndFunction( () -> (
	       stderr << "writing " << NameFile << endl;
	       NameFile << pairs NameHashTable() << endl << close;
	       ));
     )


documentOption = z -> (
     if class z != List then error "expected a list";
     if #z < 2 then error "expected a list of length at least 2";
     fn := z#0;
     opt := z#1;
     doc := drop(z,2);
     if not (options fn)#?opt then (
	  error ("expected ", name opt, " to be an option of ", name fn);
	  );
     Documentation#(fn,opt) = doc;
     )

DocumentableValueType := new MutableHashTable
DocumentableValueType#HashTable = true
DocumentableValueType#Function = true
DocumentableValueType#BasicList = true
DocumentableValueType#Nothing = true

UndocumentableLabel := new MutableHashTable
UndocumentableLabel.environment = true
UndocumentableLabel.commandLine = true

nodeName := ""
nodeBaseFilename := ""
exampleCounter := 0
exampleOutputFile := null
exampleResults := {}
makeBaseFilename := () -> (
     if (NameHashTable())#?nodeName
     then (NameHashTable())#nodeName
     else (
	  if phase === 4 then error(
	       "documentation node '", nodeName, "' has no sequence number");
	  (NameHashTable())#nodeName = concatenate(
	       "../tmp/Examples/", fourDigits (#(NameHashTable()))
	       )
	  )
     );
processExamplesLoop := s -> (
     if class s === EXAMPLE then (
	  exampleCounter = exampleCounter + 1;
	  exampleOutputFile << s#0 << endl;
	  if exampleResults#?exampleCounter
	  then PRE exampleResults#exampleCounter
	  else (
	       if #exampleResults === exampleCounter then (
		    stderr << "warning : input file " << nodeBaseFilename 
		    << ".out terminates prematurely" << endl;
		    );
	       PRE concatenate("in = ",s#0)
	       ))
     else if class s === Sequence or basictype s === List
     then apply(s,processExamplesLoop)
     else s)
processExamples := (docBody) -> (
     examples := extractExamples docBody;
     if phase > 1 and #examples > 0 then (
	  exampleOutputFile = if phase === 2 then openOut(nodeBaseFilename | ".m2");
	  exampleOutputFile << "-- " << nodeName << endl;
	  exampleResults = try get (nodeBaseFilename | ".out") else (
	       if phase === 4 or phase === 5 then (
		    stderr << "warning : can't open input file '" 
	       	    << nodeBaseFilename << ".out'" << endl;
		    );
	       ""
	       );
	  exampleResults = lines(exampleResults,"\1");
	  exampleCounter = 0;
	  docBody = apply(docBody,processExamplesLoop);
	  close exampleOutputFile;
	  );
     if #examples > 0 then ExampleHashTable#nodeName = examples;
     docBody
     )
storeDoc := (docBody) -> (
     if phase === 0
     then (
	  if Documentation#?nodeName then err nodeName;
	  Documentation#nodeName = docBody
	  )
     else if phase === 2 or phase === 4 then (
	  if DocDatabase#?nodeName then err nodeName;
	  DocDatabase#nodeName = name docBody;
	  );
     )
document = z -> (
     if class z != List then error "expected a list";
     if #z === 0 then error "expected a nonempty list";
     key := z#0;
     if not ( class key === Symbol or class key === String )
     then error "expected first element of list to be a symbol or string";
     nodeName = string key;
     nodeBaseFilename = makeBaseFilename();
     docBody := repl toList apply(1 .. #z - 1, i -> z#i); -- drop isn't defined yet
     docBody = processExamples docBody;
     storeDoc docBody;
     if phase === 1 and not writableGlobals#?key and class key === Symbol 
     then protect key;
     if class key === Symbol and value key =!= key and not mutable key
     and not UndocumentableLabel#?key and DocumentableValueType#?(basictype value key)
     then Documentation#(value key) = nodeName;
     )

exportDocumentation = () -> (
     scan(keys Documentation, key -> (
	       if class key === Sequence
	       and (#key === 3 or #key === 4)
	       and class Documentation#key === List
	       and #(Documentation#key) > 2
	       then (
		    nodeName = name key;
		    nodeBaseFilename = makeBaseFilename();
		    z := Documentation#key;
		    docBody := drop(z,2);
		    docBody = join({ concatenate(fm1 key, " --> ", name z#0), PARA}, docBody);
		    docBody = repl docBody;
		    docBody = processExamples docBody;
		    storeDoc docBody;
		    Documentation#key = nodeName;
		    Documentation#(z#1) = key;
		    )
	       )
	  )
     )

SEEALSO = v -> (
     if class v =!= Sequence then v = seq v;
     if #v > 0 then (
	  PARA, 
	  "See also ",
	  if #v === 1 then {TO v#0}
	  else if #v === 2 then {TO v#0, " and ", TO v#1}
	  else mingle(
	       apply(v, i -> TO i),
	       append(#v-2 : ", ", ", and ")
	       ),
     	  "."))

Nothing << Thing := { Nothing,
     (x,y) -> null,
     "null << x", " -- does nothing and returns ", TT "null", ".",
     PARA,
     "The intention here is that you can use ", TT "null", " as a dummy
     output file."
     }

document { quote document,
     TT "document {quote s, d}", " -- install documentation d for symbol s.",
     PARA,
     "The documentation d should be ", TO "hypertext", ".",
     PARA,
     SEEALSO ("help", "doc", "phase", "examples")
     }

document { quote TEST,
     TT "TEST s", " -- writes the string s to a new test file.  The
     commands in that file can be run later as a test.",
     PARA,
     "Intended for internal use only."
     }

document { quote between,
     TT "between(m,v)", " -- inserts ", TT "m", " between each pair of elements 
     of the list or sequence ", TT "v", ", returning a list.",
     }

document { quote SEEALSO,
     TT "SEEALSO (\"a\",\"b\")", " -- inserts, into a documentation page, a sentence
     instructing the reader to see some other topics.",
     PARA,
     SEEALSO "document"
     }

document { quote doc,
     TT "doc s", " -- provides the online documention for the topic s, in
     internal ", TO "hypertext", " form, suitable for conversion to
     text with ", TO "text", " or to html with ", TO "html", "."
     }

document { quote help,
     -- no PARA in this documentation, so it all gets displayed.
     TT "help X", " -- displays the online documentation for ", TT "X", ".",
     BR, NOINDENT,
     TT "help \"Macaulay 2\"", " -- displays the base of the online documentation
     tree.",
     BR, NOINDENT,
     TT "help methods X", " -- displays help messages about the methods usable
     with things of type ", TT "X", ".",
     BR, NOINDENT,
     TT "help methods quote **", " -- displays help messages about the methods 
     usable with the operator ", TT "**", ".",
     BR, NOINDENT,
     TT "help methods (quote **, X)", " -- displays help messages about the 
     methods usable with the operator ", TT "**", " and a thing of
     class ", TT "X", ".",
     BR, NOINDENT,
     TT "help methods (X, Y)", " -- displays help messages about the 
     methods usable with a thing of class ", TT "X", " and a thing of class
     ", TT "Y", "."
     }

document { quote topicList,
     TT "topicList()", " -- provides a complete list of topics on which help 
     is available.",
     PARA,
     "Intended to be used in programs.  Users will prefer 
     to use ", TO "topics", ".",
     PARA,
     SEEALSO "help"
     }

document { quote topics,
     TT "topics  ", " -- displays a list of topics on which help is available.",
     PARA,
     "topics() -- Does the same in a function or file.",
     PARA,
     SEEALSO "help"
     }

document { quote apropos,
     TT "apropos s", " -- displays a list of global symbols which match
     the pattern specified by the string s.",
     PARA,
     "The pattern may contain '*'s as wild card characters.",
     EXAMPLE "apropos \"scan\""
     }

document { quote examples,
     TT "examples f", " -- returns a list of strings containing examples
     of code using the function ", TT "f", " provided in the documentation
     of ", TT "f", ".",
     PARA,
     EXAMPLE ///examples partitions///,
     EXAMPLE ///print \ examples partitions;///,
     SEEALSO ("document", "printExamples")
     }

TEST ///
     assert( class examples MutableList === List )
     assert( # examples MutableList > 0 )
///



printExamples = f -> scan(examples f, i -> << i << endl)

document { quote printExamples,
     TT "printExamples f", " -- prints out the examples of code using
     the function ", TT "f", " provided in the documentation for
     ", TT "f", ".",
     PARA,
     EXAMPLE "printExamples partition",
     SEEALSO ("examples", "document")
     }

document { quote Documentation,
     TT "Documentation", " -- a hash table which is used to store
     pointers to documentation of functions, symbols, and methods.",
     PARA,
     "This hash table is used by the routines that display 
     documentation, and its format may change.",
     PARA,
     "The documentation is stored both in a hash table in memory, and in a 
     database file.  Combined, the two look like a single hash table, but
     the ", TO "phase", " variable controls whether entries stored in it 
     persist to the next session.",
     PARA,
     "The key may be anything, and if the value is a string, then
     that string is taken to be the name of the thing, (which can be used for
     when printing the thing).  The search for documentation continues 
     with the name.",
     PARA,
     "The key may be anything, and if the value is a symbol, then
     the symbol is one whose value is the thing, (which can be used for
     when printing the thing), and the search for 
     documentation continues with the symbol.",
     PARA,
     "The key may be a string.  If the value is a database, then the
     documentation is to be found there.  If the value is a list of
     type ", TO "SEQ", " then it's the documentation itself.",
     PARA,
     "The key may be a sequence such as ", TT "(quote +,X,Y)", "
     which is used to access the documentation installed when the method
     for adding an instance of class X to an instance of class Y was
     defined.  In this case the value is the list presented at that time,
     i.e., a list of the form ", TT "{Z, (x,y) -> ... , documentation ... }",
     ".",
     SEEALSO ":="
     }

TEST ///
assert( null =!= doc sin)
assert( null =!= doc "sin")
assert( null =!= doc quote sin)
///

