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

-- The documentation is stored both in a hash table in memory, and in a 
-- database file.  Combined, the two look like a single hash table, but
-- the 'phase' variable controls whether entries stored in it persist
-- to the next session.

-- Here's the way keys are used in the hash table Documentation:

--     (method, type, ...)      --> { returntype, function, documentation, ... }
--     thing	      	   	--> "nameofthing" (redirection)
--     thing	      	   	--> a symbol (redirection)
--     "nodename"               --> SEQ { documentation, ... }
--     	    	      	   	      or
--     	    	      	   	    a data base (redirection)

-- If the value stored is a string, then it is intended that that
-- string can be used in printing as the name of the thing, and
-- as a key when looking up the documentation further.

-- If the value stored is a symbol, then it is intended that that the
-- name of the symbol can be used in printing or as a key, provided
-- the value of the symbol is still equal to the thing.

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
     then SEQ apply(elements z, repl)
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

storeDoc := (nodeName,docBody) -> (
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
keysDoc := () -> join(keys DocDatabase, keys Documentation)

-----------------------------------------------------------------------------

topicList = () -> sort(select( keysDoc(), i -> basictype i === String))

examples = x -> (
     if Documentation#?x then x = Documentation#x;
     x = string x;
     if ExampleHashTable#?x then ExampleHashTable#x else {}
     )

fm := (o,s) -> (
     if class s === Sequence then (
	  if #s === 4 then (
	       if class s#0 === ScriptedFunctor
	       then (
		    if s#0 .? subscript
		    then o << s#0 << "_" << s#1 << "(" << s#2 << "," << s#3 << ")"
		    else o << s#0 << "^" << s#1 << "(" << s#2 << "," << s#3 << ")"
		    )
	       else if s#0 === NewOfFromMethod
	       then o << "new " << s#1 << " of " << s#2 << " from " << s#3
	       else if s#0 === cohomology
	       then o << "HH_" << s#1 << "^" << s#2 << " " << s#3
	       else if s#0 === homology
	       then o << "HH^" << s#1 << "_" << s#2 << " " << s#3
	       else o << s#0 << "(" << s#1 << "," << s#2 << "," << s#3 << ")"
	       )
	  else if #s === 3 then (
	       if class s#0 === Symbol then (
		    if s#0 === NewFromMethod
		    then o << "new " << s#1 << " from " << s#2
		    else if s#0 === NewOfMethod
		    then o << "new " << s#1 << " of " << s#2
		    else if string s#0 === " "
		    then o << s#1 << " " << s#2
		    else o << s#1 << " " << s#0 << " " << s#2
		    )
	       else if s#0 === homology
	       then o << "HH_" << s#1 << " " << s#2
	       else if s#0 === cohomology
	       then o << "HH^" << s#1 << " " << s#2
     	       else o << s#0 << "(" << s#1 << "," << s#2 << ")"
	       )
	  else if #s === 2 then (
	       if class s#0 === ScriptedFunction then (
		    hh := s#0;
		    if hh.?subscript and hh.?superscript
		    then (
			 o << s#0 << " _ " << s#1
			 << "  or  " << s#0 << " ^ " << s#1
			 )
		    else if hh.?subscript then o << s#0 << " _ " << s#1
		    else o << s#0 << " ^ " << s#1
		    )
	       else if s#0 === NewMethod then o << "new " << s#1
	       else o << s#0 << " " << s#1
	       )
	  else o << s
	  )
     else o << s
     )

hr := (o) -> o << "-----------------------------------------------------------------------------" << endl

briefHelp := (o,s) -> (
     d := doc s;
     if d === null 
     then (
	  fm (o,s);
	  o << " : no documentation available" << endl;
	  false
	  )
     else if class d === List then (
	  fm (o,s);
	  o << " --> " << d#0 << endl;
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
	  fm (o,s);
	  o << " : no documentation available" << endl;
	  )
     else if class d === List then (
	  fm (o,s);
	  o << " --> " << d#0 << endl;
	  if #d > 2 then o << endl << text repl drop(d,2) << endl;
	  )
     else o << text d << endl;
     if class s =!= Sequence then (
	  m := methods s;
	  if #m > 0 then (
	       scan(m, meth -> (
			 hr o; 
			 if briefHelp(o,meth) then (
			      o << endl
			      << "Type 'help "
			      << saveMethod meth 
			      << "' for more help." << endl;
			      );
			 ));
	       hr o;
	       );
	  );
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
     if getenv "TERM" === "emacs" or version#"OS" === "Windows NT" 
     then pager = null;
     o := if pager === null then stdout else openOut concatenate("!", pager );
     if class s === List
     then (
	  scan(s, i -> ( hr o; help2 (o,i); ));
	  hr o;
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
filename := ""
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
     then join apply(unlist x, extractExamples)
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

err1 := () -> (
     stderr << "warning : input file " 
     << filename 
     << ".out terminates prematurely" << endl;
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

document = z -> (
     if class z != List then error "expected a list";
     if #z === 0 then error "expected a nonempty list";
     label := z#0;
     if not ( class label === Symbol or class label === String )
     then error "expected first element of list to be a symbol";
     nodeName := string label;
     filename = (
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
     docBody := repl elements apply(1 .. #z - 1, i -> z#i);
     -- drop isn't defined yet
     if phase === 1 and not writableGlobals#?label and class label === Symbol 
     then protect label;
     examples := extractExamples docBody;
     if phase > 1 and #examples > 0 then (
	  if phase === 2 then (
	       -- write example input to file
	       file := openOut (filename | ".m2");
	       file << "-- " << nodeName << endl;
	       if phase === 0 and file =!= null then (
		    << "-- " << nodeName << " -- " << filename << ".m2" << endl;
		    );
	       );
	  result := try get (filename | ".out") else (
	       if phase === 4 or phase === 5 then (
		    stderr << "warning : can't open input file '" 
	       	    << filename << ".out'" << endl;
		    );
	       ""
	       );
	  result = lines(result,"\1");
	  exprCounter := 0;
	  fun1 := s -> (
	       if class s === EXAMPLE then (
		    if phase === 2 then file << s#0 << endl;
		    exprCounter = exprCounter + 1;
		    if phase === 2 then PRE concatenate("in = ",s#0)
		    else if exprCounter < #result
		    then PRE result#exprCounter
		    else (
			 if exprCounter === #result then err1();
			 PRE concatenate("in = ",s#0)
			 ))
	       else if class s === Sequence or basictype s === List
	       then apply(s,fun1)
	       else s);
	  docBody = apply(docBody,fun1);
	  close file;
	  );
     storeDoc(nodeName,docBody);
     if #examples > 0 then ExampleHashTable#nodeName = examples;
     if value label =!= label 
     and not mutable label
     and label =!= quote environment
     and label =!= quote commandLine
     and ( 
	  basictype value label === HashTable
	  or basictype value label === Function
	  or basictype value label === BasicList
	  or basictype value label === Nothing
	  )
     then (
	  Documentation#(value label) = nodeName;
	  );
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
     TT "help methods quote **", " -- displays help messages about the accessible
     with the operator ", TT "**", ".",
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

TEST ///
help sin
help "sin"
help quote sin
///

