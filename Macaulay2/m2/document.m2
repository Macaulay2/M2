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

DocHashTable := new MutableHashTable
ExampleHashTable := new MutableHashTable
DocDatabase := null
docExtension := () -> if phase === 2 or phase === 3 then ".pre" else ".doc"
between = (m,v) -> mingle(v,#v-1:m)
docFilename := () -> (
     v := lines(commandLine#0,"/");
     v = apply(#v-2, i -> v#i);		  -- drop isn't defined yet
     concatenate(between("/",v),"/bin/Macaulay2", docExtension()))
if phase === 1 then addStartFunction( 
     () -> DocDatabase = (
	  try (
	       g := openDatabase docFilename();
	       -- << "--using help file " << docFilename() << endl;
	       g)
	  else (
	       stderr << "--warning: couldn't open help file "
	       << docFilename() << endl;
	       new MutableHashTable)))

if phase === 2 or phase === 4 then DocDatabase = openDatabaseOut docFilename()
Names := new MutableHashTable
name Function := f -> (
     if Names#?f then string Names#f 
     else "--function--"
     )

GlobalAssignHook Function := (X,x) -> if not Names#?x then Names#x = X
--GlobalReleaseHook Function := (F,f) -> (
--     stderr << "warning: " << string F << " redefined" << endl;
--     )

topicList = () -> sort(select(
	  join(keys DocDatabase, keys DocHashTable), 
	  i -> basictype i === String))

doc = x -> (
     if Names#?x then x = Names#x;
     x = string x;
     if DocHashTable#?x then DocHashTable#x
     else
     if DocDatabase#?x then evaluate DocDatabase#x
     )

examples = x -> (
     if Names#?x then x = Names#x;
     x = string x;
     if ExampleHashTable#?x then ExampleHashTable#x else {}
     )

help = s -> (
     d := doc s;
     if d =!= null then << endl << text d << endl
     else (
	  << "No documentation available on " << name s << "." << endl;
	  << "Use \"topics()\" to see the list of help topics." << endl;
	  );
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
writableGlobal := x -> writableGlobals#x = true
writableGlobal quote oooo
writableGlobal quote ooo
writableGlobal quote oo
writableGlobal quote path
writableGlobal quote phase
-- writableGlobal quote writeExamples
-- writableGlobal quote readExamples
writableGlobal quote hypertex

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
     openOut concatenate("Tests/", fourDigits testFileCounter, ".m2") 
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

NameFile := "Examples/FileNames"

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

repl := z -> (
     if class z === List or class z === Sequence 
     then SEQ apply(elements z, repl)
     else if instance(z,BasicList)
     then apply(z,repl)
     else z
     )

document = z -> (
     if class z != List then error "expected a list";
     if #z === 0 then error "expected a nonempty list";
     label := z#0;
     nodeName := (
	  if class label === Sequence 
     	  then concatenate(name label#1, " ", string label#0, " ", name label#2)
	  else string label
	  );
     filename = (
	  if (NameHashTable())#?nodeName
	  then (NameHashTable())#nodeName
	  else (
	       if phase === 4 then error(
		    "documentation node '", nodeName, "' has no sequence number");
	       (NameHashTable())#nodeName = concatenate(
		    "Examples/", fourDigits (#(NameHashTable()))
		    )
	       )
	  );
     valuelabel := (
	  if class label === Sequence 
	  then (
	       if # label =!= 3 then error "expected sequence of length 3";
	       tmp := lookup label;
	       if tmp === null 
	       then error concatenate ("expected a method for ", nodeName);
	       tmp
	       )
	  else value label
	  );
     docBody := repl elements apply(1 .. #z - 1, i -> z#i);
     -- drop isn't defined yet
     if phase === 1 and not writableGlobals#?label and class label === Symbol 
     then protect label;
     examples := extractExamples docBody;
     if phase > 1 and #examples > 0 then (
	  if phase === 2 then (
	       -- write example input to file
	       file := (
		    try openOut (filename | ".m2")
		    else (
			 stderr << "failed to open '" << file << "'";
			 null
			 )
		    );
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
     if phase === 0
     then DocHashTable#nodeName = docBody
     else if phase === 2 or phase === 4 then (
	  if DocDatabase#?nodeName then (
	       stderr << "warning: documentation already provided for '"
	       << nodeName << "'" << endl;
	       )
	  else DocDatabase#nodeName = name docBody;
	  );
     if #examples > 0 then ExampleHashTable#nodeName = examples;
     if valuelabel =!= label
     and label =!= quote environment
     and ( 
	  basictype valuelabel === HashTable
	  or basictype valuelabel === Function
	  or basictype valuelabel === BasicList
	  )
     then (
	  Names#valuelabel = nodeName;
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
     TT "help s", " -- displays the online documentation for ", TT "s", ".",
     BR,
     NOINDENT, TT "help \"Macaulay 2\"", " -- displays the base of the documentation
     tree.",
     SEEALSO("topics")
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
     EXAMPLE ///print \ partitions;///,
     SEEALSO ("document", "printExamples");
     }

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

