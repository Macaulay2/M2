--		Copyright 1994-1999 by Daniel R. Grayson

Function \ Sequence := Function \ List := (f,v) -> apply(v,f)
Sequence / Function := List / Function := (v,f) -> apply(v,f)

use = identity				  -- just temporary, until methods.m2

globalAssignFunction = (X,x) -> (
     if not x#?(quote name) then (
	  x.symbol = X;
	  x.name = string X;
	  );
     use x;
     )

globalReleaseFunction = (X,x) -> (
     if x.?symbol and X === x.symbol
     then (
	  remove(x,quote name);
	  remove(x,quote symbol);
	  )
     )

GlobalAssignHook Type := globalAssignFunction
GlobalReleaseHook Type := globalReleaseFunction
-----------------------------------------------------------------------------
endFunctions := {}
addEndFunction = g -> (
     endFunctions = append(endFunctions,g);
     g)
runEndFunctions = () -> scan(endFunctions, f -> f())

exit1 := ret -> (runEndFunctions(); exit ret)
erase quote exit
exit = exit1

erase quote [

between = (m,v) -> mingle(v,#v-1:m)

-----------------------------------------------------------------------------

DocDatabase := null
addEndFunction(() -> (
	  if class DocDatabase === Database then (
	       close DocDatabase;
	       DocDatabase = null;
	       )))

docExtension := () -> (
     if phase === 2 then "-tmp"		  -- writing, to be renamed -pre
     else if phase === 3 then "-pre"	  -- reading temporary one renamed
     else if phase === 4 then "-tmp"	  -- writing, to be renamed -doc
     else "-doc"			  -- reading
     )

docFilename := () -> (
     progname := commandLine#0;
     if substring(progname,0,1) === "\"" then progname = substring(progname,1);
     if version#"operating system" === "MACOS" then "::cache:Macaulay2-doc"
     else concatenate(
	  between(pathSeparator,drop(lines(progname,pathSeparator),-2)),
	  pathSeparator, "cache", pathSeparator, "Macaulay2", docExtension()))

if phase === 1 then addStartFunction( 
     () -> (
	  filename := docFilename();
	  try (
	       DocDatabase = openDatabase filename;
	       -- << "--using help file " << filename << endl;
	       )
	  else (
	       stderr << "--warning: couldn't open help file " << filename << endl;
	       DocDatabase = new MutableHashTable;
	       )))

if phase === 2 or phase === 4 then DocDatabase = openDatabaseOut docFilename()

Documentation = new MutableHashTable

DocumentableValueType := new MutableHashTable
DocumentableValueType#HashTable = true
DocumentableValueType#Function = true
DocumentableValueType#BasicList = true
DocumentableValueType#Nothing = true

UndocumentableLabel := new MutableHashTable
UndocumentableLabel.environment = true
UndocumentableLabel.commandLine = true

documentableValue := key -> (
     class key === Symbol
     and value key =!= key
     and not mutable key
     and not UndocumentableLabel#?key
     and DocumentableValueType#?(basictype value key)
     )

scanPairs(symbolTable(),
     (name,symbol) -> if documentableValue symbol then Symbols#(value symbol) = symbol
     )


repl := z -> (
     if class z === List or class z === Sequence then SEQ apply(toList z, repl)
     else if class z === TO then z
     else if instance(z,MarkUpList) then apply(z,repl)
     else if instance(z,MarkUpType) then z{}
     else z
     )

haveDoc := x -> Documentation#?x or class x === String and DocDatabase#?x
getDoc := x -> if Documentation#?x then Documentation#x else value DocDatabase#x

doc = x -> (
     if haveDoc x then getDoc x
     else (
	  x = formatDocumentTag x;
     	  if haveDoc x then getDoc x else null
	  )
     )

err := nodeName -> (
     stderr
     << concatenate ("warning: documentation already provided for '", nodeName, "'") 
     << newline
     << flush;
     )

keysDoc := () -> (
	if DocDatabase === null
	then keys Documentation
	else join(keys DocDatabase, keys Documentation)
	)

-----------------------------------------------------------------------------

topicList = () -> sort select(keysDoc(), i -> class i === String)

getExampleInputs := t -> (
     if instance(t, ExampleTABLE) then apply(toList t, first)
     else if instance(t,BasicList) then join apply(toSequence t, getExampleInputs)
     else {})

examples = x -> getExampleInputs doc x

formatDocumentTag = s -> concatenate (
     if class s === Sequence then (
	  if #s === 4 then (
	       if class s#0 === ScriptedFunctor
	       then (
		    if s#0 .? subscript
		    then (toString s#0, "_", toString s#1, "(", toString s#2, ",", toString s#3, ")")
		    else (toString s#0, "^", toString s#1, "(", toString s#2, ",", toString s#3, ")")
		    )
	       else if s#0 === NewOfFromMethod
	       then ("new ", toString s#1, " of ", toString s#2, " from ", toString s#3)
	       else if s#0 === cohomology
	       then ("HH_", toString s#1, "^", toString s#2, " ", toString s#3)
	       else if s#0 === homology
	       then ("HH^", toString s#1, "_", toString s#2, " ", toString s#3)
	       else (toString s#0, "(", toString s#1, ",", toString s#2, ",", toString s#3, ")")
	       )
	  else if #s === 3 then (
	       if class s#0 === Symbol then (
		    if s#0 === NewFromMethod
		    then ("new ", toString s#1, " from ", toString s#2)
		    else if s#0 === NewOfMethod
		    then ("new ", toString s#1, " of ", toString s#2)
		    else if s#0 === quote " "
		    then (toString s#1, " ", toString s#2)
		    else (formatDocumentTag s#1, " ", toString s#0, " ", toString s#2)
		    )
	       else if s#0 === homology
	       then ("HH_", toString s#1, " ", toString s#2)
	       else if s#0 === cohomology
	       then ("HH^", toString s#1, " ", toString s#2)
     	       else (toString s#0, "(", toString s#1, ",", toString s#2, ")")
	       )
	  else if #s === 2 then (
	       if class s#0 === ScriptedFunction then (
		    hh := s#0;
		    if hh.?subscript and hh.?superscript
		    then (
			 (toString s#0, " _ ", toString s#1, "  or  ", toString s#0, " ^ ", toString s#1)
			 )
		    else if hh.?subscript then (toString s#0, " _ ", toString s#1)
		    else (toString s#0, " ^ ", toString s#1)
		    )
	       else if s#0 === homology
	       then ("HH ", toString s#1)
	       else if s#0 === cohomology
	       then ("HH ", toString s#1)
	       else if s#0 === NewMethod then ("new ", toString s#1)
	       else if s#0 === quote ~ then (toString s#1, " ", toString s#0) -- postfix!
	       else if class s#0 === Symbol then (toString s#0, " ", toString s#1)
	       else (toString s#0, " ", toString s#1)
	       )
	  else (toString s)
	  )
     else if class s === Option and #s === 2 and class s#0 === Function then (
	  (toString s#0, "(", toString s#1, " => ...)")
	  )
     else if class s === String then (
	  if s#0 === "\"" and s#-1 === "\"" then value s
	  else if substring(s,0,6) === "quote " then substring(s,6)
	  else s)
     else if class s === Symbol then toString s
     else toString s
     )

hr := (o) -> o << "-----------------------------------------------------------------------------" << endl

--briefHelp := (o,s) -> (
--     d := doc s;
--     if d === null 
--     then (
--	  o << formatDocumentTag s << " --> Thing" << endl;		  -- no documentation available
--	  false
--	  )
--     else if class d === List then (
--	  o << formatDocumentTag s << " --> " << d#0 << endl;
--	  d = drop(d,2);
--     	  i := 0;
--     	  while i < #d and d#i =!= PARA do i = i+1;
--	  thereWasMore := i < #d;
--	  d = take(d,i);
--	  if #d > 0 then o << endl << text repl d << endl;
--	  thereWasMore)
--     else (
--	  o << text d << endl;
--	  false))

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
     then o << "No documentation available for '" << formatDocumentTag s << "'." << endl
     else o << "Documentation for " << formatDocumentTag s << ":" << endl << endl << text d << endl;
     )

OS := "operating system"

help = s -> (
     pager := getenv "PAGER";
     if pager === "" 
     or getenv "TERM" === "emacs" 
     then pager = null;
     o := if pager === null then stdio else openOut concatenate("!", pager );
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
     if o =!= stdio then close o;
     )

topics = Command (
     () -> (
	  << columnate(
	       topicList(),
	       if width stdio == 0 then 79 else width stdio - 1) << endl;
	  )
     )

apropos = (pattern) -> (
     mat := "*" | toString pattern | "*";
     sort select( keys symbolTable(), i -> match(i,mat)))

testFileCounter := 0
exprCounter := 0
file := null

fourDigits := i -> (
     s := toString i;
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
     then toList x
     else if basictype x === BasicList or basictype x === Sequence
     then join apply(toSequence x, extractExamples)
     else {})

NameFile := "../tmp/Examples/FileNames"

saveNameHashTable := null
NameHashTable := () -> (
     NameHashTable = () -> saveNameHashTable;
     saveNameHashTable = (
     	  if phase === 4 then hashTable value get NameFile
     	  else (
      	       try new MutableHashTable from hashTable value get NameFile
     	       else new MutableHashTable
	       )
	  )
     )
if phase === 2 then (
     addEndFunction( () -> (
	       stderr << "writing " << NameFile << endl;
	       NameFile << toExternalString pairs NameHashTable() << endl << close;
	       ));
     )

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

processExample := x -> (
     exampleCounter = exampleCounter + 1;
     exampleOutputFile << x << endl;
     if exampleResults#?exampleCounter
     then {x, CODE exampleResults#exampleCounter}
     else (
	  if #exampleResults === exampleCounter then (
	       stderr << "warning : input file " << nodeBaseFilename 
	       << ".out terminates prematurely" << endl;
	       );
	  {x, CODE concatenate("in = ",x)}
	  ))

processExamplesLoop := s -> (
     if class s === EXAMPLE then ExampleTABLE apply(select(toList s, i -> i =!= null), processExample)
     else if class s === Sequence or instance(s,MarkUpList)
     then apply(s,processExamplesLoop)
     else s)

processExamples := (docBody) -> (
     examples := extractExamples docBody;
     if phase > 1 and #examples > 0 then (
	  exampleOutputFile = if phase === 2 then openOut(nodeBaseFilename | ".m2");
	  -- exampleOutputFile << "-- " << nodeName << endl;
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
	  DocDatabase#nodeName = toExternalString docBody;
	  );
     )

document = z -> (
     if class z != List then error "expected a list";
     if #z === 0 then error "expected a nonempty list";
     key := z#0;
     if class key === Option then (
	  -- a key of the form f=>p indicates that we are documenting the option named
	  -- p for the function f
	  fn := key#0;
	  opt := key#1;
	  if not (options fn)#?opt then error("expected ", toString opt, " to be an option of ", toString fn);
	  );
     if class key === Sequence and class lookup key =!= Function then (
	  error("expected a method for ", formatDocumentTag key);
	  );
     if class key === ZZ then error "oops";
     -- if documentableValue key then Symbols#(value key) = key;
     nodeName = formatDocumentTag key;
     Documentation#key = nodeName;
     if substring(nodeName,0,6) === "quote " then Documentation#(substring(nodeName,6)) = nodeName;
     nodeBaseFilename = makeBaseFilename();
     docBody := repl toList apply(1 .. #z - 1, i -> z#i); -- drop isn't defined yet
     docBody = processExamples docBody;
     storeDoc docBody;
     )

SEEALSO = v -> (
     if class v =!= List then v = {v};
     if #v > 0 then (
	  PARA, 
	  "See also ",
	  if #v === 1 then TO v#0
	  else if #v === 2 then {TO v#0, " and ", TO v#1}
	  else mingle(
	       apply(v, i -> TO {i}),
	       append(#v-2 : ", ", ", and ")
	       ),
     	  "."))

RETURNS = s -> (
     "The return value is of type ", TT s, ".  See ", TO s, " for information
     about how to use such values."
     )

Nothing << Thing := (x,y) -> null

printExamples = f -> scan(examples f, i -> << i << endl)
