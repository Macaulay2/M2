--		Copyright 1994-1999 by Daniel R. Grayson

-----------------------------------------------------------------------------
-- configuration
-----------------------------------------------------------------------------
CachePrefix := "cache-tmp"
NameFile := concatenate(CachePrefix,"/Examples/FileNames")
-----------------------------------------------------------------------------
-- initialization and finalization
-----------------------------------------------------------------------------
DocDatabase := null
addEndFunction(() -> (
	  if class DocDatabase === Database then (
	       close DocDatabase;
	       DocDatabase = null;
	       )))

-----------------------------------------------------------------------------
-- formatting document tags
-----------------------------------------------------------------------------
Strings := hashTable { Sequence => "(...)", List => "{...}", Array => "[...]" }
toStr := s -> if Strings#?s then Strings#s else toString s
formatDocumentTag           = method(SingleArgumentDispatch => true)
formatDocumentTag Thing    := s -> toString s
formatDocumentTag Option   := s -> concatenate(toString s#0, "(", toString s#1, " => ...)")
fSeq := new HashTable from {
     (4,NewOfFromMethod) => s -> ("new ", toString s#1, " of ", toString s#2, " from ", toStr s#3),
     (4,cohomology     ) => s -> ("HH_", toStr s#1, "^", toStr s#2, " ", toStr s#3),
     (4,homology       ) => s -> ("HH^", toStr s#1, "_", toStr s#2, " ", toStr s#3),
     (3,NewFromMethod  ) => s -> ("new ", toString s#1, " from ", toStr s#2),
     (3,NewOfMethod    ) => s -> ("new ", toString s#1, " of ", toString s#2),
     (3,symbol " "     ) => s -> (toStr s#1, " ", toStr s#2),
     (3,homology       ) => s -> ("HH_", toStr s#1, " ", toStr s#2),
     (3,cohomology     ) => s -> ("HH^", toStr s#1, " ", toStr s#2),
     (2,homology       ) => s -> ("HH ", toStr s#1),
     (2,cohomology     ) => s -> ("HH ", toStr s#1),
     (2,NewMethod      ) => s -> ("new ", toString s#1),
     (2,symbol ~       ) => s -> (toStr s#1, " ", toStr s#0), -- postfix operator
     (3,class,Symbol   ) => s -> (toStr s#1, " ", toString s#0, " ", toStr s#2),-- infix operator
     (2,class,Symbol   ) => s -> (toString s#0, " ", toStr s#1),-- prefix operator
     (2,class,ScriptedFunctor) => s -> (
	  hh := s#0;
	  if hh.?subscript and hh.?superscript then toString s
	  else if hh.?subscript   then (toString s#0, " _ ", toStr s#1)
	  else if hh.?superscript then (toString s#0, " ^ ", toStr s#1)
	  else toString s),
     (4,class,ScriptedFunctor) => s -> (
	  if s#0 .? subscript
	  then (toString s#0, "_", toStr s#1, "(", toStr s#2, ",", toStr s#3, ")")
	  else (toString s#0, "^", toStr s#1, "(", toStr s#2, ",", toStr s#3, ")")),
     4 => s -> (toString s#0, "(", toStr s#1, ",", toStr s#2, ",", toStr s#3, ")"),
     3 => s -> (toString s#0, "(", toStr s#1, ",", toStr s#2, ")"),
     2 => s -> (toString s#0, " ", toStr s#1)
     }
formatDocumentTag Sequence := s -> concatenate (
     if #s == 0                           then toString
     else if fSeq#?(#s,s#0)               then fSeq#(#s,s#0)
     else if fSeq#?(#s, class, class s#0) then fSeq#(#s,class, class s#0)
     else if fSeq#?#s                     then fSeq##s
					  else toString) s

-----------------------------------------------------------------------------
-- verifying the keys
-----------------------------------------------------------------------------
verifyTag := method(SingleArgumentDispatch => true)
verifyTag Thing    := s -> null
verifyTag Sequence := s -> (
     if class lookup s =!= Function then error("no method installed for '", formatDocumentTag s, "'"))
verifyTag Option   := s -> (
     fn := s#0;
     opt := s#1;
     if not (options fn)#?opt then error("expected ", toString opt, " to be an option of ", toString fn))

-----------------------------------------------------------------------------
-- installing the documentation
-----------------------------------------------------------------------------

Nothing << Thing := (x,y) -> null			    -- turning off the output is easy to do
docExtension := () -> (
     if phase === 2 then "-tmp"		  -- writing, to be renamed -pre externally
     else if phase === 3 then "-pre"	  -- reading
     else if phase === 4 then "-tmp"	  -- writing, to be renamed -doc externally
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
     () -> DocDatabase = (
	  try openDatabase docFilename()
	  else ( stderr << "--warning: couldn't open help file " << docFilename() << endl; new MutableHashTable)))
if phase === 2 or phase === 4 then DocDatabase = openDatabaseOut docFilename()

Documentation = new MutableHashTable
DocumentableValueType := hashTable { 
     Boolean => true, 
     HashTable => true, 
     Function => true, 
     BasicList => true, 
     Nothing => true,
     File => true,
     }
UndocumentableValue := hashTable { symbol environment => true, symbol commandLine => true }
documentableValue := key -> (
     class key === Symbol and value key =!= key
     and not UndocumentableValue#?key and DocumentableValueType#?(basictype value key))
scanPairs(symbolTable(), (name,sym) -> if documentableValue sym then Symbols#(value sym) = sym)

fixup := method(SingleArgumentDispatch => true)
fixup Nothing    := z -> z				    -- null
fixup String     := z -> z				    -- "..."
fixup List       := z -> SEQ apply(z, fixup)		    -- {...} becomes SEQ{...}
fixup Sequence   := z -> SEQ toList apply(z, fixup)	    -- (...) becomes SEQ{...}
fixup MarkUpList := z -> apply(z,fixup)			    -- recursion
fixup TO         := z -> z				    -- TO{x}
fixup MarkUpType := z -> z{}				    -- convert PARA to PARA{}
fixup Thing      := z -> error("unrecognizable item inside documentation: ", toString z)

duplicateDocError := nodeName -> (
     stderr
     << concatenate ("warning: documentation already provided for '", nodeName, "'") 
     << newline << flush;
     )

testFileCounter := 0
exprCounter := 0
file := null
fourDigits := i -> ( i = toString i; concatenate(4-#i:"0", i) )
-----------------------------------------------------------------------------
saveNameHashTable := null
     NameHashTable := () -> if saveNameHashTable =!= null then saveNameHashTable else saveNameHashTable = (
	  if phase === 4 then hashTable value get NameFile
	  else (
	       try  new MutableHashTable from hashTable value get NameFile
	       else new MutableHashTable
	       )
	  )
     addEndFunction( () -> (
	       if phase === 2 then (
		    stderr << "writing " << NameFile << endl;
		    NameFile << toExternalString pairs NameHashTable() << endl << close;);
	       saveNameHashTable = null))
-----------------------------------------------------------------------------
nodeBaseFilename := ""
exampleCounter := 0
exampleOutputFile := null
exampleResults := {}
makeBaseFilename := nodeName -> (
     if (NameHashTable())#?nodeName then (NameHashTable())#nodeName
     else (
	  if phase === 4 then error("internal error: documentation node '", nodeName, "' has no sequence number");
	  (NameHashTable())#nodeName = concatenate( CachePrefix,"/Examples/", fourDigits(#(NameHashTable())))))
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
extractExamples            := method(SingleArgumentDispatch => true)
extractExamples Thing      := x -> {}
extractExamples EXAMPLE    := x -> toList x
extractExamples MarkUpList := x -> join apply(toSequence x, extractExamples)
processExamples := (docBody) -> (
     examples := extractExamples docBody;
     if phase > 1 and #examples > 0 then (
	  exampleOutputFile = if phase === 2 then openOut(nodeBaseFilename | ".m2");
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
     docBody )
storeDocLocally := (nodeName,docBody) -> (
     if Documentation#?nodeName then duplicateDocError nodeName;
     Documentation#nodeName = docBody;
     )
storeDoc := (nodeName,docBody) -> (
     if phase === 0 then storeDocLocally(nodeName,docBody)
     else if phase === 2 or phase === 4 then (
	  if DocDatabase#?nodeName then duplicateDocError nodeName;
	  if mutable DocDatabase then DocDatabase#nodeName = docBody
	  else storeDocLocally(nodeName,docBody)))
document = method()
document List := z -> (
     if #z === 0 then error "expected a nonempty list";
     key := z#0;
     verifyTag key;
     body := drop(z,1);
     skey := toExternalString key;
     nodeName := formatDocumentTag key;
     nodeBaseFilename = makeBaseFilename nodeName;
     if nodeName != key then storeDoc(toExternalString nodeName,"documentation("|skey|")");
     storeDoc(skey,toExternalString processExamples fixup body);
     )

-----------------------------------------------------------------------------
-- getting help from the documentation
-----------------------------------------------------------------------------

topicList = () -> sort join(
     if DocDatabase === null then {} else keys DocDatabase,
     keys Documentation)

getExampleInputs := method()
getExampleInputs Thing        := t -> {}
getExampleInputs ExampleTABLE := t -> apply(toList t, first)
getExampleInputs MarkUpList   := t -> join apply(toSequence t, getExampleInputs)

examples = x -> getExampleInputs documentation x
printExamples = f -> scan(examples f, i -> << i << endl)

topics = Command (
     () -> (
	  wid := if width stdio == 0 then 79 else width stdio - 1;
	  << columnate( topicList(), wid) << endl;
	  )
     )

apropos = (pattern) -> (
     mat := "*" | toString pattern | "*";
     sort select( keys symbolTable(), i -> match(i,mat)))

getDoc := x -> (
     x = formatDocumentTag x;
     value if Documentation#?x then Documentation#x else if DocDatabase#?x then DocDatabase#x )

noBriefDocThings := hashTable { symbol <  => true, symbol >  => true, symbol == => true }
noBriefDocClasses := hashTable { String => true, Option => true, Sequence => true }
briefDocumentation = x -> (
     if noBriefDocClasses#?(class x) or noBriefDocThings#?x then null
     else (
	 d := getDoc x;
	 if d =!= null then (
	      i := 0;
	      while i < #d and class d#i =!= PARA do i = i+1;
	      if i > 0 then << endl << text take(d,i) << endl ) ) )

headline := memoize (
     key -> (
	  d := getDoc key;
	  if d =!= null and class first d === HEADLINE 
	  then SEQ join( {"  --  "}, toList first d ) ) )

items := s -> apply(s, i -> SEQ{ TO formatDocumentTag i, headline i } )
smenu := s -> MENU sort items s
menu := s -> MENU items s

ancestors1 := X -> if X === Thing then {Thing} else prepend(X, ancestors1 parent X)
ancestors := X -> if X === Thing then {} else ancestors1(parent X)

usage := s -> (
     o := getDoc s;
     SEQ { PARA{}, "Usage:", PARA{}, if o === null then "No other documentation found." else o }
     )

vowels := hashTable apply(characters "aeiouAEIOU", i -> i => true)
indefinite := s -> concatenate(if vowels#?(s#0) then "an " else "a ", s)

documentation = method()
documentation(Thing,Thing,Thing) := documentation(Thing,Thing) := s -> null
documentation Thing := s -> if Symbols#?s then documentation Symbols#s
type := s -> SEQ {"Class of ", toString s, " and its ancestors:", menu ancestors1 class s}
documentation Symbol := s -> (
     a := apply(options s, f -> f => s);
     SEQ {
     	  type s,
	  if #a > 0 then SEQ {"Functions with optional argument ", toString s, ":", smenu a},
	  usage s
     	  } )
documentation Type := X -> (
     a := apply(select(pairs typicalValues, (key,Y) -> Y===X), (key,Y) -> key);
     b := select(values symbolTable(), 
	  y -> not mutable y and value y =!= X and instance(value y, Type) and parent value y === X);
     c := apply(
	  select(methods X, key -> not typicalValues#?key or typicalValues#key =!= X),
	  formatDocumentTag);
     d := ancestors X;
     SEQ {
     	  type X,
	  if #d > 0 then SEQ {"Ancestors of ", toString X, ":", menu d},
	  if #b > 0 then SEQ {"Types of ", toString X, ":", smenu b},
	  if #a > 0 then SEQ {"Making ", indefinite toString X, ":", smenu a},
	  if #c > 0 then SEQ {"Handling ", indefinite toString X, "s:", smenu c},
	  usage X
	  })

fmeth := f -> (
     b := methods f;
     if methodFunctionOptions#?f and not methodFunctionOptions#f.SingleArgumentDispatch
     then b = select(b, x -> x =!= (f,Sequence));
     if #b > 0 then SEQ {"Ways to use ",toString f,":", smenu b} )     

optargs := f -> (
     a := apply(keys options f, s -> f => s);
     if #a > 0 then SEQ {"Optional arguments:", smenu a} )

ret := k -> (
     t := (
	  if typicalValues#?k 
	  then typicalValues#k 
	  else if class k === Sequence and typicalValues#?(k#0)
	  then typicalValues#(k#0)
	  else Thing
	  );
     if t =!= Thing then SEQ {"Type of value returned is typically ", TO toString t, ".", PARA{}}
     )

documentation(Function          ) :=  f      -> SEQ { type f, ret f, fmeth f, optargs f, usage f }
documentation(Function,Type     ) := (f,X)   -> SEQ { ret(f,X), optargs f, usage(f,X) }
documentation(Symbol  ,Type,Type) :=
documentation(Function,Type,Type) := (f,X,Y) -> SEQ { ret(f,X,Y), optargs f, usage(f,X,Y) }
documentation(Option) := v -> (
     (fn, opt) -> SEQ { PARA{}, "default: ", toString opt, " => ", toString (options fn)#opt, usage f }
     ) toSequence v

help2 := (o,s) -> (
     d := documentation s;
     if d === null 
     then o << "No documentation available for '" << formatDocumentTag s << "'." << endl
     else o << "Documentation for " << formatDocumentTag s << ":" << endl << endl << text d << endl;
     )

hr := (o) -> o << "-----------------------------------------------------------------------------" << endl

help1 := (o,s) -> (
     if class s === List 
     then scan(s, i -> ( hr o; help2 (o,i); ))
     else ( o << endl; help2 (o,s); );
     o )

help = s -> (
     if getenv "PAGER" === "" or getenv "TERM" === "emacs" 
     then help1(stdio,s)
     else help1(openOut concatenate("!", getenv "PAGER"), s) << close;
     )

-----------------------------------------------------------------------------
-- helper functions useable in documentation
-----------------------------------------------------------------------------

TEST = (e) -> if phase === 2 then (
     testFileCounter = testFileCounter + 1;
     openOut concatenate(CachePrefix,"/Tests/", fourDigits testFileCounter, ".m2") 
     << e << endl << close;
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
