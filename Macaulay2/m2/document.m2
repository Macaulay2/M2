--		Copyright 1994-1999 by Daniel R. Grayson

-----------------------------------------------------------------------------
-- configuration
-----------------------------------------------------------------------------
maximumCodeWidth := 81
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
	  between(pathSeparator,drop(separate(progname,pathSeparator),-2)),
	  pathSeparator, "cache", pathSeparator, "Macaulay2", docExtension()))
if phase === 1 then addStartFunction( 
     () -> DocDatabase = (
	  try openDatabase docFilename()
	  else ( stderr << "--warning: couldn't open help file " << docFilename() << endl; new MutableHashTable)))
if phase === 2 or phase === 4 then DocDatabase = openDatabaseOut docFilename()
Documentation = new MutableHashTable
duplicateDocError := nodeName -> (
     stderr << concatenate ("warning: documentation already provided for '", nodeName, "'") 
     << newline << flush; )
storeDoc := (nodeName,docBody) -> (
     -- note: nodeName and docBody should both be strings which can be evaluated with 'value'.
     -- That usually means making then with 'toExternalString'.
     if mutable DocDatabase then (
	  if DocDatabase#?nodeName then duplicateDocError nodeName;
	  DocDatabase#nodeName = docBody;
	  )
     else (
     	  if Documentation#?nodeName then duplicateDocError nodeName;
     	  Documentation#nodeName = docBody;
	  )
     )

-----------------------------------------------------------------------------
-- getting database records
-----------------------------------------------------------------------------

getRecord := key -> (
     if Documentation#?key then Documentation#key else if DocDatabase#?key then DocDatabase#key
     )

betterStringKey := key -> (
     if class key === String and (
     	  d := getRecord key;
	  d =!= null and substring(d,0,5) === "goto "
	  )
     then substring(d,5)
     else key)

betterKey  := key -> value           betterStringKey toExternalString key
getDoc     := key -> value getRecord betterStringKey toExternalString key
getDocBody := key -> (
     a := getDoc key;
     if a =!= null then select(a, s -> class s =!= Option))

-----------------------------------------------------------------------------
-- formatting document tags
-----------------------------------------------------------------------------
Strings := hashTable { Sequence => "(...)", List => "{...}", Array => "[...]" }
toStr := s -> if Strings#?s then Strings#s else toString s
formatDocumentTag           = method(SingleArgumentDispatch => true)
unformatTag                := new MutableHashTable
unformat                   := s -> if unformatTag#?s then unformatTag#s else s
record                     := f -> x -> (val := f x; unformatTag#val = x; val)
	  
alphabet := set characters "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'"

formatDocumentTag Thing    := s -> toString s
formatDocumentTag Function := record toString
formatDocumentTag Symbol   := record(
     s -> (
	  n := toString s;
	  if value s === s then n
	  else if n === "" or n === " " then concatenate("symbol \"",n,"\"")
	  else if alphabet#?(n#0) then concatenate("symbol ",n)
	  else n
	  )
     )

after := (w,s) -> mingle(w,#w:s)
formatDocumentTag Option   := record(
     s -> concatenate (
	  if class s#0 === Sequence 
	  then (
	       if class s#0#0 === Symbol
	       then ( "(", formatDocumentTag s#0, ", ", toString s#1, " => ...)" )
	       else ( toString s#0#0, "(", after(toString \ drop(s#0,1), ", "), " ", toString s#1, " => ...)" )
	       )
	  else ( toString s#0, "(..., ", toString s#1, " => ...)" )
	  )
     )

fSeqInitialize := (toString,toStr) -> new HashTable from {
     (4,NewOfFromMethod) => s -> ("new ", toString s#1, " of ", toString s#2, " from ", toStr s#3),
     (4,cohomology,ZZ  ) => s -> ("HH_", toStr s#1, "^", toStr s#2, " ", toStr s#3),
     (4,homology,ZZ    ) => s -> ("HH^", toStr s#1, "_", toStr s#2, " ", toStr s#3),
     (3,NewFromMethod  ) => s -> ("new ", toString s#1, " from ", toStr s#2),
     (3,NewOfMethod    ) => s -> ("new ", toString s#1, " of ", toString s#2),
     (3,symbol " "     ) => s -> (toStr s#1, " ", toStr s#2),
     (3,homology,ZZ    ) => s -> ("HH_", toStr s#1, " ", toStr s#2),
     (3,cohomology,ZZ  ) => s -> ("HH^", toStr s#1, " ", toStr s#2),
     (2,homology       ) => s -> ("HH ", toStr s#1),
     (2,cohomology     ) => s -> ("HH ", toStr s#1),
     (2,NewMethod      ) => s -> ("new ", toString s#1),
     (2,symbol ~       ) => s -> (toStr s#1, " ", toStr s#0), -- postfix operator
     (2,symbol !       ) => s -> (toStr s#1, " ", toStr s#0), -- postfix operator
     (3,class,Symbol   ) => s -> (toStr s#1, " ", toString s#0, " ", toStr s#2),-- infix operator
     (2,class,Symbol   ) => s -> (toString s#0, " ", toStr s#1),-- prefix operator
     (2,class,ScriptedFunctor,ZZ) => s -> (
	  hh := s#0;
	  if hh.?subscript and hh.?superscript then toString s
	  else if hh.?subscript   then (toString s#0, " _ ", toStr s#1)
	  else if hh.?superscript then (toString s#0, " ^ ", toStr s#1)
	  else (toString s#0, " ", toStr s#1)),
     (3,class,ScriptedFunctor,ZZ) => s -> (
	  if s#0 .? subscript
	  then (toString s#0, "_", toStr s#1, "(", toStr s#2, ")")
	  else (toString s#0, "^", toStr s#1, "(", toStr s#2, ")")),
     (4,class,ScriptedFunctor,ZZ) => s -> (
	  if s#0 .? subscript
	  then (toString s#0, "_", toStr s#1, "(", toStr s#2, ",", toStr s#3, ")")
	  else (toString s#0, "^", toStr s#1, "(", toStr s#2, ",", toStr s#3, ")")),
     4 => s -> (toString s#0, "(", toStr s#1, ",", toStr s#2, ",", toStr s#3, ")"),
     3 => s -> (toString s#0, "(", toStr s#1, ",", toStr s#2, ")"),
     2 => s -> (toString s#0, " ", toStr s#1)
     }

fSeq := null
formatDocumentTag Sequence := record(
     s -> (
	  if fSeq === null then (
	       fSeq = fSeqInitialize(toString,toStr);
	       );
	  concatenate (
	       if #s == 0                             then toString
	       else if fSeq#?(#s,s#0)                 then fSeq#(#s,s#0)
	       else if #s >= 1 and fSeq#?(#s,s#0,s#1) then fSeq#(#s,s#0,s#1)
	       else if #s >= 1 and fSeq#?(#s, class, class s#0, s#1) 
	       					      then fSeq#(#s, class, class s#0, s#1)
	       else if fSeq#?(#s, class, class s#0)   then fSeq#(#s, class, class s#0)
	       else if fSeq#?#s                       then fSeq#(#s)
						      else toString) s))

formatDocumentTagTO := method(SingleArgumentDispatch => true)
fSeqTO := null
formatDocumentTagTO Sequence := (
     s -> (
	  if fSeqTO === null then (
	       fSeqTO = fSeqInitialize(i -> TO i, i -> TO i);
	       );
	  (
	       if #s == 0                               then toString
	       else if fSeqTO#?(#s,s#0)                 then fSeqTO#(#s,s#0)
	       else if #s >= 1 and fSeqTO#?(#s,s#0,s#1) then fSeqTO#(#s,s#0,s#1)
	       else if #s >= 1 and fSeqTO#?(#s, class, class s#0, s#1) 
	       					        then fSeqTO#(#s, class, class s#0, s#1)
	       else if fSeqTO#?(#s, class, class s#0)   then fSeqTO#(#s, class, class s#0)
	       else if fSeqTO#?#s                       then fSeqTO#(#s)
						        else toString) s))

-----------------------------------------------------------------------------
-- verifying the keys
-----------------------------------------------------------------------------
verifyTag := method(SingleArgumentDispatch => true)
verifyTag Thing    := s -> null
 -- verifyTag Sequence := s -> (
 --      if class lookup s =!= Function then error("no method installed for '", formatDocumentTag s, "'"))
 -- verifyTag Option   := s -> (
 --      fn := s#0;
 --      opt := s#1;
 --      if not (options fn)#?opt then error("expected ", toString opt, " to be an option of ", toString fn))

-----------------------------------------------------------------------------
-- html input
-----------------------------------------------------------------------------

html = method(SingleArgumentDispatch=>true, TypicalValue => String)
text = method(SingleArgumentDispatch=>true, TypicalValue => String)
tex = method(SingleArgumentDispatch=>true, TypicalValue => String)
texMath = method(SingleArgumentDispatch=>true, TypicalValue => String)
mathML = method(SingleArgumentDispatch=>true, TypicalValue => String)

MarkUpList = new Type of BasicList
MarkUpList.synonym = "mark-up list"

     MarkUpType = new Type of Type
MarkUpType.synonym = "mark-up type"
EmptyMarkUpType = new Type of MarkUpType
EmptyMarkUpType.synonym = "empty mark-up type"
     MarkUpType List := (h,y) -> new h from y
EmptyMarkUpType List := (h,y) -> if #y === 0 then new h from y else error "expected empty list"
     MarkUpType Thing := (h,y) -> new h from {y}
     MarkUpType\List := (h,y) -> (i -> h i) \ y
     List/MarkUpType := (y,h) -> y / (i -> h i)
EmptyMarkUpType Thing := (h,y) -> error "expected empty list"

makeList := method()
makeList MarkUpType := X -> toString X
makeList Type       := X -> concatenate("new ", toString X, " from ")
toExternalString MarkUpList := s -> concatenate(makeList class s, toExternalString toList s)
toString         MarkUpList := s -> concatenate(makeList class s, toString         toList s)

htmlMarkUpType := s -> (
     on := "<" | s | ">";
     off := "</" | s | ">";
     t -> concatenate(on, apply(t,html), off))

MarkUpType.GlobalAssignHook = (X,x) -> (
     if not x.?name then (
	  x.Symbol = X;
	  x.name = string X;
     	  html x := htmlMarkUpType string X;
	  );
     )

new MarkUpType := theMarkUpType -> new theMarkUpType of MarkUpList

BR         = new EmptyMarkUpType
NOINDENT   = new EmptyMarkUpType
HR         = new EmptyMarkUpType
PARA       = new MarkUpType
EXAMPLE    = new MarkUpType
TABLE      = new MarkUpType
ExampleTABLE = new MarkUpType
PRE        = new MarkUpType
TITLE      = new MarkUpType
HEAD       = new MarkUpType
BODY       = new MarkUpType
IMG	   = new MarkUpType
HTML       = new MarkUpType
CENTER     = new MarkUpType
H1         = new MarkUpType
H2         = new MarkUpType
BIG        = new MarkUpType
H3         = new MarkUpType
H4         = new MarkUpType
H5         = new MarkUpType
H6         = new MarkUpType
LISTING    = new MarkUpType
LITERAL    = new MarkUpType
XMP        = new MarkUpType
BLOCKQUOTE = new MarkUpType
VAR        = new MarkUpType
DFN        = new MarkUpType
STRONG     = new MarkUpType
BIG        = new MarkUpType
SMALL      = new MarkUpType
SAMP       = new MarkUpType
KBD        = new MarkUpType
SUB        = new MarkUpType
SUP        = new MarkUpType
ITALIC     = new MarkUpType
UNDERLINE  = new MarkUpType
TEX	   = new MarkUpType
SEQ	   = new MarkUpType
new SEQ from List := (SEQ,v) -> select (splice apply(v,
	  i -> if class i === SEQ then toSequence i
	  else if class i === List then toSequence SEQ i
	  else i ),
     j -> j =!= null)
TT         = new MarkUpType
EM         = new MarkUpType
CITE       = new MarkUpType
BOLD       = new MarkUpType
CODE       = new MarkUpType
HREF       = new MarkUpType
ANCHOR     = new MarkUpType
SHIELD     = new MarkUpType
MENU       = new MarkUpType
UL         = new MarkUpType
OL         = new MarkUpType
NL         = new MarkUpType
DL 	   = new MarkUpType
TO         = new MarkUpType
TOH        = new MarkUpType

MarkUpList ^ MarkUpList := (x,y) -> SEQ{x,SUP y}
MarkUpList _ MarkUpList := (x,y) -> SEQ{x,SUB y}

-----------------------------------------------------------------------------
-- installing the documentation
-----------------------------------------------------------------------------

Nothing << Thing := (x,y) -> null			    -- turning off the output is easy to do
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
fixup Option     := z -> z#0 => fixup z#1		    -- Headline => "...", Usage => "..."
fixup TO         := z -> z				    -- TO{x}
fixup TOH        := z -> z				    -- TOH{x}
fixup MarkUpType := z -> z{}				    -- convert PARA to PARA{}
fixup Thing      := z -> error("unrecognizable item inside documentation: ", toString z)

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
exampleResultsFound := false
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
	  if exampleResultsFound and #exampleResults === exampleCounter then (
	       stderr << "warning : input file " << nodeBaseFilename 
	       << ".out terminates prematurely" << endl;
	       );
	  {x, CODE concatenate("in = ",x)}
	  ))
processExamplesLoop := s -> (
     if class s === EXAMPLE then {
	  PARA{},
	  ExampleTABLE apply(select(toList s, i -> i =!= null), processExample),
	  PARA{}}
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
	  exampleResults = try (
	       exampleResultsFound = true;
	       get (nodeBaseFilename | ".out")
	       ) else (
	       exampleResultsFound = false;
	       if phase === 4 or phase === 5 then (
		    stderr << "warning : can't open input file '" 
	       	    << nodeBaseFilename << ".out'" << endl;
		    );
	       ""
	       );
	  exampleResults = separate(exampleResults,"\1");
	  exampleCounter = 0;
	  docBody = apply(docBody,processExamplesLoop);
	  close exampleOutputFile;
	  );
     docBody )
document = method()
document List := z -> (
     if #z === 0 then error "expected a nonempty list";
     key := z#0;
     verifyTag key;
     body := drop(z,1);
     skey := toExternalString key;
     nodeName := formatDocumentTag key;
     nodeBaseFilename = makeBaseFilename nodeName;
     if nodeName =!= key then storeDoc(toExternalString nodeName,"goto "|skey);
     storeDoc(skey,toExternalString processExamples fixup body);
     )

-----------------------------------------------------------------------------
-- getting help from the documentation
-----------------------------------------------------------------------------

topicList = () -> sort join(
     if DocDatabase === null then {} else value \ keys DocDatabase,
     keys Documentation)

getExampleInputs := method(SingleArgumentDispatch => true)
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
-----------------------------------------------------------------------------
-- more general methods
-----------------------------------------------------------------------------
lookupQ := s -> (youngest s)#?s
nextMoreGeneral2 := (f,A) -> (
     A' := A;
     l := false;
     while not l and A' =!= Thing do (
	  A' = parent A';
	  l = lookupQ(f,A');
	  );
     if l then (f,A'))
nextMoreGeneral3 := (f,A,B) -> (
     A' := A;
     B' := B;
     l := false;
     while not l and (A',B') =!= (Thing,Thing) do (
	  if B' =!= Thing then B' = parent B' else (
	       B' = B;
	       A' = parent A');
	  l = lookupQ(f,A',B'););
     if l then (f,A',B'))
nextMoreGeneral4 := (f,A,B,C) -> (
     A' := A;
     B' := B;
     C' := C;
     l := false;
     while not l and (A',B',C') =!= (Thing,Thing,Thing) do (
	  if C' =!= Thing then C' = parent C'
	  else (
	       C' = C;
	       if B' =!= Thing then B' = parent B'
	       else (
		    B' = B;
		    A' = parent A'));
	  l = lookupQ(f,A',B',C');
	  );
     if l then (f,A',B',C'))
nextMoreGeneral := s -> (
     if class s === Sequence then (
     	  if #s === 2 then nextMoreGeneral2 s else
     	  if #s === 3 then nextMoreGeneral3 s else
     	  if #s === 4 then nextMoreGeneral4 s)
     else if class s === Option and #s === 2 then s#1
     )

getOption := (s,tag) -> (
     if class s === SEQ then (
     	  x := select(1, toList s, i -> class i === Option and #i === 2 and first i === tag);
     	  if #x > 0 then x#0#1))

getHeadline := key -> (
     d := getOption(getDoc key, Headline);
     if d =!= null then SEQ join( {"  --  ", SEQ d} )
     )

getUsage := key -> (
     x := getOption(getDoc key, Usage);
     if x =!= null then SEQ x)

getSynopsis := key -> getOption(getDoc key, Synopsis)

evenMoreGeneral := key -> (
     t := nextMoreGeneral key;
     if t === null and class key === Sequence then key#0 else t)
headline = memoize (
     key -> (
	  key = unformat key;
	  while ( d := getHeadline key ) === null and ( key = evenMoreGeneral key ) =!= null do null;
	  d))

moreGeneral := s -> (
     n := nextMoreGeneral s;
     if n =!= null then SEQ { "Next more general method: ", TO n, headline n, PARA{} }
     )
-----------------------------------------------------------------------------

optTO := i -> (
     if getDoc i =!= null then SEQ{ TO i, headline i }
     else if class i === Sequence then SEQ formatDocumentTagTO i
     else TT formatDocumentTag i
     )

smenu := s -> MENU (optTO \ last \ sort apply(s , i -> {formatDocumentTag i, i}) )
 menu := s -> MENU (optTO \ s)

ancestors1 := X -> if X === Thing then {Thing} else prepend(X, ancestors1 parent X)
ancestors := X -> if X === Thing then {} else ancestors1(parent X)

vowels := set characters "aeiouAEIOU"
indefinite := s -> concatenate(if vowels#?(s#0) and not match(s,"one *") then "an " else "a ", s)

synonym := X -> if X.?synonym then X.synonym else "object of class " | toString X

synonymAndClass := X -> (
     if X.?synonym
     then SEQ {indefinite X.synonym, " (of class ", TO X, ")"}
     else SEQ {"an object of class ", TO X}
     )     

justClass := X -> SEQ {"an instance of class ", TO X}

usage := s -> (
     o := getDocBody s;
     if o === null and class s === Symbol then o = getDoc toString s;
     if o =!= null then SEQ {o, PARA{}}
     )

title := s -> SEQ { CENTER { BIG formatDocumentTag s, headline s }, PARA{} }

inlineMenu := x -> between(", ", TO \ x)

type := s -> (
     X := class s;
     SEQ {
	  if class s =!= Thing 
	  and class s =!= Symbol
	  and class s =!= Function
	  then SEQ {
	       "The ",
	       if X.?synonym then X.synonym else "object",
	       " ", toString s, " is a member of the class ", TO class s, ".\n"
	       },
	  if instance(s,Type) then (
	       SEQ {
		    if s.?synonym then SEQ {
			 "Each object of class ", toString s, " is called ", indefinite s.synonym, ".\n"
			 },
		    if parent s =!= Thing then SEQ {
			 "Each ", synonym s, " is also a member of class ", TO parent s, ".",
			 PARA{},
			 "More general types (whose methods may also apply) :",
			 SHIELD MENU (
			      Y := parent s;
			      while Y =!= Thing list TO toString Y do Y = parent Y
			      )
			 }
		    }
	       ),
	  PARA{}
	  }
     )

optargs := method(SingleArgumentDispatch => true)

optargs Thing := x -> null
     
optargs Function := f -> (
     o := options f;
     if o =!= null then SEQ {
	  "Optional arguments :", PARA{},
	  SHIELD smenu apply(keys o, t -> f => t)})

optargs Sequence := s -> (
     o := options s;
     if o =!= null then SEQ {
	  "Optional arguments :", PARA{}, 
	  SHIELD smenu apply(keys o, t -> s => t)}
     else optargs s#0)

synopsis := method(SingleArgumentDispatch => true)
synopsis Thing := synopsis Function := f -> (
     SYN := getSynopsis f;
     if SYN =!= null then (
	  t := i -> if SYN#?(i+1) then (
	       if class SYN#i === Option 
	       then SEQ { TT SYN#i#0, if SYN#i#1 =!= null then SEQ {": ", SEQ SYN#i#1} }
	       else SEQ SYN#i
	       );
	  SEQ {
	       BOLD "Synopsis:",
	       SHIELD MENU {
		    if SYN#?0 then SEQ { "Usage: ", TT SYN#0},
		    if SYN#?1 then SEQ { "Input:", MENU { t 1, t 2, t 3 } },
		    if SYN#?1 and SYN#-1 =!= null then SEQ { "Output:", MENU { t(-1) } }
		    }
	       }
	  )
     )

typicalValue := k -> (
     if typicalValues#?k then typicalValues#k 
     else if class k === Sequence and typicalValues#?(k#0) then typicalValues#(k#0)
     else Thing
     )

synopsis Sequence := s -> (
     t := typicalValue s;
     desc1 := desc2 := desc3 := descv := ".";
     retv := arg1 := arg2 := arg3 := null;
     SYN := getSynopsis s;
     d := x -> if x === null then "." else SEQ { ": ", SEQ x };
     if SYN =!= null then (
	  if SYN#?1 and s#?1 then (
	       if class SYN#1 === Option then (arg1 = TT SYN#1#0; desc1 = d SYN#1#1;)
	       else desc1 = d SYN#1;
	  if SYN#?2 and s#?2 then (
	       if class SYN#2 === Option then (arg2 = TT SYN#2#0; desc2 = d SYN#2#1;)
	       else desc2 = d SYN#2);
	  if SYN#?3 and s#?3 then (
	       if class SYN#3 === Option then (arg3 = TT SYN#3#0; desc3 = d SYN#3#1;)
	       else desc3 = d SYN#3);
     	  if SYN#?-1 then (
	       if class SYN#-1 === Option then (retv = TT SYN#-1#0; descv = d SYN#-1#1;)
	       else descv = d SYN#-1);
	       );
	  );
     SEQ {
	  BOLD "Synopsis:",
	  SHIELD MENU {
	       if SYN#?0 then SEQ{ "Usage: ", TT SYN#0},
	       SEQ { if class s#0 === Function then "Function: " else "Operator: ", TO s#0, headline s#0 },
	       SEQ { "Input:",
		    MENU {
			 if arg1 === null
			 then SEQ {justClass s#1, desc1 }
			 else SEQ {arg1, ", ", justClass s#1, desc1 }, 
			 if #s > 2 then
			 if arg2 === null
			 then SEQ {justClass s#2, desc2 } 
			 else SEQ {arg2, ", ", justClass s#2, desc2 }, 
			 if #s > 3 then
			 if arg3 === null
			 then SEQ {justClass s#3, desc3 }
			 else SEQ {arg3, ", ", justClass s#3, desc3 }
			 }
		    },
	       if t =!= Thing or retv =!= null or descv =!= "." 
	       then SEQ {
	       	    "Output:",
	       	    MENU {
		    	 if retv === null
		    	 then SEQ {justClass t, descv}
		    	 else SEQ {retv, ", ", justClass t  , descv}
		    	 }
		    },
	       optargs s,
	       moreGeneral s
     	       }
	  }
     )

unDocumentable := method(SingleArgumentDispatch => true)
unDocumentable Thing := x ->false
unDocumentable Function := f -> class f === Function and match(toString f, "--Function*--")
unDocumentable Sequence := s -> #s > 0 and unDocumentable s#0

documentableMethods := s -> select(methods s, i -> not unDocumentable i)

fmeth := f -> (
     b := documentableMethods f;
     if methodFunctionOptions#?f and not methodFunctionOptions#f.SingleArgumentDispatch
     then b = select(b, x -> x =!= (f,Sequence));
     if #b > 0 then SEQ {"Ways to use ", TT toString f," :", BR{}, SHIELD smenu b} )

noBriefDocThings := hashTable { symbol <  => true, symbol >  => true, symbol == => true }
noBriefDocClasses := hashTable { String => true, Option => true, Sequence => true }
briefDocumentation = x -> (
     if noBriefDocClasses#?(class x) or noBriefDocThings#?x then null
     else (
	  r := getUsage x;
	  if r =!= null then << endl << text r << endl
	  else (
	       r = synopsis x;
	       if r =!= null then << endl << text r << endl
	       else (
		    if headline x =!= null then << endl << headline x << endl;
		    if class x === Function then (
			 s := fmeth x;
		    	 if s =!= null then << endl << text s << endl;)))))

documentation = method(SingleArgumentDispatch => true)
documentation String := s -> (
     if unformatTag#?s then documentation unformatTag#s
     else (
	  key := betterKey s;
	  if key =!= s then documentation key 
	  else SEQ { title s, getDocBody s }
	  )
     )
documentation Thing := s -> SEQ { title s, usage s, type s }
binary := set binaryOperators; erase symbol binaryOperators
prefix := set prefixOperators; erase symbol prefixOperators
postfix := set postfixOperators; erase symbol postfixOperators
other := set otherOperators; erase symbol otherOperators
operatorSet = binary + prefix + postfix + other
op := s -> if operatorSet#?s then (
     ss := toString s;
     SEQ {
	  if binary#?s then SEQ {
	       NOINDENT{}, 
	       "This operator may be used as a binary operator in an expression \n",
	       "like ", TT ("x "|ss|" y"), ".  The user may install ", TO {"binary method", "s"}, " \n",
	       "for handling such expressions with code such as ",
	       if ss == " "
	       then PRE ("         X Y := (x,y) -> ...")
	       else PRE ("         X "|ss|" Y := (x,y) -> ..."), 
	       NOINDENT{},
	       "where ", TT "X", " is the class of ", TT "x", " and ", TT "Y", " is the \n",
	       "class of ", TT "y", ".", PARA{}
	       },
	  if prefix#?s then SEQ {
	       NOINDENT{}, 
	       "This operator may be used as a prefix unary operator in an expression \n",
	       "like ", TT (ss|" y"), ".  The user may install a method for handling \n",
	       "such expressions with code such as \n",
	       PRE ("           "|ss|" Y := (y) -> ..."),
	       NOINDENT{},
	       "where ", TT "Y", " is the class of ", TT "y", ".", PARA{}
	       },
	  if postfix#?s then SEQ {
	       NOINDENT{}, 
	       "This operator may be used as a postfix unary operator in an expression \n",
	       "like ", TT ("x "|ss), ".  The user may install a method for handling \n",
	       "such expressions with code such as \n",
	       PRE ("         X "|ss|"   := (x,y) -> ..."),
	       NOINDENT{}, "where ", TT "X", " is the class of ", TT "x", ".", PARA{}
	       },
	  }
     )

optionFor := s -> unique select( value \ values symbolTable(), f -> class f === Function and (options f)#?s)

documentation Symbol := s -> (
     a := apply(select(optionFor s,f -> not unDocumentable f), f -> f => s);
     b := documentableMethods s;
     SEQ {
	  title s, 
	  usage s,
	  op s,
     	  type s,
	  if #a > 0 then SEQ {"Functions with optional argument named ", toString s, " :", PARA{}, SHIELD smenu a, PARA{}},
	  if #b > 0 then SEQ {"Methods for ", toString s, " :", PARA{}, SHIELD smenu b, PARA{}} 
     	  }
     )

documentation Type := X -> (
     syms := values symbolTable();
     a := apply(select(pairs typicalValues, (key,Y) -> Y===X and not unDocumentable key), (key,Y) -> key);
     b := toString \ select(syms, 
	  y -> not mutable y and value y =!= X and instance(value y, Type) and parent value y === X);
     c := select(documentableMethods X, key -> not typicalValues#?key or typicalValues#key =!= X);
     e := toString \ select(syms, y -> not mutable y and class value y === X);
     SEQ {
	  title X, 
	  synopsis X,
     	  type X,
	  if #b > 0 then SEQ {
	       "Types of ", if X.?synonym then X.synonym else toString X, " :", PARA{},
	       smenu b, PARA{}},
	  usage X,
	  if #a > 0 then SEQ {
	       "Functions and methods returning a ",
	       indefinite synonym X, " :", PARA{},
	       smenu a, PARA{}
	       },
	  if #c > 0 then SEQ {"Methods for using ", indefinite synonym X, " :", PARA{}, smenu c, PARA{}},
	  if #e > 0 then SEQ {"Fixed objects of class ", toString X, " :", PARA{}, SHIELD smenu e, PARA{}},
	  })

documentation HashTable := x -> (
     c := documentableMethods x;
     SEQ {
	  title x,
	  synopsis x,
	  usage x,
     	  type x,
	  if #c > 0 then SEQ {"Functions installed in ", toString x, " :", PARA{}, SHIELD smenu c, PARA{}},
	  })

ret := k -> (
     t := typicalValue k;
     if t =!= Thing then SEQ {"Class of returned value: ", TO t, headline t}
     )
seecode := x -> (
     f := lookup x;
     n := code f;
     if n =!= null 
     and height n + depth n <= 10 and width n <= maximumCodeWidth
     then SEQ { "Code:", PRE concatenate between(newline,netRows n) }
     )

documentation Function := f -> SEQ { 
     title f, synopsis f, usage f, type f, ret f, fmeth f, optargs f, seecode f 
     }

documentation Option := v -> (
     (fn, opt) -> (
	  SEQ { 
	       title v,
	       synopsis v,
	       usage v,
	       BOLD "See also:",
	       SHIELD MENU {
		    SEQ{ "Default value: ",
			 if class (options fn)#opt =!= ZZ
			 then TOH toString (options fn)#opt 
			 else     toString (options fn)#opt 
			 },
		    SEQ{ if class fn === Sequence then "Method: " else "Function: ", TOH fn },
		    SEQ{ "Option name: ", TOH opt }
		    }
	       }
	  )
     ) toSequence v

documentation Sequence := s -> (
     if null === lookup s then error("expected ", toString s, " to be a method");
     SEQ {
	  title s, 
	  synopsis s,
	  usage s,
	  seecode s
	  }
     )

    hr1 := newline | "-----------------------------------------------------------------------------" | newline
 -- hr1 := "-----------------------------------------------------------------------------"

    hr := v -> concatenate mingle(#v + 1 : hr1 , v)
 -- hr := v -> stack       mingle(#v + 1 : hr1 , v)

help = method(SingleArgumentDispatch => true)
help List := v -> hr apply(v, help)
help Thing := s -> (
     d := documentation s;
     if d === null 
     then "No documentation available for '" |formatDocumentTag s | "'."
     else "Documentation for " | toExternalString formatDocumentTag s | " :" |newline| text d
 --  else "Documentation for " | formatDocumentTag s | " :" || "  " | net d
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
     if #v > 0 then SEQ { PARA{}, BOLD "See also:", SHIELD MENU (TO \ v) })

CAVEAT = v -> SEQ { PARA{}, BOLD "Caveat:", MENU { SEQ v } }

-----------------------------------------------------------------------------
-- html output
-----------------------------------------------------------------------------
htmlLiteralTable := new MutableHashTable
scan(characters ascii(0 .. 255), c -> htmlLiteralTable#c = c)
htmlLiteralTable#"\"" = "&quot;"
htmlLiteralTable#"<" = "&lt;"
htmlLiteralTable#"&" = "&amp;"
htmlLiteralTable#">" = "&gt;"
htmlLiteral := s -> concatenate apply(characters s, c -> htmlLiteralTable#c)

htmlExtraLiteralTable := copy htmlLiteralTable
htmlExtraLiteralTable#" " = "&nbsp;"
htmlExtraLiteral := s -> concatenate apply(characters s, c -> htmlExtraLiteralTable#c)
-----------------------------------------------------------------------------
ttLiteralTable := new MutableHashTable
scan(0 .. 255, c -> ttLiteralTable#(ascii{c}) = concatenate(///{\char ///, string c, "}"))
scan(characters ascii(32 .. 126), c -> ttLiteralTable#c = c)
-- scan(characters "\\{}$&#^_%~", c -> ttLiteralTable#c = concatenate("{\\char ", string (ascii c)#0, "}"))
-- scan(characters "$%&#_", c -> ttLiteralTable#c = concatenate("\\",c))

cmrLiteralTable := copy ttLiteralTable

ttBreak :=
///
\leavevmode\hss\endgraf
///

(
if #newline === 1 
then ttLiteralTable#newline = ttBreak 
else if #newline === 2 then (
     ttLiteralTable#(newline#0) = "";
     ttLiteralTable#(newline#1) = ttBreak;
     )
)

ttLiteralTable#" " = ///\ ///
ttLiteralTable#"\t" = "\t"
ttLiteralTable#"`" = "{`}"     -- break ligatures ?` and !` in font \tt
                               -- see page 381 of TeX Book
ttLiteral := s -> concatenate apply(characters s, c -> ttLiteralTable#c)
-----------------------------------------------------------------------------
cmrLiteralTable#"\n" = "\n"
cmrLiteralTable#"\r" = "\r"
cmrLiteralTable#"\t" = "\t"
cmrLiteralTable#"\\" = "{\\tt \\char`\\\\}"
cmrLiteralTable# "<" = "{\\tt \\char`\\<}"
cmrLiteralTable# ">" = "{\\tt \\char`\\>}"
cmrLiteralTable# "|" = "{\\tt \\char`\\|}"
cmrLiteralTable# "{" = "{\\tt \\char`\\{}"
cmrLiteralTable# "}" = "{\\tt \\char`\\}}"
cmrLiteral := s -> concatenate apply(characters s, c -> cmrLiteralTable#c)
-----------------------------------------------------------------------------

html String := htmlLiteral
mathML String := htmlLiteral
tex String := cmrLiteral
texMath String := s -> (
     if #s === 1 then s
     else concatenate("\\text{", cmrLiteral s, "}")
     )
text String := identity

text Thing := toString

texMath List := x -> concatenate("\\{", between(",", apply(x,texMath)), "\\}")
texMath Array := x -> concatenate("[", between(",", apply(x,texMath)), "]")
texMath Sequence := x -> concatenate("(", between(",", apply(x,texMath)), ")")

texMath HashTable := x -> if x.?texMath then x.texMath else texMath expression x

tex HashTable := x -> (
     if x.?tex then x.tex 
     else if x.?texMath then concatenate("$",x.texMath,"$")
     else if x.?name then x.name
     else tex expression x
     )

mathML Nothing := texMath Nothing := tex Nothing := html Nothing := text Nothing := x -> ""
mathML Symbol := x -> concatenate("<ci>",string x,"</ci>")

tex Function := x -> "--Function--"

tex Boolean := tex Symbol := 
text Symbol := text Boolean := 
html Symbol := html Boolean := string

texMath Function := texMath Boolean := x -> "\\text{" | tex x | "}"

html MarkUpList := x -> concatenate apply(x,html)
text MarkUpList := x -> concatenate apply(x,text)
tex MarkUpList := x -> concatenate apply(x,tex)
net MarkUpList := x -> peek x
texMath MarkUpList := x -> concatenate apply(x,texMath)
mathML MarkUpList := x -> concatenate apply(x,mathML)

--html MarkUpType := H -> html H{}
--text MarkUpType := H -> text H{}
--tex MarkUpType := H -> tex H{}
--net MarkUpType := H -> net H{}
--texMath MarkUpType := H -> tex H{}

html BR := x -> ///
<BR>
///
text BR := x -> ///
///
tex  BR := x -> ///
\hfil\break
///

html NOINDENT := x -> ""
net NOINDENT := x -> ""
text NOINDENT := x -> ""
tex  NOINDENT := x -> ///
\noindent\ignorespaces
///

html BIG := x -> concatenate( "<B><FONT SIZE=18>", apply(x, html), "</FONT></B>" )

html HEAD := html CENTER := 
x -> concatenate(newline, 
     "<", toString class x, ">", newline,
     apply(x, html), newline,
     "</", toString class x, ">", newline
     )

html TITLE := 
x -> concatenate(newline, 
     "<", toString class x, ">", apply(x, html), "</", toString class x, ">", newline
     )

html HR := x -> ///
<HR>
///
text HR := x -> ///
-----------------------------------------------------------------------------
///
tex  HR := x -> ///
\hfill\break
\hbox to\hsize{\leaders\hrule\hfill}
///

html PARA := x -> (
     if #x === 0 
     then ///
<P>
///
     else concatenate(///
<P>
///,
          apply(x,html),
          ///
</P>
///
          )
     )

tex PARA := x -> concatenate(///
\par
///,
     apply(x,tex))

text PARA := x -> concatenate(newline, newline, apply(x,text))

text EXAMPLE := x -> concatenate apply(x,i -> text PRE i)
html EXAMPLE := x -> concatenate html ExampleTABLE apply(toList x, x -> {x, CODE concatenate("in = ",x)})

text TABLE := x -> concatenate(newline, newline, apply(x, row -> (row/text, newline))) -- not good yet
text ExampleTABLE := x -> concatenate(newline, newline, apply(x, y -> (text y#1, newline)))
net ExampleTABLE := x -> "    " | stack between("",apply(toList x, y -> "" | net y#1 || ""))

tex TABLE := x -> concatenate applyTable(x,tex)
texMath TABLE := x -> concatenate (
     ///
\matrix{
///,
     apply(x,
	  row -> (
	       apply(row,item -> (texMath item, "&")),
	       ///\cr
///
	       )
	  ),
     ///}
///
     )

tex ExampleTABLE := x -> concatenate apply(x,y -> tex y#1)

html TABLE := x -> concatenate(
     newline,
     "<TABLE>",
     newline,
     apply(x, row -> ( 
	       "  <TR>",
	       newline,
	       apply(row, item -> ("    <TD ALIGN=CENTER>", html item, "</TD>",newline)),
	       "  </TR>",
	       newline)),
     "</TABLE>",
     newline
     )			 

html ExampleTABLE := x -> concatenate(
     newline,
     "<P>",
     "<CENTER>",
     "<TABLE cellspacing='0' cellpadding='12' border='4' bgcolor='#80ffff' width='100%'>",
     newline,
     apply(x, 
	  item -> (
	       "  <TR>", newline,
	       "    <TD NOWRAP>", html item#1, "</TD>", newline,
	       "  </TR>", newline
	       )
	  ),
     "</TABLE>",
     "</CENTER>",
     "</P>"
     )			 

net PRE := x -> net concatenate x
text PRE   := x -> concatenate(
     newline,
     demark(newline,
	  apply(lines concatenate x, s -> concatenate("     ",s))),
     newline
     )
html PRE   := x -> concatenate( 
     "<PRE>", 
     html demark(newline,
	  apply(lines concatenate x, s -> concatenate("     ",s))),
     "</PRE>"
     )

shorten := s -> (
     while #s > 0 and s#-1 == "" do s = drop(s,-1);
     while #s > 0 and s#0 == "" do s = drop(s,1);
     s)
tex PRE := x -> concatenate (
     ///\par
\vskip 4 pt
{%
     \tt
     \baselineskip=9.5pt
///,
     between(newline, 
	  shorten lines concatenate x
	  / (line ->
	       if #line <= maximumCodeWidth then line
	       else concatenate(substring(line,0,maximumCodeWidth - 4), " ..."))
	  / ttLiteral
	  / (line -> if line === "" then ///\penalty-500/// else line)
	  / (line -> (line,///\leavevmode\hss\endgraf///))
	  ),
     ///
     }
\par
\noindent
///
     )

html BODY := x -> concatenate(
     "<BODY bgcolor='#e4e4ff'>",
     newline,
     apply(x, html),
     newline,
     "</BODY>",
     newline
     )

html IMG  := x -> "<IMG src=\"" | x#0 | "\">"
text IMG  := x -> ""
tex  IMG  := x -> ""

html LISTING := t -> "<LISTING>" | concatenate toSequence t | "</LISTING>";

texMath STRONG := tex STRONG := x -> concatenate("{\\bf ",apply(x,tex),"}")

texMath ITALIC := tex ITALIC := x -> concatenate("{\\sl ",apply(x,tex),"}")
html ITALIC := x -> concatenate("<I>",apply(x,html),"</I>")

texMath TEX := tex TEX := x -> concatenate toList x

texMath SEQ := tex SEQ := x -> concatenate(apply(x, tex))
text SEQ := x -> concatenate(apply(x, text))
html SEQ := x -> concatenate(apply(x, html))
net SEQ := x -> (
     x = toList x;
     p := join({-1},positions(x,i -> class i === PARA or class i === BR),{#x});
     stack apply(#p - 1, 
	  i -> horizontalJoin apply(take(x,{p#i+1, p#(i+1)-1}), net)
	  )
     )

tex Sequence := tex List := tex Array := x -> concatenate("$",texMath x,"$")

text Sequence := x -> concatenate("(", between(",", apply(x,text)), ")")
text List := x -> concatenate("{", between(",", apply(x,text)), "}")

html Sequence := x -> concatenate("(", between(",", apply(x,html)), ")")
html List := x -> concatenate("{", between(",", apply(x,html)), "}")

tex     TT := x -> concatenate   (///{\tt {}///, ttLiteral concatenate (tex     \ toList x), "}")
texMath TT := x -> concatenate   (///{\tt {}///, ttLiteral concatenate (texMath \ toList x), "}")
text    TT := x -> concatenate   ("'", text \ toList x, "'")
net     TT := x -> horizontalJoin splice ("'", net  \ toSequence x, "'")

net CODE := x -> stack lines concatenate x

html CODE   := x -> concatenate( 
     "<CODE>", 
     demark( ("<BR>",newline), apply(lines concatenate x, htmlExtraLiteral) ),
     "</CODE>"
     )

html HREF := x -> (
     "<A HREF=\"" | x#0 | "\">" | html x#-1 | "</A>"
     )
text HREF := x -> "\"" | x#-1 | "\""
tex HREF := x -> (
     concatenate(
	  ///\special{html:<A href="///, ttLiteral x#0, ///">}///,
	  tex x#-1,
	  ///\special{html:</A>}///
	  )
     )

html ANCHOR := x -> (
     "<A name=\"" | x#0 | "\">" | html x#-1 | "</A>"
     )
text ANCHOR := x -> "\"" | x#-1 | "\""
tex ANCHOR := x -> (
     concatenate(
	  ///\special{html:<A name="///, ttLiteral x#0, ///">}///,
	  tex x#-1,
	  ///\special{html:</A>}///
	  )
     )

html SHIELD := x -> concatenate apply(x,html)
text SHIELD := x -> concatenate apply(x,text)
net SHIELD := x -> horizontalJoin apply(x,net)

html TEX := x -> x#0

addHeadlines := x -> apply(x, i -> if instance(i,TO) then SEQ{ i, headline i#0 } else i)

html MENU := x -> concatenate (
     newline,
     "<MENU>", newline,
     apply(addHeadlines x, s -> if s =!= null then ("<LI>", html s, newline)),
     "</MENU>", newline, 
     "<P>", newline)

addHeadlines1 := x -> apply(x, i -> if instance(i,TO) then SEQ{ "help ", i, headline i#0 } else i)

text MENU := x -> concatenate(
     newline,
     apply(addHeadlines1 x, s -> if s =!= null then ("    ", text s, newline))
     )

net MENU := x -> "    " | stack apply(toList addHeadlines x, net)

tex MENU := x -> concatenate(
     ///
\begingroup\parindent=40pt
///,
     apply(addHeadlines x, x -> if x =!= null then ( ///\item {$\bullet$}///, tex x, newline)),
     "\\endgroup", newline, newline)


html UL   := x -> concatenate(
     "<UL>", newline,
     apply(x, s -> ("<LI>", html s, newline)),
     "</UL>", newline, 
     "<P>", newline)

text UL   := x -> concatenate(
     newline,
     apply(x, s -> ("    ", text s, newline)))

html OL   := x -> concatenate(
     "<OL>", newline,
     apply(x,s -> ("<LI>", html s, newline)),
     "</OL>", newline, 
     "<P>", newline
     )
text OL   := x -> concatenate(
     newline,
     apply(x,s -> ("    ", text s, newline)))

html NL   := x -> concatenate(
     "<NL>", newline,
     apply(x, s -> ("<LI>", html s, newline)),
     "</NL>", newline, 
     "<P>", newline)
text NL   := x -> concatenate(
     newline,
     apply(x,s -> ("    ",text s, newline)))

html DL   := x -> (
     "<DL>" 
     | concatenate apply(x, p -> (
	       if class p === List or class p === Sequence then (
		    if # p === 2 then "<DT>" | html p#0 | "<DD>" | html p#1
		    else if # p === 1 then "<DT>" | html p#0
		    else error "expected a list of length 1 or 2"
		    )
	       else "<DT>" | html p
	       ))
     | "</DL>")	  
text DL   := x -> concatenate(
     newline, 
     newline,
     apply(x, p -> (
	       if class p === List or class p === Sequence then (
		    if # p === 2 
		    then (
			 "    ", text p#0, newline,
			 "    ", text p#1,
			 newline,
			 newline)
		    else if # p === 1 
		    then ("    ", 
			 text p#0, 
			 newline, 
			 newline)
		    else error "expected a list of length 1 or 2"
		    )
	       else ("    ", 
		    text p#0, 
		    newline, 
		    newline)
	       )),
     newline,
     newline)

texMath SUP := x -> concatenate( "^{", apply(x, tex), "}" )
texMath SUB := x -> concatenate( "_{", apply(x, tex), "}" )

text SUP := x -> "^" | text x#0
text SUB := x -> "_" | text x#0

net  TO := text TO := x -> concatenate ( "\"", formatDocumentTag x#0, "\"", drop(toList x, 1) )
html TO := x -> concatenate ( "<A HREF=\"", "\">", html formatDocumentTag x#0, "</A>", drop(toList x,1) )
tex  TO := x -> tex TT formatDocumentTag x#0

             toh := op -> x -> op SEQ{ new TO from x, headline x#0 }
            htoh := op -> x -> op SEQ{ new TO from x, headline x#0 }
net  TOH :=  toh net
text TOH := htoh text
html TOH :=  toh html
tex  TOH :=  toh tex

html LITERAL := x -> x#0
html EmptyMarkUpType := html MarkUpType := X -> html X{}
html ITALIC := htmlMarkUpType "I"
html UNDERLINE := htmlMarkUpType "U"
html TEX := x -> x#0	    -- should do something else!
html BOLD := htmlMarkUpType "B"

html Option := x -> toString x
text Option := x -> toString x

net BIG := x -> net x#0
net CENTER := x -> net x#0

