--		Copyright 1994-2002 by Daniel R. Grayson

debugDoc = () -> commandInterpreter local symbol

maximumCodeWidth := 120

DocDatabase = null

local exampleBaseFilename
local exampleOutputFilename
local currentNodeName
fixup := method(SingleArgumentDispatch => true)

-----------------------------------------------------------------------------
-- sublists, might be worthy making public
-----------------------------------------------------------------------------
sublists := (x,f,g,h) -> (
     -- x is a list with elements i
     -- apply g to those i for which f i is true
     -- apply h to the sublists, possibly empty, including those at the beginning and end, of elements between the ones for which f i is true
     -- return the results in the same order
     p := positions(toSequence x, f);
     mingle(
	  apply( prepend(-1,p), append(p,#x), (i,j) -> h take(x,{i+1,j-1})),
	  apply( p, i -> g x#i)))

-----------------------------------------------------------------------------
-- unformatting document tags
-----------------------------------------------------------------------------
-- we need to be able to do this only for the document tags we have shown to the user in formatted form 
unformatTag := new MutableHashTable
record      := f -> x -> (
     val := f x; 
     if val =!= x then unformatTag#val = x; 
     val)

-----------------------------------------------------------------------------
-- normalizing document tags
-----------------------------------------------------------------------------
   -- The normalized form for simple objects will be the symbol whose value is the object
   -- (We don't document objects that are not stored in global variables.)
   -- This allows us to write documentation links like
   --                TO "sin" 
   -- or
   --	             TO sin
   -- or
   --	             TO symbol sin
   -- and have them all get recorded the same way
normalizeDocumentTag := method(SingleArgumentDispatch => true)
normalizeDocumentTag   String := key -> if isGlobalSymbol key then getGlobalSymbol key else key
normalizeDocumentTag   Symbol := identity
normalizeDocumentTag Sequence := identity
normalizeDocumentTag  Nothing := key -> symbol null
normalizeDocumentTag    Thing := key -> (
     if ReverseDictionary#?key then return ReverseDictionary#key;
     error("encountered unidentifiable document tag: ",key);
     )

isDocumentableThing  := method(SingleArgumentDispatch => true)
isDocumentableThing    String := key -> true
isDocumentableThing    Symbol := key -> true
isDocumentableThing  Sequence := key -> false 		    -- we're not looking for documentable methods here, just documentable objects
isDocumentableThing   Nothing := key -> true

isDocumentableMethod := method(SingleArgumentDispatch => true)
isDocumentableMethod Sequence := key -> all(key,isDocumentableMethod)
isDocumentableMethod    Thing := key -> false
isDocumentableMethod   Symbol := key -> isGlobalSymbol toString key and getGlobalSymbol toString key === key
isDocumentableMethod     Type := 
isDocumentableMethod Function := 
isDocumentableThing     Thing := key -> (
     if ReverseDictionary#?key then return true;
     false)
-----------------------------------------------------------------------------
-- identifying the package of a document tag
-----------------------------------------------------------------------------
-- If we don't find it in another package, then we assume it's in the current package (which might be null)
-- That way we can compute the package during the loading of a package, while just some of the documentation has been installed.
-- Missing documentation can be detected when the package is closed, or later.
packageTag = method(SingleArgumentDispatch => true)	    -- assume the input key has been normalized
packageTag   Symbol := key -> package key
packageTag   String := key -> (
     r := scan(packages, pkg -> if pkg#"raw documentation"#?key then break pkg);
     if r === null then currentPackage else r)
packageTag  Package := identity
packageTag Sequence := key -> youngest \\ package \ key
packageTag    Thing := key -> ( p := package key; if p === null then currentPackage else p)
-----------------------------------------------------------------------------
-- formatting document tags
-----------------------------------------------------------------------------
   -- The formatted form should be a human-readable string, and different normalized tags should yield different formatted tags.
   -- The formatted tag is used for two purposes:
   --    for display in menus and links
   --    as the key for access in a database, where the key must be a string

Strings := hashTable { Sequence => "(...)", List => "{...}", Array => "[...]" }
toStr := s -> if Strings#?s then Strings#s else toString s
formatDocumentTag           = method(SingleArgumentDispatch => true)
	  
alphabet := set characters "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'"

formatDocumentTag Thing    := toString
formatDocumentTag String   := s -> s

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
     (3,class,Symbol   ) => s -> (toStr s#1, " ", toString s#0, " ", toStr s#2),-- infix operator
     (3,class,Sequence ) => s -> (toStr s#1, " ", toString s#0#0, " ", toStr s#2, " ", toString s#0#1, " ... "),-- infix assignment operator (really a ternary operator!)
     (2,class,Symbol   ) => s -> (toString s#0, " ", toStr s#1),-- prefix operator
     (2,class,Sequence ) => s -> (toString s#0#0, " ", toStr s#1, " ", toString s#0#1, " ... "),-- prefix assignment operator (need to handle the postfix assignment operators still!)
     (2,symbol ~       ) => s -> (toStr s#1, " ", toStr s#0), -- postfix operator
     (2,symbol !       ) => s -> (toStr s#1, " ", toStr s#0), -- postfix operator
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
     5 => s -> (toString s#0, "(", toStr s#1, ",", toStr s#2, ",", toStr s#3, ",", toStr s#4, ")"),
     4 => s -> (toString s#0, "(", toStr s#1, ",", toStr s#2, ",", toStr s#3, ")"),
     3 => s -> (toString s#0, "(", toStr s#1, ",", toStr s#2, ")"),
     2 => s -> (toString s#0, " ", toStr s#1),
     (Symbol,5)=>s -> ( toString s#0, "(", toStr s#1, ",", toStr s#2, ",", toStr s#3, ",", toString s#-1, "=>...)" ),
     (Symbol,4)=>s -> ( toString s#0, "(", toStr s#1, ",", toStr s#2, ",", toString s#-1, "=>...)" ),
     (Symbol,3)=>s -> ( toString s#0, "(", toStr s#1, ",", toString s#-1, "=>...)" ),
     (Symbol,2)=>s -> ( toString s#0, "(..., ", toString s#-1, "=>...)" )
     }

fSeq := fSeqInitialize(toString,toStr)
formatDocumentTag Sequence := record(
     s -> concatenate (
	  if #s == 0                                            then toString
	  else if             fSeq#?(#s,s#0)                    then fSeq#(#s,s#0)
	  else if #s >= 1 and fSeq#?(#s,s#0,s#1)                then fSeq#(#s,s#0,s#1)
	  else if #s >= 1 and fSeq#?(#s, class, class s#0, s#1) then fSeq#(#s, class, class s#0, s#1)
	  else if             fSeq#?(#s, class, class s#0)      then fSeq#(#s, class, class s#0)
	  else if             fSeq#?(class s#-1,#s)             then fSeq#(class s#-1,#s)
	  else if             fSeq#?#s                          then fSeq#(#s)
								else toString) s)

fSeqTO := fSeqInitialize(i -> fixup TO i, i -> fixup TO i)
formatDocumentTagTO := method(SingleArgumentDispatch => true)
formatDocumentTagTO Thing := x -> TT formatDocumentTag x
formatDocumentTagTO Sequence := (
     s -> SEQ toList (
	  if #s == 0                                              then (s -> error("unknown document tag: ", toString s))
	  else if             fSeqTO#?(#s,s#0)                    then fSeqTO#(#s,s#0)
	  else if #s >= 1 and fSeqTO#?(#s,s#0,s#1)                then fSeqTO#(#s,s#0,s#1)
	  else if #s >= 1 and fSeqTO#?(#s, class, class s#0, s#1) then fSeqTO#(#s, class, class s#0, s#1)
	  else if             fSeqTO#?(#s, class, class s#0)      then fSeqTO#(#s, class, class s#0)
	  else if             fSeqTO#?#s                          then fSeqTO#(#s)
	                                                          else (s -> error("unknown document tag: ", toString s))) s)

-----------------------------------------------------------------------------
-- verifying the tags
-----------------------------------------------------------------------------
-- here we check that the method a putative document tag documents is actually installed
verifyTag := method(SingleArgumentDispatch => true)
verifyTag Thing    := s -> null
verifyTag Sequence := s -> (
     if s#?-1 and class s#-1 === Symbol then (		    -- e.g., (res,Strategy) or (res,Module,Strategy)
	  fn := drop(s,-1);
	  opt := s#-1;
	  if #fn === 1 then (
	       fn = fn#0;
	       if not instance(fn, Function) then error "expected first element of document tag for optional argument to be a function";
	       )
	  else (
	       if not instance(lookup fn, Function) then error("no method installed for document tag '", formatDocumentTag fn, "'");
	       fn = fn#0;
	       );
	  if not (options fn)#?opt then error("expected ", opt, " to be an option of ", fn))
     else (						    -- e.g., (res,Module) or (symbol **, Module, Module)
	  if class lookup s =!= Function then error("documentation provided for '", formatDocumentTag s, "' but no method installed")))
verifyTag Option   := s -> error "old style option documentation tag"
-----------------------------------------------------------------------------
-- making document tags
-----------------------------------------------------------------------------
-- We need three bits of information about a document tag:
--     the original key	    	    e.g., (operator **,Module,Module)
--     the formatted key            e.g., "Module ** Module"
--     the package                  e.g., Macaulay2, or null if there is none
--     the package title            e.g., "Macaulay2", or "" if there is none
-- Here we assemble them together, so we don't have to recompute the information later.
DocumentTag = new Type of BasicList
DocumentTag.synonym = "document tag"
makeDocumentTag = method(SingleArgumentDispatch => true, Options => {
	  FormattedKey => null,
	  Package => null
	  })
makeDocumentTag DocumentTag := opts -> tag -> tag
makeDocumentTag Thing := opts -> key -> (
     key = normalizeDocumentTag key;
     verifyTag key;
     fkey := if opts#FormattedKey =!= null then opts#FormattedKey else formatDocumentTag key;
     pkg := if opts#Package =!= null then opts#Package else packageTag key;
     if pkg === null then error ("can't determine correct package for document tag '",key,"'");
     title := if pkg === null then "" else pkg#"title";
     new DocumentTag from {key,fkey,pkg,title})
-- a bit of experimentation...
DocumentTag.Key = method(SingleArgumentDispatch => true)
DocumentTag.Key DocumentTag := x -> x#0
err := x -> error "expected a document tag; perhaps the function 'hypertext' has not yet been run on hypertext"
DocumentTag.Key Thing := err
DocumentTag.FormattedKey = method(SingleArgumentDispatch => true)
DocumentTag.FormattedKey DocumentTag := x -> x#1
DocumentTag.FormattedKey Thing := err
DocumentTag.Package = method(SingleArgumentDispatch => true)
DocumentTag.Package DocumentTag := x -> x#2
DocumentTag.Package Thing := err
DocumentTag.Title = method(SingleArgumentDispatch => true)
DocumentTag.Title DocumentTag := x -> x#3
DocumentTag.Title Thing := err
DocumentTag ? DocumentTag := (x,y) -> x#1 ? y#1
DocumentTag ? String := (x,y) -> x#1 ? y
String ? DocumentTag := (x,y) -> x ? y#1
net DocumentTag := x -> concatenate ( DocumentTag.Title x, " :: ", DocumentTag.FormattedKey x )
toString DocumentTag := x -> error "who wants a string?"
package DocumentTag := DocumentTag.Package
packageTag DocumentTag := DocumentTag.Package
hasDocumentation := key -> isDocumentableThing key and (
     tag := makeDocumentTag key;
     pkg := DocumentTag.Package tag;
     pkg =!= null and pkg#"raw documentation"#?(DocumentTag.FormattedKey tag))

-----------------------------------------------------------------------------
-- fixing up hypertext
-----------------------------------------------------------------------------
nonnull := x -> select(x, i -> i =!= null)
trimline0 := x -> selectRegexp ( "^(.*[^ ]|) *$",1, x)
trimline  := x -> selectRegexp ( "^ *(.*[^ ]|) *$",1, x)
trimline1 := x -> selectRegexp ( "^ *(.*)$",1, x)
addspaces0:= x -> if x#?0 then if x#-1=="." then concatenate(x,"  ") else concatenate(x," ") else concatenate(x," ")
addspaces := x -> if x#?0 then if x#-1=="." then concatenate(x,"  ") else concatenate(x," ") else x

flat := method()
flat Thing := identity
flat SEQ := x -> toSequence x
flat Nothing := x -> ()
fixflat := z -> splice apply(z, i -> flat fixup i)

fixup Thing      := z -> error("unrecognizable item inside documentation: ", toString z)
fixup MarkUpListParagraph := z -> splice apply(z,fixup)
fixup MarkUpList := x -> (
     x = splice apply(x, fixup);
     if any(toSequence x, i -> instance(i, MarkUpListParagraph))
     then PARA sublists( x, i -> instance(i,MarkUpListParagraph), identity, i -> PARA i)
     else fixflat x)
fixup Nothing    := x -> ()				       -- so it will get removed by splice later
fixup BR         := identity
fixup PRE        := identity
fixup CODE       := identity
fixup LITERAL    := identity
fixup ANCHOR     := identity
fixup List       := z -> fixup SEQ z
fixup Sequence   := z -> fixup SEQ z
fixup Option     := z -> z#0 => fixup z#1		       -- Headline => "...", ...
fixup UL         := z -> splice apply(nonnull z, i -> PARA fixup if class i === TO then TOH {i#0} else i)
fixup TO         := x -> TO if x#?1 then { makeDocumentTag x#0, concatenate drop(toSequence x,1) } else { makeDocumentTag x#0 }
fixup TO2        := x -> TO2{ makeDocumentTag x#0, concatenate drop(toSequence x,1) }
fixup TOH        := x -> TOH{ makeDocumentTag x#0 }
fixup MarkUpType := z -> z{}				       -- convert PARA to PARA{}
fixup Function   := z -> z				       -- allow Function => f 
fixup String     := s -> (				       -- remove clumsy newlines within strings
     ln := lines s;
     if not ln#?1 then return s;
     concatenate ({addspaces0 trimline0 ln#0}, addspaces \ trimline \take(ln,{1,#ln-2}), {trimline1 ln#-1}))

fixup1 := method(SingleArgumentDispatch => true)
fixup1 Thing := identity
fixup1 Nothing := x -> ()
fixup1 Hypertext := fixup1 SEQ := toSequence
fixuptop := s -> Hypertext deepSplice apply(toList s, fixup1)

new Hypertext from List := (h,x) -> splice apply(x, i -> flat i)
hypertext = x -> Hypertext fixup x

-----------------------------------------------------------------------------
-- installing the documentation
-----------------------------------------------------------------------------

Nothing << Thing := (x,y) -> null			    -- turning off the output is easy to do
DocumentableValueType := set { 
     Boolean, 
     HashTable, 
     Function, 
     BasicList, 
     Nothing,
     File
     }
UndocumentableValue := hashTable { symbol environment => true, symbol commandLine => true }
documentableValue := key -> (
     class key === Symbol and value key =!= key
     and not UndocumentableValue#?key and DocumentableValueType#?(basictype value key))

---- how could this have worked (so soon)?
-- scan(flatten(pairs \ globalDictionaries), (name,sym) -> if documentableValue sym then Symbols#(value sym) = sym)


file := null

-----------------------------------------------------------------------------
-- getting database records
-----------------------------------------------------------------------------

extractBody := x -> if x.?Description then x.Description
getRecord := (pkg,key) -> pkg#"raw documentation"#key	    -- for Databases, insert 'value' here
getPackage := key -> scan(value \ values PackageDictionary, pkg -> if pkg#?"raw documentation" and pkg#"raw documentation"#?key then break pkg)
getDoc := key -> (
     fkey := formatDocumentTag key;
     pkg := getPackage fkey;
     if pkg =!= null then getRecord(pkg,fkey))
if debugLevel > 10 then getDoc = on (getDoc, Name => "getDoc")
getOption := (key,tag) -> (
     s := getDoc key;
     if s =!= null and s#?tag then s#tag)
getBody := key -> getOption(key,Description)
-----------------------------------------------------------------------------
-- process examples
-----------------------------------------------------------------------------

extractExamplesLoop            := method(SingleArgumentDispatch => true)
extractExamplesLoop Thing      := x -> {}
extractExamplesLoop EXAMPLE    := toList
extractExamplesLoop MarkUpList := x -> join apply(toSequence x, extractExamplesLoop)

extractExamples := (docBody) -> (
     examples := extractExamplesLoop docBody;
     if #examples > 0 then currentPackage#"example inputs"#currentNodeName = examples;
     docBody)

M2outputRE := "(\n\n)i+[1-9][0-9]* : "
M2outputREindex := 1
separateM2output = method()
separateM2output String := r -> (
     while r#?0 and r#0 == "\n" do r = substring(1,r);
     while r#?-1 and r#-1 == "\n" do r = substring(0,#r-1,r);
     separateRegexp(M2outputRE,M2outputREindex,r))

getFileName := body -> (
     x := select(1, body, i -> class i === Option and #i === 2 and first i === FileName);
     if #x > 0 then x#0#1 else null
     )

makeFileName := (fkey,filename,pkg) -> (			 -- may return 'null'
     if pkg#?"package prefix" and pkg#"package prefix" =!= null 
     then pkg#"package prefix" | LAYOUT#"packageexamples" pkg#"title" | if filename =!= null then filename else toFilename fkey
     )

exampleResultsFound := false
exampleResults := {}
exampleCounter := 0
checkForExampleOutputFile := (node,pkg) -> (
     exampleCounter = 0;
     if pkg#"example results"#?node then (
	  exampleResults = pkg#"example results"#node;
	  exampleResultsFound = true;
	  exampleOutputFilename = exampleBaseFilename | ".out";
	  )
     else (
	  exampleResults = {};
	  exampleResultsFound = false;
	  exampleOutputFilename = null;
	  ))
--      exampleOutputFilename = null;
--      if debugLevel > 1 then stderr << "exampleBaseFilename = " << exampleBaseFilename << endl;
--      if exampleBaseFilename =!= null then (
-- 	  exampleOutputFilename = exampleBaseFilename | ".out";
-- 	  if debugLevel > 0 then (
-- 	       if debugLevel > 1 then stderr << "checking for example results in file '" << exampleOutputFilename << "' : " << (if fileExists exampleOutputFilename then "it exists" else "it doesn't exist") << endl;
-- 	       );
-- 	  if fileExists exampleOutputFilename then (
-- 	       -- read, separate, and store example results
-- 	       exampleResults = pkg#"example results"#node = drop(separateM2output get exampleOutputFilename,-1);
-- 	       if debugLevel > 1 then stderr << "node " << node << " : " << boxList \\ net \ exampleResults << endl;
-- 	       exampleResultsFound = true)))
processExample := x -> (
     a :=
     if exampleResultsFound and exampleResults#?exampleCounter
     then {x, CODE exampleResults#exampleCounter}
     else (
	  if exampleResultsFound and #exampleResults === exampleCounter then (
	       stderr << "warning : example results file " << exampleOutputFilename << " terminates prematurely" << endl;
	       );
	  {x, CODE concatenate("i", toString (exampleCounter+1), " : ",x)}
	  );
     exampleCounter = exampleCounter + 1;
     a)
processExamplesLoop := s -> (
     if class s === EXAMPLE then ExampleTABLE apply(select(toList s, i -> i =!= null), processExample)
     else if class s === Sequence or instance(s,MarkUpList)
     then apply(s,processExamplesLoop)
     else s)
processExamples := (pkg,fkey,docBody) -> (
     exampleBaseFilename = makeFileName(fkey,getFileName docBody,pkg);
     checkForExampleOutputFile(fkey,pkg);
     processExamplesLoop docBody)

-----------------------------------------------------------------------------
-- 'document' function
-----------------------------------------------------------------------------

nonNull := x -> select(x,t->t=!=null)
fixupList := x -> apply(nonNull x,fixup)
enlist := x -> if class x === List then x else {x}
chkIsString := key -> val -> if class val === String then val else error("expected ",toString key," option to be a string")
fixupTable := new HashTable from {
     Key => identity,
     symbol DocumentTag => identity,
     Usage => val -> fixup val,
     Function => val -> fixup val,
     FormattedKey => chkIsString FormattedKey,
     Inputs => val -> fixupList val,
     Outputs => val -> fixupList val,
     Results => val -> fixupList val,
     OldSynopsis => identity,				    -- old
     FileName => chkIsString FileName,
     Headline => chkIsString Headline,
     Description => val -> extractExamples hypertext val,
     Examples => val -> if val =!= {} and val =!= null then fixup PARA { SUBSECTION "Examples", extractExamples hypertext val },
     Caveat => v -> if v =!= null then fixup PARA { SUBSECTION "Caveat", SEQ v },
     ProgrammingHint => v -> if v =!= null then fixup PARA { SUBSECTION "Programming Hint", SEQ v },
     SeeAlso => v -> if v =!= {} and v =!= null then fixup PARA { SUBSECTION "See also", UL (TO \ enlist v) },
     Subnodes => v -> MENU apply(nonNull enlist v, x -> fixup (
	       if class x === TO then x
	       else if class x === TOH then TO {x#0}
	       else if class x === String then x
	       else error ("unrecognizable Subnode list item: ",x)))
     }
caveat := key -> getOption(key,Caveat)
programmingHint := key -> getOption(key,ProgrammingHint)
seealso := key -> getOption(key,SeeAlso)
theMenu := key -> getOption(key,Subnodes)
theExamples := key -> getOption(key,Examples)
documentOptions := new HashTable from {
     Key => true,
     Usage => true,
     FormattedKey => true,
     Function => true,
     Inputs => true,
     Outputs => true,
     Results => true,
     OldSynopsis => true,				    -- old
     FileName => true,
     Headline => true,
     Examples => true,
     SeeAlso => true,
     Caveat => true,
     ProgrammingHint => true,
     Subnodes => true }
reservedNodeNames := set apply( {"Top", "Table of Contents", "Combined Index"}, toLower )

document = method( SingleArgumentDispatch => true )

document List := z -> document toSequence z
document Thing := z -> document singleton z
document Sequence := args -> (
     if currentPackage === null then error "encountered 'document' command, but no package is open";
     opts := new MutableHashTable;
     scan(args, arg -> if class arg === Option then (
	       key := arg#0;
	       if not documentOptions#?key then error("--warning: ignoring unknown documentation option '", key, "'");
	       if opts#?key then error("option ",key," encountered twice");
	       opts#key = arg#1));
     args = select(args, arg -> class arg =!= Option);
     if not opts.?Key then if args#?0 then (
	  opts.Key = args#0;
	  args = drop(args,1);
	  ) else error "missing Key";
     opts.DocumentTag = tag := makeDocumentTag(opts.Key, Package => currentPackage, FormattedKey => if opts.?FormattedKey then opts.FormattedKey);
     currentNodeName = DocumentTag.FormattedKey tag;
     if reservedNodeNames#?(toLower currentNodeName) then error("'document' encountered a reserved node name '",currentNodeName,"'");
     pkg := DocumentTag.Package tag;
     opts.Description = toList args;
     exampleBaseFilename = makeFileName(currentNodeName,if opts.?FileName then opts.FileName,currentPackage);
     if currentPackage#"raw documentation"#?currentNodeName then error ("warning: documentation already provided for '", currentNodeName, "'");
     opts = new HashTable from apply(pairs opts,(key,val) -> (key,fixupTable#key val));
     currentPackage#"raw documentation"#currentNodeName = opts;
     currentNodeName = null;
     )

-----------------------------------------------------------------------------
-- getting help from the documentation
-----------------------------------------------------------------------------

topicList = () -> sort flatten apply(values PackageDictionary, p -> keys (value p)#"raw documentation")

getExampleInputs := method(SingleArgumentDispatch => true)
getExampleInputs Thing        := t -> {}
getExampleInputs ExampleTABLE := t -> apply(toList t, first)
getExampleInputs MarkUpList   := t -> join apply(toSequence t, getExampleInputs)

examples = x -> getExampleInputs documentation x
printExamples = f -> scan(examples f, i -> << i << endl)
topics = Command (() -> pager columnate(if printWidth != 0 then printWidth else 80, format \ topicList()))
apropos = (pattern) -> sort select(flatten \\ keys \ globalDictionaries, i -> match(toString pattern,i))
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
	  if #s === 0 then null 
	  else if class s#-1 === Symbol then unSingleton drop(s,-1) -- dropping optional argument tag
	  else if #s === 2 then nextMoreGeneral2 s 
	  else if #s === 3 then nextMoreGeneral3 s 
	  else if #s === 4 then nextMoreGeneral4 s))

evenMoreGeneral := key -> (
     t := nextMoreGeneral key;
     if t === null and class key === Sequence then key#0 else t)

headline = method(SingleArgumentDispatch => true)
headline DocumentTag := tag -> (
     pkg := DocumentTag.Package tag;
     fkey := DocumentTag.FormattedKey tag;
     if pkg =!= null and pkg#"raw documentation"#?fkey and pkg#"raw documentation"#fkey#?Headline then pkg#"raw documentation"#fkey#Headline
     else headline DocumentTag.Key tag			    -- revert to old method
     )
headline Thing := memoize (				    -- save this stuff, too!
     key -> (
	  while (
	       d := getOption(key,Headline);
	       d === null
	       )
	  and ( 
	       key = evenMoreGeneral key;
	       key =!= null
	       )
	  do null;
	  if d =!= null then d))

commentize := s -> if s =!= null then concatenate(" -- ",s)

moreGeneral := s -> (
     n := nextMoreGeneral s;
     if n =!= null then fixup SEQ { "Next more general method: ", TO n, commentize headline n }
     )

-----------------------------------------------------------------------------

optTO := i -> if getDoc i =!= null then fixup SEQ{ TOH {i} } else formatDocumentTagTO i
optTOCLASS := i -> (if getDoc i =!= null then (
	  -- we might want a new type of TO so that in info mode this would look like this:
	  --      * alpha (a StateTable) -- recognizing alphabetic letters  (*note: alpha::.)
	  -- fixup SEQ { TO i, " (", OFCLASS class value i, ")", commentize headline i }
	  fixup SEQ{ TOH {i} }
	  )
     else formatDocumentTagTO i)

smenu := s -> UL (optTO \ last \ sort apply(s , i -> {formatDocumentTag i, i}) )
smenuCLASS := s -> UL (optTOCLASS \ last \ sort apply(s , i -> {formatDocumentTag i, i}) )
 menu := s -> UL (optTO \ s)

vowels := set characters "aeiouAEIOU"
indefiniteArticle := s -> if vowels#?(s#0) and not match("^one ",s) then "an " else "a "
indefinite := s -> concatenate(indefiniteArticle s, s)
synonym = X -> if X.?synonym then X.synonym else "object of class " | toString X

synonymAndClass := X -> fixup (
     if X.?synonym then SEQ {indefinite X.synonym, " (of class ", TO X, ")"}
     else SEQ {"an object of class ", TO X}
     )     

justClass := X -> fixup SEQ {"an instance of class ", TO X}

OFCLASS = X -> fixup (
     if parent X === Nothing then error "expected a class";
     if X.?synonym then SEQ {indefiniteArticle X.synonym, TO2 {X, X.synonym}}
     else SEQ {"an object of class ", TO X}
     )

makeDocBody := method(SingleArgumentDispatch => true)
makeDocBody Thing := key -> (
     fkey := formatDocumentTag key;
     pkg := getPackage fkey;
     if pkg =!= null then (
	  rec := getRecord(pkg,fkey);
	  docBody := extractBody rec;
	  if docBody =!= null and #docBody > 0 then (
	       docBody = processExamples(pkg, fkey, docBody);
	       if class key === String 
	       then PARA {docBody}
	       else SEQ { SUBSECTION "Description", PARA {docBody} })))

title := s -> ( HEADER1 formatDocumentTag s, HEADER2 headline s )

type := S -> fixup (
     s := value S;
     PARA deepSplice { "The object ", TO S, " is ", OFCLASS class s,
     	  if parent s =!= Nothing then (
     	       f := (T -> while T =!= Thing list parent T do T = parent T) s;
	       (
		    if #f>1 then ", with ancestor classes " else if #f == 1 then ", with ancestor class " else ", with no ancestor class.", 
		    toSequence between(" < ", f / (T -> TO T)) 
		    )
	       ),
	  "."
     	  }
     )

istype := X -> parent X =!= Nothing
alter1 := x -> (
     if class x === Option and #x === 2 then (
	  if istype x#0 then SEQ { OFCLASS x#0, if x#1 =!= "" and x#1 =!= null then SEQ { ", ", x#1 } }
	  else error "expected type to left of '=>'"
	  )
     else x)
alter := x -> (
     if class x === Option and #x === 2 then (
	  if istype x#0 then SEQ { OFCLASS x#0, if x#1 =!= "" and x#1 =!= null then SEQ { ", ", x#1 } }
	  else if class x#0 === String then (
	       if class x#1 === Option and #x#1 === 2 then (
		    if istype x#1#0 then SEQ { TT x#0, ", ", OFCLASS x#1#0, if x#1#1 =!= "" and x#1#1 =!= null then SEQ { ", ", x#1#1 } }
		    else error "expected type to left of '=>'"
		    )
	       else SEQ { TT x#0, if x#1 =!= "" and x#1 =!= null then SEQ { ", ", x#1 } }
	       )
	  else error "expected string or type to left of '=>'"
	  )
     else SEQ x)

typicalValue := k -> (
     if typicalValues#?k then typicalValues#k 
     else if class k === Sequence and typicalValues#?(k#0) then typicalValues#(k#0)
     else Thing
     )

types := method(SingleArgumentDispatch => true)
types Thing := x -> ({},{})
types Function := x -> ({},{typicalValue x})
types Sequence := x -> (
     if #x > 1 and instance(x#-1,Symbol) 
     then ({},{})					    -- it's an option ...
     -- then types unSingleton drop(x,-1)
     else ( drop(toList x,1), { typicalValue x } ))

isopt := x -> class x === Option and #x === 2

merget := (v,v') -> apply(v,v',(a,t) -> (
	  if t =!= Thing then (
	       if isopt a then (
		    if isopt a#1 then (
			 if a#1#0 =!= t then error "type mismatch"
			 else a
			 )
		    else (
			 if istype a#0 then (
			      if a#0 =!= t then error "type mismatch"
			      else a
			      )
			 else a#0 => t => a#1			      
			 )
		    )
	       else t => a)
	  else a))

optargs := method(SingleArgumentDispatch => true)
optargs Thing := x -> null
optargs Function := f -> (
     o := options f;
     if o =!= null then PARA { "Optional arguments [default] :", smenu apply(keys o, t -> f => t)})
optargs Sequence := s -> (
     o := options s;
     if o =!= null then PARA { "Optional arguments [default] :", smenu apply(keys o, t -> s => t)}
     else optargs s#0)

emptyOptionTable := new OptionTable from {}
getOptionDefaultValues := method(SingleArgumentDispatch => true)
getOptionDefaultValues Symbol := x -> if value x =!= x then getOptionDefaultValues value x else emptyOptionTable
getOptionDefaultValues Thing := x -> emptyOptionTable
getOptionDefaultValues Function := f -> (
     o := options f;
     if o =!= null then o else emptyOptionTable)
getOptionDefaultValues Sequence := s -> (
     o := options s;
     if o =!= null then o else if class s#0 === Function then getOptionDefaultValues s#0 else emptyOptionTable)

synopsisOpts := new OptionTable from {			    -- old
     Usage => null,
     Function => null,
     Inputs => {},
     Outputs => {},
     Results => {}
     }
synopsis := method(SingleArgumentDispatch => true)
synopsis Thing := key -> (
     -- we still want to put
     --	       moreGeneral s
     -- back somewhere....
     o := getDoc key;
     if o === null then o = synopsisOpts;
     inp := if o.?Inputs then o.Inputs else {};
     out := if o.?Outputs then o.Outputs else {};
     res := if o.?Results then o.Results else {};
     usa := if o.?Usage then o.Usage;
     fun := if o#?Function then o#Function;
     iso := x -> instance(x,Option) and #x==2 and instance(x#0,Symbol);
     ino := new HashTable from select(inp, x -> iso x);
     inp = select(inp, x -> not iso x);
     opt := getOptionDefaultValues key;
     ino = apply(sort unique join(keys opt,keys ino),
	  optionName -> (
	       fixup (
		    if opt#?optionName
	       	    then (
			 defaultValue := opt#optionName;
			 if ino#?optionName 
			 then SEQ { TO optionName, " => ", alter1 ino#optionName, " [", toString defaultValue, "]" }
			 else SEQ { TO optionName, " => [", toString defaultValue, "]" }
			 )
	       	    else (
			 stderr << "--warning: " << optionName << " not an option for documentation key " << key << endl;
			 SEQ { TO optionName, " => ", alter1 ino#optionName }
			 ))));
     (inp',out') := types key;
     if out' === {Thing} then out' = {};		    -- not informative enough
     if #inp === 0 then (
	  inp = apply(inp', T -> T => "");
	  )
     else if #inp' =!= 0 then (
     	  if #inp =!= #inp' then error "mismatched number of inputs";
     	  inp = merget(inp,inp');
	  );
     if class out === SEQ then out = toList out;
     if #out === 0 then (
	  out = apply(out', T -> T => "");
	  )
     else if #out' =!= 0 then (
     	  if #out =!= #out' then error "mismatched number of outputs";
     	  out = merget(out,out');
	  );
     inp = alter \ inp;
     out = alter \ out;
     if #inp > 0 or #ino > 0 or #out > 0 then (
	  fixup SEQ {				  -- to be implemented
     	       SUBSECTION "Synopsis",
	       UL {
     	       	    if usa =!= null then SEQ { "Usage: ", if class usa === String then TT usa else usa},
		    if fun =!= null then SEQ { "Function: ", TO fun }
		    else if class key === Sequence and key#?0 then (
	       		 if class key#0 === Function 
			 then SEQ { "Function: ", TO key#0 }
			 else SEQ { "Operator: ", TO key#0 }
			 ),
		    if inp#?0 then PARA1 { "Inputs:", UL inp },
		    if out#?0 then PARA1 { "Outputs:", UL out },
		    if res#?0 then PARA1 { "Results:", UL res },
		    if ino#?0 then PARA1 { "Optional inputs [default] :", UL ino }
		    }
	       }
	  ))

documentableMethods := s -> select(methods s,isDocumentableMethod)

fmeth := f -> (
     b := documentableMethods f;
     if methodFunctionOptions#?f and not methodFunctionOptions#f.SingleArgumentDispatch
     then b = select(b, x -> x =!= (f,Sequence));
     if #b > 0 then SEQ { PARA { "Ways to use ", TT toString f }, smenu b } )

noBriefDocThings := hashTable { symbol <  => true, symbol >  => true, symbol == => true }
briefDocumentation = method(SingleArgumentDispatch => true)

briefDocumentation VisibleList := x -> null

briefDocumentation Thing :=
-- briefDocumentation File := 
-- briefDocumentation BasicList := 
-- briefDocumentation Function := 
-- briefDocumentation MutableHashTable := 
-- briefDocumentation HashTable := 
x -> (
     if noBriefDocThings#?x or not isDocumentableThing x then return null;
     r := synopsis x;
     if r =!= null then << endl << r << endl
     else (
	  if headline x =!= null then << endl << commentize headline x << endl;
	  if class x === Function then (
	       s := fmeth x;
	       if s =!= null then << endl << s << endl;)))

documentation = method(SingleArgumentDispatch => true)
documentation String := key -> (
     if unformatTag#?key then documentation unformatTag#key 
     else if isGlobalSymbol key then (
	  t := getGlobalSymbol key;
	  documentation t)
     else (
	  b := makeDocBody key;
	  if b === null then b = ();
	  Hypertext fixuptop (title key, b, theExamples key, caveat key, programmingHint key, seealso key, theMenu key)))

binary := set binaryOperators
prefix := set prefixOperators
postfix := set postfixOperators
other := set otherOperators
operatorSet = set join(binaryOperators, prefixOperators, postfixOperators, otherOperators)
erase symbol binaryOperators
erase symbol prefixOperators
erase symbol postfixOperators
erase symbol otherOperators

op := s -> if operatorSet#?s then (
     ss := toString s;
     fixup SEQ {
	  if binary#?s then PARA {
	       "This operator may be used as a binary operator in an expression \n",
	       "like ", TT ("x"|ss|"y"), ".  The user may install ", TO "binary methods", " \n",
	       "for handling such expressions with code such as ",
	       if ss == " "
	       then PRE ("         X Y := (x,y) -> ...")
	       else PRE ("         X "|ss|" Y := (x,y) -> ..."), 
	       "where ", TT "X", " is the class of ", TT "x", " and ", TT "Y", " is the \n",
	       "class of ", TT "y", "."
	       },
	  if prefix#?s then PARA {
	       "This operator may be used as a prefix unary operator in an expression \n",
	       "like ", TT (ss|"y"), ".  The user may install a method for handling \n",
	       "such expressions with code such as \n",
	       PRE ("           "|ss|" Y := (y) -> ..."),
	       "where ", TT "Y", " is the class of ", TT "y", "."
	       },
	  if postfix#?s then PARA {
	       "This operator may be used as a postfix unary operator in an expression \n",
	       "like ", TT ("x "|ss), ".  The user may install a method for handling \n",
	       "such expressions with code such as \n",
	       PRE ("         X "|ss|"   := (x,y) -> ..."),
	       "where ", TT "X", " is the class of ", TT "x", "."
	       },
	  }
     )

optionFor := s -> unique select( value \ flatten(values \ globalDictionaries), f -> class f === Function and (options f)#?s) -- this is slow!

ret := k -> (
     t := typicalValue k;
     if t =!= Thing then fixup PARA {"Class of returned value: ", TO t, commentize headline t}
     )
seecode := x -> (
     f := lookup x;
     n := code f;
     if n =!= null 
     -- and height n + depth n <= 10 
     and width n <= maximumCodeWidth
     then ( SUBSECTION "Code", PRE demark(newline,unstack n) )
     )

documentationValue := method()
documentationValue(Symbol,Function) := (s,f) -> ( ret f, fmeth f )
documentationValue(Symbol,Type) := (s,X) -> (
     syms := unique flatten(values \ globalDictionaries);
     a := apply(select(pairs typicalValues, (key,Y) -> Y===X and isDocumentableMethod key), (key,Y) -> key);
     b := toString \ select(syms, y -> instance(value y, Type) and parent value y === X);
     c := select(documentableMethods X, key -> not typicalValues#?key or typicalValues#key =!= X);
     e := toString \ select(syms, y -> not mutable y and class value y === X);
     splice (
	  if #b > 0 then ( PARA {"Types of ", if X.?synonym then X.synonym else toString X, " :"}, smenu b),
	  if #a > 0 then ( PARA {"Functions and methods returning ", indefinite synonym X, " :"}, smenu a ),
	  if #c > 0 then ( PARA {"Methods for using ", indefinite synonym X, " :"}, smenu c),
	  if #e > 0 then ( PARA {"Fixed objects of class ", toString X, " :"}, smenu e)))
documentationValue(Symbol,HashTable) := (s,x) -> splice (
     c := documentableMethods x;
     if #c > 0 then (PARA {"Functions installed in ", toString x, " :"}, smenu c))
documentationValue(Symbol,Thing) := (s,x) -> ()
documentationValue(Symbol,Package) := (s,pkg) -> (
     e := pkg#"exported symbols";
     a := select(e,x -> instance(value x,Function));	    -- functions
     b := select(e,x -> instance(value x,Type));	    -- types
     m := unique flatten apply(b, T -> select(keys value T, 
	       i -> class i === Sequence and (
		    class i#0 === Symbol
		    or
		    class i#0 === Function and ReverseDictionary#?(i#0) -- some method functions are local to the package, thus not visible
		    ))); -- methods
     c := select(e,x -> instance(value x,Symbol));	    -- symbols
     d := toList(set e - set a - set b - set c);	    -- other things
     fn := pkg#"title" | ".m2";
     (
	  SUBSECTION "Version", "This documentation describes version ", pkg.Options.Version, " of the package.",
	  SUBSECTION "Source code", "The source code is in the file ", HREF { LAYOUT#"packages" | fn, fn }, ".",
	  if #pkg#"exported symbols" > 0 then (
	       SUBSECTION "Exports",
	       UL {
		    if #b > 0 then PARA1 {"Types", smenu b},
		    if #a > 0 then PARA1 {"Functions", smenu a},
		    if #m > 0 then PARA1 {"Methods", smenu m},
		    if #c > 0 then PARA1 {"Symbols", smenu c},
		    if #d > 0 then PARA1 {"Other things", smenuCLASS d}})))

documentation Symbol := S -> (
     a := apply(select(optionFor S,f -> isDocumentableMethod f), f -> f => S);
     b := documentableMethods S;
     Hypertext fixuptop ( title S, synopsis S, makeDocBody S, op S,
	  if #a > 0 then (PARA {"Functions with optional argument named ", toExternalString S, " :"}, smenu a),
	  if #b > 0 then (PARA {"Methods for ", toExternalString S, " :"}, smenu b),
     	  documentationValue(S,value S),
	  type S, theExamples S, caveat S, programmingHint S, seealso S, theMenu S ))

documentation Sequence := key -> (
     if key#?-1 and instance(key#-1,Symbol) then (		    -- optional argument
	  fn := unSingleton drop(key,-1);
	  opt := key#-1;
	  if not (options fn)#?opt then error ("function ", fn, " does not accept option key ", opt);
	  default := (options fn)#opt;
	  Hypertext fixuptop ( title key, synopsis key, makeDocBody key,
	       PARA BOLD "Further information", 
	       fixup UL {
		    SEQ{ "Default value: ", if hasDocumentation default then TOH {default} else TT toString default },
		    SEQ{ if class fn === Sequence then "Method: " else "Function: ", TOH {fn} },
		    SEQ{ "Option name: ", TOH {opt} }
		    },
	       theExamples key, programmingHint key, caveat key, seealso key, theMenu key ))
     else (						    -- method key
	  if null === lookup key then error("expected ", toString key, " to be a method");
	  Hypertext fixuptop ( title key, synopsis key, makeDocBody key, theExamples key, programmingHint key, caveat key, seealso key, theMenu key )))

documentation Thing := x -> if ReverseDictionary#?x then return documentation ReverseDictionary#x else SEQ{ " -- undocumented -- "}

pager = x -> (
     if height stdio > 0
     then "!" | (if getenv "PAGER" == "" then "more" else getenv "PAGER") << x << close 
     else << x << endl ;)
help = method(SingleArgumentDispatch => true)
help List := v -> (
     printWidth = printWidth - 2;
     r := boxList apply(v, x -> net documentation x);
     printWidth = printWidth + 2;
     << endl;
     pager r)
help Thing := s -> (
     if s === () then s = "initial help";
     r := documentation s;
     if r === null then r = Hypertext { "No documentation found for '", formatDocumentTag s, "'"};
     << endl;
     pager net r)
help = Command help

infoHelp = key -> (
     tag := makeDocumentTag key;
     t := infoTagConvert tag;
     run ("info "|format t);)

-----------------------------------------------------------------------------
-- helper functions useable in documentation
-----------------------------------------------------------------------------

numtests := 0

TEST = method()
TEST Function := TEST String := s -> (
     x := currentPackage#"test inputs";
     x# #x = s;
     )
TEST List := y -> TEST \ y

-----------------------------------------------------------------------------

dummyDoc := x -> document {
     if value x =!= x and (
	  class value x === Function
	  or class value x === ScriptedFunctor
	  or instance(value x, Type)
	  )
     then value x
     else x,
     Headline => "undocumented symbol", "No documentation provided yet."}

undocErr := x -> (
     pos := locate x;
     pos = if pos === null then "error: " else pos#0 | ":" | toString pos#1 | ": ";
     stderr << pos << x << " undocumented " << synonym class value x << endl;
     )

undocumentedSymbols = () -> select(
     flatten(values \ globalDictionaries), 
     x -> (
	  if (
	       -- x =!= value x and        -- ignore symbols with no value assigned
	       not DocDatabase#?(toString x)
	       ) 
     	  then (
	       undocErr x;
	       dummyDoc x;
	       true)))

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
