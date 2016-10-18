--		Copyright 1994-2006 by Daniel R. Grayson

binary := set flexibleBinaryOperators
prefix := set flexiblePrefixOperators
postfix := set flexiblePostfixOperators
operator := binary+prefix+postfix

checkLoadDocumentation = () -> (
     if (
	  not isGlobalSymbol "Macaulay2Doc"
	  or
	  class value getGlobalSymbol "Macaulay2Doc" =!= Package
	  or
	  not member(value getGlobalSymbol "Macaulay2Doc", loadedPackages)
	  or
	  not member((value getGlobalSymbol "Macaulay2Doc").Dictionary, dictionaryPath)
	  )
     then (
	  -- the documentation for things in the package Core is in the package Macaulay2Doc !
	  needsPackage "Macaulay2Doc";
	  ))

getpkg := memoize(
     title -> (
	  if PackageDictionary#?title then value PackageDictionary#title
	  else dismiss needsPackage(title,LoadDocumentation=>true)))

-----------------------------------------------------------------------------
-- normalizing document keys
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
normalizeDocumentKey := method(Dispatch => Thing)
normalizeDocumentKey   String := key -> if isGlobalSymbol key then getGlobalSymbol key else key
normalizeDocumentKey    Array := identity
normalizeDocumentKey   Symbol := identity
normalizeDocumentKey Sequence := identity
normalizeDocumentKey  Nothing := key -> symbol null
normalizeDocumentKey    Thing := key -> (
     if hasAttribute(key,ReverseDictionary) then return getAttribute(key,ReverseDictionary);
     error("can't determine symbol whose value is document tag: ",key);
     )

isDocumentableThing  = method(Dispatch => Thing)
isDocumentableThing    String := key -> false
isDocumentableThing    Symbol := key -> true
isDocumentableThing  Sequence := key -> false 		    -- we're not looking for documentable methods here, just documentable objects
isDocumentableThing   Nothing := key -> true

isDocumentableMethod = method(Dispatch => Thing)
isDocumentableMethod Sequence := key -> (
     all(key, i -> (
     	       class i === Sequence 			    -- assignment methods look like ((symbol *, symbol =), X, Y, Z)
     	       or isDocumentableMethod i))
     )
isDocumentableMethod    Thing := key -> false
isDocumentableMethod   Symbol := key -> isGlobalSymbol toString key and getGlobalSymbol toString key === key
isDocumentableMethod     Type := 
isDocumentableThing     Thing := key -> hasAttribute(key,ReverseDictionary) and isDocumentableMethod getAttribute(key,ReverseDictionary)
isDocumentableMethod Function := fn -> hasAttribute(fn,ReverseDictionary) and dictionary getAttribute(fn,ReverseDictionary) =!= null
isDocumentableMethod ScriptedFunctor := fn -> hasAttribute(fn,ReverseDictionary)

-----------------------------------------------------------------------------
-- verifying the tags
-----------------------------------------------------------------------------
-- here we check that the method a putative document tag documents is actually installed
verifyKey = method(Dispatch => Thing)
verifyKey Thing    := s -> null

methodNames = set {NewFromMethod, NewMethod, NewOfFromMethod, NewOfMethod, id, Ext, Tor}

verifyKey Sequence := s -> (				    -- e.g., (res,Module) or (symbol **, Module, Module)
     if #s == 0 then error "documentation key () encountered";
     if #s == 1 and not instance(s#0, Function) then error(
	  "documentation key ", format toString s, 
	  " encountered, but ", format toString s#0,
	  " is not a function");
     if #s > 1 
     and not instance(s#0, Function) 
     and not instance(s#0, Command)
     and not instance(s#0, ScriptedFunctor)
     and not instance(s#0, Keyword)
     and not methodNames#?(s#0)
     and not (instance(s#0,Sequence) and 2==#s#0 and s#0#1===symbol = and instance(s#0#0,Keyword))
     then error(
	  "documentation key ", format toString s, 
	  " encountered, but ", format toString s#0,
	  " is not a function, command, scripted functor, or keyword");
     if (
	  if #s > 2 then (
	       t := youngest drop(s,1);	                    -- this will all get screwed up with immutable types present
	       t#?s and instance(t#s, Function) )
	  else if #s == 2 then ( instance(s#1,HashTable) and s#1#?(s#0) and instance(s#1#(s#0), Function) )
	  else if #s == 1 then ( nullaryMethods#?s and instance(nullaryMethods#s, Function) )
	  else false
	  )
     then null
     else if #s > 1 and instance(s#0,Command) then verifyKey prepend(s#0#0,drop(s,1))
     else error("documentation key for '", formatDocumentTag s, "' encountered, but no method installed"))

verifyKey Array   := s -> (				    -- e.g., [res, Strategy]
     fn := s#0;
     opt := s#1;
     if not instance(fn, Function) and not instance(fn, Sequence)
     then error("expected first element of document key for optional argument to be a function or sequence: ", silentRobustString(40,1,s));
     if not (options fn)#?opt then error("expected ", opt, " to be an option of ", fn))

-----------------------------------------------------------------------------
-- making document tags
-----------------------------------------------------------------------------
-- We need three bits of information about a document tag:
--     the original key	    	    e.g., (operator **,Module,Module)
--     the formatted key            e.g., "Module ** Module"
--     the package                  e.g., Core, or null if there is none
--     the package title            e.g., "Core", or "" if there is none
-- Here we assemble them together, so we don't have to recompute the information later.
DocumentTag = new Type of BasicList
DocumentTag.synonym = "document tag"
new DocumentTag from List := (DocumentTag,x) -> (
     (nkey,fkey,pkg,title) := toSequence x;
     -- if class pkg =!= Package then error("document tag specifies unloaded package: ",toString pkg);
     x)
-- toExternalString DocumentTag := x -> error "can't convert DocumentTag to external string"

pkgTitle = method()
pkgTitle Package := pkg -> if pkg === Core then "Macaulay2Doc" else pkg#"title"
pkgTitle Symbol  := toString
pkgTitle String  := identity
pkgTitle Nothing := x -> ""

makeDocumentTag = method(Dispatch => Thing, Options => {
	  Package => null
	  })
makeDocumentTag DocumentTag := opts -> tag -> tag
mdt := makeDocumentTag Thing := opts -> key -> (
     nkey := normalizeDocumentKey key;
     verifyKey nkey;
     fkey := formatDocumentTag nkey;
     pkg := (
	  if class nkey === Symbol {* and package nkey =!= Core *} then package nkey
	  else if opts#Package =!= null then opts#Package 
	  else packageKey(key, fkey)
	  );
     if pkg === null then error("makeDocumentTag: package cannot be determined: ", nkey);
     new DocumentTag from {
	  if instance(nkey,Symbol) then toString nkey else nkey,
	  fkey, {* pkg *} ,pkgTitle pkg})
makeDocumentTag String := opts -> key -> (
     if match("^ |  +| $", key)
     then (
	  -- this is important, because the names of info nodes get extracted from text where
	  -- lines might be wrapped and multiple spaces are reduced to one:
	  error("expected key to have only single interior spaces:", format key);
	  );
     m := regex("[[:space:]]*::[[:space:]]*",key);
     if m === null then (mdt opts) key
     else (
	  (i,n) := m#0;
	  pkg := substring(0,i,key);
	  key = substring(i+n,key);
	  makeDocumentTag(key,opts,Package => pkg)
	  )
     )
-- a bit of experimentation...
DocumentTag.Key = method(Dispatch => Thing)
DocumentTag.Key DocumentTag := x -> x#0
err := x -> error "expected a document tag; perhaps the function 'hypertext' has not yet been run on hypertext"
DocumentTag.Key Thing := err
protect FormattedKey
DocumentTag.FormattedKey = method(Dispatch => Thing)
DocumentTag.FormattedKey DocumentTag := x -> x#1
DocumentTag.FormattedKey Thing := err
DocumentTag.Package = method(Dispatch => Thing)
DocumentTag.Package DocumentTag := x -> {* x#2 *} error "internal error: old code still using package in DocumentTag?"
DocumentTag.Package Thing := err
protect Title
DocumentTag.Title = method(Dispatch => Thing)
DocumentTag.Title DocumentTag := x -> x#3
DocumentTag.Title Thing := err
DocumentTag ? DocumentTag := (x,y) -> x#1 ? y#1
DocumentTag ? String := (x,y) -> x#1 ? y
String ? DocumentTag := (x,y) -> x ? y#1
toString DocumentTag := net DocumentTag := x -> concatenate ( DocumentTag.Title x, " :: ", DocumentTag.FormattedKey x )
package DocumentTag := DocumentTag.Package
hasDocumentation = key -> (
     tag := makeDocumentTag(key,Package=>null);
     if DocumentTag.Title tag === "" then error("key to be documented is exported by no package: ", DocumentTag.FormattedKey tag);
     pkg := getpkg DocumentTag.Title tag;
     fkey := DocumentTag.FormattedKey tag;
     null =!= fetchRawDocumentation(pkg,fkey))
-----------------------------------------------------------------------------
-- Here we introduce the class FormattedDocumentTag, which contains just the parts of a DocumentTag that are strings
--     the formatted key            e.g., "Module ** Module"
--     the package title            e.g., "Core", or "" if there is none
-- The main point is that toExternalString will work for objects of this type, and thus they can be stored externally
-- as part of the documentation.
FinalDocumentTag = new Type of BasicList
FinalDocumentTag.synonym = "final document tag"
FinalDocumentTag.FormattedKey = method(Dispatch => Thing)
FinalDocumentTag.FormattedKey FinalDocumentTag := x -> x#0
FinalDocumentTag.Title = method(Dispatch => Thing)
FinalDocumentTag.Title FinalDocumentTag := x -> x#1
toFinalDocumentTag = method()
toFinalDocumentTag DocumentTag := x -> new FinalDocumentTag from { DocumentTag.FormattedKey x, DocumentTag.Title x }
FinalDocumentTag ? FinalDocumentTag := (x,y) -> x#0 ? y#0
net FinalDocumentTag := x -> concatenate ( FinalDocumentTag.Title x, " :: ", FinalDocumentTag.FormattedKey x )
toString FinalDocumentTag := x -> error "who wants a string?"

-----------------------------------------------------------------------------
formattedKey = method()
formattedKey DocumentTag := tag -> DocumentTag.FormattedKey tag
formattedKey FinalDocumentTag := tag -> FinalDocumentTag.FormattedKey tag
-----------------------------------------------------------------------------

local exampleOutputFilename
local currentNodeName
local currentHelpTag
fixup := method(Dispatch => Thing)

valueWithText = s -> (
     Text := value PackageDictionary#"Text";
     if member(Text.Dictionary, dictionaryPath) then value s
     else (
     	  sav := dictionaryPath;
	  dictionaryPath = prepend(Text.Dictionary, dictionaryPath);
     	  v := value s;
     	  dictionaryPath = sav;
     	  v))

toExternalStringWithText = s -> (
     Text := value PackageDictionary#"Text";
     if member(Text.Dictionary, dictionaryPath) then toExternalString s
     else (
     	  sav := dictionaryPath;
	  dictionaryPath = prepend(Text.Dictionary, dictionaryPath);
     	  v := toExternalString s;
     	  dictionaryPath = sav;
     	  v))

rawKey := "raw documentation"
rawKeyDB := "raw documentation database"
fetchRawDocumentation = method()
fetchRawDocumentation(Package,String) := (pkg,fkey) -> (		    -- returns null if none
     d := pkg#rawKey;
     if d#?fkey then d#fkey
     else (
	  if pkg#?rawKeyDB then (
	       d = pkg#rawKeyDB;
	       if isOpen d and d#?fkey then valueWithText d#fkey)))
fetchRawDocumentation(String,String) := (pkgtitle,fkey) -> fetchRawDocumentation(getpkg pkgtitle, fkey)
fetchRawDocumentation DocumentTag := tag -> (
     fetchRawDocumentation(getpkg DocumentTag.Title tag, DocumentTag.FormattedKey tag)
     )
fetchRawDocumentation FinalDocumentTag := tag -> (
     fetchRawDocumentation(FinalDocumentTag.Title tag, FinalDocumentTag.FormattedKey tag)
     )

getPrimary = tag -> (
     while (
     	  d := fetchRawDocumentation tag;
     	  d =!= null and d#?global PrimaryTag
	  )
     do tag = d#PrimaryTag;
     tag)

fetchPrimaryRawDocumentation = method()
fetchPrimaryRawDocumentation DocumentTag := tag -> (
     while (
     	  d := fetchRawDocumentation tag;
     	  d =!= null and d#?PrimaryTag
	  )
     do tag = d#PrimaryTag;
     d)

fetchAnyRawDocumentation := (fkey) -> scan(value \ values PackageDictionary, 
     pkg -> if class pkg === Package then (
	  r := fetchRawDocumentation(pkg,fkey);
	  if r =!= null then (
	       if r#?PrimaryTag then r = fetchPrimaryRawDocumentation r#PrimaryTag;
	       break r
	       )))

proKey := "processed documentation"
-- proKeyDB := "processed documentation database"
fetchProcessedDocumentation := (pkg,fkey) -> (		    -- returns null if none
     d := pkg#proKey;
     if d#?fkey then d#fkey
     {*
     else (
	  if pkg#?proKeyDB then (
	       d = pkg#proKeyDB;
	       if isOpen d and d#?fkey then value d#fkey))
     *}
     )

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
-- identifying the package of a document tag
-----------------------------------------------------------------------------
packageKey0 = method(Dispatch => Thing)
packageKey0 Thing := key -> currentPackage
packageKey0 Array := key -> (
     n := youngest apply(toSequence key, package);
     assert( n =!= null );
     n )
packageKey0 Sequence := key -> currentPackage
packageKey = method()
packageKey(Array,String) := (key,fkey) -> packageKey0 key
packageKey(Thing,String) := (key,fkey) -> (
     r := scan(loadedPackages, pkg -> if fetchRawDocumentation(pkg,fkey) =!= null then break pkg);
     if r === null then packageKey0 key else r)
-----------------------------------------------------------------------------
-- formatting document tags
-----------------------------------------------------------------------------
   -- The formatted form should be a human-readable string, and different normalized tags should yield different formatted tags.
   -- The formatted tag is used for two purposes:
   --    for display in menus and links
   --    as the key for access in a database, where the key must be a string

formatDocumentTag           = method(Dispatch => Thing)
	  
alphabet := set characters "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'"

formatDocumentTag Thing    := toString
formatDocumentTag String   := s -> s

fSeqInitialize := (toString,toString) -> new HashTable from {
     (4,NewOfFromMethod) => s -> ("new ", toString s#1, " of ", toString s#2, " from ", toString s#3),
     (4,cohomology,ZZ  ) => s -> ("HH^", toString s#1, "(", toString s#2, ",", toString s#3, ")"),
     (4,homology,ZZ    ) => s -> ("HH_", toString s#1, "(", toString s#2, ",", toString s#3, ")"),
     (3,NewFromMethod  ) => s -> ("new ", toString s#1, " from ", toString s#2),
     (3,NewOfMethod    ) => s -> ("new ", toString s#1, " of ", toString s#2),
     (3,symbol SPACE   ) => s -> (toString s#1, " ", toString s#2),
     (2,symbol <-      ) => s -> (toString s#1, " <- Thing"),  -- assignment statement with left hand side evaluated
     (3,homology,ZZ    ) => s -> ("HH_", toString s#1, " ", toString s#2),
     (3,cohomology,ZZ  ) => s -> ("HH^", toString s#1, " ", toString s#2),
     (2,homology       ) => s -> ("HH ", toString s#1),
-- this one was wrong:
--      (2,cohomology     ) => s -> ("HH ", toString s#1),
     (2,NewMethod      ) => s -> ("new ", toString s#1),
     (3,class,Keyword  ) => s -> (toString s#1, " ", toString s#0, " ", toString s#2),-- infix operator
     (3,class,Symbol   ) => s -> (toString s#1, " ", toString s#0, " ", toString s#2),-- infix operator
     (3,class,Sequence ) => s -> (
	  -- infix assignment operator (really a ternary operator!)
	  (toString s#1, " ", toString s#0#0, " ", toString s#2, " ", toString s#0#1, " Thing")
	  ),
     (2,class,Keyword  ) => s -> (toString s#0, " ", toString s#1),-- prefix operator
     (2,class,Sequence ) => s -> (
	  op := s#0#0;
	  if prefix#?op
	  then (toString op, " ", toString s#1, " ", toString s#0#1, " Thing")
	  else (toString s#1, " ", toString op, " ", toString s#0#1, " Thing")
	  ),
     (2,symbol (*)     ) => s -> (toString s#1, " ", toString s#0), -- postfix operator
     (2,symbol ^*      ) => s -> (toString s#1, " ", toString s#0), -- postfix operator
     (2,symbol _*      ) => s -> (toString s#1, " ", toString s#0), -- postfix operator
     (2,symbol ~       ) => s -> (toString s#1, " ", toString s#0), -- postfix operator
     (2,symbol !       ) => s -> (toString s#1, " ", toString s#0), -- postfix operator
     (2,class,ScriptedFunctor) => s -> (
	  hh := s#0;
	  if hh.?subscript and hh.?superscript 
	  then (
	       stderr << "--warning: ambiguous scripted functor, with both subscript method and superscript method: " << s << endl;
	       toString s
	       )
	  else if hh.?subscript   then (toString s#0, " _ ", toString s#1)
	  else if hh.?superscript then (toString s#0, " ^ ", toString s#1)
	  else (toString s#0, " ", toString s#1)),
     (3,class,ScriptedFunctor,ZZ) => s -> (
	  if s#0 .? subscript
	  then (toString s#0, "_", toString s#1, "(", toString s#2, ")")
	  else (toString s#0, "^", toString s#1, "(", toString s#2, ")")),
     (4,class,ScriptedFunctor,ZZ) => s -> (
	  if s#0 .? subscript
	  then (toString s#0, "_", toString s#1, "(", toString s#2, ",", toString s#3, ")")
	  else (toString s#0, "^", toString s#1, "(", toString s#2, ",", toString s#3, ")")),
     5 => s -> (
	  t := if methodOptions s#0 =!= null then (methodOptions s#0).Dispatch else {Thing,Thing};
	  (toString s#0, "(", 
	       if t#?0 and t#0===Type then "type of ", toString s#1, ",",
	       if t#?1 and t#1===Type then "type of ", toString s#2, ",",
	       if t#?2 and t#2===Type then "type of ", toString s#3, ",",
	       if t#?3 and t#3===Type then "type of ", toString s#4,
	       ")")),
     4 => s -> (
	  t := if methodOptions s#0 =!= null then (methodOptions s#0).Dispatch else {Thing,Thing};
	  (toString s#0, "(", 
	       if t#?0 and t#0===Type then "type of ", toString s#1, ",",
	       if t#?1 and t#1===Type then "type of ", toString s#2, ",",
	       if t#?2 and t#2===Type then "type of ", toString s#3,
	       ")")),
     3 => s -> (
	  t := if methodOptions s#0 =!= null then (methodOptions s#0).Dispatch else {Thing,Thing};
	  (toString s#0, "(", 
	       if t#?0 and t#0===Type then "type of ", toString s#1, ",",
	       if t#?1 and t#1===Type then "type of ", toString s#2,
	       ")")),
     2 => s -> (
	  t := if methodOptions s#0 =!= null then (methodOptions s#0).Dispatch else {Thing,Thing};
	  (toString s#0, "(", 
	       if t===Type or instance(t,List) and t#?0 and t#0===Type then "type of ", toString s#1,
	       ")")),
     1 => s -> (toString s#0, "()")
     }

fSeq := fSeqInitialize(toString,toString)
formatDocumentTag Sequence := record(
     s -> concatenate (
	  if #s == 0                                           then toString
	  else if            fSeq#?(#s,s#0)                    then fSeq#(#s,s#0)
	  else if #s > 1 and fSeq#?(#s,s#0,s#1)                then fSeq#(#s,s#0,s#1)
	  else if #s > 1 and fSeq#?(#s, class, class s#0, s#1) then fSeq#(#s, class, class s#0, s#1)
	  else if            fSeq#?(#s, class, class s#0)      then fSeq#(#s, class, class s#0)
	  else if            fSeq#?(class s#-1,#s)             then fSeq#(class s#-1,#s)
	  else if            fSeq#?#s                          then fSeq#(#s)
							       else toString) s)
formatDocumentTag Array := s -> (
     if class s#0 === Sequence and # s#0 > 0
     then concatenate( toString s#0#0, "(",between(",",apply(drop(s#0,1),toString)),", ", toString s#1, " => ...)" )
     else concatenate( toString s#0, "(..., ", toString s#1, " => ...)" )
     )

-----------------------------------------------------------------------------
-- fixing up hypertext
-----------------------------------------------------------------------------
trimline0 := x -> selectRegexp ( "^((.*[^ \t])?)[ \t]*$",1, x)
trimline  := x -> selectRegexp ( "^[ \t]*((.*[^ \t])?)[ \t]*$",1, x)
trimline1 := x -> selectRegexp ( "^[ \t]*(.*)$",1, x)
addspaces0:= x -> if x#?0 then if x#-1=="." then concatenate(x,"  ") else concatenate(x," ") else concatenate(x," ")
addspaces := x -> if x#?0 then if x#-1=="." then concatenate(x,"  ") else concatenate(x," ") else x

fixup Thing      := z -> error("unrecognizable item inside documentation: ", toString z)
fixup List       := z -> fixup toSequence z
fixup Sequence   := 
fixup Hypertext  := z -> splice apply(z,fixup)
fixup LATER      := identity
fixup Nothing    := x -> ()	      -- so it will get removed by splice later
fixup Option     := identity
fixup BR         := identity
fixup PRE        := identity
fixup CODE       := identity
fixup ExampleItem := identity
fixup LITERAL    := identity
fixup ANCHOR     := identity
fixup TO         := identity
fixup TO2        := identity
fixup TOH        := identity
fixup HREF       := x -> if #x == 2 then HREF{x#0, fixup x#1} else x
deprecated := z -> error (
     if z === PARA then ( "using '", toString z, "' alone in documentation is no longer supported, use 'PARA{...}' around a paragraph" )
     else ("using '", toString z, "' alone in documentation is no longer supported, use '", toString z, "{}' instead" ) )
fixup MarkUpType := z -> (
     if z === PARA or z === BR or z === HR
     then (
	  deprecated z;
	  z{})
     else error("isolated mark up type encountered: ",toString z)
     ) -- convert PARA to PARA{}
-- fixup Function   := z -> z				       -- allow BaseFunction => f 
fixup String     := s -> (				       -- remove clumsy newlines within strings
     if not match("\n",s) then return s;
     ln := separate s;
     concatenate ({addspaces0 trimline0 ln#0}, addspaces \ trimline \take(ln,{1,#ln-2}), {trimline1 ln#-1}))

hypertext = method(Dispatch => Thing)
hypertext Hypertext := fixup
hypertext Sequence := hypertext List := x -> fixup DIV x

-----------------------------------------------------------------------------
-- installing the documentation
-----------------------------------------------------------------------------

Nothing << Thing := (x,y) -> null			    -- turning off the output is easy to do

documentableType := method()
documentableType Thing := x -> false
documentableType Boolean := documentableType HashTable := documentableType Function := documentableType BasicList := documentableType Nothing := documentableType File := x -> true
UndocumentableValue := hashTable { symbol environment => true, symbol commandLine => true }
documentableValue := key -> ( instance(key, Symbol) and value key =!= key and documentableType value key and not UndocumentableValue#?key )

file := null

-----------------------------------------------------------------------------
-- getting database records
-----------------------------------------------------------------------------
extractBody := x -> if x.?Description then x.Description
getDoc := key -> fetchAnyRawDocumentation formatDocumentTag key
getOption := (key,tag) -> (				    -- get rid of this, keep the doc from before
     s := getDoc key;
     if s =!= null and s#?tag then s#tag)
getBody := key -> getOption(key,Description)		    -- get rid of this
-----------------------------------------------------------------------------
-- process examples
-----------------------------------------------------------------------------

extractExamplesLoop            := method(Dispatch => Thing)
extractExamplesLoop Thing       := x -> {}
extractExamplesLoop ExampleItem := toList
extractExamplesLoop Sequence := 
extractExamplesLoop Hypertext := x -> join apply(toSequence x, extractExamplesLoop)

extractExamples := docBody -> (
     ex := extractExamplesLoop docBody;
     -- don't convert "ex" on the next line to a sequence, because the hash code for caching example outputs will change
     if #ex > 0 then currentPackage#"example inputs"#currentNodeName = ex;
     docBody)

M2outputRE := "(\n+)i+[1-9][0-9]* : "
M2outputREindex := 1
separateM2output = method()
separateM2output String := r -> (
     m := regex("^i1 : ",r);
     if m#?0 then r = substring(m#0#0,r);
     while r#?-1 and r#-1 == "\n" do r = substring(0,#r-1,r);
     separateRegexp(M2outputRE,M2outputREindex,r))

makeExampleOutputFileName := (fkey,pkg) -> (			 -- may return 'null'
     if pkg#?"package prefix" and pkg#"package prefix" =!= null 
     then pkg#"package prefix" | replace("PKG",pkg#"title",currentLayout#"packageexampleoutput") | toFilename fkey | ".out"
     )

exampleResults := {}
exampleCounter := 0
checkForExampleOutputFile := (node,pkg) -> (
     exampleCounter = 0;
     exampleResults = {};
     if pkg#"example results"#?node then (
	  exampleResults = pkg#"example results"#node;
	  true)
     else if exampleOutputFilename =!= null and fileExists exampleOutputFilename then (
     	  if debugLevel > 1 then stderr << "--reading example results from " << exampleOutputFilename << endl;
	  exampleResults = pkg#"example results"#node = drop(separateM2output get exampleOutputFilename,-1);
 	  true)
     else false)

currentExampleKey := ""
processExamplesStrict = true
processExamplesLoop = method(Dispatch => Thing)
processExamplesLoop TO :=
processExamplesLoop TO2 :=
processExamplesLoop TOH :=
processExamplesLoop Option :=
processExamplesLoop String := identity
processExamplesLoop Sequence := 
processExamplesLoop Hypertext := x -> apply(x,processExamplesLoop)
processExamplesLoop ExampleItem := x -> (
     ret := (
	  if exampleResults#?exampleCounter
	  then PRE exampleResults#exampleCounter
	  else (
	       if #exampleResults === exampleCounter 
	       then (
		    if processExamplesStrict
		    then error("example results terminate prematurely: ", toString currentExampleKey)
		    else stderr << "--warning: example results terminate prematurely: " << currentExampleKey << endl
		    );
	       PRE concatenate("i", toString (exampleCounter+1), " : -- example results terminated prematurely")
	       ));
     exampleCounter = exampleCounter + 1;
     ret)
processExamples := (pkg,fkey,docBody) -> (
     exampleOutputFilename = makeExampleOutputFileName(fkey,pkg);
     if checkForExampleOutputFile(fkey,pkg) then (
     	  currentExampleKey = fkey;
     	  docBody = processExamplesLoop docBody;
     	  currentExampleKey = "";
	  );
     docBody)

-----------------------------------------------------------------------------
-- 'document' function
-----------------------------------------------------------------------------
fixupEntry := method(Dispatch => Thing)
	-- "x" => List => { "a list of numbers" }
	-- "x" => List => "a list of numbers"
	-- "x" => List
	-- "x" => { "a list of numbers" }
	-- List => { "a list of numbers" }
	-- { "a list of numbers" }
	-- "a list of numbers"
	-- List
	-- "x"
fixupEntry Thing := fixup
fixupEntry Type := identity
fixupEntry Option := z -> z#0 => fixupEntry z#1
fixupList := (x,nm) -> (
     if not instance(x,List) then error(toString nm," => ... : expected a list");
     apply(nonnull x,fixupEntry))
enlist := x -> if class x === List then x else {x}
chkIsString := key -> val -> if class val === String then val else error("expected ",toString key," option to be a string")
chkIsStringFixup := key -> val -> if class val === String then fixup val else error("expected ",toString key," option to be a string")
fixupTable := new HashTable from {
     Key => identity,					    -- this item was processed earlier!
     symbol DocumentTag => identity,
     Usage => val -> (
	  if not instance(val,String) then error "Usage: expected a string";
	  val = nonempty separate val;
	  val = apply(val, i -> replace("^[[:space:]]*(.*)[[:space:]]*$","\\1",i));
	  if #val === 0 then error "Usage: expected content";
	  DL flatten { "class" => "element", DT "Usage:", DD \ TT \ val } ),
     BaseFunction => val -> (if val =!= null and not instance(val,Function) then error "expected BaseFunction option value to be a function"; val),
     Inputs => val -> (
	  val = fixupList(val,Inputs);
	  if #val === 0 then error "Inputs: expected at least one item";
	  val),
     Outputs => val -> (
	  val = fixupList(val,Outputs);
	  if #val === 0 then error "Outputs: expected at least one item";
	  val),
     Consequences => val -> (
	  val = fixupList(val,Consequences);
	  -- if #val === 0 then error "Consequences: expected at least one item";
	  val),
     Headline => chkIsStringFixup Headline,
     Heading => chkIsString Heading,
     Description => val -> extractExamples fixup val,
     Caveat  => v -> if v =!= {} then fixup DIV1 { SUBSECTION "Caveat", DIV v } else v,
     SeeAlso => v -> if v =!= {} then fixup DIV1 { SUBSECTION "See also", UL (TO \ enlist v) } else v,
     SourceCode => v -> (
	  if v =!= {} 
	  then DIV { 
	       "class" => "waystouse",
	       fixup DIV1 {
		    SUBSECTION "Code", 
		    PRE demark(
			 newline,
			 unstack stack apply(enlist v,
			      m -> (
				   f := lookup m;
				   if f === null then error("SourceCode: ", toString m, ": not a method");
				   c := code f;
				   if c === null then error("SourceCode: ", toString m, ": code for method not found");
				   c)))}}
	  else v),
     Subnodes => v -> (
	  v = nonnull enlist v;
	  if #v == 0 then error "encountered empty Subnodes list"
	  else MENU apply(v, x -> fixup (
		    if class x === TO then x
		    else if class x === TOH then TO {x#0}
		    else if class x === String then x
		    else error ("unrecognizable Subnode list item: ",x)))),
     ExampleFiles => v -> (
	  if not currentPackage.Options.AuxiliaryFiles then error "ExampleFiles option specified, but AuxiliaryFiles option is not set to 'true'";
	  if not (instance(v,List) and all(v,fn->instance(fn,String))) then error "expected ExampleFiles option to be a list of strings";
	  auxiliaryFilesDirectory := currentPackage#"source directory" | currentPackage#"title" | "/";
	  v = apply(v, fn -> auxiliaryFilesDirectory | fn);
	  for fn in v do if not fileExists fn then error ("example data file not found: ", fn);
	  currentPackage#"example data files"#currentNodeName = v;
	  "")
     }
caveat := key -> getOption(key,Caveat)
seealso := key -> getOption(key,SeeAlso)
sourcecode := key -> getOption(key,SourceCode)
theMenu := key -> getOption(key,Subnodes)
documentOptions := new OptionTable from {
     Key => null,
     Usage => null,
     BaseFunction => null,
     Inputs => null,
     Outputs => null,
     Consequences => null,
     Headline => null,
     SeeAlso => null,
     SourceCode => null,
     Caveat => null,
     Subnodes => null,
     ExampleFiles => null
     }
reservedNodeNames := set {"Top", "Table of Contents", "Symbol Index"}

storeRawDocumentation := (tag,opts) -> (
     fkey := DocumentTag.FormattedKey tag;
     if currentPackage#rawKey#?fkey and signalDocError tag
     then (
	  stderr << currentFileName << ":" << currentLineNumber() << ": warning: documentation already provided for '" << tag << "'" << endl;
	  doc := currentPackage#rawKey#fkey;
	  stderr << doc#"filename" << ":" << doc#"linenum" << ": ... here is the (end of the) previous documentation" << endl;
	  );
     currentPackage#rawKey#fkey = opts;
     )

undocumented = method(Dispatch => Thing)
undocumented List  := x -> scan(x, undocumented)
undocumented Thing := key -> if key =!= null then (
     tag := makeDocumentTag(key, Package => currentPackage);
     storeRawDocumentation(tag, new HashTable from { 
	       symbol DocumentTag => tag, 
	       "undocumented" => true,
	       "filename" => currentFileName,
	       "linenum" => currentLineNumber()
	       }))

undocumented keys undocumentedkeys
undocumentedkeys = null
undocumented' = x -> error "late use of function undocumented'"

-----------------------------------------------------------------------------
-- getting help from the documentation
-----------------------------------------------------------------------------

getExampleInputs := method(Dispatch => Thing)
getExampleInputs Thing       := t -> ()
getExampleInputs Sequence    := 
getExampleInputs Hypertext   := t -> apply(toSequence t, getExampleInputs)
getExampleInputs ExampleItem := t -> 1 : t#0

examples = method(Dispatch => Thing)
examples Hypertext := x -> stack deepSplice getExampleInputs x
examples Thing := x -> (
     checkLoadDocumentation();
     d := fetchRawDocumentation makeDocumentTag(x,Package=>null);
     if d =!= null and d.?Description then (stack deepSplice getExampleInputs d.Description)^-1)
apropos = method()
apropos String := (pattern) -> (
     last \ sort unique select(
	  flatten \\ pairs \ dictionaryPath, 
	  (nam,sym) -> match(pattern,nam) and not match("\\$",nam)
	  ))
-----------------------------------------------------------------------------
headline = method(Dispatch => Thing)
headline Thing := key -> getOption(key,Headline)	    -- old method
headline FinalDocumentTag := headline DocumentTag := tag -> (
     d := fetchPrimaryRawDocumentation tag;
     if d === null then (
	  d = fetchAnyRawDocumentation formattedKey tag;    -- this is a kludge!  Our heuristics for determining the package of a tag are bad.
	  if d === null then (
	       if signalDocError tag
	       and DocumentTag.Title tag === currentPackage#"title"
	       then (
		    stderr << "--warning: tag has no documentation: " << tag << ", key " 
	       	    << toExternalString {* toExternalString can't work for shadowed symbols without synonyms *} DocumentTag.Key tag 
	       	    << endl);
	       return null;
	       ));
     if d#?Headline then d#Headline
     else headline DocumentTag.Key tag			    -- revert to old method, eliminate?
     )
commentize = s -> if s =!= null then concatenate(" -- ",s) else ""
-----------------------------------------------------------------------------
isMissingDoc = tag -> null === fetchPrimaryRawDocumentation tag
isUndocumented = tag -> (
     d := fetchRawDocumentation tag;
     d =!= null and d#?"undocumented" and d#"undocumented" === true)
isSecondary = tag -> (
     d := fetchRawDocumentation tag;
     d =!= null and d#?PrimaryTag)
-----------------------------------------------------------------------------
-- these menus have to get sorted, so optTO and optTOCLASS return pairs:
--   the first member of the pair is the string to use for sorting
--   the second member is the corresponding hypertext entry in the UL list
counter := 0
next := () -> counter = counter + 1
optTO = i -> (
     i = makeDocumentTag(i,Package=>null);
     fkey := DocumentTag.FormattedKey i;
     if not isUndocumented i then 
     if isSecondary i 
     then (
	  primary := getPrimary i;
	  (DocumentTag.FormattedKey primary, fkey, next(), fixup if currentHelpTag === primary then fkey else SPAN {fkey, ", see ", TOH{primary}})
	  )
     else (fkey                            , fkey, next(), fixup TOH{i})      -- need an alternative here for secondary tags such as (export,Symbol)
     )
optTOCLASS := i -> (					    -- this isn't different yet, work on it!
     r := fixup TOH{i};
     (DocumentTag.FormattedKey first r, next(), r))

ul := t -> if #t =!= 0 then UL t else t
menu       := s -> ul (last \         nonnull \\ optTO      \ toList s)
smenu      := s -> ul (last \ sort \\ nonnull \\ optTO      \ toList s)
smenuCLASS := s -> ul (last \ sort \\ nonnull \\ optTOCLASS \ toList s)

vowels := set characters "aeiouAEIOU"
indefiniteArticle := s -> if vowels#?(s#0) and not match("^one ",s) then "an " else "a "
indefinite := s -> concatenate(indefiniteArticle s, s)
synonym = X -> if X.?synonym then X.synonym else "object of class " | toString X

synonymAndClass := X -> fixup (
     if X.?synonym then SPAN {indefinite X.synonym, " (of class ", TO X, ")"}
     else SPAN {"an object of class ", TO X}
     )     

justClass := X -> fixup SPAN {"an instance of class ", TO X}

ofClass = method()
ofClass List := x -> (
     if #x === 1 then ofClass x#0
     else if #x === 2 then (ofClass x#0, " or ", ofClass x#1)
     else mingle (ofClass \ x, splice( #x - 2 : ", ", ", or " )))
ofClass ImmutableType := ofClass Type := X -> fixup (
     if parent X === Nothing then error "expected a class";
     if X === Nothing then TO "null"
     else if X.?synonym then SPAN {indefiniteArticle X.synonym, TO2 {X, X.synonym}}
     else SPAN {"an object of class ", TO X}
     )

rootPath = "";
rootURI = "file://";
if version#"operating system" === "MicrosoftWindows" then (
     rootPath = first lines get "!cygpath -m /"; 	   -- e.g.: "C:/cygwin"
     rootURI = "file:///" | rootPath;		   -- e.g.: "file:///C:/cygwin"
     )

makeDocBody := method(Dispatch => Thing)
makeDocBody Thing := key -> (
     tag := makeDocumentTag(key,Package=>null);
     pkg := getpkg DocumentTag.Title tag;
     ptag := getPrimary tag;
     rec := fetchRawDocumentation ptag;
     fkey := DocumentTag.FormattedKey ptag;
     if rec =!= null then (
	  docBody := extractBody rec;
	  if docBody =!= null and #docBody > 0 then (
	       docBody = processExamples(pkg, fkey, docBody);
	       if class key === String 
	       then DIV { docBody}
	       else DIV1 { SUBSECTION "Description", DIV {docBody} })
	  else DIV { COMMENT "empty documentation body" }))

topheader := s -> (
     h := headline s;
     HEADER1 { formatDocumentTag s, if h =!= null then " -- ", h })

op := s -> if operator#?s then (
     ss := toString s;
     if match("^[[:alpha:]]*$",ss) then ss = " "|ss|" ";
     fixup DIV {
	  if binary#?s then DIV {
	       PARA {
	       	    "This operator may be used as a binary operator in an expression like ", TT ("x"|ss|"y"), ".  The user may install ", TO "binary methods", "
	       	    for handling such expressions with code such as"
		    },
	       PRE if s === symbol SPACE then "         X Y := (x,y) -> ..." else "         X "|ss|" Y := (x,y) -> ...", 
	       PARA {
		    "where ", TT "X", " is the class of ", TT "x", " and ", TT "Y", " is the class of ", TT "y", "."
		    }
	       },
	  if prefix#?s then DIV {
	       PARA {"This operator may be used as a prefix unary operator in an expression like ", TT (ss|"y"), ".  The user may ", TO2{ "installing methods", "install a method" }, " for handling 
	       	    such expressions with code such as"
	       	    },
	       PRE ("           "|ss|" Y := (y) -> ..."),
	       PARA { "where ", TT "Y", " is the class of ", TT "y", "." }
	       },
	  if postfix#?s then DIV {
	       PARA {
	       	    "This operator may be used as a postfix unary operator in an expression like ", TT ("x "|ss), ".  The user may ", TO2{ ":=", "install a method" }, " for handling 
	       	    such expressions with code such as"
		    },
	       PRE ("         X "|ss|"   := (x) -> ..."),
	       PARA { "where ", TT "X", " is the class of ", TT "x", "." }
	       }
	  }
     )

type := S -> (
     s := value S;
     if not instance(s, Function) and class s =!= Package then DIV1 {
	  "class" => "waystouse",
	  SUBSECTION "For the programmer",  
	  fixup PARA deepSplice { "The object ", TO S, " is ", ofClass class s,
	       if parent s =!= Nothing then (
		    f := (T -> while T =!= Thing list parent T do T = parent T) s;
		    if #f>1 then ", with ancestor classes " else if #f == 1 then ", with ancestor class " else ", with no ancestor class.", 
		    toSequence between(" < ", f / (T -> TO T))),
	       "."},
          op S
	  })

typicalValue := k -> (
     if typicalValues#?k then typicalValues#k 
     else if class k === Sequence and typicalValues#?(k#0) then typicalValues#(k#0)
     else Thing
     )

types := method(Dispatch => Thing)
types Thing := x -> ({},{})
types Function := x -> ({},{typicalValue x})
types Sequence := x -> (
     if #x > 1 and instance(x#-1,Symbol) 
     then ({},{})					    -- it's an option ...
     else (
	  x' := select(drop(toList x,1), T -> not ancestor(Nothing,T)); -- putting something like OO in the key indicates a fake dispatch
	  if instance(x#0,Sequence) and #x#0 === 2 and x#0#1 === symbol =
	  or #x == 2 and x#0 === symbol <-
          then ( x' | { Thing }, { Thing } )	   -- it's an assignment method
	  else ( x'            , { typicalValue x } )
	  ))

emptyOptionTable := new OptionTable from {}
getOptionDefaultValues := method(Dispatch => Thing)
getOptionDefaultValues Symbol := x -> if value x =!= x then getOptionDefaultValues value x else emptyOptionTable
getOptionDefaultValues Thing := x -> emptyOptionTable
getOptionDefaultValues Function := f -> (
     o := options f;
     if o =!= null then o else emptyOptionTable)
getOptionDefaultValues Sequence := s -> (
     o := options s;
     if o =!= null then o else if s#?0 and instance(s#0, Function) then getOptionDefaultValues s#0 else emptyOptionTable)

istype := X -> parent X =!= Nothing
isId := x -> instance(x,String) and match(///\`[[:alnum:]']+\'///,x)
mapo := f -> g := x -> if instance(x, Option) then ( f x#0 ; g x#1 ) else f x
processInputOutputItems := (key,fn) -> x -> (
     optsymb := null;
     idname := null;
     type := null;
     text := null;
     (mapo (y -> 
	       if optsymb === null and instance(y,Symbol) then optsymb = y
	       else if idname === null and isId y then idname = y
     	       else if istype y then (
		    if type === null 
		    or y === Nothing -- putting type Nothing in the doc's input list means don't display the type deduced from the description of the method
		    then type = y
		    else if type =!= y then error("type mismatch: ",toString type, " =!= ", toString y, " in documentation for ", toExternalString key)
		    )
	       else if text === null then text = y
	       else error("can't parse input/output item in documentation for ",toString key)
	       )
	  ) x;
     default := if optsymb =!= null and text =!= null and #text > 0 then (
	  if fn === null or fn === ()
	  then error ("default value for option ",toString optsymb, " not accessible, base function not specified (with BaseFunction => ...)");
     	  opts := if instance(key,Sequence) and options key =!= null then options key else options fn;
	  if not opts#?optsymb
	  then error("symbol ",optsymb," is not the name of an optional argument for function '",fn,"'");
	  t := toString opts#optsymb;
	  if not match("^\\{\\*Function",t) then SPAN{"default value " ,t});
     r := SPAN splice between_", " nonnull nonempty { 
	  if idname =!= null then TT idname, 
	  if type =!= null and type =!= Nothing then ofClass type, -- type Nothing, treated as above
	  default,
	  text
	  };
     if optsymb =!= null then r = (
	  if #r == 0
	  then SPAN between(", ", 
	       nonnull ( 
		    TO2{ 
			 [ if options key =!= null then key else fn, optsymb],
			 concatenate(toString optsymb," => ...") },
		    LATER { () -> commentize (headline [fn,optsymb]) }
		    ))
	  else SPAN (TT ( toString optsymb, " => " ), r));
     r)

document = method(Options => documentOptions)
document List := opts -> args -> (
     if opts =!= documentOptions then error "'document' expects its optional arguments inside the list";
     args = toSequence args;
     if currentPackage === null then error "encountered 'document' command, but no package is open";
     o := new MutableHashTable;
     scan(args, arg -> if class arg === Option then (
	       key := arg#0;
	       if not documentOptions#?key then error("unknown documentation option '", key, "'") ;
	       if o#?key then error("option ",toString key," encountered twice");
	       o#key = arg#1));
     args = select(args, arg -> class arg =!= Option);
     if not o.?Key then error "missing Key";
     key := o.Key;
     rest := {};
     if class key === List then (
	  key = nonnull key;
	  rest = drop(key,1);
	  o.Key = key = first key;
	  );
     o.DocumentTag = tag := makeDocumentTag(key,Package=>null);
     verfy := (key,tag) -> (
     	  if DocumentTag.Title tag =!= currentPackage#"title" 
	  then error("item to be documented comes from another package: ", DocumentTag.Title tag, " :: ", toString key));
     verfy(key,tag);
     scan(rest, secondary -> (
	       tag2 := makeDocumentTag(secondary,Package=>null);
	       verfy(secondary,tag2);
	       storeRawDocumentation(tag2, new HashTable from { 
			 PrimaryTag => tag,
			 symbol DocumentTag => tag2,
	       		 "filename" => currentFileName,
	       		 "linenum" => currentLineNumber()
			 })));
     if not o.?Headline and class key === Sequence and key#?0 then (
	  h := headline key#0;
	  if h =!= null then o.Headline = h;
	  );
     currentNodeName = DocumentTag.FormattedKey tag;
     if reservedNodeNames#?currentNodeName then error("'document' encountered a reserved node name '",currentNodeName,"'");
     o.Description = toList args;
     exampleOutputFilename = makeExampleOutputFileName(currentNodeName,currentPackage);
     scan(keys o, key -> o#key = fixupTable#key o#key);
     if o.?Headline and o.Headline === "" then remove(o,Headline);
     if o.?Usage and o.Usage === "" then remove(o,Usage);
     -- pre-processing of inputs and outputs
     if not o.?Usage and (o.?Inputs or o.?Outputs) then error "document: Inputs or Outputs specified, but Usage not provided";
     inp := if o.?Inputs then o.Inputs else {};
     out := if o.?Outputs then o.Outputs else {};
     iso := x -> instance(x,Option) and #x==2 and instance(x#0,Symbol);
     ino := select(inp, x -> iso x);
     inoh:= new HashTable from ino;
     inp = select(inp, x -> not iso x);
     opt := getOptionDefaultValues key;
     fn := if instance(key, Sequence) then key#0 else if instance(key,Symbol) then value key else key;
     if not isSubset(keys inoh, keys opt) then error concatenate("not among the options for ", toString fn, ": ", between_", " keys (set keys inoh - set keys opt));
     ino = join(ino, sortByName (keys opt - set keys inoh));
     if o.?Usage then (
	  (inp',out') := types key;
	  inp' = select(inp', T -> T =!= Nothing);
	  out' = select(out', T -> T =!= Nothing);
	  if out' === {Thing} then out' = {};		    -- not informative enough
	  if #inp === 0 then inp = inp';
	  if #out === 0 then out = out';
	  if #inp' =!= 0 then (
	       if #inp =!= #inp' then error ("mismatched number of inputs in documentation for ", toExternalString key);
	       inp = apply(inp',inp,(T,v) -> T => v);
	       );
	  if #out' =!= 0 then (
	       if #out =!= #out' then error ("mismatched number of outputs in documentation for ", toExternalString key);
	       out = apply(out',out,(T,v) -> T => v);
	       );
	  );
     proc := processInputOutputItems(key,fn);
     inp = proc \ inp;
     out = proc \ out;
     ino = proc \ ino;
     if #inp>0 then o.Inputs = inp else remove(o,Inputs);
     if #out>0 then o.Outputs = out else remove(o,Outputs);
     if #ino>0 then o#"optional inputs" = ino else remove(o,"optional inputs");
     -- end of pre-processing
     scan(keys fixupTable, sym -> if o#?sym then (
	       if sym === Consequences then scan(o#sym, x -> validate DIV x)
	       else if sym === Key then null
	       else if sym === BaseFunction then null
	       else if sym === symbol DocumentTag then null
	       else validate DIV o#sym
	       )
	  );
     o#"filename" = currentFileName;
     o#"linenum" = currentLineNumber();
     o = new HashTable from o;
     assert( not o.?BaseFunction or instance( o.BaseFunction, Function ) );
     storeRawDocumentation(tag, o);
     currentNodeName = null;
     )

SYNOPSIS = method(
     Dispatch => Thing,
     TypicalValue => Hypertext,
     Options => {
	  Heading => "Synopsis",
	  Usage => "",
	  BaseFunction => null,
	  Inputs => {},
     	  Consequences => {},
	  -- Caveat => {},
	  -- SeeAlso => {},
	  -- SourceCode => {},
	  Outputs => {}
	  }
     )
SYNOPSIS List := o -> x -> SYNOPSIS splice (o, toSequence x)
SYNOPSIS Thing := SYNOPSIS Sequence := o -> x -> (
     o = applyPairs(o, (k,v) -> (k,if v =!= {} then fixupTable#k v else v));
     fn := o#BaseFunction;
     proc := processInputOutputItems(,fn);
     fixup DIV nonnull {
	  SUBSECTION o.Heading,
	  UL {
	       LI o.Usage,
	       if # o.Inputs > 0 then LI { "Inputs:", UL ( proc \ o.Inputs ) },
	       if # o.Outputs > 0 then LI { "Outputs:", UL ( proc \ o.Outputs ) },
	       if # o.Consequences > 0 then LI { "Consequences:", UL o.Consequences }
	       },
	  x
	  })

documentableMethods := s -> select(methods s,isDocumentableMethod)

fmeth := f -> (						    -- compare with documentationValue(Symbol,Function) below
     b := documentableMethods f;
     if #b > 0 then (
	  c := smenu b;
	  if #c > 0 then DIV1 { SUBSECTION { "Ways to use ", toString f }, c } 
	  )
     )

briefSynopsis := key -> (
     o := getDoc key;
     if o === null then return null;
     r := nonnull {
	  if o.?Usage then o.Usage,
	  if o.?BaseFunction then (
	       assert( o.BaseFunction =!= null );
	       SPAN { "Function: ", TO o.BaseFunction })
	  else if instance(key, Sequence) and key#?0 then (
	       if instance(key#0, Function) then SPAN { "Function: ", TO key#0 }
	       else if instance(key#0, Keyword) then SPAN { "Operator: ", TO key#0 }
	       else if instance(key#0, ScriptedFunctor) then SPAN { "Scripted functor: ", TO key#0 }
	       else if instance(key#0,Sequence) and #key#0 === 2 and key#0#1 === symbol =
	       then SPAN { "Operator: ", TO key#0#0 }	    -- assignment operator for this operator
	       ),
	  if o.?Inputs then DIV1 { "Inputs:", UL o.Inputs },
	  if o#?"optional inputs" then DIV1 { TO2{ "using functions with optional inputs", "Optional inputs"}, ":", UL o#"optional inputs" },
	  if o.?Outputs then DIV1 { "Outputs:", UL o.Outputs },
	  if o.?Consequences and #o.Consequences > 0 then DIV1 { "Consequences:", UL o.Consequences },
	  };
     if #r > 0 then fixup UL r)

synopsis := key -> (
     s := briefSynopsis key;
     if s =!= null then DIV1 { SUBSECTION "Synopsis", s }
     )

noBriefDocThings := hashTable { symbol <  => true, symbol >  => true, symbol == => true }
briefDocumentation = method(Dispatch => Thing)

briefDocumentation VisibleList := x -> null

briefDocumentation Thing := x -> (
     if noBriefDocThings#?x or not isDocumentableThing x then return null;
     if package x === Core then checkLoadDocumentation();
     r := briefSynopsis normalizeDocumentKey x;
     if r =!= null then << endl << r << endl
     else (
	  if headline x =!= null then << endl << commentize headline x << endl;
	  );
     if instance(x, Function) or instance(x, ScriptedFunctor) then (
	  s := fmeth x;
	  if s =!= null then << endl << s << endl;))

help = method(Dispatch => Thing)
help String := key -> (
     checkLoadDocumentation();
     if unformatTag#?key then help unformatTag#key 
     else if isGlobalSymbol key then (
	  t := getGlobalSymbol key;
	  help t)
     else (
	  b := makeDocBody key;
	  if b === null then (
	       stderr << "--warning: there is no documentation for '" << key << "'" << endl;
	       b = ();
	       );
	  fixup DIV {topheader key, b, caveat key, seealso key, theMenu key}))

instances = method()
instances Type := HashTable => X -> hashTable apply(select(flatten(values \ dictionaryPath), i -> instance(value i,X)), i -> (i,value i))

reverseOptionTable := null
addro := (sym,meth) -> (
     if not reverseOptionTable#?sym then reverseOptionTable#sym = new MutableHashTable;
     reverseOptionTable#sym#meth = true;
     )
initializeReverseOptionTable := () -> (
     reverseOptionTable = new MutableHashTable;
     scan(dictionaryPath, 
	  d -> scan(values d,
	       x -> (
		    x = value x;
		    if instance(x, Type) then (
			 scan(pairs x, (m,mf) -> (
				   if (instance(m,Sequence) or instance(m,MethodFunctionWithOptions)) and instance(mf,Function) 
				   then (
					om := options mf;
					if om =!= null then (
					     if instance(m,MethodFunctionWithOptions) then m = (m,x);
					     scanKeys(om, s -> addro(s,m)))))))
		    else if instance(x, Function) then (
			 o := options x;
			 if o =!= null then scanKeys(o, s -> addro(s,x)))))))
optionFor := s -> (
     initializeReverseOptionTable();
     ret := if reverseOptionTable#?s then keys reverseOptionTable#s else {};
     reverseOptionTable = null;
     ret)

documentationValue := method()

documentationValue(Symbol,Type) := (s,X) -> (
     syms := unique flatten(values \ dictionaryPath);
     a := smenu apply(select(pairs typicalValues, (key,Y) -> Y===X and isDocumentableMethod key), (key,Y) -> key);
     b := smenu(toString \ select(syms, y -> instance(value y, Type) and parent value y === X));
     c := smenu select(documentableMethods X, key -> not typicalValues#?key or typicalValues#key =!= X);
     e := smenu(toString \ select(syms, y -> not mutable y and class value y === X));
     DIV splice (
	  "class" => "waystouse",
	  if #b > 0 then ( SUBSECTION {"Types of ", if X.?synonym then X.synonym else toString X, " :"}, b),
	  if #a > 0 then ( SUBSECTION {"Functions and methods returning ", indefinite synonym X, " :"}, a ),
	  if #c > 0 then ( SUBSECTION {"Methods that use ", indefinite synonym X, " :"}, c),
	  if #e > 0 then ( SUBSECTION {"Fixed objects of class ", toString X, " :"}, e)))

documentationValue(Symbol,ScriptedFunctor) := 
documentationValue(Symbol,Keyword) := 
documentationValue(Symbol,Function) := (s,f) -> (	    -- compare with fmeth above
     a := smenu documentableMethods f;
     if #a > 0 then DIV ( "class" => "waystouse", SUBSECTION {"Ways to use ", TT toString f, " :"}, a))

documentationValue(Symbol,Thing) := (s,x) -> ()

authorDefaults := new HashTable from { Name => "Anonymous", Email => null, HomePage => null }
documentationValue(Symbol,Package) := (s,pkg) -> if pkg =!= Core then (
     e := toSequence pkg#"exported symbols";
     a := select(e,x -> instance(value x,Function) or instance(value x,Command)); -- functions and commands
     b := select(e,x -> instance(value x,Type));	    -- types
     m := unique flatten for T in b list for i in keys value T list (-- methods
	  if (
	       class i === Sequence and #i > 1
	       and ( 
		    instance(i#0, Symbol) and i#1 =!= symbol =
		    or
		    instance(i#0, Function)
		    ) 
	       and isDocumentableMethod i)
	  then i
	  else if (instance(i,Keyword) or instance(i,Function) or instance(i,ScriptedFunctor)) and isDocumentableMethod (i,value T)
	  then (i,value T)
	  else continue);
     c := select(e,x -> instance(value x,Symbol));	    -- symbols
     d := toList(set e - set a - set b - set c); -- other things
     fn := pkg#"title" | ".m2";
     au := pkg.Options.Authors;
     (
     	  if #au > 0 then DIV1 {
     	       SUBSECTION (if #au === 1 then "Author" else "Authors"), 
	       fixup UL apply(au,
		    au -> (
			 if class au =!= List then error "expected author to be a list";
			 (defs,args) := override (authorDefaults, toSequence au);
			 if #args > 0 then error "expected Author to be a list of options";
			 nam := defs.Name;
			 if defs.HomePage =!= null then nam = HREF{defs.HomePage, nam};
			 em := defs.Email;
			 if em =!= null then em = SPAN{" <",HREF{concatenate("mailto:",em),em},">"};
			 DIV1 {nam,em}
			 )
		    )
	       },
	  if (cert := pkg.Options.Certification) =!= null then (
	       if not instance(cert,List) then error(toString pkg, ": Certification option: expected a list");
	       if not all(cert,x -> instance(x,Option) and #x==2) then error(toString pkg, ": Certification option: expected a list of options");
	       cert = new HashTable from cert;
	       DIV1 { 
		    SUBSECTION {
			 "Certification ",
			 IMG { "src" => replace("PKG","Style",currentLayout#"package") | "GoldStar.png", "alt" => "a gold star"}
			 },
		    PARA {
			 "Version ",BOLD cert#"version at publication"," of this package was accepted for
			 publication in ",HREF{cert#"volume URI","volume " | cert#"volume number"}," of the 
			 journal ",HREF{cert#"journal URI",cert#"journal name"}," on ",cert#"acceptance date",", in the 
			 article ",HREF{cert#"published article URI",cert#"article title"},".  That version can be 
			 obtained ", HREF{cert#"published code URI","from the journal"}, " or from the ", EM "Macaulay2", " source code
			 repository, ", 
			 -- make a hot link if it's github, but not if it's svn:
			 if match("github.com",cert#"repository code URI") 
			 then HREF{cert#"repository code URI",cert#"repository code URI"}
			 else TT cert#"repository code URI",
			 ", ",
			 -- github calls it a commit number, svn calls it a release number:
			 if match("github.com",cert#"repository code URI") then "commit" else "release",
			 " number ",toString cert#"release at publication","."
			 }
		    }
	       ),
	  DIV1 { SUBSECTION "Version", "This documentation describes version ", BOLD pkg.Options.Version, " of ",
	       if pkg#"title" === "Macaulay2Doc" then "Macaulay2" else pkg#"title",
	       "." },
	  if pkg#"title" =!= "Macaulay2Doc" 
	  then DIV1 {
	       SUBSECTION "Source code",
	       "The source code from which this documentation is derived is in the file ", HREF { installationLayout#"packages" | fn, fn }, ".",
	       if pkg#?"auxiliary files" then (
		    "  The auxiliary files accompanying it are in the
		    directory ", HREF { installationLayout#"packages" | pkg#"title" | "/", pkg#"title" | "/" }, "."
		    )
	       },
	  if #e > 0 then DIV1 {
	       SUBSECTION "Exports",
	       DIV1 {
	       	    "class" => "exports",
		    fixup UL {
			 if #b > 0 then DIV1 {"Types", smenu b},
			 if #a > 0 then DIV1 {"Functions and commands", smenu a},
			 if #m > 0 then DIV1 {"Methods", smenu m},
			 if #c > 0 then DIV1 {"Symbols", smenu c},
			 if #d > 0 then DIV1 {"Other things", smenuCLASS d}}}}))

theAugmentedMenu := S -> (
     f := value S;
     menu := theMenu S;
     if menu === null then menu = MENU {};
     methsInMenu := DocumentTag.Key \ first \ select(toList menu, item -> class item === TO);
     otherMeths := documentableMethods f - set methsInMenu;
     if #otherMeths > 0 then (
	  menu = join(
	       menu,
	       { concatenate( if #menu > 0 then "Other ways to use " else "Ways to use ", TT toString f, ":" ) },
	       fixup apply(otherMeths, m -> TO m));
	  );
     if #menu > 0 then menu
     )

help Symbol := S -> (
     checkLoadDocumentation();
     -- s := value S;
     if package S === Core then checkLoadDocumentation();
     currentHelpTag = makeDocumentTag(S,Package=>null);
     a := smenu apply(select(optionFor S,f -> isDocumentableMethod f), f -> [f,S]);
     -- b := smenu documentableMethods s;
     ret := fixup DIV { topheader S, synopsis S, makeDocBody S,
	  if #a > 0 then DIV1 { SUBSECTION {"Functions with optional argument named ", toExternalString S, " :"}, a},
-- 	  if #b > 0 then DIV ( "class" => "waystouse", SUBSECTION {"Ways to use ", toExternalString s, " :"}, b),
          caveat S, seealso S,
     	  documentationValue(S,value S),
	  sourcecode S, type S, 
	  theMenu S
	  -- if instance(value S, Function) then theAugmentedMenu S else theMenu S
	  };
     currentHelpTag = null;
     ret)

help DocumentTag := tag -> help DocumentTag.Key tag

help Array := key -> (		    -- optional argument
     checkLoadDocumentation();
     fn := key#0;
     opt := key#1;
     if not (options fn)#?opt then error ("function ", fn, " does not accept option key ", opt);
     default := (options fn)#opt;
     fixup DIV {
	  topheader key, synopsis key, makeDocBody key,
	  SUBSECTION "Further information", 
	  UL {
	       SPAN{ "Default value: ", if isDocumentableThing default and hasDocumentation default then TO {default} else TT toString default },
	       SPAN{
		    assert ( fn =!= null ); 
		    if class fn === Sequence then "Method: " else "Function: ", TOH {fn}
		    },
	       SPAN{ "Option name: ", TOH {opt} }
	       },
	  caveat key, seealso key, theMenu key })

help Sequence := key -> (						    -- method key
     checkLoadDocumentation();
     if key === () then return if inDebugger then debuggerUsageMessage else help "initial help" ;
     if null === lookup key then error("expected ", toString key, " to be a method");
     currentHelpTag = makeDocumentTag(key,Package=>null);
     ret := fixup DIV { topheader key, synopsis key, makeDocBody key, caveat key, sourcecode key, seealso key, theMenu key };
     currentHelpTag = null;
     ret)

help List := v -> DIV between(hr,help \ v) 

help Thing := x -> if hasAttribute(x,ReverseDictionary) then return help getAttribute(x,ReverseDictionary) else error "no documentation found"

tmp := help
help = Command help
setAttribute(tmp,ReverseDictionary,symbol help)		    -- kludge

pager = x -> (
     if height stdio > 0
     then "!" | (if getenv "PAGER" == "" then "more" else getenv "PAGER") << x << close 
     else << x << endl ;)

infoHelp = key -> (
     tag := makeDocumentTag(key,Package=>null);
     t := infoTagConvert tag;
     chkrun ("info "|format t);)


-----------------------------------------------------------------------------

mat := (pat,line) -> class line === String and match(pat,line)

tutorial = x -> (
     x = lines x;
     x = select(x, line -> not mat("^[[:space:]]*$",line));
     head := false;
     x = apply(x, line -> (
	       if mat("^---",line) then (head = not head;) 
	       else if head then HEADER4 replace("^-- *","",line)
	       else line));
     p1 := reverse positions(x, line -> mat("^--\\^$",line));
     p2 := reverse positions(x, line -> mat("^--\\$$",line));
     if #p1 != #p2 then error "unmatched --^ --$ pairs";
     scan(#p1, 
	  i -> x = join(
	       take(x,{0,p1#i-1}),
	       {concatenate between_newline take(x,{p1#i+1,p2#i-1})},
	       take(x,{p2#i+1,#x-1})));
     p1 = reverse positions(x, line -> mat("^--PRE\\^$",line));
     p2 = reverse positions(x, line -> mat("^--PRE\\$$",line));
     if #p1 != #p2 then error "unmatched --PRE^ --PRE$ pairs";
     scan(#p1, 
	  i -> x = join(
	       take(x,{0,p1#i-1}),
	       {PRE concatenate between_newline apply(take(x,{p1#i+1,p2#i-1}),line -> replace("^--","",line))},
	       take(x,{p2#i+1,#x-1})));
     x = apply(x, line -> if mat("^--$",line) then PARA{} else line);
     x = sublists(x,
	  line -> class line === String and match("^--",line),
	  sublist -> PARA TEX concatenate between(newline,apply(sublist,line -> replace("^-- *","",line))),
	  identity);
     x = sublists(x,
	  line -> class line === String,
	  sublist -> EXAMPLE sublist,
	  identity);
     x )

Wikipedia = method(TypicalValue => Hypertext)
Wikipedia String := s -> PARA { "See ", HREF{ "http://en.wikipedia.org/wiki/" | s }, "."}

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
