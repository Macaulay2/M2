--		Copyright 1994-2002 by Daniel R. Grayson

checkLoadDocumentation = () -> if not isGlobalSymbol "Macaulay2" or class value getGlobalSymbol "Macaulay2" =!= Package then (
     -- the documentation for things in the package Macaulay2Core is in the package Macaulay2 !
     oldnotify := notify;
     notify = false;
     loadPackage "Macaulay2";
     notify = oldnotify;
     )

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
normalizeDocumentKey := method(SingleArgumentDispatch => true)
normalizeDocumentKey   String := key -> if isGlobalSymbol key then getGlobalSymbol key else key
normalizeDocumentKey    Array := identity
normalizeDocumentKey   Symbol := identity
normalizeDocumentKey Sequence := identity
normalizeDocumentKey  Nothing := key -> symbol null
normalizeDocumentKey    Thing := key -> (
     if ReverseDictionary#?key then return ReverseDictionary#key;
     error("encountered unidentifiable document tag: ",key);
     )

isDocumentableThing  = method(SingleArgumentDispatch => true)
isDocumentableThing    String := key -> true
isDocumentableThing    Symbol := key -> true
isDocumentableThing  Sequence := key -> false 		    -- we're not looking for documentable methods here, just documentable objects
isDocumentableThing   Nothing := key -> true

errorMethod := key -> typicalValues#?key and typicalValues#key === Error

isDocumentableMethod = method(SingleArgumentDispatch => true)
isDocumentableMethod Sequence := key -> (
     all(key, i -> (
     	       class i === Sequence 			    -- assignment methods look like ((symbol *, symbol =), X, Y, Z)
     	       or isDocumentableMethod i)) 
     and not methodDispatchFunctions#?(functionBody lookup key)
     and not errorMethod key
     -- and not isUndocumented makeDocumentTag key
     )
isDocumentableMethod    Thing := key -> false
isDocumentableMethod   Symbol := key -> isGlobalSymbol toString key and getGlobalSymbol toString key === key
isDocumentableMethod     Type := 
isDocumentableThing     Thing := key -> ReverseDictionary#?key
isDocumentableMethod Function := fn -> ReverseDictionary#?fn and dictionary ReverseDictionary#fn =!= null
isDocumentableMethod ScriptedFunctor := fn -> ReverseDictionary#?fn

-----------------------------------------------------------------------------
-- verifying the tags
-----------------------------------------------------------------------------
-- here we check that the method a putative document tag documents is actually installed
verifyKey = method(SingleArgumentDispatch => true)
verifyKey Thing    := s -> null

verifyKey Sequence := s -> (				    -- e.g., (res,Module) or (symbol **, Module, Module)
     if not (
	  if #s > 2 then (
	       t := youngest drop(s,1);	                    -- this will all get screwed up with immutable types present
	       t#?s
	       and
	       instance(t#s, Function)
	       )
	  else if #s == 2 then (
	       s#1#?(s#0)
	       and
	       instance(s#1#(s#0), Function)
	       )
	  else (
	       false
	       )
	  )
     then error("documentation key for '", formatDocumentTag s, "' encountered, but no method installed"))

verifyKey Array   := s -> (				    -- e.g., [res, Strategy]
     fn := s#0;
     opt := s#1;
     if not instance(fn, Function) then error "expected first element of document key for optional argument to be a function";
     if not (options fn)#?opt then error("expected ", opt, " to be an option of ", fn))

-----------------------------------------------------------------------------
-- making document tags
-----------------------------------------------------------------------------
-- We need three bits of information about a document tag:
--     the original key	    	    e.g., (operator **,Module,Module)
--     the formatted key            e.g., "Module ** Module"
--     the package                  e.g., Macaulay2Core, or null if there is none
--     the package title            e.g., "Macaulay2Core", or "" if there is none
-- Here we assemble them together, so we don't have to recompute the information later.
DocumentTag = new Type of BasicList
DocumentTag.synonym = "document tag"
new DocumentTag from List := (DocumentTag,x) -> (
     (nkey,fkey,pkg,title) := toSequence x;
     -- if class pkg =!= Package then error("document tag specifies unloaded package: ",toString pkg);
     x)
-- toExternalString DocumentTag := x -> error "can't convert DocumentTag to external string"

pkgTitle = method()
pkgTitle Package := pkg -> pkg#"title"
pkgTitle Symbol  := toString
pkgTitle String  := identity
pkgTitle Nothing := x -> ""

makeDocumentTag = method(SingleArgumentDispatch => true, Options => {
	  Package => null
	  })
makeDocumentTag DocumentTag := opts -> tag -> tag
mdt := makeDocumentTag Thing := opts -> key -> (
     nkey := normalizeDocumentKey key;
     verifyKey nkey;
     fkey := formatDocumentTag nkey;
     pkg := if opts#Package =!= null then opts#Package else packageKey fkey;
     new DocumentTag from {nkey,fkey,pkg,pkgTitle pkg})
makeDocumentTag String := opts -> key -> (
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
toString DocumentTag := net DocumentTag := x -> concatenate ( DocumentTag.Title x, " :: ", DocumentTag.FormattedKey x )
package DocumentTag := DocumentTag.Package
hasDocumentation = key -> (
     tag := makeDocumentTag key;
     pkg := DocumentTag.Package tag;
     fkey := DocumentTag.FormattedKey tag;
     null =!= fetchRawDocumentation(pkg,fkey))
-----------------------------------------------------------------------------
-- Here we introduce the class FormattedDocumentTag, which contains just the parts of a DocumentTag that are strings
--     the formatted key            e.g., "Module ** Module"
--     the package title            e.g., "Macaulay2Core", or "" if there is none
-- The main point is that toExternalString will work for objects of this type, and thus they can be stored externally
-- as part of the documentation.
FinalDocumentTag = new Type of BasicList
FinalDocumentTag.synonym = "final document tag"
FinalDocumentTag.FormattedKey = method(SingleArgumentDispatch => true)
FinalDocumentTag.FormattedKey FinalDocumentTag := x -> x#0
FinalDocumentTag.Title = method(SingleArgumentDispatch => true)
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

local exampleBaseFilename
local currentNodeName
local currentHelpTag
fixup := method(SingleArgumentDispatch => true)

rawKey := "raw documentation"
rawKeyDB := "raw documentation database"
fetchRawDocumentation = method()
fetchRawDocumentation(Symbol,String) := (pkg,fkey) -> (
     if toString pkg === "Macaulay2" then (
	  erase pkg;
	  checkLoadDocumentation();
	  pkg = value getGlobalSymbol "Macaulay2";
	  assert(class pkg === Package);
	  fetchRawDocumentation(pkg,fkey))
     else error("package ", toString pkg, " not loaded, and its documentation is not available"))
fetchRawDocumentation(Package,String) := (pkg,fkey) -> (		    -- returns null if none
     d := pkg#rawKey;
     if d#?fkey then d#fkey
     else (
	  if pkg#?rawKeyDB then (
	       d = pkg#rawKeyDB;
	       if isOpen d and d#?fkey then value d#fkey)))
fetchRawDocumentation(String,String) := (pkgtitle,fkey) -> (
     needsPackage pkgtitle;				    -- maybe later we should dismiss this package if it wasn't already open!
     if PackageDictionary#?pkgtitle and instance(pkg := value PackageDictionary#pkgtitle, Package) then fetchRawDocumentation(pkg,fkey)
     else (stderr << "--warning: no package named " << pkgtitle << endl;)
     )
fetchRawDocumentation DocumentTag := tag -> (
     fetchRawDocumentation(DocumentTag.Package tag, DocumentTag.FormattedKey tag)
     )
fetchRawDocumentation FinalDocumentTag := tag -> (
     fetchRawDocumentation(FinalDocumentTag.Title tag, FinalDocumentTag.FormattedKey tag)
     )

getPrimary = tag -> (
     while (
     	  d := fetchRawDocumentation tag;
     	  d =!= null and d#?PrimaryTag
	  )
     do tag = d#PrimaryTag;
     tag)

fetchPrimaryRawDocumentation := tag -> (
     while (
     	  d := fetchRawDocumentation tag;
     	  d =!= null and d#?PrimaryTag
	  )
     do tag = d#PrimaryTag;
     d)

fetchAnyRawDocumentation := (fkey) -> scan(value \ values PackageDictionary, 
     pkg -> if class pkg === Package then (
	  r := fetchRawDocumentation(pkg,fkey);
	  if r =!= null then break r))

proKey := "processed documentation"
proKeyDB := "processed documentation database"
fetchProcessedDocumentation := (pkg,fkey) -> (		    -- returns null if none
     d := pkg#proKey;
     if d#?fkey then d#fkey
     else (
	  if pkg#?proKeyDB then (
	       d = pkg#proKeyDB;
	       if isOpen d and d#?fkey then value d#fkey)))

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
-- If we don't find it in a package, then we assume it's missing, and assign it to the "Missing" package.  -- or not...
-- That way we can compute the package during the loading of a package, while just some of the documentation has been installed.
-- Missing documentation can be detected when the package is closed, or later.
packageKey = method(SingleArgumentDispatch => true)	    -- assume the input key has been normalized
--packageKey DocumentTag := DocumentTag.Package
--packageKey   Symbol := key -> package key
packageKey   String := fkey -> (
     -- checkLoadDocumentation();
     r := scan(loadedPackages, pkg -> if fetchRawDocumentation(pkg,fkey) =!= null then break pkg);
     if r === null then (
	  -- if debugLevel > 0 then error "debug me";
	  -- value PackageDictionary#"Missing"
	  currentPackage
	  )
     else r)
--packageKey  Package := identity
--packageKey    Array := key -> youngest \\ package \ toSequence key
--packageKey Sequence := key -> youngest splice apply(key, i -> if class i === Sequence then apply(i,package) else package i)
--packageKey    Thing := key -> ( p := package key; if p === null then currentPackage else p)

-- here is an alternative method -- it's fishy that we have both!
-- getPackage := fkey -> scan(value \ values PackageDictionary, pkg -> if null =!= fetchRawDocumentation(pkg,fkey) then break pkg)
-----------------------------------------------------------------------------
-- formatting document tags
-----------------------------------------------------------------------------
   -- The formatted form should be a human-readable string, and different normalized tags should yield different formatted tags.
   -- The formatted tag is used for two purposes:
   --    for display in menus and links
   --    as the key for access in a database, where the key must be a string

Strings := hashTable { 
     -- I'm not sure I like this any longer;
     -- Sequence => "(...)", List => "{...}", Array => "[...]" 
     }
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
-- this one was wrong:
--      (2,cohomology     ) => s -> ("HH ", toStr s#1),
     (2,NewMethod      ) => s -> ("new ", toString s#1),
     (3,class,Keyword  ) => s -> (toStr s#1, " ", toString s#0, " ", toStr s#2),-- infix operator
     (3,class,Symbol   ) => s -> (toStr s#1, " ", toString s#0, " ", toStr s#2),-- infix operator
     (3,class,Sequence ) => s -> (toStr s#1, " ", toString s#0#0, " ", toStr s#2, " ", toString s#0#1, " Thing"),-- infix assignment operator (really a ternary operator!)
     (2,class,Keyword  ) => s -> (toString s#0, " ", toStr s#1),-- prefix operator
     (2,class,Sequence ) => s -> (toString s#0#0, " ", toStr s#1, " ", toString s#0#1, " Thing"),-- prefix assignment operator (need to handle the postfix assignment operators still!)
     (2,symbol (*)     ) => s -> (toStr s#1, " ", toStr s#0), -- postfix operator
     (2,symbol ~       ) => s -> (toStr s#1, " ", toStr s#0), -- postfix operator
     (2,symbol !       ) => s -> (toStr s#1, " ", toStr s#0), -- postfix operator
     (2,class,ScriptedFunctor) => s -> (
	  hh := s#0;
	  if hh.?subscript and hh.?superscript 
	  then (
	       stderr << "--warning: ambiguous scripted functor, with both subscript method and superscript method: " << s << endl;
	       toString s
	       )
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
     2 => s -> (toString s#0, " ", toStr s#1)
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
formatDocumentTag Array := s -> concatenate( toString s#0, "(..., ", toString s#1, " => ...)" )

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
fixup MarkUpList := z -> splice apply(z,fixup)
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
-- phase this out -- each PARA has to have a scope
doneit := false;
deprecated := z -> if not doneit then (
     doneit = true;
     stderr << "--warning: using '" << z << "' alone in documentation is deprecated, use '" << z << "{}' instead" << endl;
     if z === PARA then stderr << "--        andgroup PARA{...} around entire paragraphs" << endl;
     )
fixup MarkUpType := z -> (
     if z === PARA or z === BR or z === HR
     then (
	  deprecated z;
	  z{})
     else error("isolated mark up type encountered: ",toString z)
     ) -- convert PARA to PARA{}
fixup Function   := z -> z				       -- allow Function => f 
fixup String     := s -> (				       -- remove clumsy newlines within strings
     if not match("\n",s) then return s;
     ln := separate s;
     concatenate ({addspaces0 trimline0 ln#0}, addspaces \ trimline \take(ln,{1,#ln-2}), {trimline1 ln#-1}))

hypertext = method(SingleArgumentDispatch => true)
hypertext MarkUpList := fixup
hypertext Sequence := hypertext List := x -> fixup DIV x

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
     instance(key, Symbol) and value key =!= key
     and not UndocumentableValue#?key and DocumentableValueType#?(basictype value key))

---- how could this have worked (so soon)?
-- scan(flatten(pairs \ dictionaryPath), (name,sym) -> if documentableValue sym then Symbols#(value sym) = sym)


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

extractExamplesLoop            := method(SingleArgumentDispatch => true)
extractExamplesLoop Thing       := x -> {}
extractExamplesLoop ExampleItem := toList
extractExamplesLoop Sequence := 
extractExamplesLoop MarkUpList := x -> join apply(toSequence x, extractExamplesLoop)

extractExamples := docBody -> (
     ex := extractExamplesLoop docBody;
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

makeFileName := (fkey,pkg) -> (			 -- may return 'null'
     if pkg#?"package prefix" and pkg#"package prefix" =!= null 
     then pkg#"package prefix" | LAYOUT#"packageexamples" pkg#"title" | toFilename fkey
     )

exampleResults := {}
exampleCounter := 0
checkForExampleOutputFile := (node,pkg) -> (
     exampleCounter = 0;
     exampleResults = {};
     if pkg#"example results"#?node then (
	  exampleResults = pkg#"example results"#node;
	  true)
     else if exampleBaseFilename =!= null and fileExists (exampleBaseFilename | ".out") then (
     	  if debugLevel > 1 then stderr << "--reading example results from " << (exampleBaseFilename | ".out") << endl;
	  exampleResults = pkg#"example results"#node = drop(separateM2output get (exampleBaseFilename | ".out"),-1);
 	  true)
     else false)

currentExampleKey := ""
processExamplesLoop = method(SingleArgumentDispatch => true)
processExamplesLoop TO :=
processExamplesLoop TO2 :=
processExamplesLoop TOH :=
processExamplesLoop Option :=
processExamplesLoop String := identity
processExamplesLoop Sequence := 
processExamplesLoop MarkUpList := x -> apply(x,processExamplesLoop)
processExamplesLoop ExampleItem := x -> (
     ret := (
	  if exampleResults#?exampleCounter
	  then PRE exampleResults#exampleCounter
	  else (
	       if #exampleResults === exampleCounter then stderr << "--warning: example results terminate prematurely: " << currentExampleKey << endl;
	       PRE concatenate("i", toString (exampleCounter+1), " : ",x)
	       ));
     exampleCounter = exampleCounter + 1;
     ret)
processExamples := (pkg,fkey,docBody) -> (
     exampleBaseFilename = makeFileName(fkey,pkg);
     if checkForExampleOutputFile(fkey,pkg) then (
     	  currentExampleKey = fkey;
     	  docBody = processExamplesLoop docBody;
     	  currentExampleKey = "";
	  );
     docBody)

-----------------------------------------------------------------------------
-- 'document' function
-----------------------------------------------------------------------------
fixupEntry := method(SingleArgumentDispatch => true)
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
fixupList := x -> (
     if not instance(x,List) then error "expected documentation option to be a list";
     apply(nonnull x,fixupEntry))
enlist := x -> if class x === List then x else {x}
chkIsStringFixup := key -> val -> if class val === String then fixup val else error("expected ",toString key," option to be a string")
fixupTable := new HashTable from {
     Key => x -> if class x === List then nonnull x else x,
     symbol DocumentTag => identity,
     Usage => val -> (
	  if not instance(val,String) then error("expected Usage option to be a string");
	  TABLE TR {
	       TD { "valign" => "top" , "Usage:" },
	       TD between_(BR{}) (TT \ nonempty separate val)
	       } ),
     Function => val -> fixup val,
     Inputs => val -> fixupList val,
     Outputs => val -> fixupList val,
     Consequences => val -> fixupList val,
     Headline => chkIsStringFixup Headline,
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
		    else error ("unrecognizable Subnode list item: ",x))))
     }
caveat := key -> getOption(key,Caveat)
seealso := key -> getOption(key,SeeAlso)
sourcecode := key -> getOption(key,SourceCode)
theMenu := key -> getOption(key,Subnodes)
documentOptions := new HashTable from {
     Key => true,
     Usage => true,
     Function => true,
     Inputs => true,
     Outputs => true,
     Consequences => true,
     Headline => true,
     SeeAlso => true,
     SourceCode => true,
     Caveat => true,
     Subnodes => true
     }
reservedNodeNames := set apply( {"Top", "Table of Contents", "Symbol Index"}, toLower )

storeRawDocumentation := (tag,opts) -> (
     key := DocumentTag.FormattedKey tag;
     if currentPackage#rawKey#?key and signalDocError tag
     then stderr << currentFileName << ":" << currentLineNumber() << ": warning: documentation already provided for '" << tag << "'" << endl;
     currentPackage#rawKey#key = opts;
     )

undocumented = method(SingleArgumentDispatch => true)
undocumented List  := x -> scan(x, undocumented)
undocumented Thing := key -> if key =!= null then (
     tag := makeDocumentTag(key, Package => currentPackage);
     storeRawDocumentation(tag, new HashTable from { symbol DocumentTag => tag, "undocumented" => true}))

undocumented keys undocumentedkeys
undocumentedkeys = null

-----------------------------------------------------------------------------
-- getting help from the documentation
-----------------------------------------------------------------------------

getExampleInputs := method(SingleArgumentDispatch => true)
getExampleInputs Thing        := t -> {}
getExampleInputs MarkUpList   := t -> join apply(toSequence t, getExampleInputs)

examples = x -> stack getExampleInputs help x
apropos = method()
apropos String := (pattern) -> last \ sort select(flatten \\ pairs \ dictionaryPath, (nam,sym) -> match(pattern,nam) and not match("\\$",nam))
-----------------------------------------------------------------------------
headline = method(SingleArgumentDispatch => true)
headline Thing := key -> getOption(key,Headline)	    -- old method
headline FinalDocumentTag := headline DocumentTag := tag -> (
     d := fetchPrimaryRawDocumentation tag;
     if d === null then (
	  -- if debugLevel > 0 and formattedKey tag == "Ring ^ ZZ" then error "debug me";
	  d = fetchAnyRawDocumentation formattedKey tag;    -- this is a kludge!  Our heuristics for determining the package of a tag are bad.
	  if d === null then (
	       if signalDocError tag then stderr << "--warning: tag has no documentation: " << tag << ", key " << toExternalString DocumentTag.Key tag << endl;
	       return "missing documentation";
	       ));
     if d#?Headline then d#Headline
     else headline DocumentTag.Key tag			    -- revert to old method, eliminate?
     )
commentize = s -> if s =!= null then concatenate(" -- ",s)
-----------------------------------------------------------------------------
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
optTO = i -> (
     i = makeDocumentTag i;
     fkey := DocumentTag.FormattedKey i;
     if not isUndocumented i then 
     if isSecondary i 
     then (
	  primary := getPrimary i;
	  (DocumentTag.FormattedKey primary, fkey, fixup if currentHelpTag === primary then fkey else SPAN {fkey, ", see ", TOH{primary}})
	  )
     else (fkey                            , fkey, fixup TOH{i})      -- need an alternative here for secondary tags such as (export,Symbol)
     )
optTOCLASS := i -> (					    -- this isn't different yet, work on it!
     r := fixup TOH{i};
     (DocumentTag.FormattedKey first r, r))

ul := t -> if #t =!= 0 then UL t else t
menu       := s -> ul (last \         nonnull \\ optTO      \ s)
smenu      := s -> ul (last \ sort \\ nonnull \\ optTO      \ s)
smenuCLASS := s -> ul (last \ sort \\ nonnull \\ optTOCLASS \ s)

vowels := set characters "aeiouAEIOU"
indefiniteArticle := s -> if vowels#?(s#0) and not match("^one ",s) then "an " else "a "
indefinite := s -> concatenate(indefiniteArticle s, s)
synonym = X -> if X.?synonym then X.synonym else "object of class " | toString X

synonymAndClass := X -> fixup (
     if X.?synonym then SPAN {indefinite X.synonym, " (of class ", TO X, ")"}
     else SPAN {"an object of class ", TO X}
     )     

justClass := X -> fixup SPAN {"an instance of class ", TO X}

ofClass = X -> fixup (
     if parent X === Nothing then error "expected a class";
     if X === Nothing then TO "null"
     else if X.?synonym then SPAN {indefiniteArticle X.synonym, TO2 {X, X.synonym}}
     else SPAN {"an object of class ", TO X}
     )

makeDocBody := method(SingleArgumentDispatch => true)
makeDocBody Thing := key -> (
     fkey := formatDocumentTag key;
     pkg := packageKey fkey;
     if pkg =!= null then (
	  rec := fetchRawDocumentation(pkg,fkey);
	  docBody := extractBody rec;
	  if docBody =!= null and #docBody > 0 then (
	       docBody = processExamples(pkg, fkey, docBody);
	       if class key === String 
	       then DIV {docBody}
	       else DIV1 { SUBSECTION "Description", DIV {docBody} })))

topheader := s -> (
     h := headline s;
     HEADER1 { formatDocumentTag s, if h =!= null then " -- ", h })

binary := set flexibleBinaryOperators
prefix := set flexiblePrefixOperators
postfix := set flexiblePostfixOperators
operator := binary+prefix+postfix

op := s -> if operator#?s then (
     ss := toString s;
     if match("^[[:alpha:]]*$",ss) then ss = " "|ss|" ";
     fixup DIV {
	  if binary#?s then PARA {
	       "This operator may be used as a binary operator in an expression like ", TT ("x"|ss|"y"), ".  The user may install ", TO "binary methods", "
	       for handling such expressions with code such as ",
	       if ss == " "
	       then PRE ("         X Y := (x,y) -> ...")
	       else PRE ("         X "|ss|" Y := (x,y) -> ..."), 
	       "where ", TT "X", " is the class of ", TT "x", " and ", TT "Y", " is the class of ", TT "y", "."
	       },
	  if prefix#?s then PARA {
	       "This operator may be used as a prefix unary operator in an expression like ", TT (ss|"y"), ".  The user may install a method for handling 
	       such expressions with code such as",
	       PRE ("           "|ss|" Y := (y) -> ..."),
	       "where ", TT "Y", " is the class of ", TT "y", "."
	       },
	  if postfix#?s then PARA {
	       "This operator may be used as a postfix unary operator in an expression like ", TT ("x "|ss), ".  The user may install a method for handling 
	       such expressions with code such as",
	       PRE ("         X "|ss|"   := (x,y) -> ..."),
	       "where ", TT "X", " is the class of ", TT "x", "."
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

types := method(SingleArgumentDispatch => true)
types Thing := x -> ({},{})
types Function := x -> ({},{typicalValue x})
types Sequence := x -> (
     if #x > 1 and instance(x#-1,Symbol) 
     then ({},{})					    -- it's an option ...
     else (
	  if instance(x#0,Sequence) and #x#0 === 2 and x#0#1 === symbol =
          then ( drop(toList x,1) | { Thing }, { typicalValue x } ) -- it's an assignment method
	  else ( drop(toList x,1)            , { typicalValue x } )
	  ))

emptyOptionTable := new OptionTable from {}
getOptionDefaultValues := method(SingleArgumentDispatch => true)
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
	  t := toString (options fn)#optsymb;
	  if not match("^--Function",t) then SPAN{"default value " ,t});
     r := SPAN splice between_", " nonnull nonempty { 
	  if idname =!= null then TT idname, 
	  if type =!= null and type =!= Nothing then ofClass type, -- type Nothing, treated as above
	  default,
	  text
	  };
     if optsymb =!= null then r = (
	  if #r == 0
	  then SPAN between(", ", nonnull ( TO2{ [fn,optsymb], concatenate(toString optsymb," => ...") }, LATER { () -> commentize (headline [fn,optsymb]) } ))
	  else SPAN (TT ( toString optsymb, " => " ), r));
     r)

sortByName := v -> last \ sort \\ (i -> (toString i, i)) \ v

document = method()
document List := args -> (
     args = toSequence args;
     if currentPackage === null then error "encountered 'document' command, but no package is open";
     o := new MutableHashTable;
     scan(args, arg -> if class arg === Option then (
	       key := arg#0;
	       if not documentOptions#?key then error("unknown documentation option '", key, "'") ;
	       if o#?key then error("option ",key," encountered twice");
	       o#key = arg#1));
     args = select(args, arg -> class arg =!= Option);
     if not o.?Key then error "missing Key";
     key := o.Key;
     rest := {};
     if class key === List then (
	  rest = drop(key,1);
	  o.Key = key = first key;
	  );
     o.DocumentTag = tag := makeDocumentTag(key, Package => currentPackage);
     scan(rest, secondary -> (
	       tag2 := makeDocumentTag secondary;
	       storeRawDocumentation(tag2, new HashTable from { PrimaryTag => tag, symbol DocumentTag => tag2 })));
     if not o.?Headline and class key === Sequence and key#?0 then (
	  h := headline key#0;
	  if h =!= null then o.Headline = h;
	  );
     currentNodeName = DocumentTag.FormattedKey tag;
     if reservedNodeNames#?(toLower currentNodeName) then error("'document' encountered a reserved node name '",currentNodeName,"'");
     pkg := DocumentTag.Package tag;
     o.Description = toList args;
     exampleBaseFilename = makeFileName(currentNodeName,currentPackage);
     scan(keys o, key -> o#key = fixupTable#key o#key);
     if o.?Headline and o.Headline === "" then remove(o,Headline);
     if o.?Usage and o.Usage === "" then remove(o,Usage);
     -- pre-processing of inputs and outputs
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
     	  outp = apply(out',out,(T,v) -> T => v);
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
	       else if sym === Function then null
	       else if sym === symbol DocumentTag then null
	       else validate DIV o#sym
	       )
	  );
     o = new HashTable from o;
     storeRawDocumentation(tag, o);
     currentNodeName = null;
     )

synopsisOpts := new OptionTable from {			    -- old
     Usage => null,
     Function => null,
     Inputs => {},
     Outputs => {},
     Consequences => {}
     }

briefSynopsis := key -> (
     -- we still want to put
     --	       moreGeneral s
     -- back somewhere....
     o := getDoc key;
     if o === null then return null;
     r := nonnull {
	  if o.?Usage then o.Usage,
	  if o#?Function then SPAN { "Function: ", TO o#Function }
	  else if instance(key, Sequence) and key#?0 then (
	       if instance(key#0, Function) then SPAN { "Function: ", TO key#0 }
	       else if instance(key#0, Keyword) then SPAN { "Operator: ", TO key#0 }
	       else if instance(key#0,Sequence) and #key#0 === 2 and key#0#1 === symbol "="
	       then SPAN { "Operator: ", TO key#0#0 }	    -- assignment operator for this operator
	       ),
	  if o.?Inputs then DIV1 { "Inputs:", UL o.Inputs },
	  if o.?Outputs then DIV1 { "Outputs:", UL o.Outputs },
	  if o.?Consequences and #o.Consequences > 0 then DIV1 { "Consequences:", UL o.Consequences },
	  if o#?"optional inputs" then DIV1 { TO2{ "using functions with optional inputs", "Optional inputs"}, ":", UL o#"optional inputs" }
	  };
     if #r > 0 then fixup UL r)

synopsis := key -> (
     s := briefSynopsis key;
     if s =!= null then DIV1 { SUBSECTION "Synopsis", s }
     )

documentableMethods := s -> select(methods s,isDocumentableMethod)

fmeth := f -> (						    -- compare with documentationValue(Symbol,Function) below
     b := documentableMethods f;
     if #b > 0 then (
	  c := smenu b;
	  if #c > 0 then DIV1 { SUBSECTION { "Ways to use ", TT toString f }, c } 
	  )
     )

noBriefDocThings := hashTable { symbol <  => true, symbol >  => true, symbol == => true }
briefDocumentation = method(SingleArgumentDispatch => true)

briefDocumentation VisibleList := x -> null

briefDocumentation Thing := x -> (
     if noBriefDocThings#?x or not isDocumentableThing x then return null;
     if package x === Macaulay2Core then checkLoadDocumentation();
     r := briefSynopsis normalizeDocumentKey x;
     if r =!= null then << endl << r << endl
     else (
	  if headline x =!= null then << endl << commentize headline x << endl;
	  if instance(x, Function) or class x === ScriptedFunctor then (
	       s := fmeth x;
	       if s =!= null then << endl << s << endl;)))

ignoreDocumentationErrors = true

page = (title,body) -> HTML { HEAD { TITLE title }, BODY body }

help = method(SingleArgumentDispatch => true)
help String := key -> (
     checkLoadDocumentation();
     if unformatTag#?key then help unformatTag#key 
     else if isGlobalSymbol key then (
	  t := getGlobalSymbol key;
	  help t)
     else (
	  b := makeDocBody key;
	  if b === null then (
	       if ignoreDocumentationErrors
	       then stderr << "--warning: there is no documentation for '" << key << "'" << endl
	       else error("there is no documentation for '"|key|"'");
	       b = ();
	       );
	  fixup page(formatDocumentTag key, {topheader key, b, caveat key, seealso key, theMenu key})))

optionFor := s -> unique select( value \ flatten(values \ dictionaryPath), f -> instance(f, Function) and (options f)#?s) -- this is slow!

--ret := k -> (
--     t := typicalValue k;
--     if t =!= Thing then fixup DIV {"Class of returned value: ", TO t, commentize headline t}
--     )

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

documentationValue(Symbol,Function) := (s,f) -> (	    -- compare with fmeth above
     a := smenu documentableMethods f;
     if #a > 0 then DIV ( "class" => "waystouse", SUBSECTION {"Ways to use ", TT toString f, " :"}, a))

documentationValue(Symbol,Keyword) := (s,k) -> (
     a := smenu documentableMethods k;
     if #a > 0 then DIV ( "class" => "waystouse", SUBSECTION {"Ways to use ", TT toString k, " :"}, a))

documentationValue(Symbol,Thing) := (s,x) -> ()

authorDefaults := new HashTable from { Name => "Anonymous", Email => null, HomePage => null }
documentationValue(Symbol,Package) := (s,pkg) -> if pkg =!= Macaulay2Core then (
     e := toSequence pkg#"exported symbols";
     a := select(e,x -> instance(value x,Function));	    -- functions
     b := select(e,x -> instance(value x,Type));	    -- types
     -- this doesn't get the methods of the form f T := ... :
     m := unique flatten apply(b, T -> select(keys value T, 
	       i -> class i === Sequence and #i > 1 and ( instance(i#0, Symbol) and i#1 =!= symbol = or instance(i#0, Function) ) and isDocumentableMethod i)); -- methods
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
	  DIV1 { SUBSECTION "Version", "This documentation describes version ", pkg.Options.Version, " of ", pkg#"title", "." },
	  if pkg#"title" =!= "Macaulay2" then DIV1 {SUBSECTION "Source code", "The source code is in the file ", HREF { LAYOUT#"packages" | fn, fn }, "."},
	  if #e > 0 then DIV1 {
	       SUBSECTION "Exports",
	       fixup UL {
		    if #b > 0 then DIV1 {"Types", smenu b},
		    if #a > 0 then DIV1 {"Functions", smenu a},
		    if #m > 0 then DIV1 {"Methods", smenu m},
		    if #c > 0 then DIV1 {"Symbols", smenu c},
		    if #d > 0 then DIV1 {"Other things", smenuCLASS d}}}))

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
     -- s := value S;
     if package S === Macaulay2Core then checkLoadDocumentation();
     currentHelpTag = makeDocumentTag S;
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
	       SPAN{ if class fn === Sequence then "Method: " else "Function: ", TOH {fn} },
	       SPAN{ "Option name: ", TOH {opt} }
	       },
	  caveat key, seealso key, theMenu key })

help Sequence := key -> (						    -- method key
     checkLoadDocumentation();
     if key === () then return help "initial help";
     if null === lookup key then error("expected ", toString key, " to be a method");
     currentHelpTag = makeDocumentTag key;
     ret := fixup DIV { topheader key, synopsis key, makeDocBody key, caveat key, sourcecode key, seealso key, theMenu key };
     currentHelpTag = null;
     ret)

help List := v -> DIV between(hr,help \ v)

help Thing := x -> if ReverseDictionary#?x then return help ReverseDictionary#x else error "no documentation found"

help = Command help

pager = x -> (
     if height stdio > 0
     then "!" | (if getenv "PAGER" == "" then "more" else getenv "PAGER") << x << close 
     else << x << endl ;)

infoHelp = key -> (
     tag := makeDocumentTag key;
     t := infoTagConvert tag;
     run ("info "|format t);)


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
	  sublist -> TEX concatenate between(newline,apply(sublist,line -> replace("^-- *","",line))),
	  identity);
     x = sublists(x,
	  line -> class line === String,
	  sublist -> EXAMPLE sublist,
	  identity);
     x )

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
