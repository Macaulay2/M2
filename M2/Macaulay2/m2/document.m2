--		Copyright 1994-2006 by Daniel R. Grayson

-- TODO: deprecate
rootPath = "";
rootURI = "file://";

-----------------------------------------------------------------------------
-- Local variables
-----------------------------------------------------------------------------

currentNodeName = null
--currentHelpTag -- bring back from validate.m2

-- TODO: move things that depend on this to help.m2?
prefix   := set flexiblePrefixOperators

reservedNodeNames := {"Top", "Table of Contents", "Symbol Index"}

methodNames := set {NewFromMethod, NewMethod, NewOfFromMethod, NewOfMethod, id, Ext, Tor}

-----------------------------------------------------------------------------
-- Local utilities
-----------------------------------------------------------------------------

-- TODO: make these local
indefiniteArticle = s -> if match("[aeiouAEIOU]", s#0) and not match("^one ", s) then "an " else "a "
indefinite        = s -> concatenate(indefiniteArticle s, s)

-- TODO: deprecate this function, because we should install the Macaulay2Doc package first
-- called also from help.m2
checkLoadDocumentation = () -> (
    if not isGlobalSymbol "Macaulay2Doc"
    or not instance(pkg := value getGlobalSymbol "Macaulay2Doc", Package)
    or not member(pkg, loadedPackages) or not member(pkg.Dictionary, dictionaryPath)
    -- the documentation for things in the package Core is in the package Macaulay2Doc
    then needsPackage "Macaulay2Doc")

enlist := x -> if instance(x, List) then x else {x}

-----------------------------------------------------------------------------
-- lookInPackage (TODO: find a better name?)
-----------------------------------------------------------------------------
-- This is only used to inquire about a symbol from the Text package.
-- Probably only necessary because Text documents Hypertext objects.
-- Is there an alternative way?
lookInPackage = (pkg, symb, func) -> (
    if member(pkg.Dictionary, dictionaryPath) then return func symb;
    dictionaryPath = prepend(pkg.Dictionary, dictionaryPath);
    first (func symb, dictionaryPath = drop(dictionaryPath, 1)))

-----------------------------------------------------------------------------
-- verifying the document Key
-----------------------------------------------------------------------------
-- here we check that the method a putative document tag documents is actually installed
verifyKey := method(Dispatch => Thing)
verifyKey Thing    := key -> key
verifyKey Sequence := key -> ( -- e.g., (res, Module) or (symbol **, Module, Module)
    if      #key == 0 then error "documentation key () encountered"
    else if #key == 1 and not instance(key#0, Function) then
        error("documentation key ", format toString key, " encountered, but ", format toString key#0, " is not a function")
    else if #key  > 1
    and not any({Keyword, Command, Function, ScriptedFunctor}, type -> instance(key#0, type)) and not methodNames#?(key#0)
    and not (instance(key#0, Sequence) and 2 == #key#0 and key#0#1 === symbol= and instance(key#0#0, Keyword)) then
        error("documentation key ", format toString key, " encountered, but ", format toString key#0, " is not a function, command, scripted functor, or keyword");
    --
    if (
	-- this will all get screwed up with immutable types present
	if      #key  > 2 then ( t := youngest drop(key, 1); t#?key            and instance(t#key,         Function) )
	else if #key == 2 then ( instance(key#1, HashTable) and key#1#?(key#0) and instance(key#1#(key#0), Function) )
	else if #key == 1 then ( nullaryMethods#?key and instance(nullaryMethods#key, Function) )
	else false) then null
    else if #key > 1 and instance(key#0, Command) then verifyKey prepend(key#0#0, drop(key, 1))
    else error("documentation key for ", format formatDocumentTag key, " encountered, but no method installed"))
verifyKey Array    := key -> (
    (fn, opt) := (key#0, key#1); -- e.g., [res, Strategy]
    if not instance(fn, Function) and not instance(fn, Sequence) then
        error("expected first element of document key for optional argument to be a function or sequence: ", silentRobustString(40, 1, key));
    if not (options fn)#?opt then error("expected ", opt, " to be an option of ", fn))

-----------------------------------------------------------------------------
-- identifying the package of a document tag
-----------------------------------------------------------------------------
-- TODO: deprecate this
packageKey0 := method(Dispatch => Thing)
packageKey0 Thing    := key -> currentPackage
packageKey0 Sequence := key -> currentPackage		    -- this is a kludge, which allows Schubert2 to document (symbol SPACE,OO,RingElement)
-- packageKey0 Sequence :=				    -- this might be the right way to do it
packageKey0 Array    := key -> ( n := youngest apply(toSequence key, package); assert(n =!= null); n )

packageKey := method()
packageKey(Array, String) := (key, fkey) -> packageKey0 key
packageKey(Thing, String) := (key, fkey) -> (
    r := scan(loadedPackages, pkg -> if fetchRawDocumentation(pkg,fkey) =!= null then break pkg);
    if r === null then packageKey0 key else r)

-----------------------------------------------------------------------------
-- normalizeDocumentKey
-----------------------------------------------------------------------------
-- The normalized form for simple objects will be the symbol whose value is the object
-- This allows us to write documentation links like TO "sin", TO sin, or TO symbol sin
-- and have them all get recorded the same way.
-- But there is a problem with this whole idea -- what about keys in other packages, which can't be
-- loaded now, because they might try to load the package currently being loaded?  Why not just normalize
-- to the string form, tacking on the package name, if given a symbol?

-- TODO: used only once in help.m2
normalizeDocumentKey = method(Dispatch => Thing, Options => { Package => null })
normalizeDocumentKey    Array := opts -> identity
normalizeDocumentKey   Symbol := opts -> identity
normalizeDocumentKey Sequence := opts -> identity
normalizeDocumentKey  Nothing := opts -> key -> symbol null
normalizeDocumentKey   String := opts -> key -> (
    pkg := opts#Package;
    if pkg =!= null then (
	if     instance(pkg, String)  then return if match("::", key) then key else concatenate(pkg, "::", key);
	if not instance(pkg, Package) then error("expected ", toString pkg, " to be a package");
	if pkg.Dictionary#?key        then return pkg.Dictionary#key else key);
    if isGlobalSymbol key then getGlobalSymbol key else key)
normalizeDocumentKey    Thing := opts -> key -> (
    if hasAttribute(key, ReverseDictionary) then getAttribute(key, ReverseDictionary)
    else error("can't determine symbol whose value is document tag: ", key))

-----------------------------------------------------------------------------
-- DocumentTag type declarations and basic constructors
-----------------------------------------------------------------------------
-- We need three bits of information about a document tag:
--     the original key		    e.g., (operator **,Module,Module)
--     the formatted key            e.g., "Module ** Module"
--     the package name             e.g., "Core", or "" if there is none
-- Here we assemble them together, so we don't have to recompute the information later.

DocumentTag = new Type of BasicList
DocumentTag.synonym = "document tag"

DocumentTag.Key = method(Dispatch => Thing)
DocumentTag.Key DocumentTag := tag -> tag#0

format   DocumentTag := tag -> tag#1
package  DocumentTag := tag -> tag#2
toString DocumentTag :=
net      DocumentTag := tag -> concatenate (package tag, " :: ", format tag)

DocumentTag ? DocumentTag := (x, y) -> x#1 ? y#1
DocumentTag ? String      := (x, y) -> x#1 ? y
String      ? DocumentTag := (x, y) -> x   ? y#1

-- helper for parsing " pkg :: key " to ("pkg", "key")
parseDocumentTag := key -> (
    segments := separate("^[[:space:]]*|[[:space:]]*::[[:space:]]*|[[:space:]]*$", key);
    segments  = select(segments, segment -> segment =!= "");
    if      #segments == 0 then error("encountered empty documentation tag: ", format key)
    else if #segments == 1 then (null,       segments#0)
    else if #segments == 2 then (segments#0, segments#1)
    else error("encountered invalid documentation tag: ", format key))

-- helper for printing the formal name of a package
pkgTitle := method()
pkgTitle Package := pkg -> if pkg === Core then "Macaulay2Doc" else pkg#"pkgname"
pkgTitle Symbol  := toString
pkgTitle String  := identity
pkgTitle Nothing := x -> ""

makeDocumentTag' := opts -> key -> (
    nkey := normalizeDocumentKey(key, opts);
    verifyKey nkey;
    fkey := formatDocumentTag nkey;
    local pkg;
    (pkg, fkey) = parseDocumentTag fkey;
    pkg = if class nkey === Symbol -* and package nkey =!= Core *- then package nkey
    else if opts#Package =!= null then opts#Package else packageKey(key, fkey);
    if pkg === null then error("makeDocumentTag: package cannot be determined: ", nkey);
    new DocumentTag from { if instance(nkey, Symbol) then toString nkey else nkey, fkey, pkgTitle pkg })

makeDocumentTag = method(Dispatch => Thing, Options => { Package => null })
makeDocumentTag DocumentTag := opts -> tag -> tag
makeDocumentTag Thing       := opts -> key -> (makeDocumentTag' opts) key
makeDocumentTag String      := opts -> key -> (
    -- this is important, because the names of info nodes get extracted from text where
    -- lines might be wrapped and multiple spaces are reduced to one:
    if match("^ |  +| $", key) then error("expected key to have only single interior spaces:", format key);
    local pkg;
    (pkg, key) = parseDocumentTag key;
    if pkg =!= null and opts#Package =!= null and pkg =!= opts#Package then
        error ("mismatching packages ", pkg, " and ", opts#Package, " specified for key ", key);
    if pkg === null then pkg = opts#Package;
    (makeDocumentTag' new OptionTable from {Package => pkg}) key)

-----------------------------------------------------------------------------
-- unformatting document tags
-----------------------------------------------------------------------------
-- we need to be able to do this only for the document tags we have shown to the user in formatted form
-- TODO: https://github.com/Macaulay2/M2/issues/1317
unformatTag = new MutableHashTable
record := func -> symb -> ( val := func symb; if val =!= symb then unformatTag#val = symb; val )

-----------------------------------------------------------------------------
-- formatting document tags
-----------------------------------------------------------------------------
-- The formatted form should be a human-readable string, and different
-- normalized tags should yield different formatted tags.
-- The formatted tag is used for two purposes:
--    for display in menus and links
--    as the key for access in a database, where the key must be a string

fSeq := new HashTable from {
    (4, NewOfFromMethod) => s -> ("new ", toString s#1, " of ", toString s#2, " from ", toString s#3),
    (3, NewFromMethod  ) => s -> ("new ", toString s#1,                       " from ", toString s#2),
    (3, NewOfMethod    ) => s -> ("new ", toString s#1, " of ", toString s#2),
    (2, NewMethod      ) => s -> ("new ", toString s#1),

    -- cohomology and cohomology
    (4, cohomology, ZZ ) => s -> ("HH^", toString s#1, "(", toString s#2, ",", toString s#3, ")"),
    (3, cohomology, ZZ ) => s -> ("HH^", toString s#1, " ", toString s#2),
--  (2, cohomology     ) => s -> ("HH ", toString s#1), -- this one was wrong
    (4, homology, ZZ   ) => s -> ("HH_", toString s#1, "(", toString s#2, ",", toString s#3, ")"),
    (3, homology, ZZ   ) => s -> ("HH_", toString s#1, " ", toString s#2),
    (2, homology       ) => s -> ("HH ", toString s#1),

    (3, class, Keyword ) => s -> (toString s#1, " ", toString s#0, " ", toString s#2), -- infix operator
    (3, class, Symbol  ) => s -> (toString s#1, " ", toString s#0, " ", toString s#2), -- infix operator
    -- infix assignment operator (really a ternary operator!)
    (3, class, Sequence) => s -> (toString s#1, " ", toString s#0#0, " ", toString s#2, " ", toString s#0#1, " Thing"),
    (2, class, Keyword ) => s -> (toString s#0, " ", toString s#1), -- prefix operator
    (2, class, Sequence) => s -> (
	op := s#0#0;
	if prefix#?op
	then (toString op, " ", toString s#1, " ", toString s#0#1, " Thing")
	else (toString s#1, " ", toString op, " ", toString s#0#1, " Thing")),

    (3, symbol SPACE   ) => s -> (toString s#1, " ", toString s#2),
    (2, symbol <-      ) => s -> (toString s#1, " <- Thing"),       -- assignment statement with left hand side evaluated
    (2, symbol (*)     ) => s -> (toString s#1, " ", toString s#0), -- postfix operator
    (2, symbol ^*      ) => s -> (toString s#1, " ", toString s#0), -- postfix operator
    (2, symbol _*      ) => s -> (toString s#1, " ", toString s#0), -- postfix operator
    (2, symbol ~       ) => s -> (toString s#1, " ", toString s#0), -- postfix operator
    (2, symbol !       ) => s -> (toString s#1, " ", toString s#0), -- postfix operator

    -- ScriptedFunctors
    (4, class, ScriptedFunctor, ZZ) => s -> (
	hh := s#0;
	if hh.?subscript
	then (toString hh, "_", toString s#1, "(", toString s#2, ",", toString s#3, ")")
	else (toString hh, "^", toString s#1, "(", toString s#2, ",", toString s#3, ")")),
    (3, class, ScriptedFunctor, ZZ) => s -> (
	hh := s#0;
	if hh.?subscript
	then (toString hh, "_", toString s#1, " ", toString s#2)
	else (toString hh, "^", toString s#1, " ", toString s#2)),
    (2, class, ScriptedFunctor    ) => s -> (
	hh := s#0;
	if hh.?subscript and hh.?superscript then (
	    stderr << "--warning: ambiguous scripted functor, with both subscript method and superscript method: " << s << endl;
	    toString s)
	else if hh.?subscript   then (toString hh, " _ ", toString s#1)
	else if hh.?superscript then (toString hh, " ^ ", toString s#1)
	else (toString hh, " ", toString s#1)),

    -- TODO: Methods?
    5 => s -> (
	t := if methodOptions s#0 =!= null then (methodOptions s#0).Dispatch else {Thing, Thing};
	(toString s#0, "(",
	    if t#?0 and t#0===Type then "type of ", toString s#1, ",",
	    if t#?1 and t#1===Type then "type of ", toString s#2, ",",
	    if t#?2 and t#2===Type then "type of ", toString s#3, ",",
	    if t#?3 and t#3===Type then "type of ", toString s#4, ")")),
    4 => s -> (
	t := if methodOptions s#0 =!= null then (methodOptions s#0).Dispatch else {Thing, Thing};
	(toString s#0, "(",
	    if t#?0 and t#0===Type then "type of ", toString s#1, ",",
	    if t#?1 and t#1===Type then "type of ", toString s#2, ",",
	    if t#?2 and t#2===Type then "type of ", toString s#3, ")")),
    3 => s -> (
	t := if methodOptions s#0 =!= null then (methodOptions s#0).Dispatch else {Thing, Thing};
	(toString s#0, "(",
	    if t#?0 and t#0===Type then "type of ", toString s#1, ",",
	    if t#?1 and t#1===Type then "type of ", toString s#2, ")")),
    2 => s -> (
	t := if methodOptions s#0 =!= null then (methodOptions s#0).Dispatch else {Thing, Thing};
	(toString s#0, "(",
	    if t===Type or instance(t,List) and t#?0 and t#0===Type then "type of ", toString s#1, ")")),
    1 => s -> (toString s#0, "()"),
    }

formatDocumentTag = method(Dispatch => Thing)
formatDocumentTag Thing    := toString
formatDocumentTag String   := s -> s
formatDocumentTag Array    := s -> (
    if instance(s#0, Sequence) and 0 < #s#0
    then concatenate(toString s#0#0, "(", between(",", apply(drop(s#0, 1), toString)), ", ", toString s#1, " => ...)")
    else concatenate(toString s#0,   "(..., ", toString s#1, " => ...)"))
formatDocumentTag Sequence := record(
    s -> concatenate (
	if #s == 0                                           then toString
	else if            fSeq#?(#s, s#0)                   then fSeq#(#s, s#0)
	else if #s > 1 and fSeq#?(#s, s#0, s#1)              then fSeq#(#s, s#0, s#1)
	else if #s > 1 and fSeq#?(#s, class, class s#0, s#1) then fSeq#(#s, class, class s#0, s#1)
	else if            fSeq#?(#s, class, class s#0)      then fSeq#(#s, class, class s#0)
	else if            fSeq#?(class s#-1, #s)            then fSeq#(class s#-1, #s)
	else if            fSeq#?#s                          then fSeq#(#s)
	else toString) s)

-----------------------------------------------------------------------------
-- storeRawDocumentation
-----------------------------------------------------------------------------
fixup Thing      := z -> error("unrecognizable item ",toString z," of class ",toString class z," encountered while processing documentation node ", toString currentHelpTag)
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
fixup String     := s -> demark_" " separate("[ \t]*\r?\n[ \t]*", s) -- remove clumsy newlines within strings

-- TODO: move this, and above, to hypertext.m2
hypertext = method(Dispatch => Thing)
hypertext Hypertext := fixup
hypertext Sequence := hypertext List := x -> fixup DIV x

-----------------------------------------------------------------------------
-- fetchRawDocumentation, fetchRawDocumentationNoLoad
-----------------------------------------------------------------------------
fetchRawDocumentation = method()
fetchRawDocumentation DocumentTag      :=  tag            -> fetchRawDocumentation(getpkg package tag, format tag)
fetchRawDocumentation(String,  String) := (pkgname, fkey) -> fetchRawDocumentation(getpkg pkgname, fkey)
fetchRawDocumentation(Package, String) := (pkg,     fkey) -> ( -- returns null if none
    rawdoc := pkg#rawKey;
    if rawdoc#?fkey then rawdoc#fkey else if pkg#?rawKeyDB then (
	rawdoc = pkg#rawKeyDB;
	if isOpen rawdoc and rawdoc#?fkey then lookInPackage(getpkg "Text", rawdoc#fkey, value)))

fetchRawDocumentationNoLoad = method()
fetchRawDocumentationNoLoad(Nothing, Thing)  := (pkg,     fkey) -> null
fetchRawDocumentationNoLoad DocumentTag      :=  tag            -> fetchRawDocumentationNoLoad(getpkgNoLoad package tag, format tag)
fetchRawDocumentationNoLoad(String,  String) := (pkgname, fkey) -> fetchRawDocumentationNoLoad(getpkgNoLoad pkgname, fkey)
fetchRawDocumentationNoLoad(Package, String) := (pkg,     fkey) -> ( -- returns null if none
    rawdoc := pkg#rawKey;
    if rawdoc#?fkey then rawdoc#fkey else if pkg#?rawKeyDB then (
	rawdoc = pkg#rawKeyDB;
	if isOpen rawdoc and rawdoc#?fkey then lookInPackage(getpkg "Text", rawdoc#fkey, value)))

-----------------------------------------------------------------------------
-- fetchPrimaryRawDocumentation, fetchAnyRawDocumentation
-----------------------------------------------------------------------------
getPrimaryTag = method()
getPrimaryTag DocumentTag := tag -> (
    while (rawdoc := fetchRawDocumentation tag; rawdoc =!= null and rawdoc#?PrimaryTag)
    do tag = rawdoc#PrimaryTag;
    tag)

fetchPrimaryRawDocumentation = method()
fetchPrimaryRawDocumentation DocumentTag := tag -> fetchRawDocumentation getPrimaryTag tag

fetchAnyRawDocumentation = method()
fetchAnyRawDocumentation DocumentTag := tag  -> fetchAnyRawDocumentation format tag
fetchAnyRawDocumentation String      := fkey -> scan(keys PackageDictionary, pkg -> (
	rawdoc := fetchRawDocumentation(pkg, fkey);
	if rawdoc =!= null then (
	    break if rawdoc#?PrimaryTag then fetchPrimaryRawDocumentation rawdoc#PrimaryTag else rawdoc)))

-----------------------------------------------------------------------------
-- inquiring the status of a key or DocumentTag
-----------------------------------------------------------------------------
isMissingDoc    := tag -> fetchPrimaryRawDocumentation tag === null
isSecondaryTag   = tag -> ( d := fetchRawDocumentation tag; d =!= null and d#?PrimaryTag )
isUndocumented   = tag -> ( d := fetchRawDocumentation tag; d =!= null and d#?"undocumented" and d#"undocumented" === true )
hasDocumentation = key -> (
    tag := makeDocumentTag(key, Package => null);
    -- TODO: does this error belong here?
    if package tag === "" then error("key to be documented is exported by no package: ", format tag);
    null =!= fetchRawDocumentation tag)

locate DocumentTag := tag -> (
    checkLoadDocumentation();
    raw := fetchAnyRawDocumentation tag;
    if raw =!= null then (raw#"filename", toString raw#"linenum",,,,,)) -- TODO: (filename, start,startcol, stop,stopcol, pos,poscol)

-----------------------------------------------------------------------------
-- helpers for the document function
-----------------------------------------------------------------------------

commentize = s -> if s =!= null then concatenate(" -- ", s) else ""

headline = method(Dispatch => Thing)
headline Thing := key -> (
    s := fetchRawDocumentationNoLoad makeDocumentTag key;
    if s =!= null and s#?Headline then s#Headline)
headline DocumentTag := tag -> (
     d := fetchPrimaryRawDocumentation tag;
     if d === null then (
	  -- this branch does get used, but why not combine fetchPrimaryRawDocumentation and fetchAnyRawDocumentation?
	  d = fetchAnyRawDocumentation format tag;    -- this is a kludge!  Our heuristics for determining the package of a tag are bad.
	  if d === null then (
	       if signalDocError tag and package tag === currentPackage#"pkgname" then (
		   dtag := DocumentTag.Key tag;
		   stderr << "--warning: tag has no documentation: " << tag << ", key " << toExternalString dtag << ", package " << package dtag << endl;
		   );
	       return null;
	       ));
     if d#?Headline then d#Headline
     )

emptyOptionTable := new OptionTable from {}
getOptionDefaultValues := method(Dispatch => Thing)
getOptionDefaultValues Symbol   := x -> if value x =!= x then getOptionDefaultValues value x else emptyOptionTable
getOptionDefaultValues Thing    := x -> emptyOptionTable
getOptionDefaultValues Function := f -> (
     o := options f;
     if o =!= null then o else emptyOptionTable)
getOptionDefaultValues Sequence := s -> (
     o := options s;
     if o =!= null then o else if s#?0 and instance(s#0, Function) then getOptionDefaultValues s#0 else emptyOptionTable)

istype := X -> parent X =!= Nothing
isId := x -> instance(x,String) and match(///\`[[:alnum:]']+\'///,x)
mapo := f -> g := x -> if instance(x, Option) then ( f x#0 ; g x#1 ) else f x

-- TODO: simplify this
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
	  if fn === null or fn === () then
	      error("default value for option ", toString optsymb, " not accessible, base function not specified (with BaseFunction => ...)");
	  opts := if instance(key, Sequence) and options key =!= null then options key else options fn;
	  if not opts#?optsymb then error("symbol ", optsymb, " is not the name of an optional argument for function ", format fn);
	  t := toString opts#optsymb;
	  if not match("^\\{\\*Function", t) then SPAN{"default value ", t});
     r := SPAN splice between_", " nonnull nonempty {
	  if idname =!= null then TT idname,
	  if type =!= null and type =!= Nothing then ofClass type, -- type Nothing, treated as above
	  default,
	  text};
     if optsymb =!= null then r = (
	  if #r == 0
	  then SPAN between(", ",
	       nonnull (
		    TO2{
			 [ if options key =!= null then key else fn, optsymb],
			 concatenate(toString optsymb," => ...") },
		    LATER { () -> commentize (headline (
				   if options key =!= null and (options key)#?optsymb
				   then [key,optsymb]
				   else if options fn =!= null and (options fn)#?optsymb
				   then [fn,optsymb]
				   else error (toString optsymb, " is not an option for ", toString key, ", nor for ", toString fn)
				   )) }
		    ))
	  else SPAN (TT ( toString optsymb, " => " ), r));
     r)

-- "x" => List => { "a list of numbers" }
-- "x" => List => "a list of numbers"
-- "x" => List
-- "x" => { "a list of numbers" }
-- List => { "a list of numbers" }
-- { "a list of numbers" }
-- "a list of numbers"
-- List
-- "x"
fixupEntry := method(Dispatch => Thing)
fixupEntry Thing  := fixup
fixupEntry Type   := identity
fixupEntry Option := z -> z#0 => fixupEntry z#1

fixupList := (x, nm, l) -> (
    if not instance(x, List) then error(toString nm, " => ... : expected a list");
    if #x < l then error(toString nm, " => ... : expected at least ", l, " item(s)");
    apply(nonnull x, fixupEntry))

-- TODO: do this processing later, not here
getUsage := val -> (
    if not instance(val, String) then error "Usage: expected a string";
    val = apply(nonempty separate val, u -> replace("^[[:space:]]*(.*)[[:space:]]*$", "\\1", u));
    if #val === 0 then error "Usage: expected content";
    DL flatten { "class" => "element", DT "Usage: ", DD \ TT \ val } )

getHeadline   := (val, key)   -> if instance(val, String) then fixup val else error("expected ", toString key, " option to be a string")
getSubsection := (val, title) -> fixup DIV { SUBSECTION title, val }
getSourceCode :=  val         -> DIV {"class" => "waystouse",
    fixup DIV {SUBSECTION "Code", PRE demark_newline unstack stack apply(enlist val, m -> (
		f := lookup m; if f === null then error("SourceCode: ", toString m, ": not a method");
		c := code f;   if c === null then error("SourceCode: ", toString m, ": code for method not found");
		reproduciblePaths toString c))}}
getSubnodes := val -> (
    val = nonnull enlist val;
    if #val == 0 then error "encountered empty Subnodes list"
    else MENU apply(val, x -> fixup (
	    if class x === TO then x
	    else if class x === TOH then TO {x#0}
	    else if class x === String then x
	    else error ("unrecognizable Subnode list item: ", x))))
getExampleFiles := val -> (
    if not currentPackage.Options.AuxiliaryFiles
    then error "ExampleFiles option specified, but AuxiliaryFiles option is not set to 'true'";
    if not (instance(val, List) and all(val, fn -> instance(fn, String)))
    then error "expected ExampleFiles option to be a list of strings";
    auxiliaryFilesDirectory := currentPackage#"source directory" | currentPackage#"pkgname" | "/";
    val = apply(val, fn -> auxiliaryFilesDirectory | fn);
    for fn in val do if not fileExists fn then error ("example data file not found: ", fn);
    currentPackage#"example data files"#currentNodeName = val; "")
getBaseFunction := val -> (
    if val =!= null and not instance(val, Function)
    then error "expected BaseFunction option value to be a function" else val)

typicalValue := k -> (
    if typicalValues#?k then typicalValues#k
    else if class k === Sequence and typicalValues#?(k#0) then typicalValues#(k#0)
    else Thing)

getTypes := method(Dispatch => Thing)
getTypes Thing    := x -> ({},{})
getTypes Function := x -> ({},{typicalValue x})
getTypes Sequence := x -> (
     if #x > 1 and instance(x#-1,Symbol)
     then ({},{})					    -- it's an option ...
     else (
	  x' := select(drop(toList x,1), T -> not ancestor(Nothing,T)); -- putting something like OO in the key indicates a fake dispatch
	  if instance(x#0,Sequence) and #x#0 === 2 and x#0#1 === symbol =
	  or #x == 2 and x#0 === symbol <-
          then ( x' | { Thing }, { Thing } )	   -- it's an assignment method
	  else ( x'            , { typicalValue x } )
	  ))

KeywordFunctions := new HashTable from {
    symbol DocumentTag => identity, -- TODO: what is this for?
    Key             => identity, -- TODO: where is this checked?
    Headline        => val -> getHeadline(val, Headline),
    Heading         => val -> getHeadline(val, Heading),
    BaseFunction    => val -> getBaseFunction val,
    Usage           => val -> getUsage val,
    Inputs          => val -> fixupList(val, Inputs, 1),
    Outputs         => val -> fixupList(val, Outputs, 1),
    Consequences    => val -> fixupList(val, Consequences, 0),
    Description     => val -> extractExamples fixup val,
    Acknowledgement => val -> getSubsection(val, "Acknowledgement"),
    Contributors    => val -> getSubsection(val, "Contributors"),
    References      => val -> getSubsection(val, "References"),
    Caveat          => val -> getSubsection(val, "Caveat"),
    SeeAlso         => val -> getSubsection(UL (TO \ enlist val), "See also"),
    Subnodes        => val -> getSubnodes val,
    SourceCode      => val -> getSourceCode val,
    ExampleFiles    => val -> getExampleFiles val,
    }

documentOptions := new OptionTable from {
    Key => null,
    Headline => null,
    BaseFunction => null,
    Usage => null,
    Inputs => null,
    Outputs => null,
    Consequences => null,
    Acknowledgement => null,
    Contributors => null,
    References => null,
    Caveat => null,
    SeeAlso => null,
    Subnodes => null,
    SourceCode => null,
    ExampleFiles => null
    }

document = method(Dispatch => Thing, Options => documentOptions)
document List := opts -> args -> (
    if opts =!= documentOptions then error "'document' expects its optional arguments inside the list";
    if currentPackage === null  then error "encountered 'document' command, but no package is open";
    o := new MutableHashTable;
    scan(args, arg -> if instance(arg, Option) then (
	    key := arg#0;
	    if not documentOptions#?key then error("unknown documentation option ", format toString key) ;
	    if o#?key then error("option ", toString key, " encountered twice");
	    o#key = arg#1));
    -- Set the description
    o.Description = select(args, arg -> not instance(arg, Option));
    -- Set the primary key
    key := if o.?Key then o.Key else error "missing Key";
    rest := if instance(key, List) then (
	key = unique nonnull key;
	o.Key = first key;
	drop(key, 1)) else {};
    key = o.Key;
    fn := if instance(key, Sequence) then key#0 else if instance(key, Symbol) then value key else key;
    -- Set the document tag
    o.DocumentTag = tag := makeDocumentTag(key, Package => null);
    -- Check that all tags belong to this package and
    -- point the secondary keys to the primary one
    verfy := (key, tag) -> (
	if package tag =!= currentPackage#"pkgname"
	then error("item to be documented comes from another package: ", package tag, " :: ", toString key));
    verfy(key, tag);
    scan(rest, secondary -> (
	    tag2 := makeDocumentTag(secondary, Package => null);
	    verfy(secondary, tag2);
	    storeRawDocumentation(tag2, new HashTable from {
		    PrimaryTag => tag,
		    symbol DocumentTag => tag2,
		    "filename" => currentFileName,
		    "linenum" => currentLineNumber()
		    })));
    -- Check BaseFunction
    assert(not o.?BaseFunction or instance(o.BaseFunction, Function));
    -- Set the headline
    o.Headline = if o.?Headline then o.Headline else if instance(key, Sequence) and key#?0 then (
	title := headline key#0; if title =!= null then title else "") else "";
    if o.Headline === "" then remove(o, Headline);
    --
    currentNodeName = format tag;
    if member(currentNodeName, reservedNodeNames) then error("'document' encountered a reserved node name ", format currentNodeName);
    exampleOutputFilename = makeExampleOutputFileName(currentNodeName, currentPackage);
    -- Process all keywords
    scan(keys o, key -> if o#key =!= {} then o#key = KeywordFunctions#key o#key);
    -- Process Usage, Inputs, Outputs, Options, and Consequences
    if o.?Usage and o.Usage === "" then remove(o, Usage);
    if not o.?Usage and (o.?Inputs or o.?Outputs)
    then error "document: Inputs or Outputs specified, but Usage not provided";
    inp := if o.?Inputs then o.Inputs else {};
    out := if o.?Outputs then o.Outputs else {};
    iso := x -> instance(x, Option) and #x==2 and instance(x#0, Symbol);
    ino := select(inp, x -> iso x);
    inoh:= new HashTable from ino;
    inp  = select(inp, x -> not iso x);
    opt := getOptionDefaultValues key;
    if not isSubset(keys inoh, keys opt)
    then error concatenate("not among the options for ", toString fn, ": ", between_", " keys (set keys inoh - set keys opt));
    ino = join(ino, sortByName (keys opt - set keys inoh));
    if o.?Usage then (
	(inp', out') := getTypes key;
	inp' = select(inp', T -> T =!= Nothing);
	out' = select(out', T -> T =!= Nothing);
	if out' === {Thing} then out' = {};		    -- not informative enough
	if #inp === 0 then inp = inp';
	if #out === 0 then out = out';
	if #inp' =!= 0 then (
	    if #inp =!= #inp' then error ("mismatched number of inputs in documentation for ", toExternalString key);
	    inp = apply(inp',inp,(T,v) -> T => v));
	if #out' =!= 0 then (
	    if #out =!= #out' then error ("mismatched number of outputs in documentation for ", toExternalString key);
	    out = apply(out',out,(T,v) -> T => v)));
    proc := processInputOutputItems(key, fn);
    inp = proc \ inp;
    out = proc \ out;
    ino = proc \ ino;
    if #inp > 0 then o.Inputs = inp else remove(o, Inputs);
    if #out > 0 then o.Outputs = out else remove(o, Outputs);
    if o.?Consequences and #o.Consequences == 0 then remove(o, Consequences);
    -- Generate Hypertext containers
    scan(keys KeywordFunctions, sym -> if o#?sym then (
	    if sym === Consequences then scan(o#sym, x -> validate DIV x)
	    else if sym === Key then null
	    else if sym === BaseFunction then null
	    else if sym === symbol DocumentTag then null
	    else validate DIV o#sym));
    -- Set the location of the documentation
    o#"filename" = currentFileName;
    o#"linenum"  = currentLineNumber();
    o = new HashTable from o;
    storeRawDocumentation(tag, o);
    currentNodeName = null;
    )

-----------------------------------------------------------------------------
-- undocumented
-----------------------------------------------------------------------------

undocumented = method(Dispatch => Thing)
undocumented List  := L   -> scan(L, undocumented)
undocumented Thing := key -> if key =!= null then (
    tag := makeDocumentTag(key, Package => currentPackage);
    storeRawDocumentation(tag, new HashTable from {
	    symbol DocumentTag => tag,
	    "undocumented" => true,
	    "filename" => currentFileName,
	    "linenum" => currentLineNumber()}))

-- TODO: what does this do?
undocumented keys undocumentedkeys
undocumentedkeys = null
undocumented' = x -> error "late use of function undocumented'"

-----------------------------------------------------------------------------
-- SYNOPSIS
-----------------------------------------------------------------------------

SYNOPSIS = method(
    Dispatch => Thing,
    TypicalValue => Hypertext,
    Options => {
	BaseFunction => null,
	Heading => "Synopsis",
	Usage => "",
	Inputs => {},
	Outputs => {},
	Consequences => {}
	})
SYNOPSIS List     := o -> x -> SYNOPSIS splice (o, toSequence x)
SYNOPSIS Thing    :=
SYNOPSIS Sequence := o -> x -> (
    o = applyPairs(o, (k, v) -> (k, if v =!= {} then KeywordFunctions#k v else v));
    fn := o#BaseFunction;
    proc := processInputOutputItems(, fn);
    fixup DIV nonnull {
	SUBSECTION o.Heading,
	UL {
	    LI      o.Usage,
	    if 0 < #o.Inputs       then LI { "Inputs:",       UL ( proc \ o.Inputs ) },
	    if 0 < #o.Outputs      then LI { "Outputs:",      UL ( proc \ o.Outputs ) },
	    if 0 < #o.Consequences then LI { "Consequences:", UL          o.Consequences }
	    },
	x})

-----------------------------------------------------------------------------
-- miscellaneous
-----------------------------------------------------------------------------

mat := (pat, line) -> class line === String and match(pat, line)

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

synonym = X -> if X.?synonym then X.synonym else "object of class " | toString X

findSynonyms = method()
findSynonyms Symbol := x -> (
    result := {};
    scan(dictionaryPath, dict -> scan(pairs dict, (name, symb) ->
	    if x === symb and getGlobalSymbol name === symb then result = append(result, name)));
    sort unique result)

instances = method()
instances Type := HashTable => X -> hashTable apply(select(flatten(values \ dictionaryPath), i -> instance(value i,X)), i -> (i,value i))

-- TODO: make this unnecessary
ofClass = method()
ofClass Type          :=
ofClass ImmutableType := T -> fixup (
    if parent T === Nothing then error "expected a class";
    if T === Nothing then TO "null"
    else if T.?synonym then SPAN {indefiniteArticle T.synonym, TO2 {T, T.synonym}}
    else SPAN {"an instance of the type ", if isGlobalSymbol toString T then TO T else TT toString T})
ofClass List          := x -> (
    if #x === 1 then ofClass x#0
    else if #x === 2 then (ofClass x#0, " or ", ofClass x#1)
    else mingle (ofClass \ x, splice( #x - 2 : ", ", ", or " )))

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
