--		Copyright 1994-2006 by Daniel R. Grayson

needs "code.m2"
needs "hypertext.m2"
needs "methods.m2"
needs "packages.m2"
needs "reals.m2" -- for ImmutableType
needs "validate.m2" -- for fixup

-- TODO: deprecate
rootPath = "";
rootURI = "file://";

-----------------------------------------------------------------------------
-- Local variables
-----------------------------------------------------------------------------

currentDocumentTag = null
--currentHelpTag -- bring back from validate.m2

reservedNodeNames := {"Top", "Table of Contents"}

-- TODO: handle this in methods from code.m2
methodNames := set {NewFromMethod, NewMethod, NewOfFromMethod, NewOfMethod, id, Ext, Tor}

-----------------------------------------------------------------------------
-- Local utilities
-----------------------------------------------------------------------------

-- TODO: make these local
indefiniteArticle = s -> if match("[aeiouAEIOU]", s#0) and not match("^one ", s) then "an " else "a "
indefinite        = s -> concatenate(indefiniteArticle s, s)

enlist := x -> if instance(x, List) then nonnull x else {x}

errorLocation := () -> minimizeFilename toString currentPosition()

-----------------------------------------------------------------------------
-- verifying the document Key
-----------------------------------------------------------------------------
-- here we check that the method a putative document tag documents is actually installed
-- TODO: simplify this using methods from code.m2
verifyKey = method(Dispatch => Thing)
verifyKey Thing    := key -> key
verifyKey Sequence := key -> ( -- e.g., (res, Module) or (symbol **, Module, Module)
    if      #key == 0 then error("documentation key () encountered at ", errorLocation())
    else if #key == 1 and not instance(key#0, Function)
    then if signalDocumentationError key
    then printerr("error: documentation key ", format toString key,
	" encountered at ", errorLocation(), " but ", format toString key#0, " is not a function")
    else if #key  > 1
    and not any({Keyword, Command, Function, ScriptedFunctor}, type -> instance(key#0, type)) and not methodNames#?(key#0)
    and not (instance(key#0, Sequence) and 2 == #key#0 and key#0#1 === symbol= and instance(key#0#0, Keyword))
    then if signalDocumentationError key
    then printerr("error: documentation key ", format toString key,
	" encountered at ", errorLocation(), " but ", format toString key#0,
	" is not a function, command, scripted functor, or keyword");
    --
    if  isUnaryAssignmentOperator key           -- e.g., ((?, =), Type), or (?, =)
    or isBinaryAssignmentOperator key then true -- e.g., ((?, =), Type, Type)
    else if (
	-- this will all get screwed up with immutable types present
	if      #key  > 2 then ( t := youngest drop(key, 1); t#?key            and instance(t#key,         Function) )
	else if #key == 2 then ( instance(key#1, HashTable) and key#1#?(key#0) and instance(key#1#(key#0), Function) )
	else if #key == 1 then ( nullaryMethods#?key and instance(nullaryMethods#key, Function) )
	else false) then null
    else if #key > 1 and instance(key#0, Command) then verifyKey prepend(key#0#0, drop(key, 1))
    else if signalDocumentationError key
    then printerr("error: documentation key for ", format formatDocumentTag key,
	" encountered at ", errorLocation(), " but no method installed"))
verifyKey Array    := key -> (
    (nkey, opt) := (key#0, key#1);                    -- e.g., [(res, Module), Strategy]
    if instance(opt,  Option)   then opt = first opt; -- e.g., [(res, Module), Strategy => FastNonminimal]
    fn := if instance(nkey, Function) then nkey
    else  if instance(nkey, Sequence) then ( verifyKey nkey; first nkey )
    else if signalDocumentationError key
    then printerr("error: expected ", format toString nkey, " to be a function or existing method key in document tag for optional argument: ", silentRobustString(40, 1, key));
    if  not (options nkey)#?opt
    and not (options   fn)#?opt
    then if signalDocumentationError key
    then printerr("error: expected ", format toString  opt, " to be an optional argument for ", nkey, " in document tag for optional argument: ", silentRobustString(40, 1, key)))

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

-- TODO: make this a MutableHashTable, so we can cache values in it
-- the problem is that unique doesn't work with MutableHashTables
DocumentTag = new Type of HashTable
DocumentTag.synonym = "document tag"

format   DocumentTag := tag -> tag.Format
package  DocumentTag := tag -> getpkg tag.Package
toString DocumentTag :=
net      DocumentTag := tag -> concatenate (tag.Package, " :: ", format tag)

-- FIXME: this is kind of a hack
toExternalString DocumentTag := tag -> (
    "new DocumentTag from " | toExternalString {
	if instance(tag.Key, Symbol) then toString tag.Key else tag.Key, tag.Format, tag.Package})

new DocumentTag from BasicList := (T, t) -> (
    new HashTable from {
	Key                => t#0,
	Format             => t#1,
	symbol Package     => t#2,
	"RawDocumentation" => if t#?3 then t#3 else null
	})

DocumentTag ? DocumentTag := (x, y) -> x.Format ? y.Format
DocumentTag ? String      := (x, y) -> x.Format ? y
String      ? DocumentTag := (x, y) -> x        ? y.Format

-- helper for parsing " pkg :: key -- headline" to ("pkg", "key")
parseDocumentTag := key -> (
    key = first separate(" -- ", key); -- the spaces are there so "--" can be a valid key
    segments := separate("^[[:space:]]*|[[:space:]]*::[[:space:]]*|[[:space:]]*$", key);
    segments  = select(segments, segment -> segment =!= "");
    -- this is important, because the names of info nodes get extracted from text where
    -- lines might be wrapped and multiple spaces are reduced to one:
    if any(segments, segment -> match(" {2,}", segment))
    then error("expected key to have only single interior spaces:", format key);
    if      #segments == 0 then error("encountered empty documentation tag: ", format key)
    else if #segments == 1 then (null,       segments#0)
    else if #segments == 2 then (segments#0, segments#1)
    else error("encountered invalid documentation tag: ", format key))

makeDocumentTag' := opts -> key -> (
    nkey := normalizeDocumentKey(key, opts);
    verifyKey nkey;
    fkey := formatDocumentTag nkey;
    local pkg;
    (pkg, fkey) = parseDocumentTag fkey;
    -- Try to detect the package
    pkg = if pkg =!= null                    then pkg
    else  if opts#Package =!= null           then opts#Package
    else  if isMember(fkey, allPackages())   then fkey
    -- for these three types, the method package actually calls
    -- makeDocumentTag, so we can't use it, and need workarounds:
    else  if instance(nkey, Array)           then youngest toSequence(package' \ splice nkey)
    else  if instance(nkey, String)          then currentPackage -- FIXME
    -- Note: make sure Schubert2 can document (symbol SPACE, OO, RingElement)
    else  if instance(nkey, Sequence)        then youngest (package' \ splice nkey)
    else  if (pkg' := package' nkey) =!= null then pkg'
    else  if (pkg'  = package' fkey) =!= null then pkg';
    -- If not detected, signal an error and failover to currentPackage
    if pkg === null then (
	if currentDocumentTag === null   then error("makeDocumentTag: package cannot be determined: ", nkey) else
	if signalDocumentationError fkey then (
	    loc := locate currentDocumentTag;
	    printerr("error: reference ", format fkey, " was not found in any package. ",
		"First mentioned near:\n  ", loc#0, ":", toString loc#1));
	pkg = currentPackage);
    new DocumentTag from new HashTable from {
	Key            => nkey,
	Format         => fkey,
	symbol Package => (
	    if instance(pkg, Package) then if pkg === Core then "Macaulay2Doc" else pkg#"pkgname" else
	    if instance(pkg, Symbol) then toString pkg else
	    if instance(pkg, String) then pkg else ""),
	"RawDocumentation" => null
	})

makeDocumentTag = method(Dispatch => Thing, Options => { Package => null })
makeDocumentTag DocumentTag := opts -> identity
makeDocumentTag List        := opts -> L   -> apply(L, key -> makeDocumentTag(key, opts))
makeDocumentTag Thing       := opts -> key -> (makeDocumentTag' opts) key
makeDocumentTag String      := opts -> key -> (
    local pkg;
    (pkg, key) = parseDocumentTag key;
    if pkg =!= null and opts#Package =!= null and pkg =!= toString opts#Package
    then error ("mismatching packages ", pkg, " and ", toString opts#Package, " specified for key ", key);
    if pkg === null then pkg = opts#Package;
    (makeDocumentTag' new OptionTable from {Package => pkg}) key)

-- before creating links, we recreate the document tag as a hack to
-- correct its package, if it is incorrect (e.g. truncate, quotient)
-- TODO: can this be modified to fix the tag in-place? then we would only need to
-- fix the tag in (validate, TO), rather than also in (info, TO) and (html, TO).
fixup DocumentTag := DocumentTag => tag -> (
    tag' := if (rawdoc := fetchAnyRawDocumentation tag) =!= null then rawdoc.DocumentTag else tag;
    if package tag =!= package tag' then printerr("warning: ambiguous reference ",
	format toString tag, " and ", format toString tag', " when processsing ",
	toString locate currentDocumentTag);
    tag')

-----------------------------------------------------------------------------
-- formatting document tags
-----------------------------------------------------------------------------
-- The formatted form should be a human-readable string, and different
-- normalized tags should yield different formatted tags.
-- The formatted tag is used for two purposes:
--    for display in menus and links
--    as the key for access in a database, where the key must be a string

prefix := set flexiblePrefixOperators

typicalValue := k -> (
    if  typicalValues#?k     then typicalValues#k
    else if instance(k, Sequence)
    and typicalValues#?(k#0) then typicalValues#(k#0)
    else Thing)

fSeq := new HashTable from splice {
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
    (2, class, Keyword ) => s -> (toString s#0, " ", toString s#1), -- prefix operator
    (3, symbol SPACE   ) => s -> (toString s#1, " ", toString s#2),
    (2, symbol (*)     ) => s -> (toString s#1, " ", toString s#0), -- postfix operator
    (2, symbol ^*      ) => s -> (toString s#1, " ", toString s#0), -- postfix operator
    (2, symbol _*      ) => s -> (toString s#1, " ", toString s#0), -- postfix operator
    (2, symbol ~       ) => s -> (toString s#1, " ", toString s#0), -- postfix operator
    (2, symbol ^~      ) => s -> (toString s#1, " ", toString s#0), -- postfix operator
    (2, symbol _~      ) => s -> (toString s#1, " ", toString s#0), -- postfix operator
    (2, symbol !       ) => s -> (toString s#1, " ", toString s#0), -- postfix operator
    (2, symbol ^!      ) => s -> (toString s#1, " ", toString s#0), -- postfix operator
    (2, symbol _!      ) => s -> (toString s#1, " ", toString s#0), -- postfix operator
    --(2, symbol ^#      ) => s -> (toString s#1, " ", toString s#0), -- postfix operator
    --(2, symbol _#      ) => s -> (toString s#1, " ", toString s#0), -- postfix operator

    -- assignment methods
    (3, class, Sequence) => s -> (toString s#1, " ", toString s#0#0, " ",
	toString s#2, " ", toString s#0#1, " ", toString typicalValue s),
    (2, class, Sequence) => s -> (
	op := s#0#0;
	if prefix#?op
	then (toString op, " ", toString s#1, " ", toString s#0#1, " ",
	    toString typicalValue s)
	else (toString s#1, " ", toString op, " ", toString s#0#1, " ",
	    toString typicalValue s)),
    (2, symbol <-      ) => s -> (toString s#1, " <- ",
	toString typicalValue s),
    apply(augmentedAssignmentOperators, op -> (2, op) => s ->
	(toString s#1, " ", toString op, " ", toString typicalValue s)),

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
	    printerr("warning: ambiguous scripted functor, with both subscript method and superscript method: ", toString s);
	    toString s)
	else if hh.?subscript   then (toString hh, " _ ", toString s#1)
	else if hh.?superscript then (toString hh, " ^ ", toString s#1)
	else (toString hh, " ", toString s#1)),

    -- Methods
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
formatDocumentTag String   := identity
formatDocumentTag Array    := s -> (
    (fn, opt, val) := (s#0, s#1, "..."); -- TODO: eventually support [(func, X, Y), A => 1, B => 2, C => 3]
    if instance(opt, Option)  then (opt, val) = toSequence opt;
    if instance(fn, Sequence) and 0 < #fn
    then concatenate(toString fn#0, "(", between(",", apply(drop(fn, 1), toString)), ",", toString opt, "=>", toString val, ")")
    else concatenate(toString fn,   "(...,", toString opt, "=>", toString val, ")"))
formatDocumentTag Sequence := s -> concatenate (
    if #s == 0                                           then toString
    else if            fSeq#?(#s, s#0)                   then fSeq#(#s, s#0)
    else if #s > 1 and fSeq#?(#s, s#0, s#1)              then fSeq#(#s, s#0, s#1)
    else if #s > 1 and fSeq#?(#s, class, class s#0, s#1) then fSeq#(#s, class, class s#0, s#1)
    else if            fSeq#?(#s, class, class s#0)      then fSeq#(#s, class, class s#0)
    else if            fSeq#?(class s#-1, #s)            then fSeq#(class s#-1, #s)
    else if            fSeq#?#s                          then fSeq#(#s)
    else toString) s

-----------------------------------------------------------------------------
-- storeRawDocumentation
-----------------------------------------------------------------------------
storeRawDocumentation := (tag, rawdoc) -> (
    fkey := format tag;
    if currentPackage#rawKey#?fkey and signalDocumentationError tag then (
	newloc := toString new FilePosition from (
	    minimizeFilename(
		currentPackage#"source directory" | rawdoc#"filename"),
 	    rawdoc#"linenum", 0);
	rawdoc = currentPackage#rawKey#fkey;
	oldloc := toString locate rawdoc.DocumentTag;
	printerr("error: documentation already provided for ", format tag);
	printerr(newloc, ": ... here is the (end of the) new documentation");
	printerr(oldloc, ": ... here is the (end of the) previous documentation"));
    currentPackage#rawKey#fkey = rawdoc)

-----------------------------------------------------------------------------
-- fetchRawDocumentation, fetchRawDocumentationNoLoad
-----------------------------------------------------------------------------
fetchRawDocumentation = method()
fetchRawDocumentation DocumentTag      :=  tag            -> fetchRawDocumentation(getpkg tag.Package, format tag)
fetchRawDocumentation(String,  String) := (pkgname, fkey) -> fetchRawDocumentation(getpkg pkgname, fkey)
fetchRawDocumentation(Package, String) := (pkg,     fkey) -> ( -- returns null if none
    rawdoc := pkg#rawKey;
    if rawdoc#?fkey then rawdoc#fkey else if pkg#?rawKeyDB then (
	rawdoc = pkg#rawKeyDB;
	if isOpen rawdoc and rawdoc#?fkey then evaluateWithPackage(getpkg "Text", rawdoc#fkey, value)))

fetchRawDocumentationNoLoad = method()
fetchRawDocumentationNoLoad(Nothing, Thing)  := (pkg,     fkey) -> null
fetchRawDocumentationNoLoad DocumentTag      :=  tag            -> fetchRawDocumentationNoLoad(getpkgNoLoad tag.Package, format tag)
fetchRawDocumentationNoLoad(String,  String) := (pkgname, fkey) -> fetchRawDocumentationNoLoad(getpkgNoLoad pkgname, fkey)
fetchRawDocumentationNoLoad(Package, String) := (pkg,     fkey) -> ( -- returns null if none
    rawdoc := pkg#rawKey;
    if rawdoc#?fkey then rawdoc#fkey else if pkg#?rawKeyDB then (
	rawdoc = pkg#rawKeyDB;
	if isOpen rawdoc and rawdoc#?fkey then evaluateWithPackage(getpkg "Text", rawdoc#fkey, value)))

-----------------------------------------------------------------------------
-- getPrimaryTag, fetchAnyRawDocumentation
-----------------------------------------------------------------------------
getPrimaryTag = method()
getPrimaryTag DocumentTag := tag -> (
    -- TODO: slow if package isn't loaded
    if (rawdoc := fetchRawDocumentation tag) =!= null
    and rawdoc#?PrimaryTag then rawdoc#PrimaryTag else tag)

-- TODO: somehow cache this
fetchAnyRawDocumentation = method()
fetchAnyRawDocumentation DocumentTag := tag  -> (
    rawdoc := fetchRawDocumentation getPrimaryTag tag;
    if rawdoc =!= null then rawdoc else fetchAnyRawDocumentation format tag)
-- TODO: if Package$Core was the same as Macaulay2Doc, this would not be necessary
fetchAnyRawDocumentation String      := fkey -> scan(prepend("Macaulay2Doc", loadedPackages), pkg -> (
	rawdoc := fetchRawDocumentation getPrimaryTag makeDocumentTag(fkey, Package => pkg);
	if rawdoc =!= null then break rawdoc))

-----------------------------------------------------------------------------
-- store and fetch Processed Documentation
-----------------------------------------------------------------------------
-- TODO: improve this
storeProcessedDocumentation = (pkg, tag, opts, verboseLog) -> (
    fkey := format tag;
    verboseLog("processing   ", toString tag);
    pkg#"processed documentation"#fkey = (
	processExamplesStrict = not opts.IgnoreExampleErrors;
	-- sort of a kludge: what if an error occurs and the variable isn't reset?
	first (help tag, processExamplesStrict = true )))

fetchProcessedDocumentation = (pkg, fkey) -> (
    if pkg#"processed documentation"#?fkey then pkg#"processed documentation"#fkey
    else error("internal error: documentation node not processed yet: ", fkey));

-----------------------------------------------------------------------------
-- inquiring the status of a key or DocumentTag
-----------------------------------------------------------------------------
isMissingDoc     = tag -> ( d := fetchRawDocumentation tag; d === null )
isSecondaryTag   = tag -> ( d := fetchRawDocumentation tag; d =!= null and d#?PrimaryTag )
isUndocumented   = tag -> ( d := fetchRawDocumentation tag; d =!= null and d#?"undocumented" and d#"undocumented" === true )
hasDocumentation = key -> null =!= fetchAnyRawDocumentation makeDocumentTag(key, Package => null)

locate DocumentTag := tag -> new FilePosition from (
    rawdoc := fetchAnyRawDocumentation tag;
    if rawdoc =!= null then (
	pkg := package' rawdoc.DocumentTag;
	src := minimizeFilename(pkg#"source directory" | rawdoc#"filename");
	src, rawdoc#"linenum", 0)
    else (currentFileName, currentRowNumber(), currentColumnNumber()))

-----------------------------------------------------------------------------
-- helpers for the document function
-----------------------------------------------------------------------------

-- e.g. gb vs (codim, Ideal)
getOptionBase := key -> (
    if instance(key, Symbol) then key = value key;
    try if options key   =!= null then return key;
    try if options key#0 =!= null then return key#0;
    key)

emptyOptionTable := new OptionTable from {}
-- TODO: this should return either an OptionTable or true, if any option is accepted
getOptionDefaultValues := method(Dispatch => Thing)
getOptionDefaultValues Thing    := x -> emptyOptionTable
getOptionDefaultValues Symbol   := x -> if instance(f := value x, Function) then getOptionDefaultValues f else emptyOptionTable
getOptionDefaultValues Sequence := s -> options s ?? getOptionDefaultValues s#0 ?? emptyOptionTable
getOptionDefaultValues Function := f -> options f ?? emptyOptionTable

isType := T -> instance(T, Type)
isTypeSignature := L -> instance(L, List) and all(L, isType)

processSignature := (tag, fn) -> (type0, item) -> (
    if tag === null then tag = makeDocumentTag fn;
    -- "inp" => ZZ => ("hypertext sequence")
    --  opt  => ZZ => ("hypertext sequence")
    optbase := getOptionBase tag.Key;
    optsymb := null; --  opt
    inpname := null; -- "inp"
    type := null;    -- ZZ or {Ideal,Module}
    text := null;    -- ("hypertext sequence")
    -- 'type0' is the type in the primary document tag, e.g. Module,
    -- while 'type' may be Ideal or {Ring,Ideal,Module}, etc.

    -- checking for various pieces of the synopsis item
    -- allow either a variable name or a visible list for rare exceptions
    isVariable   := y -> match(///\`[[:alnum:]']+\'///, y) or match(///(\".+\")|(\(.+\))|(\[.+\])|(\{.+\})|(<\|.+\|>)///, y);
    isOptionName := y -> all({text, inpname, optsymb}, x -> x === null) and instance(y, Symbol);
    isInputName  := y -> all({text, inpname, optsymb}, x -> x === null) and instance(y, String) and isVariable y;
    -- putting null or Nothing as input type means don't display the type deduced from the description of the method
    -- putting a list like {ZZ,QQ} as input type means either is acceptable
    isInputType  := y -> y === Nothing or isType y and isInputType {y} or isTypeSignature y and (
	-- TODO: the other types are not sanity checked
	type0 === null or if isMember(type0, y) then true else error("type mismatch: ",
	    type0, " not among types ", y, " in documentation for ", format tag));
    isInputText  := y -> text === null and any({String, Hypertext, List, Sequence}, T -> instance(y, T));

    -- parse the chain of options
    if debugLevel > 1 then printerr("raw synopsis item:\t", toExternalString item);
    -- e.g: given 1=>2=>3, applies the lambda function to 1, then 2, then 3
    mapo := f -> g := x -> if instance(x, Option) then ( f x#0 ; g x#1 ) else f x;
    (mapo (y ->
	    if          null === y then null
	    else if isOptionName y then optsymb = y -- option symbol, e.g Strategy
	    else if  isInputName y then inpname = y --    input name, e.g n
	    else if  isInputType y then    type = y --    input type, e.g ZZ or {Ideal,Module}
	    else if  isInputText y then    text = y --   description, e.g {"hypertext sequence"}
	    else error("encountered unrecognizable synopsis item ", toString y, " in documentation for ", format tag))
	) item;
    -- if no type information is provided,
    -- get type from the method sequence
    if type === null then type = type0;
    if debugLevel > 1 then printerr("parsed synopsis item:\t", toExternalString (optsymb, inpname, type, text));

    result := if optsymb === null then {
	-- e.g: n, an integer, then description
	if inpname =!= null then TT inpname,
	if    type =!= null and type =!= Nothing then ofClass type, -- type Nothing, treated as above
	text}
    else if optbase =!= null then (
	opts := getOptionDefaultValues optbase;
	if opts === true then opts = new OptionTable from { optsymb => null };
	if not opts#?optsymb then error("symbol ", optsymb, " is not the name of an optional argument for function ", toExternalString optbase);
	opttag := getPrimaryTag makeDocumentTag([optbase, optsymb], Package => package' tag);
	name := if tag === opttag then TT toString optsymb else TO2 { opttag, toString optsymb };
	type  = if type =!= null and type =!= Nothing then ofClass type else TT "..."; -- type Nothing is treated as above
	maybeformat := if instance(opts#optsymb, String) then format else identity;
	defval := SPAN{"default value ", maybeformat reproduciblePaths replace("^Function\\[.*\\]", "Function[]", toString opts#optsymb)};
	text = if text =!= null and #text > 0 then text else if tag =!= opttag then LATER {() -> nonnull SPAN headline opttag};
	text = if text =!= null and #text > 0 then (", ", text);
	-- e.g: Key => an integer, default value 42, the meaning of the universe
	{ (name, TT " => ", type), nonnull (defval, text) })
    else {TT {toString optsymb, " => ..."}};
    SPAN nonnull deepSplice between_", " nonnull nonempty result)

getSignature := method(Dispatch => Thing)
getSignature Thing    := x -> ({},{})
getSignature Array    := x -> ({},{})
getSignature Function := x -> ({},{typicalValue x})
getSignature Sequence := x -> (
    (inputs, outputs) := (
	-- putting something like OO in the key indicates a fake dispatch
	x' := select(drop(toList x, 1), T -> not ancestor(Nothing, T));
	-- assignment methods
	-- TODO: should we worry about the possibility of the input
	-- and output types being different?
	if (instance(x#0, Sequence) and #x#0 === 2 and x#0#1 === symbol=
	    or #x === 2 and x#0 === symbol<-
	    or isMember(x#0, augmentedAssignmentOperators))
	then (append(x', typicalValue x), {typicalValue x})
	-- for "new T from x", T is already known, so we just care about x
	else if x#0 === NewFromMethod
	then ( {x#2} , { typicalValue x } )
	else (  x'   , { typicalValue x } ));
    -- When T is not exported, its class evaluates to Symbol instead of Type
    inputs  = apply(inputs,  T -> if instance(T, Symbol) then value T else T);
    outputs = apply(outputs, T -> if instance(T, Symbol) then value T else T);
    (inputs, outputs))

isOption := opt -> instance(opt, Option) and #opt == 2 and instance(opt#0, Symbol);

processUsage := (tag, fn, o) -> (
    if not o.?Usage then (
	if o.?Inputs or o.?Outputs then error "document: Inputs or Outputs specified, but Usage not provided";
	return ({}, {}, {}));
    -- inputs, outputs, and options provided in the documentation
    arguments := partition(isOption, if o.?Inputs then o.Inputs else {}, {false, true});
    inputList  := arguments#false;
    optionList := arguments#true;
    outputList := if o.?Outputs then o.Outputs else {};
    optionNames := keys hashTable optionList;
    -- default inputs, outputs, and options of the function or method
    (inputs0, outputs0) := getSignature tag.Key;
    options0 := getOptionDefaultValues  tag.Key;
    -- e.g. map(Module, Nothing, Matrix)
    inputs0  = select( inputs0, T -> T =!= Nothing);
    outputs0 = select(outputs0, T -> T =!= Nothing and T =!= Thing);
    options0 = if options0 === true then optionNames else keys options0;
    -- check that option names are valid
    if not isSubset(optionNames, options0) then error("not among the options for ", format tag, ": ",
	between_", " (optionNames - set options0));
    optionList = join(optionList, sortByName (options0 - set optionNames));
    --
    if  #inputList === 0 then  inputList =  inputs0;
    if #outputList === 0 then outputList = outputs0;
    if #inputs0  == 0 then  inputs0 =  #inputList:null else if  #inputList =!=  #inputs0 then error ("mismatched number of inputs in documentation for ",  format tag);
    if #outputs0 == 0 then outputs0 = #outputList:null else if #outputList =!= #outputs0 then error ("mismatched number of outputs in documentation for ", format tag);
    inputList  = apply(inputs0,  inputList,  identity);
    outputList = apply(outputs0, outputList, identity);
    optionList = apply(optionList, option -> (null, option));
    apply((inputList, outputList, optionList), x -> apply(x, processSignature(tag, fn))))

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

-- TODO: do this processing in help.m2, not here
getUsage := val -> (
    if not instance(val, String) then error "Usage: expected a string";
    val = apply(nonempty separate val, u -> replace("^[[:space:]]*(.*)[[:space:]]*$", "\\1", u));
    if #val === 0 then error "Usage: expected content";
    DL flatten { "class" => "element", DT "Usage: ", DD \ M2CODE \ val } )

getHeadline   := (val, key)   -> (
    title := if instance(val, String) then fixup val else error("expected ", toString key, " option to be a string");
    if #title > 200 then   error("document: documentation headlines must be less than 100 characters long:\n  " | format title);
    if #title > 100 then warning("document: documentation headlines must be less than 100 characters long:\n  " | format title);
    title)
getSubsection := (val, title) -> fixup DIV { SUBSECTION title, val }
getSourceCode :=  val         -> DIV {"class" => "waystouse",
    fixup DIV {SUBSECTION "Code", PRE M2CODE demark_newline unstack stack apply(enlist val, m -> (
		f := lookup m; if f === null then error("SourceCode: ", toString m, ": not a method");
		c := code f;   if c === null then error("SourceCode: ", toString m, ": code for method not found");
		reproduciblePaths toString net c))}}
getSubnodes := val -> (
    if #val == 0 then error "encountered empty Subnodes list"
    else MENU apply(val, x -> fixup (
	    if      instance(x, TOH)       then TO {x#0}
	    else if instance(x, String)    then x
	    else if instance(x, Hypertext) then x
	    else error ("unrecognizable Subnode list item: ", x))))
getExampleFiles := val -> (
    if not currentPackage.Options.AuxiliaryFiles
    then error "ExampleFiles option specified, but AuxiliaryFiles option is not set to 'true'";
    if not (instance(val, List) and all(val, fn -> instance(fn, String)))
    then error "expected ExampleFiles option to be a list of strings";
    auxiliaryFilesDirectory := currentPackage#"source directory" | currentPackage#"pkgname" | "/";
    val = apply(val, fn -> auxiliaryFilesDirectory | fn);
    for fn in val do if not fileExists fn then error ("example data file not found: ", fn);
    currentPackage#"example data files"#(format currentDocumentTag) = val; "")
getBaseFunction := val -> (
    if val =!= null and not instance(val, Function)
    then error "expected BaseFunction option value to be a function" else val)

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
    Citation        => identity, -- TODO: eventually might want to process this
    Caveat          => val -> getSubsection(val, "Caveat"),
    SeeAlso         => val -> getSubsection(UL (TO \ enlist val), "See also"),
    Subnodes        => val -> getSubnodes enlist val,
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
    Citation => null,
    Caveat => null,
    SeeAlso => null,
    Subnodes => null,
    SourceCode => null,
    ExampleFiles => null
    }

-- TODO: improve logging
document = method(Dispatch => Thing, Options => documentOptions)
document List := opts -> args -> (
    verboseLog := if debugLevel > 2 then printerr else identity;
    if opts =!= documentOptions then error "'document' expects its optional arguments inside the list";
    if currentPackage === null  then error "encountered 'document' command, but no package is open";
    o := new MutableHashTable;
    scan(args, arg -> if instance(arg, Option) then (
	    key := arg#0;
	    if not documentOptions#?key then error("unknown documentation option ", format toString key) ;
	    if o#?key then error("option ", toString key, " encountered twice");
	    o#key = arg#1));
    -- Set the description
    o.Description = select(nonnull args, arg -> not instance(arg, Option));
    -- Set the primary key
    key := if o.?Key then o.Key else error "missing Key";
    rest := if instance(key, List) then (
	key = unique nonnull key;
	o.Key = first key;
	drop(key, 1)) else {};
    key = o.Key;
    fn := if instance(key, Sequence) then key#0 else if instance(key, Symbol) then value key else key;
    -- Set the document tag
    currentDocumentTag = o.DocumentTag = tag := makeDocumentTag(key, Package => currentPackage);
    fkey := format tag;
    verboseLog("Processing documentation for ", fkey);
    if isMember(fkey, reservedNodeNames) then error("'document' encountered a reserved node name ", fkey);
    -- Check that all tags belong to this package and
    -- point the secondary keys to the primary one
    verfy := (key, tag) -> (
	if tag.Package =!= currentPackage#"pkgname"
	then error("item to be documented comes from another package: ", toString tag));
    verfy(key, tag);
    scan(rest, secondary -> (
	    tag2 := makeDocumentTag(secondary, Package => currentPackage);
	    verfy(secondary, tag2);
	    storeRawDocumentation(tag2, new HashTable from {
		    PrimaryTag => tag, -- tag must be primary
		    symbol DocumentTag => tag2,
		    "filename" => relativizeFilename(
			currentPackage#"source directory", currentFileName),
		    "linenum" => currentRowNumber()
		    })));
    -- Check BaseFunction
    assert(not o.?BaseFunction or instance(o.BaseFunction, Function));
    -- Set the headline
    o.Headline = if o.?Headline then o.Headline else if instance(key, Sequence) and key#?0 then (
	title := headline key#0; if title =!= null then title else "") else "";
    if o.Headline === ""                        then remove(o, Headline);
    if o.?Usage        and o.Usage === ""       then remove(o, Usage);
    if o.?Consequences and #o.Consequences == 0 then remove(o, Consequences);
    -- Process all keywords
    scan(keys o, key -> if o#key =!= {} then o#key = KeywordFunctions#key o#key);
    -- Process Inputs, Outputs, Options
    (inp, out, ino) := processUsage(tag, fn, o);
    if #inp > 0 then o.Inputs  = inp else remove(o, Inputs);
    if #out > 0 then o.Outputs = out else remove(o, Outputs);
    if #ino > 0 then o.Options = ino else remove(o, Options);
    -- Set the location of the documentation
    o#"filename" = relativizeFilename(
	currentPackage#"source directory", currentFileName);
    o#"linenum"  = currentRowNumber();
    currentDocumentTag = null;
    storeRawDocumentation(tag, new HashTable from o))

-----------------------------------------------------------------------------
-- undocumented
-----------------------------------------------------------------------------

undocumented = method(Dispatch => Thing)
undocumented List  := L   -> scan(L, undocumented)
undocumented Thing := key -> if key =!= null then (
    tag := makeDocumentTag(key, Package => currentPackage);
    storeRawDocumentation(tag, new HashTable from {
	    symbol DocumentTag => tag,
	    "undocumented"     => true,
	    "filename"         => relativizeFilename(
		currentPackage#"source directory", currentFileName),
	    "linenum"          => currentRowNumber()
	    }))

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
    proc := item -> (processSignature(, fn))(null, item);
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
    if T === Nothing then TO "Macaulay2Doc :: null"
    else if T.?synonym then SPAN {indefiniteArticle T.synonym, TO2 {T, T.synonym}}
    else SPAN {"an instance of the type ", if isGlobalSymbol toString T then TO T else TT toString T})
ofClass List          := x -> (
    if #x === 1 then ofClass x#0
    else if #x === 2 then (ofClass x#0, " or ", ofClass x#1)
    else mingle (ofClass \ x, splice( #x - 2 : ", ", ", or " )))

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
