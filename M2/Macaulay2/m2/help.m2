-----------------------------------------------------------------------------
-- Methods for getting help and accessing the documentation
-----------------------------------------------------------------------------
-* Exported:
 * help
 * (symbol?, Thing)
 * viewHelp
 * infoHelp
 * apropos
 * about
 * pager
 *-

-----------------------------------------------------------------------------
-- Local variables
-----------------------------------------------------------------------------

-- set by about and used by (help, ZZ)
lastabout := null

authorDefaults    := new HashTable from { Name => "Anonymous", Email => null, HomePage => null }

noBriefDocThings  := new HashTable from {
    symbol<  => true,
    symbol>  => true,
    symbol== => true
    }

binary   := set flexibleBinaryOperators
prefix   := set flexiblePrefixOperators
postfix  := set flexiblePostfixOperators
operator := binary + prefix + postfix

-----------------------------------------------------------------------------
-- Local utilities
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- these menus have to get sorted, so optTO and optTOCLASS return sequence:
--   the first three members of the pair are used for sorting
--   the last member is the corresponding hypertext entry in the UL list
-----------------------------------------------------------------------------

-- TODO: something like
--    * validate, see validate(Hypertext) -- blah blah
-- does not work with (symbol *, String), and the issue is here
counter := 0
next := () -> counter = counter + 1
optTO := key -> (
    tag := makeDocumentTag(key, Package => package key);
    fkey := format tag;
    if isUndocumented tag then return;
    if isSecondaryTag tag then (
	ptag := getPrimaryTag tag;
	(format ptag, fkey, next(), fixup if currentHelpTag === ptag then fkey else SPAN {TT format fkey, " -- see ", TOH{ptag}}))
    -- need an alternative here for secondary tags such as (export,Symbol)
    else (fkey, fkey, next(), TOH{tag}))
-- this isn't different yet, work on it!
optTOCLASS := key -> (format first (r := TOH{key}), next(), r)

-- TODO: duplicate of ul in hypertext.m2
ul := t -> if #t =!= 0 then UL t else t
menu       := s -> ul (last \         nonnull \\ optTO      \ toList s)
smenu      := s -> ul (last \ sort \\ nonnull \\ optTO      \ toList s)
smenuCLASS := s -> ul (last \ sort \\ nonnull \\ optTOCLASS \ toList s)

-- this is a simplified version of submenu in SimpleDoc
-- used in html.m2 and format.m2
redoMENU = contents -> (
    contents = deepApply'(contents, identity, item -> instance(item, BasicList) and not isLink item);
    DIV prepend(
	HEADER3 "Menu",
	nonnull sublists(contents,
	    line    -> isLink line,
	    section -> UL apply(section, line -> (
		    if instance(line, TO2) then return line;
		    link := optTO line#0;
		    if link === null then error("undocumented menu item ", toString line#0);
		    last link)),
	    line -> if instance(line, Hypertext) then line else HEADER4 {line}))
    )

-----------------------------------------------------------------------------
-- Handling method options
-----------------------------------------------------------------------------

reverseOptionTable := null

addro := (sym, meth) -> (
    if not reverseOptionTable#?sym then reverseOptionTable#sym = new MutableHashTable;
    reverseOptionTable#sym#meth = true;)

initializeReverseOptionTable := () -> (
    reverseOptionTable = new MutableHashTable;
    scan(dictionaryPath, dict -> scan(values dict, S -> (
		X := value S;
		if instance(X, Function) then (
		    opts := options X;
		    if opts =!= null then scanKeys(opts, symb -> addro(symb, X)))
		else if instance(X, Type) then scan(pairs X, (methodKey, methodFunction) -> (
			if (instance(methodKey,      MethodFunctionWithOptions) or
			    instance(methodKey,      Sequence))
			and instance(methodFunction, Function) then (
			    opts := options methodFunction;
			    if opts =!= null then (
				if instance(methodKey, MethodFunctionWithOptions) then methodKey = (methodKey, X);
				scanKeys(opts, symb -> addro(symb, methodKey))))
			))
		))))

-----------------------------------------------------------------------------
-- isDocumentableThing and documentableMethods
-----------------------------------------------------------------------------

-- we're not looking for documentable methods here, just documentable objects
isDocumentableThing := method(Dispatch => Thing)
isDocumentableThing    String :=
isDocumentableThing  Sequence := key -> false
isDocumentableThing   Nothing :=
isDocumentableThing    Symbol := key -> true
isDocumentableThing     Thing :=
isDocumentableThing      Type := key -> hasAttribute(key, ReverseDictionary) and isDocumentableMethod getAttribute(key, ReverseDictionary)

-- assignment methods look like ((symbol *, symbol =), X, Y, Z)
isDocumentableMethod = method(Dispatch => Thing)
isDocumentableMethod    Thing := key -> false
isDocumentableMethod Sequence := key -> all(key, s -> isDocumentableMethod s)
isDocumentableMethod   Symbol := key -> isGlobalSymbol toString key and getGlobalSymbol toString key === key
isDocumentableMethod     Type := key -> isDocumentableThing key

isDocumentableMethod Function        := fn -> hasAttribute(fn, ReverseDictionary) and dictionary getAttribute(fn,ReverseDictionary) =!= null
isDocumentableMethod ScriptedFunctor := fn -> hasAttribute(fn, ReverseDictionary)

documentableMethods := key -> select(methods key, isDocumentableMethod)

-----------------------------------------------------------------------------
-- documentationValue
-----------------------------------------------------------------------------

-- specialized templates for documentation nodes
documentationValue := method(TypicalValue => Hypertext)
documentationValue(Symbol, Thing) := (S, X) -> ()
-- e.g. Macaulay2Doc :: MethodFunction
documentationValue(Symbol, Type)  := (S, T) -> (
    syms := unique flatten(values \ dictionaryPath);
    -- constructors of T
    a := smenu apply(select(pairs typicalValues, (key, Y) -> Y === T and isDocumentableMethod key), (key, Y) -> key);
    -- types that inherit from T
    b := smenu(toString \ select(syms, y -> instance(value y, Type) and parent value y === T));
    -- functions on T
    c := smenu select(documentableMethods T, key -> not typicalValues#?key or typicalValues#key =!= T);
    -- objects of type T
    e := smenu(toString \ select(syms, y -> not mutable y and instance(value y, T)));
    DIV nonnull splice ( "class" => "waystouse",
	if #b > 0 then ( SUBSECTION {"Types of ", TT if T.?synonym then T.synonym else toString T, " :"}, b),
	if #a > 0 then ( SUBSECTION {"Functions and methods returning ",     indefinite synonym T, " :"}, a),
	if #c > 0 then ( SUBSECTION {"Methods that use ",                    indefinite synonym T, " :"}, c),
	if #e > 0 then ( SUBSECTION {"Fixed objects of class ",                     TT toString T, " :"}, e)))
-- e.g. Macaulay2Doc :: Strategy
documentationValue(Symbol, Symbol) := (S, S') -> (
    initializeReverseOptionTable();
    -- functions that take S as option
    opts := if reverseOptionTable#?S then keys reverseOptionTable#S else {};
    reverseOptionTable = null;
    a := smenu apply(select(opts, f -> isDocumentableMethod f), f -> [f, S]);
    if #a > 0 then DIV { -- "class" => "waystouse", -- we want this one to be larger
	 SUBSECTION {"Functions with optional argument named ", TT toString S, " :"}, a})
-- e.g. Macaulay2Doc :: help
documentationValue(Symbol, Command)         := (S, c) -> documentationValue(S, c#0)
-- e.g. Macaulay2Doc :: sum
documentationValue(Symbol, ScriptedFunctor) :=
documentationValue(Symbol, Function)        :=
documentationValue(Symbol, Keyword)         := (S, f) -> (
    -- methods of f
    a := smenu documentableMethods f;
    if #a > 0 then DIV nonnull splice ( "class" => "waystouse",
	SUBSECTION {"Ways to use ", TT toString f, ":"}, a))

-- TODO: simplify this process
-- e.g. Macaulay2Doc :: Macaulay2Doc
documentationValue(Symbol, Package)         := (S, pkg) -> if pkg =!= Core then (
    -- package filename
    fn := pkg#"pkgname" | ".m2";
    -- authors
    au := pkg.Options.Authors;
    -- exported symbols
    e := toSequence pkg#"exported symbols";
    -- functions and commands
    a := select(e, x -> instance(value x, Function) or instance(value x, Command));
    -- types
    b := select(e, x -> instance(value x, Type));
    -- methods
    m := unique flatten for T in b list (
	for i in keys value T list (
	    if (instance(i, Sequence)
		and #i > 1 and (
		    instance(i#0, Symbol) and i#1 =!= symbol= or
		    instance(i#0, Function))
		and isDocumentableMethod i)       then  i
	    else
	    if (instance(i, Keyword) or
		instance(i, Function) or
		instance(i, ScriptedFunctor))
	    and isDocumentableMethod (i, value T) then (i, value T)
	    else continue));
    -- symbols
    c := select(e, x -> instance(value x, Symbol));
    -- other things
    d := toList(set e - set a - set b - set c);
    -- the result
    DIV nonnull splice (
	if #au > 0 then DIV {
	    SUBSECTION (if #au === 1 then "Author" else "Authors"),
	    fixup UL apply(au, au -> (
		    (defs, args) := override(authorDefaults, toSequence au);
		    LI {
			if defs.HomePage === null then defs.Name else HREF{defs.HomePage, defs.Name},
			if defs.Email    =!= null then SPAN{" <", HREF{concatenate("mailto:", defs.Email), defs.Email}, ">"}}))
	    },
	if (cert := pkg.Options.Certification) =!= null then (
	    cert  = new HashTable from cert;
	    -- TODO: compare with the one in installPackage.m2
	    star := IMG { "src" => replace("PKG", "Style",currentLayout#"package") | "GoldStar.png", "alt" => "a gold star"};
	    commit := replace("(?<=/blob/)master", toString cert#"release at publication", cert#"repository code URI");
	    DIV {
		SUBSECTION {"Certification ", star},
		PARA {
		    "Version ", BOLD cert#"version at publication", " of this package was accepted for publication",
		    " in ",     HREF{cert#"volume URI", "volume " | cert#"volume number"},
		    " of ",     HREF{cert#"journal URI",            cert#"journal name"},
		    " on ",          cert#"acceptance date", ", in the article ",
		                HREF{cert#"published article URI",  cert#"article title"}, ".",
		    " That version can be obtained",
		    " from ",   HREF{cert#"published code URI", "the journal"}, " or",
		    " from ",   HREF{commit, ("the ", EM "Macaulay2", " source code repository")},
		    "."}}
	    ),
	DIV {
	    SUBSECTION "Version",
	    PARA { "This documentation describes version ", BOLD pkg.Options.Version, " of ",
		if pkg#"pkgname" === "Macaulay2Doc" then "Macaulay2" else pkg#"pkgname", "." }},
	if pkg#"pkgname" =!= "Macaulay2Doc" then DIV {
	    SUBSECTION "Source code",
	    PARA { "The source code from which this documentation is derived is in the file ",
		HREF { if installLayout =!= null then installLayout#"packages" | fn else pkg#"source file", fn }, ".",
		if pkg#?"auxiliary files" then (
		    " The auxiliary files accompanying it are in the directory ",
		    HREF { if installLayout =!= null then installLayout#"packages" | pkg#"pkgname" | "/" else pkg#"auxiliary files", pkg#"pkgname" | "/" }, ".")
		}
	    },
	if #e > 0 then DIV {
	    SUBSECTION "Exports",
	    DIV { "class" => "exports",
		fixup UL {
		    if #b > 0 then LI {"Types",                  smenu b},
		    if #a > 0 then LI {"Functions and commands", smenu a},
		    if #m > 0 then LI {"Methods",                smenu m},
		    if #c > 0 then LI {"Symbols",                smenu c},
		    if #d > 0 then LI {"Other things",      smenuCLASS d}}}
	    }))

-----------------------------------------------------------------------------
-- Handling operators
-----------------------------------------------------------------------------

getOperator := key -> if operator#?key then (
    op := toString key;
    if match("^[[:alpha:]]*$", op) then op = " " | op | " ";
    fixup DIV (
	if binary#?key then {
	    PARA {"This operator may be used as a binary operator in an expression like ", TT ("x" | op | "y"), ". ",
		"The user may install ", TO "Macaulay2Doc :: binary methods", "for handling such expressions with code such as"},
	    PRE if key === symbol SPACE
	    then "         X Y := (x,y) -> ..."
	    else "         X "|op|" Y := (x,y) -> ...",
	    PARA {"where ", TT "X", " is the class of ", TT "x", " and ", TT "Y", " is the class of ", TT "y", "."}},
	if prefix#?key then {
	    PARA {"This operator may be used as a prefix unary operator in an expression like ", TT (op | "y"), ". ",
		"The user may ", TO2{ "Macaulay2Doc :: installing methods", "install a method" }, " for handling such expressions with code such as"},
	    PRE ("           "|op|" Y := (y) -> ..."),
	    PARA {"where ", TT "Y", " is the class of ", TT "y", "."}},
	if postfix#?key then {
	    PARA {"This operator may be used as a postfix unary operator in an expression like ", TT ("x" | op), ". ",
		"The user may ", TO2{ "Macaulay2Doc :: :=", "install a method" }, " for handling such expressions with code such as"},
	    PRE ("         X "|op|"   := (x) -> ..."),
	    PARA {"where ", TT "X", " is the class of ", TT "x", "."}}
	))

-- TODO: expand this
getTechnical := (S, s) -> DIV ( "class" => "waystouse",
    SUBSECTION "For the programmer",
    fixup PARA deepSplice {
	"The object ", TO S, " is ", ofClass class s,
	if parent s =!= Nothing then (
	    f := drop(ancestors s, 1);
	    if #f > 1 then ", with ancestor classes " else if #f == 1 then ", with ancestor class " else ", with no ancestor class.",
	    toSequence between(" < ", f / (T -> TO T))),
	"."},
    getOperator S)

-----------------------------------------------------------------------------
-- helper functions for help
-----------------------------------------------------------------------------

getOption := (rawdoc, tag) -> if rawdoc =!= null and rawdoc#?tag then rawdoc#tag

-- Compare with SYNOPSIS in document.m2
getSynopsis := (key, tag, rawdoc) -> (
    if rawdoc === null then return null;
    result := nonnull {
	if rawdoc.?BaseFunction then SPAN { "Function: ", TO rawdoc.BaseFunction }
	else if instance(key, Sequence) and key#?0 then (
	    if  instance(key#0, ScriptedFunctor) then SPAN { "Scripted functor: ", TO key#0 }
	    else if instance(key#0, Keyword)     then SPAN { "Operator: ",         TO key#0 }
	    else if instance(key#0, Function)    then SPAN { "Function: ",         TO key#0 }
	    else if instance(key#0, Sequence) and #key#0 === 2 and key#0#1 === symbol=
	    then SPAN { "Operator: ", TO key#0#0 }), -- assignment operator for this operator
	if rawdoc.?Usage        then                           rawdoc.Usage, -- TODO: handle getUsage here
	if rawdoc.?Inputs       then  LI { "Inputs:",       UL rawdoc.Inputs },
	if rawdoc.?Options      then  LI { TO2{"Macaulay2Doc :: using functions with optional inputs", "Optional inputs"}, ":", UL rawdoc.Options },
	if rawdoc.?Outputs      then  LI { "Outputs:",      UL rawdoc.Outputs },
	if rawdoc.?Consequences then DIV { "Consequences:", UL rawdoc.Consequences }};
    if #result > 0 then fixup UL result)

getDefaultOptions := (fn, opt) -> DIV (
    SUBSECTION "Further information", UL {
	SPAN{ "Default value: ",
	    if   isDocumentableThing default
	    and hasDocumentation     default
	    then TO {default} else TT toString default },
	SPAN{
	    if class fn === Sequence then "Method: " else "Function: ",
	    TOH {fn}},
	SPAN{ "Option name: ", TOH {opt} }
	})

getDescription := (key, tag, rawdoc) -> (
    desc := getOption(rawdoc, Description);
    if desc =!= null and #desc > 0 then (
	desc = processExamples(package tag, format tag, desc);
	if instance(key, String) then DIV { desc } -- overview key
	else DIV { SUBSECTION "Description", desc })
    else DIV { COMMENT "empty documentation body" })

-- This is the overall template of a documentation page
-- for specialized templates, see documentationValue above
getBody := (key, tag, rawdoc) -> (
    if tag === null or rawdoc === null
    then printerr("warning: there is no documentation for ", formatDocumentTag key);
    currentHelpTag = tag;
    result := fixup DIV nonnull splice (
	HEADER1{ formatDocumentTag key,
	    if (title    :=    headline key              ) =!= null then " -- ", title },
	(   if (synopsis := getSynopsis(key, tag, rawdoc)) =!= null then DIV { SUBSECTION "Synopsis", synopsis } ),
	getDescription(key, tag, rawdoc),
	if instance(key, Array) then getDefaultOptions(key#0, key#1),
	getOption(rawdoc, Acknowledgement),
	getOption(rawdoc, Contributors),
	getOption(rawdoc, References),
	getOption(rawdoc, Caveat),
	getOption(rawdoc, SourceCode),
	getOption(rawdoc, SeeAlso),
	if instance(key, Symbol) then (
	    documentationValue(key, value key),
	    getTechnical(key, value key))
	else if instance(key, Array) then (
	    documentationValue(key#1, value key#1)),
	getOption(rawdoc, Subnodes));
    currentHelpTag = null;
    result)

-----------------------------------------------------------------------------
-- View help within Macaulay2
-----------------------------------------------------------------------------

-- TODO: help symbol% before Macaulay2Doc is installed doesn't work
help = method(Dispatch => Thing)
-- overview nodes and formatted documentation keys
help String := key -> (
    rawdoc := fetchAnyRawDocumentation makeDocumentTag key;
    tag := getOption(rawdoc, symbol DocumentTag);
    if tag.?Key and tag.Key =!= key then help tag.Key
    else if      isGlobalSymbol key then help getGlobalSymbol key
    else getBody(key, tag, rawdoc))

-- Methods
help Sequence := key -> (
    if key === () then return if inDebugger then debuggerUsageMessage else help "initial help";
    if lookup key === null then error("expected ", toString key, " to be a method");
    rawdoc := fetchAnyRawDocumentation makeDocumentTag key;
    tag := getOption(rawdoc, symbol DocumentTag);
    getBody(key, tag, rawdoc))

-- Options
help Array := key -> (
    (fn, opt) := (key#0, key#1);
    assert ( fn =!= null );
    default := if (options fn)#?opt then (options fn)#opt
    else error("function ", fn, " does not accept option key ", opt);
    rawdoc := fetchAnyRawDocumentation makeDocumentTag key;
    tag := getOption(rawdoc, symbol DocumentTag);
    getBody(key, tag, rawdoc))

-- everything else: Symbols, Types, ScriptedFunctors, Functions, Keywords, and Packages
help Symbol := key -> (
    rawdoc := fetchAnyRawDocumentation makeDocumentTag key;
    tag := getOption(rawdoc, symbol DocumentTag);
    getBody(key, tag, rawdoc))

help DocumentTag := tag -> help tag.Key
help Thing := x -> if hasAttribute(x, ReverseDictionary) then help getAttribute(x, ReverseDictionary) else error "no documentation found"
help List  := l -> DIV between(HR{}, help \ l)
help ZZ    := i -> (
    if     lastabout === null then error "no previous 'about' response";
    if not lastabout#?i       then error("previous 'about' response contains no entry numbered ", i);
    help lastabout#i)

-- so the user can cut paste the menu line "* sum" to get help!
* String := x -> help x

-- Turning help into a Command ensures that entering "help"
-- prints the "initial help" node instead of "MethodFunction"
help = new Command from help
-- This ensures that "methods help" and "?help" work as expected
setAttribute(help#0, ReverseDictionary, symbol help)
-- TODO: make this automatic for Commands

-----------------------------------------------------------------------------
-- View help in a browser or via the info command
-----------------------------------------------------------------------------
-- the top level help page
frontpage := applicationDirectory() | topFileName;

viewHelp = method(Dispatch => Thing)
viewHelp String := key -> viewHelp makeDocumentTag key
viewHelp Thing  := key -> (
    if key === () then (
        if fileExists frontpage then show URL { frontpage }
	-- TODO: generate this on-demand
        else error("missing documentation index: ", frontpage, ". Run makePackageIndex() or start M2 without -q"))
    else viewHelp makeDocumentTag key)
viewHelp DocumentTag := tag -> (
    tag = getOption(fetchAnyRawDocumentation tag, symbol DocumentTag);
    docpage := concatenate htmlFilename tag;
    if fileExists docpage then show URL { docpage } else show help tag)

viewHelp = new Command from viewHelp
-- This ensures that "methods viewHelp" and "?viewHelp" work as expected
setAttribute(viewHelp#0, ReverseDictionary, symbol viewHelp)

infoHelp = key -> (
    tag := makeDocumentTag(key, Package => null);
    chkrun ("info " | format infoTagConvert tag);)

-----------------------------------------------------------------------------
-- View brief documentation within Macaulay2 using symbol?
-----------------------------------------------------------------------------
briefDocumentation = method(Dispatch => Thing)
briefDocumentation VisibleList := key -> null
briefDocumentation Thing       := key -> (
    if noBriefDocThings#?key or not isDocumentableThing key then return;
    rawdoc := fetchAnyRawDocumentation makeDocumentTag key;
    tag := getOption(rawdoc, symbol DocumentTag);
    -- TODO: when is this not null
    synopsis := getSynopsis(key, tag, rawdoc);
    if synopsis =!= null then << endl << synopsis << endl
    else if (title := headline key) =!= null then << endl << key << commentize title << endl;
    synopsis = documentationValue(getGlobalSymbol toString key, key);
    if synopsis =!= null then << endl << synopsis << endl;)

? ScriptedFunctor :=
? Function :=
? Command  :=
? Keyword  :=
? Package  :=
? Symbol   :=
? Type     := briefDocumentation

-----------------------------------------------------------------------------
-- get a list of commands whose name matches the regex
-----------------------------------------------------------------------------
apropos = method()
apropos String := (pattern) -> (
    last \ sort unique select(flatten \\ pairs \ dictionaryPath,
        (name, sym) -> match(pattern, name) and not match("\\$", name)))

-----------------------------------------------------------------------------
-- get a list of commands whose documentation matches the regex
-----------------------------------------------------------------------------
matchfun := (re, db) -> key -> (
    -- not quite right, because the body might assert that the key is undocumented.
    -- We need a quicker way to identify undocumented keys.
    if db === null then match(re, key)
    -- not quite right, because this string might occur in the raw documentation as
    -- part of the description. Unlikely, though.
    else if instance(db, Database) then (
        (match(re, key) or match(re, db#key)) and not match(///"undocumented" => true///, db#key))
    else if instance(db, HashTable) then (
        not db#key#?"undocumented" and (match(re, key) or db#key.?Description and match(re,toString db#key.Description))))

about = method(Options => {Body => false})
about Type     :=
about Symbol   :=
about Function := o -> f -> about("\\b" | toString f | "\\b", o)
about String   := o -> re -> lastabout = (
    packagesSeen := new MutableHashTable;
    NumberedVerticalList sort join(
        flatten for pkg in loadedPackages list (
            pkgname := pkg#"pkgname";
            if packagesSeen#?pkgname then continue else packagesSeen#pkgname = 1;
            keyList := join (
                if not pkg#?rawKeyDB then {}
                else select(keys pkg#rawKeyDB,
                    matchfun_re if o.Body then pkg#rawKeyDB),
                select(keys pkg#"raw documentation",
                    matchfun_re if o.Body then pkg#"raw documentation"));
            apply(keyList, key -> pkgname | "::" | key)),
        flatten for pkg in getPackageInfoList() list (
            pkgname := pkg#"name";
            if packagesSeen#?pkgname then continue else packagesSeen#pkgname = 1;
            dbname := pkg#"doc db file name";
            dbkeys := keys fetchDocKeys pkg;
            db := if o.Body then openDatabase dbname;
            keyList := select(dbkeys, matchfun_re db);
            if o.Body then close db;
            apply(keyList, key -> pkgname | "::" | key))))

-- TODO: should this go to system?
pager = x -> if height stdio > 0
    then "!" | (if getenv "PAGER" == "" then "more" else getenv "PAGER") << x << close else << x << endl
