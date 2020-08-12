-----------------------------------------------------------------------------
-- Methods for getting help and accessing the documentation
-----------------------------------------------------------------------------
-* Summary:
 * help
 * viewHelp
 * infoHelp
 * (symbol?, Function)
 * (symbol?, Type) (not yet implemented)
 * examples
 * apropos
 * about
 * pager
 *-

-----------------------------------------------------------------------------
-- Local variables
-----------------------------------------------------------------------------

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

-- TODO: get rid of this, keep the doc from before
getDoc    :=  key       -> fetchRawDocumentation makeDocumentTag key
getOption := (key, tag) -> (
     s := getDoc key;
     if s =!= null and s#?tag then s#tag)

makeDocBody := key -> (
    ptag := getPrimaryTag makeDocumentTag key;
    rdoc := fetchRawDocumentation ptag;
    if rdoc =!= null then (
	docBody := if rdoc.?Description then rdoc.Description;
	if docBody =!= null and #docBody > 0 then (
	    docBody = processExamples(package ptag, format ptag, docBody);
	    if instance(key, String) then DIV { docBody } -- overview key
	    else DIV { SUBSECTION "Description", docBody })
	else DIV { COMMENT "empty documentation body" }))

op := s -> if operator#?s then (
    ss := toString s;
    if match("^[[:alpha:]]*$", ss) then ss = " " | ss | " ";
    fixup DIV (
	if binary#?s then {
	    PARA {"This operator may be used as a binary operator in an expression like ", TT ("x" | ss | "y"), ". ",
		"The user may install ", TO "binary methods", "for handling such expressions with code such as"},
	    PRE if s === symbol SPACE
	    then "         X Y := (x,y) -> ..."
	    else "         X "|ss|" Y := (x,y) -> ...",
	    PARA {"where ", TT "X", " is the class of ", TT "x", " and ", TT "Y", " is the class of ", TT "y", "."}},
	if prefix#?s then {
	    PARA {"This operator may be used as a prefix unary operator in an expression like ", TT (ss | "y"), ". ",
		"The user may ", TO2{ "installing methods", "install a method" }, " for handling such expressions with code such as"},
	    PRE ("           "|ss|" Y := (y) -> ..."),
	    PARA {"where ", TT "Y", " is the class of ", TT "y", "."}},
	if postfix#?s then {
	    PARA {"This operator may be used as a postfix unary operator in an expression like ", TT ("x" | ss), ". ",
		"The user may ", TO2{ ":=", "install a method" }, " for handling such expressions with code such as"},
	    PRE ("         X "|ss|"   := (x) -> ..."),
	    PARA {"where ", TT "X", " is the class of ", TT "x", "."}}
	))

-- TODO: expand this
technical := (S, s) -> (
    DIV {
	"class" => "waystouse",
	SUBSECTION "For the programmer",
	fixup PARA deepSplice {
	    "The object ", TO S, " is ", ofClass class s,
	    if parent s =!= Nothing then (
		f := drop(ancestors s, 1);
		if #f > 1 then ", with ancestor classes " else if #f == 1 then ", with ancestor class " else ", with no ancestor class.",
		toSequence between(" < ", f / (T -> TO T))),
	    "."},
	op S})

-- TODO: is this duplicate?
briefSynopsis := key -> (
    o := getDoc key;
    if o === null then return null;
    r := nonnull {
	if o.?BaseFunction then SPAN { "Function: ", TO o.BaseFunction }
	else if instance(key, Sequence) and key#?0 then (
	    if instance(key#0, ScriptedFunctor) then SPAN { "Scripted functor: ", TO key#0 }
	    else if instance(key#0, Keyword)    then SPAN { "Operator: ",         TO key#0 }
	    else if instance(key#0, Function)   then SPAN { "Function: ",         TO key#0 }
	    else if instance(key#0, Sequence) and #key#0 === 2 and key#0#1 === symbol=
	    then SPAN { "Operator: ", TO key#0#0 }), -- assignment operator for this operator
	if o.?Usage        then                           o.Usage, -- TODO: handle getUsage here
	if o.?Inputs       then  LI { "Inputs:",       UL o.Inputs },
	if o.?Outputs      then  LI { "Outputs:",      UL o.Outputs },
	if o.?Consequences then DIV { "Consequences:", UL o.Consequences }};
    if #r > 0 then fixup UL r)

synopsis := key -> ( s := briefSynopsis key; if s =!= null then DIV { SUBSECTION "Synopsis", s } )

-----------------------------------------------------------------------------
-- these menus have to get sorted, so optTO and optTOCLASS return pairs:
--   the first member of the pair is the string to use for sorting
--   the second member is the corresponding hypertext entry in the UL list
counter := 0
next := () -> counter = counter + 1
-- TODO: used only once in format.m2
optTO := i -> (
     i = makeDocumentTag(i, Package => null);
     fkey := format i;
     if not isUndocumented i then
     if isSecondaryTag i then (
	  primary := getPrimaryTag i;
	  (format primary, fkey, next(), fixup if currentHelpTag === primary then fkey else SPAN {fkey, ", see ", TOH{primary}})
	  )
     else (fkey, fkey, next(), fixup TOH{i})      -- need an alternative here for secondary tags such as (export,Symbol)
     )
optTOCLASS := i -> (					    -- this isn't different yet, work on it!
     r := fixup TOH{i};
     (format first r, next(), r))

ul := t -> if #t =!= 0 then UL t else t
menu       := s -> ul (last \         nonnull \\ optTO      \ toList s)
smenu      := s -> ul (last \ sort \\ nonnull \\ optTO      \ toList s)
smenuCLASS := s -> ul (last \ sort \\ nonnull \\ optTOCLASS \ toList s)

redoMENU = r -> DIV prepend(
     HEADER3 "Menu",
     nonnull sublists(toList r,
	  x -> class x === TO,
	  v -> if #v != 0 then UL apply(v, i -> (
		    t := optTO i#0;
		    if t === null then error("undocumented menu item ",toString i#0);
		    last t)),
	  x -> HEADER4 {x}
	  ))

-----------------------------------------------------------------------------

-- TODO
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

-----------------------------------------------------------------------------
-- isDocumentableThing
-----------------------------------------------------------------------------

-- we're not looking for documentable methods here, just documentable objects
isDocumentableThing  = method(Dispatch => Thing)
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

-- TODO: only used in help.m2
documentableMethods = s -> select(methods s, isDocumentableMethod)

-----------------------------------------------------------------------------
-- documentationValue
-----------------------------------------------------------------------------

documentationValue := method()
documentationValue(Symbol, Thing) := (s, x) -> ()
documentationValue(Symbol, Type)  := (s, X) -> (
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

documentationValue(Symbol, ScriptedFunctor) :=
documentationValue(Symbol, Function)        :=
documentationValue(Symbol, Keyword)         := (s, f) -> (
    a := smenu documentableMethods f;
    if #a > 0 then DIV ( "class" => "waystouse", SUBSECTION {"Ways to use ", TT toString f, ":"}, a))

documentationValue(Symbol, Package)         := (s, pkg) -> if pkg =!= Core then (
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
     fn := pkg#"pkgname" | ".m2";
     au := pkg.Options.Authors;
     (
	  if #au > 0 then DIV {
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
			 LI {nam,em}
			 )
		    )
	       },
	  if (cert := pkg.Options.Certification) =!= null then (
	       if not instance(cert,List) then error(toString pkg, ": Certification option: expected a list");
	       if not all(cert,x -> instance(x,Option) and #x==2) then error(toString pkg, ": Certification option: expected a list of options");
	       cert = new HashTable from cert;
	       DIV {
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
	  DIV { SUBSECTION "Version", PARA { "This documentation describes version ", BOLD pkg.Options.Version, " of ",
	       if pkg#"pkgname" === "Macaulay2Doc" then "Macaulay2" else pkg#"pkgname",
	       "." } },
	  if pkg#"pkgname" =!= "Macaulay2Doc"
	  then DIV {
	       SUBSECTION "Source code",
	       PARA { "The source code from which this documentation is derived is in the file ",
	       HREF {
		    if installLayout =!= null
		    then installLayout#"packages" | fn
		    else pkg#"source file",
		    fn },
	       ".",
	       if pkg#?"auxiliary files" then (
		    "  The auxiliary files accompanying it are in the
		    directory ",
		    HREF {
			 if installLayout =!= null
			 then installLayout#"packages" | pkg#"pkgname" | "/"
			 else pkg#"auxiliary files",
			 pkg#"pkgname" | "/"
			 },
		    "."
		    )
		}
	       },
	  if #e > 0 then DIV {
	       SUBSECTION "Exports",
	       DIV {
		    "class" => "exports",
		    fixup UL {
			 if #b > 0 then LI {"Types", smenu b},
			 if #a > 0 then LI {"Functions and commands", smenu a},
			 if #m > 0 then LI {"Methods", smenu m},
			 if #c > 0 then LI {"Symbols", smenu c},
			 if #d > 0 then LI {"Other things", smenuCLASS d}}}}))

-----------------------------------------------------------------------------
-- help
-----------------------------------------------------------------------------

topheader := s -> (
     h := headline s;
     HEADER1 { formatDocumentTag s, if h =!= null then " -- ", h })

acknowledgement := key -> getOption(key, Acknowledgement)
contributors    := key -> getOption(key, Contributors)
references      := key -> getOption(key, References)
caveat          := key -> getOption(key, Caveat)
seealso         := key -> getOption(key, SeeAlso)
sourcecode      := key -> getOption(key, SourceCode)
theMenu         := key -> getOption(key, Subnodes)

help = method(Dispatch => Thing)
help String := key -> (
    checkLoadDocumentation();
    if unformatTag#?key then help unformatTag#key
    else if isGlobalSymbol key then ( help getGlobalSymbol key )
    else (
	body := makeDocBody key;
	if body === null then (
	    stderr << "--warning: there is no documentation for '" << key << "'" << endl;
	    body = ());
	fixup DIV {
	    topheader key,
	    body,
	    acknowledgement key,
	    contributors key,
	    references key,
	    caveat key,
	    seealso key,
	    theMenu key}))

-- Methods
help Sequence := key -> (
    checkLoadDocumentation();
    if key === () then return if inDebugger then debuggerUsageMessage else help "initial help";
    if lookup key === null then error("expected ", toString key, " to be a method");
    currentHelpTag = makeDocumentTag(key, Package => null);
    ret := fixup DIV {
	topheader key,
	synopsis key,
	makeDocBody key,
	acknowledgement key,
	contributors key,
	references key,
	caveat key,
	sourcecode key,
	seealso key,
	theMenu key};
    currentHelpTag = null;
    ret)

-- Symbols
help Symbol   := S -> (
    if package S === Core then checkLoadDocumentation();
    currentHelpTag = makeDocumentTag S;
    a := smenu apply(select(optionFor S, f -> isDocumentableMethod f), f -> [f, S]);
    ret := fixup DIV {
	topheader S,
	synopsis S,
	makeDocBody S,
	if #a > 0 then DIV { SUBSECTION {"Functions with optional argument named ", toExternalString S, " :"}, a},
	acknowledgement S,
	contributors S,
	references S,
	caveat S,
	seealso S,
	documentationValue(S, value S),
	sourcecode S,
	technical(S, value S),
	theMenu S};
    currentHelpTag = null;
    ret)

-- Options
help Array := key -> (
    checkLoadDocumentation();
    (fn, opt) := (key#0, key#1);
    default := if (options fn)#?opt then (options fn)#opt
    else error("function ", fn, " does not accept option key ", opt);
    fixup DIV {
	topheader key,
	synopsis key,
	makeDocBody key,
	SUBSECTION "Further information",
	UL {
	    SPAN{ "Default value: ", if isDocumentableThing default and hasDocumentation default then TO {default} else TT toString default },
	    SPAN{
		assert ( fn =!= null );
		if class fn === Sequence then "Method: " else "Function: ",
		TOH {fn}},
	    SPAN{ "Option name: ", TOH {opt} }
	    },
	acknowledgement key,
	contributors key,
	references key,
	caveat key,
	seealso key,
	theMenu key })

help DocumentTag := tag -> help DocumentTag.Key tag
help Thing := x -> if hasAttribute(x, ReverseDictionary) then help getAttribute(x, ReverseDictionary) else error "no documentation found"
help List  := l -> DIV between(HR{}, help \ l)
help ZZ    := i -> (
    if lastabout === null then error "no previous 'about' response";
    if not lastabout#?i then error("previous 'about' response contains no entry numbered ", i);
    help lastabout#i)

-- so the user can cut paste the menu line "* sum" to get help!
* String := x -> help x

-- Turning help into a Command ensures that entering "help"
-- prints the "initial help" node instead of "MethodFunction"
help = Command help

-----------------------------------------------------------------------------
-- View help in a browser or via the info command
-----------------------------------------------------------------------------
-- the top level help page
frontpage := applicationDirectory() | topFileName;

viewHelp = method(Dispatch=>Thing)
-- TODO: check that key is a formatted key
viewHelp String := key -> (
    docpage := locateDocumentationNode key;
    if docpage === null then error("missing documentation page for key ", key)
    else show new URL from { docpage })
viewHelp Thing := key -> (
    if key === () then (
        if fileExists frontpage then show URL { frontpage }
        else error("missing documentation index: ", frontpage, ". Run makePackageIndex() or start M2 without -q"))
    else (
        (prefix, tail) := htmlFilename getPrimaryTag makeDocumentTag key;
        docpage := concatenate(prefix, tail);
        if fileExists docpage then show URL { docpage }
        else error("missing documentation page: ", docpage)))

viewHelp = new Command from viewHelp

infoHelp = key -> (
    tag := makeDocumentTag(key, Package => null);
    chkrun ("info " | format infoTagConvert tag);)

-----------------------------------------------------------------------------
-- briefDocumentation
-----------------------------------------------------------------------------
briefDocumentation = method(Dispatch => Thing)
briefDocumentation VisibleList := x -> null
briefDocumentation Thing       := x -> (
    if noBriefDocThings#?x or not isDocumentableThing x then return null;
    if package x === Core then checkLoadDocumentation();
    r := briefSynopsis normalizeDocumentKey x;
    if r =!= null then << endl << r << endl
    else if headline x =!= null then << endl << commentize headline x << endl;
    if instance(x, Function) or instance(x, ScriptedFunctor) then (
	s := documentationValue(symbol x, x);
	if s =!= null then << endl << s << endl;))

? Function := briefDocumentation
-- TODO:
-- ? Type :=

-----------------------------------------------------------------------------
-- get a list of examples in a documentation node
-----------------------------------------------------------------------------
getExampleInputs := method(Dispatch => Thing)
getExampleInputs Thing     := t -> ()
getExampleInputs Sequence  :=
getExampleInputs Hypertext := t -> apply(toSequence t, getExampleInputs)
getExampleInputs ExampleItem := t -> 1 : t#0 -- a Sequence

examples = method(Dispatch => Thing)
examples Hypertext := x -> stack deepSplice getExampleInputs x
examples Thing     := x -> (
    checkLoadDocumentation();
    d := fetchRawDocumentation makeDocumentTag(x, Package => null);
    if d =!= null and d.?Description then (stack deepSplice getExampleInputs d.Description)^-1)

-----------------------------------------------------------------------------
-- get a list of commands whose name matches the regex
-----------------------------------------------------------------------------
apropos = method()
apropos String := (pattern) -> (
    last \ sort unique select(flatten \\ pairs \ dictionaryPath,
        (nam,sym) -> match(pattern,nam) and not match("\\$",nam)))

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
                if not pkg#?"raw documentation database" then {}
                else select(keys pkg#"raw documentation database",
                    matchfun_re if o.Body then pkg#"raw documentation database"),
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

pager = x -> if height stdio > 0
    then "!" | (if getenv "PAGER" == "" then "more" else getenv "PAGER") << x << close else << x << endl
