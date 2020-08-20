-----------------------------------------------------------------------------
-- Methods for getting help and accessing the documentation
-----------------------------------------------------------------------------
-- TODO: move the help method from document.m2 to this file

-----------------------------------------------------------------------------
-- Local utilities
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- View help in a browser
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
        (prefix, tail) := htmlFilename getPrimary makeDocumentTag key;
        docpage := concatenate(prefix, tail);
        if fileExists docpage then show URL { docpage }
        else error("missing documentation page: ", docpage)))

viewHelp = new Command from viewHelp

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
-- TODO: can this be simplified?x
lastabout := null

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

-- TODO: why doesn't help ZZ work?
help#0 ZZ := i -> (
    if lastabout === null then error "no previous 'about' response";
    if not lastabout#?i then error ("previous 'about' response contains no entry numbered ", i);
    help lastabout#i)
