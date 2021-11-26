--		Copyright 1993-2003 by Daniel R. Grayson
-- TODO: eventually we won't be able to keep all packages open, anyway, since 256 can be our limit on open file descriptors

needs "code.m2"
needs "files.m2"
needs "fold.m2"
needs "lists.m2"
needs "methods.m2"
needs "regex.m2"
needs "system.m2"

loadedPackages = {}

rawKey   = "raw documentation"
rawKeyDB = "raw documentation database"

-----------------------------------------------------------------------------
-- Local variables
-----------------------------------------------------------------------------

-- used to signal to newPackage to halt before loading the package
HeaderOnly := symbol HeaderOnly
-- used for passing options from loadPackage to newPackage
loadPackageOptions := new MutableHashTable
-- used for passing options from newPackage to readPackage
newPackageOptions := new MutableHashTable

-- prevent returning the same error more than once
seenWarnings := new MutableHashTable
-- reset package warnings after Core is loaded
addStartFunction(() -> seenWarnings = new MutableHashTable)

configFileString := ///--Configuration file for package "PKG", automatically generated

-- This print statement may be commented out:
if notify then stderr << "--loading configuration for package \"PKG\" from file " << currentFileName << endl

-- This file will be overwritten if a future version of the package has
-- different options, but the values will be retained and a backup file
-- will be made.

-- Look at the value of
--         options PKG
-- to see the values of the configuration options after loading the package.

{
     -- The values to the right of the double arrows may be changed
     -- by editing this file:
     VALUES
}
///

-----------------------------------------------------------------------------
-- Local utilities
-----------------------------------------------------------------------------

warn0 := (sym, front, behind, syns) -> (
    -- just for debugging:
    -- error("symbol ", format sym, " in ", toString behind, " is shadowed by a symbol in ", toString front);
    printerr("warning: symbol ", format toString sym, " in ", toString behind,
	" is shadowed by a symbol in ", toString front);
    if #syns > 0 then if #syns > 1
    then printerr("  use one of the synonyms ", demark(", ", syns))
    else printerr("  use the synonym ", syns#0)
    else printerr("  no synonym is available"))
warn := x -> if not seenWarnings#?x and debuggingMode then (warn0 x; seenWarnings#x = true)

checkShadow := () -> (
     d := dictionaryPath;
     n := #d;
     for i from 0 to n-1 do for j from i+1 to n-1 do (
	  front := d#i;
	  behind := d#j;
	  if seenWarnings#?(front, behind) then continue;
	  scan(keys front, nam -> if behind#?nam and front#nam =!= behind#nam then (
		    sym := behind#nam;
		    syns := findSynonyms sym;
		    syns = select(syns, s -> s != nam);
		    if #syns == 0 and class User === Package and User#?"private dictionary" and member(User#"private dictionary", dictionaryPath)
		    then for i from 0 do (
			 newsyn := nam | "$" | toString i;
			 if not isGlobalSymbol newsyn then (
			      User#"private dictionary"#newsyn = sym;
			      syns = {newsyn};
			      break));
		    warn(nam, front, behind, syns);
		    ));
	  if not mutable front and not mutable behind then seenWarnings#(front, behind) = true;
	  ))

isOptionList := opts -> instance(opts, List) and all(opts, opt -> instance(opt, Option) and #opt == 2)

isPackageLoaded := pkgname -> PackageDictionary#?pkgname and instance(value PackageDictionary#pkgname, Package)

-- TODO: make this local
checkPackageName = title -> (
    if not match("^[[:alnum:]]+$", title) then error("package title not alphanumeric: ", format title))

closePackage = pkg -> if pkg#?rawKeyDB then (db -> if isOpen db then close db) pkg#rawKeyDB

-----------------------------------------------------------------------------
-- Package type declarations and basic constructors
-----------------------------------------------------------------------------

Package = new Type of MutableHashTable
Package.synonym = "package"

Package.GlobalAssignHook  = (X, x) -> (
    if not hasAttribute(x, ReverseDictionary) then setAttribute(x, ReverseDictionary, X))
Package.GlobalReleaseHook = globalReleaseFunction

net      Package :=
toString Package := pkg -> if pkg#?"pkgname" then pkg#"pkgname" else "-*package*-"
texMath  Package := pkg -> texMath toString pkg
options  Package := pkg -> pkg.Options
methods  Package := memoize(pkg -> select(methods(), m -> package m === pkg))

-- TODO: should this go elsewhere?
toString Dictionary := dict -> (
    if hasAnAttribute dict then (
	if hasAttribute(dict, PrintNames)        then getAttribute(dict, PrintNames) else
	if hasAttribute(dict, ReverseDictionary) then toString getAttribute(dict, ReverseDictionary))
    else toString class dict | if length dict == 0 then "{}" else "{..." | toString length dict | "...}")

dismiss String  := pkgname -> if isPackageLoaded pkgname then dismiss value PackageDictionary#pkgname
dismiss Package := pkg     -> (
    if pkg#"pkgname" === "Core" then error "Core package cannot be dismissed";
    loadedPackages = delete(pkg, loadedPackages);
    dictionaryPath = delete(pkg.Dictionary, dictionaryPath);
    dictionaryPath = delete(pkg#"private dictionary", dictionaryPath);
    pkg)

-----------------------------------------------------------------------------
-- readPackage, loadPackage, and needsPackage
-----------------------------------------------------------------------------

-- TODO: also use this to read the contents of Configuration
-- TODO: raise an error if the package header causes new symbols to be exported
readPackage = method(TypicalValue => OptionTable, Options => { FileName => null })
readPackage Package := opts -> pkg     -> options pkg
readPackage String  := opts -> pkgname -> (
    if pkgname === "Core" then return newPackageOptions#"Core";
    remove(newPackageOptions, pkgname);
    filename := if opts.FileName === null then pkgname | ".m2" else opts.FileName;
    loadPackageOptions#pkgname = new OptionTable from { HeaderOnly => true };
    load filename;
    remove(loadPackageOptions, pkgname);
    if newPackageOptions#?pkgname then return newPackageOptions#pkgname
    else error("readPackage: ", filename, " does not contain a valid package (missing newPackage)"))

loadPackage = method(
    TypicalValue => Package,
    Options => {
	Configuration     => {},
	DebuggingMode     => null,
	FileName          => null,
	LoadDocumentation => false,
	Reload            => null
	})
loadPackage Package := opts -> pkg     -> loadPackage(toString pkg, opts ++ { Reload => true })
loadPackage String  := opts -> pkgname -> (
    if not isOptionList opts.Configuration then error("expected Configuration option to be a list of options");
    -- package name must be alphanumeric
    checkPackageName pkgname;
    -- dismiss the loaded package before reloading
    if opts.Reload === true then (
	dismiss pkgname;
	if isPackageLoaded pkgname then (
	    closePackage value PackageDictionary#pkgname;
	    -- clear out the value of the symbol
	    PackageDictionary#pkgname <- PackageDictionary#pkgname));
    --
    filename := if opts.FileName === null then pkgname | ".m2" else opts.FileName;
    -- TODO: can this be fixed?
    -- if opts.DebuggingMode =!= true then loadDepth = loadDepth - 1;
    loadPackageOptions#pkgname = opts;
    load filename;
    remove(loadPackageOptions, pkgname);
    -- if opts.DebuggingMode =!= true then loadDepth = loadDepth + 1;
    -- assert that the filename matches the package name
    if not PackageDictionary#?pkgname then error(
	"the file ", loadedFiles#(#loadedFiles - 1), " did not define a package called ", pkgname);
    value PackageDictionary#pkgname)

needsPackage = method(TypicalValue => Package, Options => options loadPackage)
needsPackage String  := opts -> pkgname -> (
    if PackageDictionary#?pkgname
    and instance(pkg := value PackageDictionary#pkgname, Package)
    and (opts.FileName === null or
	realpath opts.FileName == realpath pkg#"source file")
    then use value PackageDictionary#pkgname
    else loadPackage(pkgname, opts))

-- used as the default loadOptions in newPackage
loadPackageOptions#"default" = new MutableHashTable from options loadPackage

getpkg       = pkgname -> if isPackageLoaded pkgname then value PackageDictionary#pkgname else dismiss needsPackage pkgname
getpkgNoLoad = pkgname -> if isPackageLoaded pkgname then value PackageDictionary#pkgname

-----------------------------------------------------------------------------
-- newPackage
-----------------------------------------------------------------------------

newPackage = method(
    Options => {
	Authors                   => {},
	AuxiliaryFiles            => false,
	CacheExampleOutput        => null,
	Certification             => null,
	Configuration             => {},
	Date                      => null,
	DebuggingMode             => false,
	Headline                  => null,
	HomePage                  => null,
	InfoDirSection            => "Macaulay2 and its packages",
	Keywords                  => {"Uncategorized"},
	OptionalComponentsPresent => null,
	PackageExports            => {},
	PackageImports            => {},
	Reload                    => false,
	UseCachedExampleOutput    => null,
	Version                   => "0.0"
	})
newPackage String := opts -> pkgname -> (
    -- package name must be alphanumeric
    checkPackageName pkgname;
    -- required package values
    scan({
	    (Authors,        List),
	    (AuxiliaryFiles, Boolean),
	    (Configuration,  List),
	    (DebuggingMode,  Boolean),
	    (InfoDirSection, String),
	    (PackageExports, List),
	    (PackageImports, List),
	    (Version,        String)}, (name, type) -> if not instance(opts#name, type) then
	error("newPackage: expected ", toString name, " option of class ", toString type));
    -- TODO: add a general type checking mechanism
    scan({Certification, Configuration}, name -> if opts#name =!= null and not isOptionList opts#name then
	error("newPackage: expected ", toString name, " option to be a list of options"));
    if opts.Authors =!= null and any(opts.Authors, author -> not isOptionList author)
    then error("newPackage: expected Authors option to be a list of zero or more lists of options");
    if opts.Authors =!= null and any(opts.Authors, author -> (
	    author = new OptionTable from author;
	    author.?Name and match_{"(C|c)ontribut", "(M|m)aintain", "(A|a)uthor", "(T|t)hank"} author.Name))
    then warning("newPackage: use the Contributors or Acknowledgement keywords to acknowledge contributors in the package documentation");
    -- optional package values
    scan({
	    (Date,     String),
	    (Headline, String),
	    (HomePage, String)}, (name, type) -> if opts#name =!= null and not instance(opts#name, type) then
	error("newPackage: expected ", toString name, " option of class ", toString type));
    -- TODO: if #opts.Headline > 100 then error "newPackage: Headline is capped at 100 characters";
    -- the options coming from loadPackage are stored here
    loadOptions := if loadPackageOptions#?pkgname then loadPackageOptions#pkgname else loadPackageOptions#"default";
    -- the options are stored for readPackage
    newPackageOptions#pkgname = opts;
    -- stop if only reading the header
    if loadOptions#?HeaderOnly and loadOptions#HeaderOnly then return end;
    -- warn if the package is being reloaded
    if isPackageLoaded pkgname then (
	if opts.Reload === null then warningMessage("package ", pkgname, " being reloaded")
	else if opts.Reload === false then error("package ", pkgname, " not reloaded; try Reload => true"));
    -- load dependencies
    scan(opts.PackageExports, needsPackage);
    dismiss pkgname;
    -- the exit hook calls endPackage at the end of the file
    local hook;
    if pkgname =!= "Core" then (
	save := (dictionaryPath, loadedPackages, debuggingMode, loadDepth);
	hook = haderror -> (
	    if haderror then (
		(dictionaryPath, loadedPackages, debuggingMode, loadDepth) = save;
		if PackageDictionary#?pkgname then PackageDictionary#pkgname <- PackageDictionary#pkgname)
	    else endPackage pkgname);
	fileExitHooks = prepend(hook, fileExitHooks));
    -- processing configuration
    defaultConfiguration := new OptionTable from opts.Configuration;
    if not noinitfile then (
	setUpApplicationDirectory();
	configfilename := concatenate(applicationDirectory(), "init-", pkgname, ".m2");
	userConfiguration := if fileExists configfilename then simpleLoad configfilename else {};
	userConfiguration = new OptionTable from if isOptionList userConfiguration then userConfiguration
	else error("expected value provided by ", configfilename, " to be a list of options");
	--
	toOptions := op -> apply(pairs op, (k, v) -> k => v);
	combinedConfiguration := applyPairs(defaultConfiguration, (k, v) ->
	    (k, if userConfiguration#?k then userConfiguration#k else v));
	if set keys defaultConfiguration =!= set keys userConfiguration then (
	    if fileExists configfilename then stderr << "-- new configuration options for package " << pkgname << endl;
	    s := replace("PKG", pkgname, configFileString);
	    s = replace("VALUES", concatenate between_("," | newline | "     ") (toExternalString \ toOptions combinedConfiguration), s);
	    moveFile(configfilename, Verbose => true);	    -- move file out of way
	    stderr << "-- storing configuration for package " << pkgname << " in " << configfilename << endl;
	    configfilename << s << close);
	opts = merge(opts, new OptionTable from {Configuration => combinedConfiguration}, last));
    --
    if loadPackageOptions#?pkgname then (
	if loadOptions.?Configuration then (
	    -- now the Configuration options specified by arguments to loadPackage or needsPackage override the others
	    loadConfig := loadOptions.Configuration;
	    opts = merge(opts, new OptionTable from {
		    Configuration => first override(new OptionTable from opts.Configuration, toSequence loadConfig)}, last));
	if loadOptions.?DebuggingMode and loadOptions.DebuggingMode =!= null then (
	    opts = merge(opts, new OptionTable from {
		    DebuggingMode => loadOptions.DebuggingMode}, last)));
    --
    if opts.DebuggingMode and not debuggingMode then opts = opts ++ {DebuggingMode => false};
    if opts.OptionalComponentsPresent === null  then opts = opts ++ {OptionalComponentsPresent => opts.CacheExampleOutput =!= true};
    if opts.UseCachedExampleOutput === null     then opts = opts ++ {UseCachedExampleOutput => not opts.OptionalComponentsPresent};
    --
    packagePrefix := (
	-- Try to detect whether we are loading the package from an installed version.
	-- A better test would be to see if the raw documentation database is there...
	m := regex("(/|^)" | Layout#2#"packages" | "$", currentFileDirectory);
	if m#?1 then substring(currentFileDirectory, 0, m#1#0 + m#1#1) else (
	    m = regex("(/|^)" | Layout#1#"packages" | "$", currentFileDirectory);
	    -- this can be useful when running from the source tree, but this is a kludge
	    if m#?1 then substring(currentFileDirectory, 0, m#1#0 + m#1#1) else prefixDirectory));
    packageLayout := detectCurrentLayout packagePrefix;
    --
    newpkg := new Package from nonnull {
	"pkgname"                  => pkgname,
	symbol Options             => opts,
	symbol Dictionary          => new Dictionary, -- this is the global one
	"private dictionary"       => if pkgname === "Core" then first dictionaryPath else new Dictionary, -- this is the local one
	"close hook"               => hook,
	"configuration file name"  => configfilename,
	"old debuggingMode"        => debuggingMode,
	"previous currentPackage"  => currentPackage,
	"previous dictionaries"    => dictionaryPath,
	"previous packages"        => loadedPackages,
	"test number"              => 0,
	"test inputs"              => new MutableHashTable,
	"raw documentation"        => new MutableHashTable, -- deposited here by 'document'
	"processed documentation"  => new MutableHashTable, -- the output from 'documentation', look here first
	"undocumented keys"        => new MutableHashTable,
	"example inputs"           => new MutableHashTable,
	"example data files"       => new MutableHashTable,
	"example results"          => new MutableHashTable,
	"exported symbols"         => {},
	"exported mutable symbols" => {},
	if opts.AuxiliaryFiles then
	"auxiliary files"          => toAbsolutePath currentFileDirectory | pkgname | "/",
	"source directory"         => toAbsolutePath currentFileDirectory,
	"source file"              => toAbsolutePath currentFileName,
	if packagePrefix =!= null then
	"package prefix"           => packagePrefix
	};
    --
    if packageLayout =!= null then (
	rawdbname := databaseFilename(Layout#packageLayout, packagePrefix, pkgname);
	if fileExists rawdbname then (
	    newpkg#rawKeyDB = rawdb := openDatabase rawdbname;
	    addEndFunction(() -> if isOpen rawdb then close rawdb))
	else if notify then printerr("database not present: ", minimizeFilename rawdbname))
    else if notify then printerr("package prefix null, not opening database for package ", format pkgname);
    --
    pkgsym := (
	if PackageDictionary#?pkgname then getGlobalSymbol(PackageDictionary, pkgname)
	else PackageDictionary#("Package$" | pkgname) = getGlobalSymbol(PackageDictionary, pkgname));
    --
    global currentPackage <- newpkg;
    setAttribute(newpkg, ReverseDictionary, pkgsym);
    if instance(value pkgsym, Package) then closePackage value pkgsym;
    pkgsym <- newpkg;
    loadedPackages = {Core};
    dictionaryPath = {Core.Dictionary, OutputDictionary, PackageDictionary};
    dictionaryPath = (
	if member(newpkg.Dictionary, dictionaryPath)
	then join({newpkg#"private dictionary"},                    dictionaryPath)
	else join({newpkg#"private dictionary", newpkg.Dictionary}, dictionaryPath));
    --
    setAttribute(newpkg.Dictionary,           PrintNames, pkgname | ".Dictionary");
    setAttribute(newpkg#"private dictionary", PrintNames, pkgname | "#\"private dictionary\"");
    debuggingMode = opts.DebuggingMode;		    -- last step before turning control back to code of package
    scan(opts.PackageImports, needsPackage);
    scan(opts.PackageExports, needsPackage);
    newpkg.loadDepth = loadDepth;
    loadDepth = if pkgname === "Core" then 1 else if not debuggingMode then 2 else 3;
    newpkg)

-----------------------------------------------------------------------------
-- export, exportMutable, and exportFrom
-----------------------------------------------------------------------------

export = method(Dispatch => Thing)
export String := x -> export {x}
export List   := v -> (
    if currentPackage === null then error "no current package";
    pd := currentPackage#"private dictionary";
    d  := currentPackage.Dictionary;
    title := currentPackage#"pkgname";
    syms := new MutableHashTable;
    scan(v, sym -> (
	    local nam;
	    -- a synonym, e.g. "res" => "resolution"
	    if instance(sym, Option) then (
		nam = sym#0;
		if class nam =!= String then error("expected a string: ", nam);
		if pd#?nam then error("symbol intended as exported synonym already used internally: ", format nam, "\n", symbolLocation pd#nam, ": it was used here");
		if class sym#1 =!= String then error("expected a string: ", nam);
		sym = getGlobalSymbol(pd, sym#1))
	    else if instance(sym, String) then (
		if match("^[[:alpha:]]$", sym) then error ("cannot export single-letter symbol ", getGlobalSymbol(pd, sym));
		nam = sym;
		sym = if pd#?nam then pd#nam else getGlobalSymbol(pd, nam))
	    else error ("'export' expected a string or an option but was given ", sym, ", of class ", class sym);
	    -- we use "symbolBody" here, because a few symbols are threadlocal,
	    -- and a symbol is really a symbol closure, which include the frame
	    assert(pd#(toString sym) === sym);
	    syn := title | "$" | nam;
	    d#syn = d#nam = sym;
	    syms#sym = true));
    syms = keys syms;
    currentPackage#"exported symbols" = join(currentPackage#"exported symbols", syms);
    syms)

exportMutable = method(Dispatch => Thing)
exportMutable String := x -> exportMutable {x}
exportMutable List   := v -> currentPackage#"exported mutable symbols" = join_(currentPackage#"exported mutable symbols") (export v)

importFrom = method()
importFrom(String,  List) := (P, x) -> importFrom(getpkg P, x)
importFrom(Package, List) := (P, x) -> apply(nonnull x, s -> currentPackage#"private dictionary"#s = P#"private dictionary"#s)

exportFrom = method()
exportFrom(Package, List) := (P, x) -> export \\ toString \ importFrom(P, x)

---------------------------------------------------------------------
-- Here is where Core officially becomes a package
-- TODO: is this line necessary? when does it ever run?
addStartFunction( () -> if prefixDirectory =!= null then Core#"package prefix" = prefixDirectory )
newPackage("Core",
     Authors => {
	  {Name => "Daniel R. Grayson", Email => "dan@math.uiuc.edu", HomePage => "http://www.math.uiuc.edu/~dan/"},
	  {Name => "Michael E. Stillman", Email => "mike@math.cornell.edu", HomePage => "http://www.math.cornell.edu/People/Faculty/stillman.html"}
	  },
     DebuggingMode => debuggingMode,
     Reload => true,
     HomePage => "http://www.math.uiuc.edu/Macaulay2/",
     Version => version#"VERSION",
     Headline => "A computer algebra system designed to support algebraic geometry")
Core#"pre-installed packages" = lines get (currentFileDirectory | "installedpackages")

endPackage = method()
endPackage String := title -> (
     if currentPackage === null or title =!= currentPackage#"pkgname" then error ("package not current: ", title);
     pkg := currentPackage;
     ws := set apply(pkg#"exported mutable symbols", symbolBody);
     exportDict := pkg.Dictionary;
     scan(sortByHash values exportDict, s -> if not ws#?(symbolBody s) then (
	       protect s;
	       ---if value s =!= s and not hasAttribute(value s, ReverseDictionary) then setAttribute((value s), ReverseDictionary, s)
	       ));
     protect exportDict;
     protect pkg#"private dictionary";
     if pkg#"pkgname" === "Core" then (
	  loadedPackages = {pkg};
	  dictionaryPath = {Core.Dictionary, OutputDictionary, PackageDictionary};
	  )
     else (
	  loadedPackages = prepend(pkg, pkg#"previous packages");
	  dictionaryPath = prepend(exportDict, pkg#"previous dictionaries");
	  );
     remove(pkg, "previous dictionaries");
     remove(pkg, "previous packages");
     hook := pkg#"close hook";
     remove(pkg, "close hook");
     fileExitHooks = select(fileExitHooks, f -> f =!= hook);
     global currentPackage <- pkg#"previous currentPackage";
     remove(pkg, "previous currentPackage");
     debuggingMode = pkg#"old debuggingMode"; remove(pkg, "old debuggingMode");
     checkShadow();
     if notify then printerr("package ", format title, " loaded");
     if pkg.?loadDepth then (
	  loadDepth = pkg.loadDepth;
	  remove(pkg, loadDepth);
	  );
     b := select(values pkg#"private dictionary" - set values pkg.Dictionary, s -> mutable s and value s === s);
     if #b > 0 then (
	  b = last \ sort apply(b, s -> (hash s, s));
	  error splice ("mutable unexported unset symbol(s) in package ", pkg#"pkgname", ": ", toSequence between_", " b);
	  );
     -- TODO: check for hadDocumentationWarning and Error here?
     pkg)

beginDocumentation = () -> (
    pkgname := currentPackage#"pkgname";
    if loadPackageOptions#?pkgname and not loadPackageOptions#pkgname.LoadDocumentation
    and currentPackage#?rawKeyDB and isOpen currentPackage#rawKeyDB then (
	if notify then printerr("beginDocumentation: using documentation database, skipping the rest of ", currentFileName);
	currentPackage#"documentation not loaded" = true;
	return end);
    if notify then printerr("beginDocumentation: reading the rest of ", currentFileName);
    if not member(pkgname, {"Text", "SimpleDoc"}) then needsPackage \ {"Text", "SimpleDoc"};)

---------------------------------------------------------------------

package = method (Dispatch => Thing, TypicalValue => Package)
package Package  := identity
package Nothing  := x -> null
package Option   := o -> youngest(package \ toSequence o)
package Array    :=
package Sequence := s -> if (d := fetchAnyRawDocumentation makeDocumentTag s) =!= null then package d.DocumentTag
package String   := s -> if (d := fetchAnyRawDocumentation                 s) =!= null then package d.DocumentTag
package Thing    := x -> if (d := dictionary x)                               =!= null then package d
package Symbol   := s -> (
    if instance(value s, Package) then return value s;
    n := toString s;
    r := scan(values PackageDictionary, pkg ->
	if (pkg = value pkg).?Dictionary and pkg.Dictionary#?n and pkg.Dictionary#n === s then break pkg);
    if r =!= null then return r;
    scan(dictionaryPath, d -> if d#?n and d#n === s then if package d =!= null then break package d))
package Function   :=
package HashTable  := x -> if hasAttribute(x, ReverseDictionary) then package getAttribute(x, ReverseDictionary)
package Dictionary := d -> (
    if currentPackage =!= null
    and (  currentPackage.?Dictionary
	and currentPackage.Dictionary === d
	or currentPackage#?"private dictionary"
	and currentPackage#"private dictionary" === d) then currentPackage
    else scan(values PackageDictionary, pkg ->
	if (pkg = value pkg).?Dictionary and pkg.Dictionary === d then break pkg))

-- TODO: should this reset the values of exported mutable symbols?
use Package := pkg -> (
    scan(pkg.Options.PackageExports, needsPackage);
    loadedPackages = prepend(pkg,            delete(pkg,            loadedPackages));
    dictionaryPath = prepend(pkg.Dictionary, delete(pkg.Dictionary, dictionaryPath));
    checkShadow();
    if pkg.?use then pkg.use pkg else pkg)

debug ZZ      := i   -> debugWarningHashcode = i
debug Package := pkg -> (
    dict := pkg#"private dictionary";
    if not member(dict, dictionaryPath) then dictionaryPath = prepend(dict, dictionaryPath);
    checkShadow())

-----------------------------------------------------------------------------
-- evaluateWithPackage
-----------------------------------------------------------------------------
-- this trick allows us to take advantage of tail-call optimization
-- in order to reduce the stack size in recursive calls
pushDictionary :=  d     -> (dictionaryPath = prepend(d, dictionaryPath); d)
popDictionary  := (d, s) -> (dictionaryPath =    drop(dictionaryPath, 1); s)

-- This is only used to inquire about a symbol from the Text package.
-- Probably only necessary because Text documents Hypertext objects.
-- Is there an alternative way? Is is used by document.m2 and installPackage.m2
evaluateWithPackage = (pkg, object, func) -> (
    if member(pkg.Dictionary, dictionaryPath) then return func object;
    popDictionary(pushDictionary pkg.Dictionary, func object))

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
