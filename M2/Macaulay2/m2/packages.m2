--		Copyright 1993-2003 by Daniel R. Grayson

addStartFunction(
     () -> (
	  path = prepend("./",path);
	  if prefixDirectory =!= null then path = append(path,prefixDirectory|currentLayout#"packages");
	  ))

Package = new Type of MutableHashTable
Package.synonym = "package"
net Package := toString Package := p -> if p#?"title" then p#"title" else "{*package*}"
loadedPackages = {}
options Package := p -> p.Options

toString Dictionary := d -> (
     if hasAnAttribute d then (
	  if hasAttribute(d,PrintNames) then return getAttribute(d,PrintNames);
	  if hasAttribute(d,ReverseDictionary) then return toString getAttribute(d,ReverseDictionary);
	  );
     toString class d | if length d == 0 then "{}" else "{..." | toString length d | "...}"
     )

Package.GlobalAssignHook = (X,x) -> if not hasAttribute(x,ReverseDictionary) then setAttribute(x,ReverseDictionary,X);     -- not 'use x';
Package.GlobalReleaseHook = globalReleaseFunction

dismiss Package := pkg -> (
     if pkg#"title" === "Core" then error "Core package cannot be dismissed";
     loadedPackages = delete(pkg,loadedPackages);
     dictionaryPath = delete(pkg.Dictionary,dictionaryPath);
     dictionaryPath = delete(pkg#"private dictionary",dictionaryPath);
     pkg)
dismiss String := title -> if PackageDictionary#?title and class value PackageDictionary#title === Package then dismiss value PackageDictionary#title

loadPackage = method(
     Options => {
	  FileName => null,
	  DebuggingMode => null,
	  LoadDocumentation => false,
	  Configuration => {},
	  Reload => null
	  } )
packageLoadingOptions := new MutableHashTable
checkPackageName = title -> if not match("^[a-zA-Z0-9]+$",title) then error( "package title not alphanumeric: ",format title)

loadPackage String := opts -> pkgtitle -> (
     checkPackageName pkgtitle;
     if opts.Reload === true then (
	  dismiss pkgtitle;
	  if PackageDictionary#?pkgtitle then PackageDictionary#pkgtitle <- PackageDictionary#pkgtitle;
	  );
     filename := if opts.FileName === null then pkgtitle | ".m2" else opts.FileName;
     packageLoadingOptions#pkgtitle = opts;
     -- if opts.DebuggingMode =!= true then loadDepth = loadDepth - 1;
     -- this was bad, because loadDepth might become negative, and then it gets converted to 255 in the pseudocode
     -- another problem was that loading the file might have resulted in an error.
     load filename;
     actualFilename := loadedFiles#(#loadedFiles-1);
     -- if opts.DebuggingMode =!= true then loadDepth = loadDepth + 1;
     remove(packageLoadingOptions,pkgtitle);
     if not PackageDictionary#?pkgtitle then error("the file ", actualFilename, " did not define a package called ", pkgtitle);
     value PackageDictionary#pkgtitle)

loadPackage Package := opts -> pkg -> loadPackage(toString pkg, opts ++ { Reload => true })

needsPackage = method(
     TypicalValue => Package,
     Options => {
	  -- these are most of the options of loadPackage
	  FileName => null,
	  DebuggingMode => null,
	  LoadDocumentation => false,
	  Configuration => {}
	  })
needsPackage String := opts -> pkg -> (
     if PackageDictionary#?pkg and instance(p := value PackageDictionary#pkg, Package)
     and (opts.FileName === null or opts.FileName == p#"source file")
     then (
	  pkg = value PackageDictionary#pkg;
	  use pkg;
	  pkg)
     else loadPackage(pkg, opts)
     )

officialAuthorTags := set {Name, Email, HomePage}
checkAuthorOption := authors -> (
     if class authors =!= List then error "expected Authors => a list";
     scan(authors, author -> (
     	       if class author =!= List then error "expected Authors => a list of lists";
     	       scan(author, o -> (
	       		 if class o =!= Option or length o =!= 2 then error "expected Authors => a list of lists of options";
	       		 if not officialAuthorTags#?(first o) then error("unexpected author tag: ", toString first o);
			 if class last o =!= String then error("expected author tag value to be a string: ", toString last o))))))

newPackage = method( 
     Options => { 
	  Certification => null,
	  Version => "0.0",
	  AuxiliaryFiles => false,
	  DebuggingMode => false,
	  InfoDirSection => "Macaulay2 and its packages",
	  CacheExampleOutput => null,
	  Headline => null,
	  Authors => {}, -- e.g., Authors => { {Name => "Dan Grayson", Email => "dan@math.uiuc.edu", HomePage => "http://www.math.uiuc.edu/~dan/"} }
	  HomePage => null,
	  Date => null,
	  Configuration => {},
	  Reload => false,
	  PackageExports => {},
	  PackageImports => {}
	  })

protect Reload

configFileString =
///--Configuration file for package "PKG", automatically generated

-- This print statement may be commented out:
stderr << "--loading configuration for package \"PKG\" from file " << currentFileName << endl

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

closePackage = pkg -> (
     if pkg#?"raw documentation database"
     then (db -> if isOpen db then close db) pkg#"raw documentation database";
     )

-- gdbm makes architecture dependent files, so we try to distinguish them, in case
-- they get mixed.  Yes, that's in addition to installing them in directories that
-- are specified to be suitable for machine dependent data.
databaseSuffix = "-" | version#"endianness" | "-" | version#"pointer size" | ".db"

newPackage(String) := opts -> (title) -> (
     checkPackageName title;
     scan({(Version,String),(AuxiliaryFiles,Boolean),(DebuggingMode,Boolean),(InfoDirSection,String),
	       (PackageImports,List),(PackageExports,List),(Authors,List),(Configuration,List)},
	  (k,K) -> if not instance(opts#k,K) then error("newPackage: expected ",toString k," option of class ",toString K));
     scan({(Headline,String),(HomePage,String),(Date,String)},
	  (k,K) -> if opts#k =!= null and not instance(opts#k,K) then error("newPackage: expected ",toString k," option of class ",toString K));
     originalTitle := title;
     if PackageDictionary#?title and instance(value PackageDictionary#title,Package) then (
	  if opts.Reload === null then warningMessage("package ", title, " being reloaded")
	  else if opts.Reload === false then error("package ", title, " not reloaded; try Reload => true")
	  );
     scan(opts.PackageExports, needsPackage);
     dismiss title;
     save := (saveD := dictionaryPath, saveP := loadedPackages, debuggingMode, loadDepth);
     local hook;
     if title =!= "Core" then (
     	  hook = haderror -> (
	       if haderror then (
	       	    (dictionaryPath, loadedPackages, debuggingMode, loadDepth) = save;
		    PackageDictionary#title <- PackageDictionary#title;
		    )
	       else endPackage title
	       );
	  fileExitHooks = prepend(hook, fileExitHooks);
	  );
     fix := o -> if instance(o,OptionTable) then o else new OptionTable from o;
     defaultConfiguration := opts.Configuration;
     if not instance(defaultConfiguration, List) or not all(defaultConfiguration, x -> instance(x,Option) and #x == 2)
     then error("expected Configuration option to be a list of options");
     defaultConfiguration = new OptionTable from defaultConfiguration;
     if not noinitfile then (
	  configfilename := concatenate(applicationDirectory(), "init-",title,".m2");
	  userConfiguration := if fileExists configfilename then simpleLoad configfilename else {};
	  if not instance(userConfiguration, List) or not all(userConfiguration, x -> instance(x,Option) and #x == 2)
	  then error("expected value provided by ",configfilename," to be a list of options");
	  userConfiguration = new OptionTable from userConfiguration;
	  toOptions := op -> apply(pairs op,(k,v) -> k=>v);
	  combinedConfiguration := applyPairs(defaultConfiguration, (k,v) -> (k, if userConfiguration#?k then userConfiguration#k else v));
	  if set keys defaultConfiguration =!= set keys userConfiguration then (
	       if fileExists configfilename then stderr << "--new configuration options for package " << title << endl;
	       s := replace("PKG",title,configFileString);
	       s = replace("VALUES",concatenate between_(","|newline|"     ") (toExternalString \ toOptions combinedConfiguration),s);
	       moveFile(configfilename,Verbose=>true);	    -- move file out of way
	       stderr << "--storing configuration for package " << title << " in " << configfilename << endl;
	       configfilename << s << close;
	       );
	  opts = merge(opts, new OptionTable from {Configuration => combinedConfiguration},last);
	  );
     if packageLoadingOptions#?title then (
	  loadOptions := packageLoadingOptions#title;
	  if loadOptions.?Configuration then (
	       -- now the Configuration options specified by arguments to loadPackage or needsPackage override the others
	       loadConfig := loadOptions.Configuration;
	       if not instance(loadConfig, List) or not all(loadConfig, x -> instance(x,Option) and #x == 2)
	       then error("expected Configuration option to be a list of options");
	       opts = merge(opts, new OptionTable from {Configuration => first override(fix opts.Configuration,toSequence loadConfig)},last);
	       );
	  if loadOptions.?DebuggingMode and loadOptions.DebuggingMode =!= null then (
	       opts = merge(opts, new OptionTable from {DebuggingMode => loadOptions.DebuggingMode},last);
	       );
	  );
     if opts.DebuggingMode and not debuggingMode then opts = merge(opts, new OptionTable from {DebuggingMode => false},last);
     newpkg := new Package from nonnull {
          "title" => title,
	  symbol Options => opts,
     	  symbol Dictionary => new Dictionary, -- this is the global one
     	  "private dictionary" => if title === "Core" then first dictionaryPath else new Dictionary, -- this is the local one
     	  "close hook" => hook,
	  "configuration file name" => configfilename,
	  "previous currentPackage" => currentPackage,
	  "previous dictionaries" => saveD,
	  "previous packages" => saveP,
	  "old debuggingMode" => debuggingMode,
	  "test inputs" => new MutableHashTable,
	  "raw documentation" => new MutableHashTable,	    -- deposited here by 'document'
	  "processed documentation" => new MutableHashTable,-- the output from 'documentation', look here first
	  "example inputs" => new MutableHashTable,
	  "example data files" => new MutableHashTable,
	  "exported symbols" => {},
	  "exported mutable symbols" => {},
	  "example results" => new MutableHashTable,
	  "source directory" => toAbsolutePath currentFileDirectory,
	  if opts.AuxiliaryFiles then "auxiliary files" => toAbsolutePath currentFileDirectory | title | "/",
	  "source file" => toAbsolutePath currentFileName,
	  "undocumented keys" => new MutableHashTable,
	  "package prefix" => (
	       m := regex("(/|^)" | currentLayout#"packages" | "$", currentFileDirectory);
	       if m#?1 
	       then substring(currentFileDirectory,0,m#1#0 + m#1#1)
	       else prefixDirectory
	       ),
	  };
     newpkg#"test number" = 0;
     if newpkg#"package prefix" =!= null then (
	  -- these assignments might be premature, for any package that is loaded before dumpdata, as the "package prefix" might change:
	  rawdbname := newpkg#"package prefix" | replace("PKG",title,currentLayout#"packagecache") | "rawdocumentation" | databaseSuffix;
	  if fileExists rawdbname then (
	       rawdb := openDatabase rawdbname;
	       if notify then stderr << "--opened database: " << rawdbname << endl;
	       newpkg#"raw documentation database" = rawdb;
	       addEndFunction(() -> if isOpen rawdb then close rawdb))
	  else (
	       if notify then stderr << "--database not present: " << rawdbname << endl;
	       );
	  newpkg#"index.html" = newpkg#"package prefix" | replace("PKG",newpkg#"title",currentLayout#"packagehtml") | "index.html";
	  )
     else if notify then stderr << "--package prefix null, not opening database for package " << newpkg << endl;
     addStartFunction(() -> 
	  if not ( newpkg#?"raw documentation database" and isOpen newpkg#"raw documentation database" ) and prefixDirectory =!= null 
	  then (
	       dbname := prefixDirectory | replace("PKG",title,currentLayout#"packagecache") | "rawdocumentation" | databaseSuffix; -- what if there is more than one prefix directory?
	       if fileExists dbname then (
		    db := newpkg#"raw documentation database" = openDatabase dbname;
	       	    if notify then stderr << "--opened database: " << rawdbname << endl;
		    addEndFunction(() -> if isOpen db then close db))
	       else (
	       	    if notify then stderr << "--database not present: " << rawdbname << endl;
		    )));
     pkgsym := (
	  if PackageDictionary#?title
	  then getGlobalSymbol(PackageDictionary,title)
	  else PackageDictionary#("Package$" | title) = getGlobalSymbol(PackageDictionary,title)
	  );

     global currentPackage <- newpkg;
     setAttribute(newpkg,ReverseDictionary,pkgsym);
     if instance(value pkgsym,Package) then closePackage value pkgsym;
     pkgsym <- newpkg;
     loadedPackages = {Core};
     dictionaryPath = {Core.Dictionary, OutputDictionary, PackageDictionary};
     if Core#?"base packages" then (
	  if member(title,Core#"base packages") and title =!= "Macaulay2Doc" then (
	       if member("Macaulay2Doc",Core#"base packages") then needsPackage "Macaulay2Doc";
	       )
	  else scan(reverse Core#"base packages", needsPackage)
	  );
     dictionaryPath = (
	  if member(newpkg.Dictionary,dictionaryPath)
     	  then join({newpkg#"private dictionary"}, dictionaryPath)
	  else join({newpkg#"private dictionary",newpkg.Dictionary}, dictionaryPath));
     setAttribute(newpkg.Dictionary,PrintNames,title | ".Dictionary");
     setAttribute(newpkg#"private dictionary",PrintNames,title | "#\"private dictionary\"");
     debuggingMode = opts.DebuggingMode;		    -- last step before turning control back to code of package
     if title =!= "SimpleDoc" and title =!= "Core" and title =!= "Text" then needsPackage "SimpleDoc";
     scan(opts.PackageImports, needsPackage);
     scan(opts.PackageExports, needsPackage);
     newpkg.loadDepth = loadDepth;
     loadDepth = if title === "Core" then 1 else if not debuggingMode then 2 else 3;
     newpkg)

export = method(Dispatch => Thing)
exportFrom = method()
exportFrom(Package,List) := (P,x) -> export \\ toString \ (s -> currentPackage#"private dictionary"#s = P#"private dictionary"#s) \ x
export Symbol := x -> export {x}
export String := x -> export {x}
export List := v -> (
     if currentPackage === null then error "no current package";
     pd := currentPackage#"private dictionary";
     d := currentPackage.Dictionary;
     title := currentPackage#"title";
     syms := new MutableHashTable;
     scan(v, sym -> (
	       local nam;
     	       if instance(sym,Symbol) then error("'export' no longer accepts symbols (such as ",toString sym,"); enclose the name in quotation marks");
	       if instance(sym, Option) then (
		    nam = sym#0;			    -- synonym
     	       	    if class nam =!= String then error("expected a string: ", nam);
		    if pd#?nam then error("symbol intended as exported synonym already used internally: ",format nam, "\n", symbolLocation pd#nam, ": it was used here");
     	       	    if class sym#1 =!= String then error("expected a string: ", nam);
		    sym = getGlobalSymbol(pd,sym#1);
		    )
	       else if instance(sym, String) then (
		    if match("^[[:alpha:]]$",sym) then error ("cannot export single-letter symbol ", getGlobalSymbol(pd,sym));
		    nam = sym;
		    sym = if pd#?nam then pd#nam else getGlobalSymbol(pd,nam);
		    )
	       else error ("'export' expected a string or an option but was given ", sym, ", of class ", class sym);
	       -- we use "symbolBody" here, because a few symbols are threadlocal, and a symbol is really a symbol closure, which include the frame
	       assert(pd#(toString sym) === sym);
	       syn := title | "$" | nam;
	       d#syn = d#nam = sym;
	       syms#sym = true;
	       ));
     syms = keys syms;
     currentPackage#"exported symbols" = join(currentPackage#"exported symbols",syms);
     syms)
exportMutable = method(Dispatch => Thing)
exportMutable Symbol := x -> exportMutable {x}
exportMutable String := x -> exportMutable {x}
exportMutable List := v -> (
     syms := export v;
     currentPackage#"exported mutable symbols" = join(currentPackage#"exported mutable symbols",syms);
     syms)

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

findSynonyms = method()
findSynonyms Symbol := x -> (
     r := {};
     scan(dictionaryPath, d -> scan(pairs d, (nam,sym) -> if x === sym and getGlobalSymbol nam === sym then r = append(r,nam)));
     sort unique r)

warn0 := (sym,front,behind,syns) -> (
     -- just for debugging:
     -- error("symbol '",sym,"' in ",toString behind," is shadowed by a symbol in ",toString front);
     stderr << "--warning: symbol " << format toString sym << " in " << behind << " is shadowed by a symbol in " << front << endl;
     if #syns > 0
     then if #syns > 1
     then stderr << "--  use one of the synonyms " << demark(", ",syns) << endl
     else stderr << "--  use the synonym " << syns#0 << endl
     else stderr << "--  no synonym is available" << endl)
warnedAlready := new MutableHashTable; addStartFunction(() -> warnedAlready = new MutableHashTable)
warn := x -> if not warnedAlready#?x and debuggingMode then (warn0 x; warnedAlready#x = true)

checkShadow = () -> (
     d := dictionaryPath;
     n := #d;
     for i from 0 to n-1 do for j from i+1 to n-1 do (
	  front := d#i;
	  behind := d#j;
	  if warnedAlready#?(front,behind) then continue;
     	  scan(keys front, nam -> if behind#?nam and front#nam =!= behind#nam then (
		    sym := behind#nam;
		    syns := findSynonyms sym;
		    syns = select(syns, s -> s != nam);
		    if #syns == 0 and class User === Package and User#?"private dictionary" and member(User#"private dictionary",dictionaryPath)
		    then for i from 0 do (
			 newsyn := nam | "$" | toString i;
			 if not isGlobalSymbol newsyn then (
			      User#"private dictionary"#newsyn = sym;
			      syns = {newsyn};
			      break));
		    warn(nam,front,behind,syns);
		    ));
	  if not mutable front and not mutable behind then warnedAlready#(front,behind) = true;
	  ))

endPackage = method()
endPackage String := title -> (
     if currentPackage === null or title =!= currentPackage#"title" then error ("package not current: ",title);
     pkg := currentPackage;
     ws := set apply(pkg#"exported mutable symbols",symbolBody);
     exportDict := pkg.Dictionary;
     scan(sortByHash values exportDict, s -> if not ws#?(symbolBody s) then (
	       protect s;
	       ---if value s =!= s and not hasAttribute(value s,ReverseDictionary) then setAttribute((value s),ReverseDictionary,s)
	       ));
     if true or pkg =!= Core then (			    -- protect it later
	  protect pkg#"private dictionary";
	  protect exportDict;
	  );
     if pkg#"title" === "Core" then (
	  loadedPackages = {pkg};
	  dictionaryPath = {Core.Dictionary, OutputDictionary, PackageDictionary};
	  )
     else (
	  loadedPackages = prepend(pkg,pkg#"previous packages");
	  dictionaryPath = prepend(exportDict,pkg#"previous dictionaries");
	  );
     remove(pkg,"previous dictionaries");
     remove(pkg,"previous packages");
     hook := pkg#"close hook";
     remove(pkg,"close hook");
     fileExitHooks = select(fileExitHooks, f -> f =!= hook);
     global currentPackage <- pkg#"previous currentPackage";
     remove(pkg,"previous currentPackage");
     debuggingMode = pkg#"old debuggingMode"; remove(pkg,"old debuggingMode");
     checkShadow();
     if notify then stderr << "--package \"" << pkg << "\" loaded" << endl;
     if pkg.?loadDepth then (
	  loadDepth = pkg.loadDepth;
	  remove(pkg,loadDepth);
	  );
     b := select(values pkg#"private dictionary" - set values pkg.Dictionary, s -> mutable s and value s === s);
     if #b > 0 then (
	  b = last \ sort apply(b, s -> (hash s,s));
	  error splice ("mutable unexported unset symbol(s) in package ",pkg#"title",": ", toSequence between_", " b);
	  );
     pkg)

package = method (Dispatch => Thing)
package Dictionary := d -> (
     if currentPackage =!= null and (currentPackage.Dictionary === d or currentPackage#?"private dictionary" and currentPackage#"private dictionary" === d)
     then currentPackage 
     else scan(values PackageDictionary, pkg -> if class value pkg === Package and (value pkg).Dictionary === d then break (value pkg))
     )
package Thing := x -> (
     d := dictionary x;
     if d =!= null then package d)
package Symbol := s -> (
     n := toString s;
     r := scan(values PackageDictionary, 
	  p -> if (value p).?Dictionary and (value p).Dictionary#?n and (value p).Dictionary#n === s then break value p);
     if r =!= null then return r;
     scan(dictionaryPath, d -> if d#?n and d#n === s then (
	       if d === PackageDictionary and class value s === Package then break value s
	       else if package d =!= null then break package d)));
package HashTable := package Function := x -> if hasAttribute(x,ReverseDictionary) then package getAttribute(x,ReverseDictionary)
package Sequence := s -> youngest (package\s)

use Package := pkg -> (
     a := member(pkg,loadedPackages);
     b := member(pkg.Dictionary,dictionaryPath);
     if a and not b then error("use: package ",toString pkg," appears in loadedPackages, but its dictionary is missing from dictionaryPath");
     if b and not a then error("use: package ",toString pkg," does not appear in loadedPackages, but its dictionary appears in dictionaryPath");
     if not a and not b then (
     	  scan(pkg.Options.PackageExports, needsPackage);
     	  loadedPackages = prepend(pkg,loadedPackages);
	  --- if mutable pkg.Dictionary then error("package ", toString pkg, " not completely loaded yet");
	  dictionaryPath = prepend(pkg.Dictionary,dictionaryPath);
	  checkShadow();
	  );
     if pkg.?use then pkg.use pkg;
     )

beginDocumentation = () -> (
     if packageLoadingOptions#?(currentPackage#"title") and not packageLoadingOptions#(currentPackage#"title").LoadDocumentation
     and currentPackage#?"raw documentation database" and isOpen currentPackage#"raw documentation database" then (
	  if notify then stderr << "--beginDocumentation: using documentation database, skipping the rest of " << currentFileName << endl;
	  currentPackage#"documentation not loaded" = true;
	  return end;
	  );
     if notify then stderr << "--beginDocumentation: reading the rest of " << currentFileName << endl;
     if currentPackage#"title" != "Text" and  currentPackage#"title" != "SimpleDoc" then (
     	  needsPackage "Text";
     	  needsPackage "SimpleDoc";
	  );
     )

debug = method()
debug ZZ := i -> debugWarningHashcode = i
debug Package := pkg -> (
     d := pkg#"private dictionary";
     if not member(d,dictionaryPath) then (
	  dictionaryPath = prepend(d,dictionaryPath);
	  );
     checkShadow())

body := response -> replace("^(.|.\r\n)*\r\n\r\n","",response)
getwww := url -> (
     www := getWWW url;
     if www === null or match("^[^ ]+ 404\\b",www) then null else body www)
chkwww := url -> (
     www := getwww url;
     if www === null then error("web page not found: \"", url, "\"");
     www)
getPackage = method(Options => { 
	  Repository => "http://www.math.uiuc.edu/Macaulay2/Packages/",
	  Version => null, 
	  CurrentVersion => null,
	  UserMode => null,
	  DebuggingMode => false,
	  Configuration => {}
	  })
installMethod(getPackage, opts -> () -> lines getwww (opts.Repository | "packages" ))
getPackage String := opts -> pkgname -> (
     packages := lines getwww (opts.Repository | "packages" );
     if not member(pkgname,packages)
     then error("unknown package: ", pkgname, "; known packages: ", concatenate(between(", ",packages)));
     url := opts.Repository | pkgname | "/";
     versions := sort lines chkwww (url | "versions");
     if #versions == 0 then error "getPackage: no versions available from repository";
     if opts.Version === null then (
     	  vers := last versions;
	  if opts.CurrentVersion =!= null and not vers > opts.CurrentVersion then return;
	  )
     else (
	  vers = opts.Version;
	  if not member(vers,versions) then error("requested version not among those available: ",concatenate between(", ",versions));
	  );
     stderr << "--fetching package " << pkgname << ", version " << vers << " from " << url << endl;
     tmp := temporaryFileName();
     unwind := arg -> scan(reverse findFiles tmp, fn -> if isDirectory fn then removeDirectory fn else removeFile fn);
     makeDirectory tmp;
     tmp = tmp | "/";
     fn := pkgname | ".m2";
     m2file := getwww(url | vers | "/" | fn);
     filename := tmp | pkgname | ".m2";
     if m2file === null then (
	  fn = pkgname | ".tgz";
	  tgzfile := getwww(url | vers | "/" | fn);
	  if tgzfile === null then error "failed to download package";
	  stderr << "--file downloaded: " << fn << endl;
	  tfn := pkgname | ".tgz";
	  tgzfilenm := tmp | tfn;
	  tgzfilenm << tgzfile << close;
	  cmd := concatenate("cd ",tmp,"; tar xzf ",tfn);
	  stderr << "--- " << cmd << endl;
	  if 0 != chkrun cmd then error("getPackage: failed to untar ",tfn);
	  if not fileExists filename then error("package file ",filename," missing");
	  )
     else (
	  stderr << "--file downloaded: " << fn << endl;
	  filename << m2file << close;
	  );
     pkg := loadPackage(pkgname,
	  DebuggingMode => opts.DebuggingMode,
	  Configuration => opts.Configuration,
	  FileName => filename,
	  LoadDocumentation => true
	  );
     installPackage(pkg, 
	  IgnoreExampleErrors => true, 
	  DebuggingMode => opts.DebuggingMode, 
	  FileName => filename, 
	  UserMode => opts.UserMode);
     )

installedPackages = () -> (
 docdir := applicationDirectory() | "local/" | Layout#1#"docdir";
 if isDirectory docdir then for p in readDirectory docdir list if p =!= "." and p =!= ".." then p else continue else {}
 )

uninstallAllPackages = () -> for p in installedPackages() do (
 << "-- uninstalling package " << p << endl;
 uninstallPackage p
 )

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
