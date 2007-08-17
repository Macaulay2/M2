--		Copyright 1993-2003 by Daniel R. Grayson

addStartFunction(
     () -> (
	  path = prepend("./",path);
	  if sourceHomeDirectory =!= null then path = append(path,sourceHomeDirectory|"packages/");
	  if prefixDirectory =!= null then path = append(path,prefixDirectory|LAYOUT#"packages");
	  ))

Package = new Type of MutableHashTable
Package.synonym = "package"
net Package := toString Package := p -> if p#?"title" then p#"title" else "--package--"
loadedPackages = {}
options Package := p -> p.Options

toString Dictionary := d -> (
     if ReverseDictionary#?d then return toString ReverseDictionary#d;
     if PrintNames#?d then return PrintNames#d;
     toString class d | if length d == 0 then "{}" else "{..." | toString length d | "...}"
     )

Package.GlobalAssignHook = (X,x) -> if not ReverseDictionary#?x then ReverseDictionary#x = X;     -- not 'use x';
Package.GlobalReleaseHook = globalReleaseFunction

dismiss Package := pkg -> (
     loadedPackages = delete(pkg,loadedPackages);
     dictionaryPath = delete(pkg.Dictionary,dictionaryPath);
     dictionaryPath = delete(pkg#"private dictionary",dictionaryPath);
     -- stderr << "--previous definitions removed for package " << pkg << endl;
     )
dismiss String := title -> if PackageDictionary#?title and class value PackageDictionary#title === Package then dismiss value PackageDictionary#title

loadPackage = method(
     Options => {
	  FileName => null,
	  DebuggingMode => null,
	  LoadDocumentation => false,
	  Configuration => {}
	  } )
packageLoadingOptions := new MutableHashTable
loadPackage String := opts -> pkgtitle -> (
     filename := if opts.FileName === null then pkgtitle | ".m2" else opts.FileName;
     packageLoadingOptions#pkgtitle = opts;
     -- if opts.DebuggingMode =!= true then loadDepth = loadDepth - 1;
     -- this was bad, because loadDepth might become negative, and then it gets converted to 255 in the pseudocode
     -- another problem was that loading the file might have resulted in an error.
     load filename;
     -- if opts.DebuggingMode =!= true then loadDepth = loadDepth + 1;
     remove(packageLoadingOptions,pkgtitle);
     if not PackageDictionary#?pkgtitle then error("the file ", filename, " did not define a package ", pkgtitle);
     value PackageDictionary#pkgtitle)

needsPackage = method(Options => options loadPackage)
needsPackage String := opts -> pkg -> (
     if PackageDictionary#?pkg then (
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
	  Version => "0.0", 
	  DebuggingMode => false,
	  InfoDirSection => "Macaulay 2 and its packages",
	  Headline => null,
	  Authors => {}, -- e.g., Authors => { {Name => "Dan Grayson", Email => "dan@math.uiuc.edu", HomePage => "http://www.math.uiuc.edu/~dan/"} }
	  HomePage => null,
	  Date => null,
	  Configuration => {}
	  })

configFileString =
///--Configuration file for package "PKG", automatically generated

-- This print statement may be commented out:
stderr << "--loading configuration for package \"PKG\" from file " << currentFileName << endl

-- This file will be overwritten if a future version of the package has 
-- different options, but the values will be retained and a backup file
-- will be made.

-- Look at 
--         options PKG
-- to see the values of the configuration options after loading the package.

{
     -- The values to the right of the double arrows may be changed
     -- by editing this file:
     VALUES
}
///

-- gdbm makes architecture dependent files, so we try to distinguish them, in case
-- they get mixed
databaseSuffix = "-" | version#"endianness" | "-" | version#"pointer size" | ".db"

newPackage(String) := opts -> (title) -> (
     originalTitle := title;
     if not match("^[a-zA-Z0-9]+$",title) then error( "package title not alphanumeric: ",title);
     -- stderr << "--package \"" << title << "\" loading" << endl;
     dismiss title;
     saveD := dictionaryPath;
     saveP := loadedPackages;
     local hook;
     if title =!= "Core" then (
     	  hook = (
	       haderror -> if haderror then (
	       	    dictionaryPath = saveD;
	       	    loadedPackages = saveP;
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
     newpkg := new Package from {
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
	  "exported symbols" => {},
	  "exported mutable symbols" => {},
	  "example results" => new MutableHashTable,
	  "source directory" => currentFileDirectory,
	  "source file" => currentFileName,
	  "undocumented keys" => new MutableHashTable,
	  "package prefix" => (
	       m := regex("(/|^)" | LAYOUT#"packages" | "$", currentFileDirectory);
	       if m#?1 
	       then substring(currentFileDirectory,0,m#1#0 + m#1#1)
	       else prefixDirectory
	       ),
	  };
     newpkg#"test number" = 0;
     if newpkg#"package prefix" =!= null then (
	  -- these assignments might be premature, for any package that is loaded before dumpdata, as the "package prefix" might change:
	  rawdbname := newpkg#"package prefix" | LAYOUT#"packagecache" title | "rawdocumentation" | databaseSuffix;
	  if fileExists rawdbname then (
	       rawdb := openDatabase rawdbname;
	       newpkg#"raw documentation database" = rawdb;
	       addEndFunction(() -> if isOpen rawdb then close rawdb));
	  dbname := newpkg#"package prefix" | LAYOUT#"packagecache" title | "documentation" | databaseSuffix;
	  if fileExists dbname then (
	       db := openDatabase dbname;
	       newpkg#"processed documentation database" = db;
	       addEndFunction(() -> if isOpen db then close db);
	       );
	  newpkg#"index.html" = newpkg#"package prefix" | LAYOUT#"packagehtml" newpkg#"title" | "index.html";
	  );
     addStartFunction(() -> 
	  if not ( newpkg#?"processed documentation database" and isOpen newpkg#"processed documentation database" ) and prefixDirectory =!= null 
	  then (
	       dbname := prefixDirectory | LAYOUT#"packagecache" title | "documentation" | databaseSuffix; -- what if there is more than one prefix directory?
	       if fileExists dbname then (
		    db := newpkg#"processed documentation database" = openDatabase dbname;
		    addEndFunction(() -> if isOpen db then close db))));
     addStartFunction(() -> 
	  if not ( newpkg#?"raw documentation database" and isOpen newpkg#"raw documentation database" ) and prefixDirectory =!= null 
	  then (
	       dbname := prefixDirectory | LAYOUT#"packagecache" title | "rawdocumentation" | databaseSuffix; -- what if there is more than one prefix directory?
	       if fileExists dbname then (
		    db := newpkg#"raw documentation database" = openDatabase dbname;
		    addEndFunction(() -> if isOpen db then close db))));
     pkgsym := getGlobalSymbol(PackageDictionary,title);
     global currentPackage <- newpkg;
     ReverseDictionary#newpkg = pkgsym;
     pkgsym <- newpkg;
     loadedPackages = join( 
	  -- if title === "Core" then {} else {newpkg}, 
	  {Core} );
     dictionaryPath = join( {newpkg#"private dictionary"}, {Core.Dictionary, OutputDictionary, PackageDictionary});
     if Core#?"base packages" then (
	  if member(title,Core#"base packages") then (
	       if title =!= "Macaulay2" and member("Macaulay2",Core#"base packages") then needsPackage "Macaulay2";
	       )
	  else scan(reverse Core#"base packages", needsPackage)
	  );
     PrintNames#(newpkg.Dictionary) = title | ".Dictionary";
     PrintNames#(newpkg#"private dictionary") = title | "#\"private dictionary\"";
     debuggingMode = opts.DebuggingMode;		    -- last step before turning control back to code of package
     newpkg)

export = method(Dispatch => Thing)
export Symbol := x -> export {x}
export List := v -> (
     if currentPackage === null then error "no current package";
     pd := currentPackage#"private dictionary";
     d := currentPackage.Dictionary;
     title := currentPackage#"title";
     scan(v, sym -> (
	       local nam;
	       if class sym === Option then (
		    nam = sym#0;			    -- synonym
     	       	    if class nam =!= String then error("expected a string: ", nam);
		    if pd#?nam then error("symbol intended as exported synonym already used internally: ",format nam, "\n", symbolLocation pd#nam, ": it was used here");
		    sym = sym#1;
		    )
	       else (
		    nam = toString sym;
		    );
	       if not instance(sym,Symbol) then error ("expected a symbol: ", sym);
	       if not pd#?(toString sym) or pd#(toString sym) =!= sym 
	       then (
		    -- error ("symbol ",sym," defined elsewhere, not in current package: ", currentPackage);
		    sym = getGlobalSymbol(pd,nam);	    -- replace sym by one in the current package's private dictionary
		    );
	       syn := title | "$" | nam;
	       d#syn = d#nam = sym;
	       ));
     currentPackage#"exported symbols" = join(currentPackage#"exported symbols",select(v,s -> instance(s,Symbol)));
     )
exportMutable = method(Dispatch => Thing)
exportMutable Symbol := x -> exportMutable {x}
exportMutable List := v -> (
     export v;
     currentPackage#"exported mutable symbols" = join(currentPackage#"exported mutable symbols",select(v,s -> instance(s,Symbol)));
     )

addStartFunction( () -> if prefixDirectory =!= null then Core#"package prefix" = prefixDirectory )

saveCurrentPackage := currentPackage

newPackage("Core", 
     Authors => {
	  {Name => "Daniel R. Grayson", Email => "dan@math.uiuc.edu", HomePage => "http://www.math.uiuc.edu/~dan/"}, 
	  {Name => "Michael E. Stillman", Email => "mike@math.cornell.edu", HomePage => "http://www.math.cornell.edu/People/Faculty/stillman.html"}
	  },
     DebuggingMode => debuggingMode,
     HomePage => "http://www.math.uiuc.edu/Macaulay2/",
     Version => version#"VERSION", 
     Headline => "A computer algebra system designed to support algebraic geometry")

findSynonyms = method()
findSynonyms Symbol := x -> (
     r := {};
     scan(dictionaryPath, d -> scan(pairs d, (nam,sym) -> if x === sym and getGlobalSymbol nam === sym then r = append(r,nam)));
     sort unique r)

checkShadow = () -> (
     d := dictionaryPath;
     n := #d;
     for i from 0 to n-1 do
     for j from i+1 to n-1 do
     scan(keys d#i, nam -> if d#j#?nam and d#i#nam =!= d#j#nam then (
	       stderr << "--warning: symbol '" << nam << "' in " << d#j << " is shadowed by a symbol in " << d#i << endl;
	       sym := d#j#nam;
	       w := findSynonyms sym;
	       w = select(w, s -> s != nam);
	       if #w > 0 then stderr << "--   synonym" << (if #w > 1 then "s") << " for " << nam << ": " << demark(", ",w) << endl
	       else if class User === Package
	       and User#?"private dictionary"
	       and member(User#"private dictionary",dictionaryPath) then for i from 0 do (
		    newsyn := nam | "$" | toString i;
		    if not isGlobalSymbol newsyn then (
			 User#"private dictionary"#newsyn = sym;
			 stderr << "--   new synonym provided for '" << nam << "': " << newsyn << endl;
			 break)))))

sortByHash := v -> last \ sort \\ (i -> (hash i, i)) \ v

endPackage = method()
endPackage String := title -> (
     if currentPackage === null or title =!= currentPackage#"title" then error ("package not current: ",title);
     pkg := currentPackage;
     ws := set pkg#"exported mutable symbols";
     exportDict := pkg.Dictionary;
     scan(sortByHash values exportDict, s -> if not ws#?s then (
	       protect s;
	       if value s =!= s and not ReverseDictionary#?(value s) then ReverseDictionary#(value s) = s));
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
     checkShadow();
     remove(pkg,"previous dictionaries");
     remove(pkg,"previous packages");
     hook := pkg#"close hook";
     remove(pkg,"close hook");
     fileExitHooks = select(fileExitHooks, f -> f =!= hook);
     global currentPackage <- pkg#"previous currentPackage";
     remove(pkg,"previous currentPackage");
     debuggingMode = pkg#"old debuggingMode"; remove(pkg,"old debuggingMode");
     if notify then stderr << "--package \"" << pkg << "\" loaded" << endl;
     pkg)

package = method ()
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
     scan(dictionaryPath, d -> if d#?n and d#n === s then (
	       if d === PackageDictionary and class value s === Package then break value s
	       else if package d =!= null then break package d)));
package HashTable := package Function := x -> if ReverseDictionary#?x then package ReverseDictionary#x

use Package := pkg -> (
     a := member(pkg,loadedPackages);
     b := member(pkg.Dictionary,dictionaryPath);
     if a and not b then error("use: package ",toString pkg," appears in loadedPackages, but its dictionary is missing from dictionaryPath");
     if b and not a then error("use: package ",toString pkg," does not appear in loadedPackages, but its dictionary appears in dictionaryPath");
     if not a and not b then (
     	  loadedPackages = prepend(pkg,loadedPackages);
     	  dictionaryPath = prepend(pkg.Dictionary,dictionaryPath);
	  );
     )

beginDocumentation = () -> (
     if packageLoadingOptions#?(currentPackage#"title") and not packageLoadingOptions#(currentPackage#"title").LoadDocumentation
     and currentPackage#?"raw documentation database" and isOpen currentPackage#"raw documentation database" then (
	  if notify then stderr << "--beginDocumentation: using documentation database, skipping the rest of " << currentFileName << endl;
	  currentPackage#"documentation not loaded" = true;
	  return end;
	  )
     else (
	  if notify then stderr << "--beginDocumentation: reading the rest of " << currentFileName << endl;
	  )
     )

debug = method()
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
	  DebuggingMode => false
	  })
getPackage String := opts -> pkgname -> (
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
	  if 0 != run cmd then error("getPackage: failed to untar ",tfn);
	  if not fileExists filename then error("package file ",filename," missing");
	  )
     else (
	  stderr << "--file downloaded: " << fn << endl;
	  filename << m2file << close;
	  );
     installPackage(pkgname, IgnoreExampleErrors => true, FileName => filename, DebuggingMode => opts.DebuggingMode);
     )

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
