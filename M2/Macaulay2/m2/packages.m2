--		Copyright 1993-2003 by Daniel R. Grayson

addStartFunction(
     () -> (
	  path = prepend("./",path);
	  if sourceHomeDirectory =!= null then path = prepend(sourceHomeDirectory|"packages/",path);
	  if prefixDirectory =!= null then path = prepend(prefixDirectory|LAYOUT#"packages",path);
	  ))

Package = new Type of MutableHashTable
Package.synonym = "package"
net Package := toString Package := p -> if p#?"title" then p#"title" else "--package--"
packages = {}

hide := pkg -> (
     globalDictionaries = select(globalDictionaries, x -> x =!= pkg.Dictionary);
     packages = select(packages, x -> x =!= pkg);
     )

toString Dictionary := d -> (
     if ReverseDictionary#?d then return toString ReverseDictionary#d;
     if PrintNames#?d then return PrintNames#d;
     if length d == 0 then "Dictionary{}" else "Dictionary{..." | toString length d | "...}"
     )

installMethod(GlobalAssignHook,Package,globalAssignFunction)
installMethod(GlobalReleaseHook,Package,globalReleaseFunction)

removePackage = method()
removePackage Package := p -> (
     hide p;
     stderr << "--previous definitions removed for package " << p << endl;
     )
removePackage String := title -> if PackageDictionary#?title and class value PackageDictionary#title === Package then removePackage value PackageDictionary#title

substituteOptions := new MutableHashTable
loadPackage = method(
     Options => {
	  Title => null, 
	  DebuggingMode => null } )
loadPackage String := opts -> filename -> (
     filename = filename | ".m2";
     substituteOptions#filename = opts;
     load filename;
     remove(substituteOptions,filename);
     )

newPackage = method( 
     Options => { 
	  Using => {}, 
	  Version => "0.0", 
	  DebuggingMode => false,
	  InfoDirSection => "Math",
	  Headline => null,
	  TopNodeName => null } )
newPackage(String) := opts -> (title) -> (
     originalTitle := title;
     filename := baseFilename currentFileName;
     if substituteOptions#?filename and substituteOptions#filename#Title =!= null then title = substituteOptions#filename#Title;
     if not match("^[a-zA-Z0-9]+$",title) then error( "package title not alphanumeric: ",title);
     if class opts.Using =!= List or not all(opts.Using, pkg -> class pkg === Package) then error "expected 'Using' option to be a list of loaded packages";
     stderr << "--package " << title << " loading" << endl;
     removePackage title;
     saveD := globalDictionaries;
     saveP := packages;
     local hook;
     if title =!= "Macaulay2" then (
     	  hook = (
	       haderror -> if haderror then (
	       	    globalDictionaries = saveD;
	       	    packages = saveP;
		    )
	       else closePackage title
	       );
	  fileExitHooks = prepend(hook, fileExitHooks);
	  );
     newpkg := new Package from {
          "title" => title,
	  symbol Options => opts,
     	  symbol Dictionary => if title === "Macaulay2" then first globalDictionaries else new Dictionary, -- this is the global one
     	  "close hook" => hook,
	  "previous currentPackage" => currentPackage,
	  "previous dictionaries" => saveD,
	  "previous packages" => saveP,
	  "mutable symbols" => {},
	  "old debuggingMode" => debuggingMode,
	  "test inputs" => new MutableHashTable,
	  "raw documentation" => new MutableHashTable,	    -- deposited here by 'document'
	  "processed documentation" => new MutableHashTable,-- the output from 'documentation', look here first
	  "example inputs" => new MutableHashTable,
	  "top node name" => if opts.TopNodeName === null then title else opts.TopNodeName,
	  "exported symbols" => {},
	  "example results" => new MutableHashTable,
	  "source directory" => currentFileDirectory,
	  "package prefix" => if title === "Macaulay2" then prefixDirectory else (
	       m := matches("(/|^)" | LAYOUT#"packages" | "$", currentFileDirectory);
	       if m#?1 then substring(currentFileDirectory,0,m#1#0 + m#1#1)
	       ),
	  };
     if newpkg#"package prefix" =!= null then (
	  rawdbname := newpkg#"package prefix" | LAYOUT#"packagedoc" title | "rawdocumentation.db";
	  if fileExists rawdbname then (
	       rawdb := openDatabase rawdbname;
	       newpkg#"raw documentation database" = rawdb;
	       addEndFunction(() -> if isOpen rawdb then close rawdb));
	  dbname := newpkg#"package prefix" | LAYOUT#"packagedoc" title | "documentation.db";
	  if fileExists dbname then (
	       db := openDatabase dbname;
	       newpkg#"processed documentation database" = db;
	       addEndFunction(() -> if isOpen db then close db);
	       ));
     addStartFunction(() -> 
	  if not ( newpkg#?"processed documentation database" and isOpen newpkg#"processed documentation database" ) and prefixDirectory =!= null 
	  then (
	       dbname := prefixDirectory | LAYOUT#"packagedoc" title | "documentation.db"; -- what if there is more than one prefix directory?
	       if fileExists dbname then (
		    db := newpkg#"processed documentation database" = openDatabase dbname;
		    addEndFunction(() -> if isOpen db then close db))));
     addStartFunction(() -> 
	  if not ( newpkg#?"raw documentation database" and isOpen newpkg#"raw documentation database" ) and prefixDirectory =!= null 
	  then (
	       dbname := prefixDirectory | LAYOUT#"packagedoc" title | "rawdocumentation.db"; -- what if there is more than one prefix directory?
	       if fileExists dbname then (
		    db := newpkg#"raw documentation database" = openDatabase dbname;
		    addEndFunction(() -> if isOpen db then close db))));
     pkgsym := getGlobalSymbol(PackageDictionary,title);
     if title =!= "Macaulay2" then (
	  newpkg#"private dictionary" = new Dictionary; -- this is the local one
      	  PrintNames#(newpkg#"private dictionary") = title | "#\"private dictionary\"";
     	  newpkg#"private dictionary"#originalTitle = pkgsym;	    -- local synonym under original title, in case the package is loaded under a different title and tries to refer to itself
	  );
     global currentPackage <- newpkg;
     ReverseDictionary#newpkg = pkgsym;
     pkgsym <- newpkg;
     packages = join(
	  if title === "Macaulay2" then {} else {newpkg},
	  {Macaulay2},
	  opts.Using
	  );
     globalDictionaries = join(
	  if title === "Macaulay2" then {} else {newpkg#"private dictionary"},
	  {Macaulay2.Dictionary, OutputDictionary, PackageDictionary},
	  apply(opts.Using,pkg->pkg.Dictionary)
	  );
     PrintNames#(newpkg.Dictionary) = title | ".Dictionary";
     debuggingMode = if substituteOptions#?filename and substituteOptions#filename#DebuggingMode =!= null then substituteOptions#filename#DebuggingMode else opts.DebuggingMode;
     newpkg)

export = method(SingleArgumentDispatch => true)
export Symbol := x -> export singleton x
export Sequence := v -> export toList v
export List := v -> (
     if not all(v, x -> instance(x, Symbol)) then error "expected a list of symbols";
     if currentPackage === null then error "no current package";
     scan(v, sym -> 
	  if currentPackage =!= Macaulay2 and (
	       not currentPackage#"private dictionary"#?(toString sym)
	       or currentPackage#"private dictionary"#(toString sym) =!= sym
	       )
	  then error ("symbol ",sym," not defined in current package ", toString currentPackage)
	  );
     currentPackage#"exported symbols" = join(currentPackage#"exported symbols",v);
     if currentPackage =!= Macaulay2 then scan(v, s -> (
	       currentPackage.Dictionary#(toString s) = s;
	       currentPackage.Dictionary#(currentPackage#"title" | "$" | toString s) = s;
	       ));
     v)
exportMutable = method(SingleArgumentDispatch => true)
exportMutable Symbol := x -> exportMutable singleton x
exportMutable Sequence := v -> exportMutable toList v
exportMutable List := v -> (
     export v;
     currentPackage#"mutable symbols" = join(currentPackage#"mutable symbols",v);
     v)

addStartFunction( () -> if prefixDirectory =!= null then Macaulay2#"package prefix" = prefixDirectory )

newPackage("Macaulay2", 
     DebuggingMode => debuggingMode, 
     Version => version#"VERSION", 
     TopNodeName => "Macaulay 2",
     Headline => "A computer algebra system designed to support algebraic geometry")

exportMutable {
	  symbol oooo, symbol ooo, symbol oo, symbol path, symbol fullBacktrace, symbol backtrace, symbol packages, symbol currentFileName, symbol compactMatrixForm, symbol
	  gbTrace, symbol encapDirectory, symbol buildHomeDirectory, symbol sourceHomeDirectory, symbol prefixDirectory, symbol currentPrompts, symbol currentPackage, symbol
	  notify, symbol loadDepth, symbol printingPrecision, symbol fileExitHooks, symbol debugError, symbol errorDepth, symbol recursionLimit, symbol globalDictionaries, symbol
	  debuggingMode, symbol stopIfError, symbol debugLevel, symbol lineNumber, symbol debuggerHook, symbol printWidth, symbol backupFileRegexp
	  }

findSynonyms = method()
findSynonyms Symbol := x -> (
     r := {};
     scan(globalDictionaries, d -> scan(pairs d, (nam,sym) -> if x === sym and getGlobalSymbol nam === sym then r = append(r,nam)));
     sort unique r)

checkShadow = () -> (
     d := globalDictionaries;
     n := #d;
     for i from 0 to n-1 do
     for j from i+1 to n-1 do
     scan(keys d#i, nam -> if d#j#?nam then (
	       stderr << "--warning: symbol '" << nam << "' in " << d#j << " is shadowed by symbol in " << d#i << endl;
	       sym := d#j#nam;
	       w := findSynonyms sym;
	       w = select(w, s -> s != nam);
	       if #w > 0 then stderr << "--   synonym" << (if #w > 1 then "s") << " for " << nam << ": " << demark(", ",w) << endl
	       else if member(UserDictionary,globalDictionaries) then for i from 0 do (
		    newsyn := nam | "$" | toString i;
		    if not isGlobalSymbol newsyn then (
			 UserDictionary#newsyn = sym;
			 stderr << "--   new synonym provided for '" << nam << "': " << newsyn << endl;
			 break)))))

sortByHash := v -> last \ sort \\ (i -> (hash i, i)) \ v

closePackage = method()
closePackage String := title -> (
     if currentPackage === null or title =!= currentPackage#"title" then error ("package not current: ",title);
     pkg := currentPackage;
     scan(pkg#"mutable symbols", s -> (
	       if value s === s
	       and s =!= symbol oo
	       and s =!= symbol ooo
	       and s =!= symbol oooo
	       then stderr << "--warning: unused writable symbol '" << s << "'" << endl));
     ws := set pkg#"mutable symbols";
     dict := pkg.Dictionary;
     scan(sortByHash values dict, s -> if not ws#?s then (
	       protect s;
	       if value s =!= s and not ReverseDictionary#?(value s) then ReverseDictionary#(value s) = s));
     exportDict := pkg.Dictionary;
     if pkg =!= Macaulay2 then (			    -- protect it later
	  protect dict;					    -- maybe don't do this, as it will be private
	  protect exportDict;
	  );
     if pkg#"title" =!= "Macaulay2" then (
	  packages = prepend(pkg,pkg#"previous packages");
	  globalDictionaries = prepend(exportDict,pkg#"previous dictionaries");
     	  checkShadow();
	  );
     remove(pkg,"previous dictionaries");
     remove(pkg,"previous packages");
     hook := pkg#"close hook";
     remove(pkg,"close hook");
     fileExitHooks = select(fileExitHooks, f -> f =!= hook);
     global currentPackage <- pkg#"previous currentPackage";
     remove(pkg,"previous currentPackage");
     debuggingMode = pkg#"old debuggingMode";
     remove(pkg,"old debuggingMode");
     stderr << "--package " << pkg << " installed" << endl;
     pkg)

pushDictionary = () -> (
     d := new Dictionary;
     globalDictionaries = prepend(d,globalDictionaries);
     d)

popDictionary = d -> (
     if d =!= first globalDictionaries then error "expected argument to be current dictionary";
     globalDictionaries = drop(globalDictionaries,1);
     d)

package = method ()
package Dictionary := d -> (
     if currentPackage =!= null and (currentPackage.Dictionary === d or currentPackage#?"private dictionary" and currentPackage#"private dictionary" === d)
     then currentPackage 
     else scan(values PackageDictionary, pkg -> if (value pkg).Dictionary === d then break (value pkg))
     )
package Thing := x -> (
     d := dictionary x;
     if d =!= null then package d)
package Symbol := s -> (
     d := dictionary s;
     if d === PackageDictionary then value s
     else if d =!= null then package d )
package HashTable := package Function := x -> if ReverseDictionary#?x then package ReverseDictionary#x

Package.GlobalAssignHook = (X,x) -> if not ReverseDictionary#?x then ReverseDictionary#x = X;     -- not 'use x';
Package.GlobalReleaseHook = globalReleaseFunction
use Package := pkg -> if not member(pkg,packages) then (
     packages = prepend(pkg,packages);
     globalDictionaries = prepend(pkg.Dictionary,globalDictionaries);
     )

needsPackage = method()
needsPackage String := s -> (
     if PackageDictionary#?s then use value PackageDictionary#s
     else load (s | ".m2")
     )

beginDocumentation = () -> (
     if currentPackage#?"processed documentation database" and isOpen currentPackage#"processed documentation database" then (
	  stderr << "--beginDocumentation: using documentation database, skipping the rest of " << currentFileName << endl;
	  return end;
	  );
     )

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
