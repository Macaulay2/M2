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
packages = {}

toString Dictionary := d -> (
     if ReverseDictionary#?d then return toString ReverseDictionary#d;
     if PrintNames#?d then return PrintNames#d;
     if length d == 0 then "Dictionary{}" else "Dictionary{..." | toString length d | "...}"
     )

installMethod(GlobalAssignHook,Package,globalAssignFunction)
installMethod(GlobalReleaseHook,Package,globalReleaseFunction)

dismiss Package := pkg -> (
     packages = delete(pkg,packages);
     globalDictionaries = delete(pkg.Dictionary,globalDictionaries);
     globalDictionaries = delete(pkg#"private dictionary",globalDictionaries);
     -- stderr << "--previous definitions removed for package " << pkg << endl;
     )
dismiss String := title -> if PackageDictionary#?title and class value PackageDictionary#title === Package then dismiss value PackageDictionary#title

substituteOptions := new MutableHashTable
loadPackage = method(
     Options => {
	  DebuggingMode => null
	  } )
loadPackage String := opts -> pkgtitle -> (
     filename := pkgtitle | ".m2";
     substituteOptions#pkgtitle = opts;
     load filename;
     remove(substituteOptions,pkgtitle);
     if not PackageDictionary#?pkgtitle then error("the file ", filename, " did not define a package ", pkgtitle);
     value PackageDictionary#pkgtitle)

needsPackage = method(Options => options loadPackage)
needsPackage String := opts -> pkg -> (
     if PackageDictionary#?pkg then use value PackageDictionary#pkg
     else loadPackage(pkg, opts)
     )

newPackage = method( 
     Options => { 
	  Version => "0.0", 
	  DebuggingMode => false,
	  InfoDirSection => "Macaulay 2 and its packages",
	  Headline => null,
	  Author => null,
	  Email => null,
	  HomePage => null,
	  Date => null } )
newPackage(String) := opts -> (title) -> (
     originalTitle := title;
     if not match("^[a-zA-Z0-9]+$",title) then error( "package title not alphanumeric: ",title);
     -- stderr << "--package \"" << title << "\" loading" << endl;
     dismiss title;
     saveD := globalDictionaries;
     saveP := packages;
     local hook;
     if title =!= "Macaulay2Core" then (
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
     	  symbol Dictionary => new Dictionary, -- this is the global one
     	  "private dictionary" => if title === "Macaulay2Core" then first globalDictionaries else new Dictionary, -- this is the local one
     	  "close hook" => hook,
	  "previous currentPackage" => currentPackage,
	  "previous dictionaries" => saveD,
	  "previous packages" => saveP,
	  "old debuggingMode" => debuggingMode,
	  "old testnumber" => testnumber,
	  "test inputs" => new MutableHashTable,
	  "raw documentation" => new MutableHashTable,	    -- deposited here by 'document'
	  "processed documentation" => new MutableHashTable,-- the output from 'documentation', look here first
	  "example inputs" => new MutableHashTable,
	  "exported symbols" => new MutableList from {},
	  "exported mutable symbols" => new MutableList from {},
	  "example results" => new MutableHashTable,
	  "source directory" => currentFileDirectory,
	  "undocumented keys" => new MutableHashTable,
	  "package prefix" => (
	       m := matches("(/|^)" | LAYOUT#"packages" | "$", currentFileDirectory);
	       if m#?1 
	       then substring(currentFileDirectory,0,m#1#0 + m#1#1)
	       else prefixDirectory
	       ),
	  };
     testnumber = 0;
     if newpkg#"package prefix" =!= null then (
	  -- these assignments might be premature, for any package which is loaded before dumpdata, as the "package prefix" might change:
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
	       );
	  newpkg#"index.html" = newpkg#"package prefix" | LAYOUT#"packagehtml" newpkg#"title" | "index.html";
	  );
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
     newpkg#"private dictionary"#originalTitle = pkgsym;	    -- local synonym under original title, in case the package is loaded under a different title and tries to refer to itself
     global currentPackage <- newpkg;
     ReverseDictionary#newpkg = pkgsym;
     pkgsym <- newpkg;
     packages = join(
	  if title === "Macaulay2Core" then {} else {newpkg},
	  {Macaulay2Core}
	  );
     globalDictionaries = join(
	  {newpkg#"private dictionary"},
	  {Macaulay2Core.Dictionary, OutputDictionary, PackageDictionary}
	  );
     PrintNames#(newpkg.Dictionary) = title | ".Dictionary";
     PrintNames#(newpkg#"private dictionary") = title | "#\"private dictionary\"";
     debuggingMode = if substituteOptions#?title and substituteOptions#title#DebuggingMode =!= null then substituteOptions#title#DebuggingMode else opts.DebuggingMode;
     newpkg)

export = method(SingleArgumentDispatch => true)
export Symbol := x -> export (1:x)
export List := v -> export toSequence v
export Sequence := v -> (
     if currentPackage === null then error "no current package";
     pd := currentPackage#"private dictionary";
     d := currentPackage.Dictionary;
     title := currentPackage#"title";
     scan(v, sym -> (
	       local nam;
	       if class sym === Option then (
		    nam = sym#0;			    -- synonym
     	       	    if class nam =!= String then error("expected a string: ", nam);
		    if pd#?nam then error("symbol intended as exported synonym already used internally: ",format nam, ", at ", symbolLocation pd#nam);
		    sym = sym#1;
		    )
	       else (
		    nam = toString sym;
		    );
	       if not instance(sym,Symbol) then error ("expected a symbol: ", sym);
	       if not pd#?(toString sym) or pd#(toString sym) =!= sym then error ("symbol ",sym," defined elsewhere, not in current package: ", currentPackage);
	       syn := title | "$" | nam;
	       d#syn = d#nam = sym;
	       ));
     currentPackage#"exported symbols" = join(currentPackage#"exported symbols",select(v,s -> instance(s,Symbol)));
     v)
exportMutable = method(SingleArgumentDispatch => true)
exportMutable Symbol := x -> exportMutable (1:x)
exportMutable List := v -> exportMutable toSequence v
exportMutable Sequence := v -> (
     export v;
     currentPackage#"exported mutable symbols" = join(currentPackage#"exported mutable symbols",select(v,s -> instance(s,Symbol)));
     v)

addStartFunction( () -> if prefixDirectory =!= null then Macaulay2Core#"package prefix" = prefixDirectory )

saveCurrentPackage := currentPackage

newPackage("Macaulay2Core", 
     Author => {"Daniel R. Grayson", "Michael E. Stillman"},
     Email => {"dan@math.uiuc.edu", "mike@math.cornell.edu"},
     HomePage => {"http://www.math.uiuc.edu/~dan/", "http://www.math.cornell.edu/People/Faculty/stillman.html"},
     DebuggingMode => debuggingMode, 
     Version => version#"VERSION", 
     Headline => "A computer algebra system designed to support algebraic geometry")

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
     scan(keys d#i, nam -> if d#j#?nam and d#i#nam =!= d#j#nam then (
	       stderr << "--warning: symbol '" << nam << "' in " << d#j << " is shadowed by symbol in " << d#i << endl;
	       sym := d#j#nam;
	       w := findSynonyms sym;
	       w = select(w, s -> s != nam);
	       if #w > 0 then stderr << "--   synonym" << (if #w > 1 then "s") << " for " << nam << ": " << demark(", ",w) << endl
	       else if member(User.Dictionary,globalDictionaries) then for i from 0 do (
		    newsyn := nam | "$" | toString i;
		    if not isGlobalSymbol newsyn then (
			 User.Dictionary#newsyn = sym;
			 stderr << "--   new synonym provided for '" << nam << "': " << newsyn << endl;
			 break)))))

sortByHash := v -> last \ sort \\ (i -> (hash i, i)) \ v

closePackage = method()
closePackage String := title -> (
     if currentPackage === null or title =!= currentPackage#"title" then error ("package not current: ",title);
     pkg := currentPackage;
     ws := set pkg#"exported mutable symbols";
     exportDict := pkg.Dictionary;
     scan(sortByHash values exportDict, s -> if not ws#?s then (
	       protect s;
	       if value s =!= s and not ReverseDictionary#?(value s) then ReverseDictionary#(value s) = s));
     if true or pkg =!= Macaulay2Core then (			    -- protect it later
	  protect pkg#"private dictionary";
	  protect exportDict;
	  );
     if pkg#"title" === "Macaulay2Core" then (
	  packages = {pkg};
	  globalDictionaries = {Macaulay2Core.Dictionary, OutputDictionary, PackageDictionary};
	  )
     else (
	  packages = prepend(pkg,pkg#"previous packages");
	  globalDictionaries = prepend(exportDict,pkg#"previous dictionaries");
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
     testnumber = pkg#"old testnumber"; remove(pkg,"old testnumber");
     stderr << "--package \"" << pkg << "\" loaded" << endl;
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
     scan(globalDictionaries, d -> if d#?n and d#n === s then (
	       if d === PackageDictionary and class value s === Package then break value s
	       else if package d =!= null then break package d)));
package HashTable := package Function := x -> if ReverseDictionary#?x then package ReverseDictionary#x

Package.GlobalAssignHook = (X,x) -> if not ReverseDictionary#?x then ReverseDictionary#x = X;     -- not 'use x';
Package.GlobalReleaseHook = globalReleaseFunction
use Package := pkg -> if not member(pkg,packages) then (
     packages = prepend(pkg,packages);
     globalDictionaries = prepend(pkg.Dictionary,globalDictionaries);
     )

forceLoadDocumentation = false
beginDocumentation = () -> (
     if not forceLoadDocumentation and currentPackage#?"processed documentation database" and isOpen currentPackage#"processed documentation database" then (
	  -- stderr << "--beginDocumentation: using documentation database, skipping the rest of " << currentFileName << endl;
	  return end;
	  );
     )

debug = method()
debug Package := pkg -> (
     d := pkg#"private dictionary";
     if not member(d,globalDictionaries) then (
	  globalDictionaries = prepend(d,globalDictionaries);
	  ))


-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
