--		Copyright 1993-2003 by Daniel R. Grayson

addStartFunction(
     () -> (
	  path = prepend("./",path);
	  if sourceHomeDirectory =!= null then path = prepend(sourceHomeDirectory|"packages/",path);
	  if prefixDirectory =!= null then path = prepend(prefixDirectory|LAYOUT#"packages",path);
	  ))

packages = {}

Package = new Type of MutableHashTable
Package.synonym = "package"

hide := d -> (
     globalDictionaries = select(globalDictionaries, x -> x =!= d);
     )

reverseDictionary = x -> scan(packages, pkg -> (
	  d := pkg#"reverse dictionary";
	  if d#?x then break d#x))
reverseDictionaryRecord = (X,x) -> if X =!= x then (
     s := toString X;
     scan(packages, 
     	  pkg -> if pkg.Dictionary#?s and pkg.Dictionary#s === X and not pkg#"reverse dictionary"#?x then (	-- too bad a symbol doesn't know what dictionary it's in...
	       pkg#"reverse dictionary"#x = X; 
	       break)))
reverseDictionaryRemove = (X,x) -> (
     s := toString X;
     scan(packages, 
     	  pkg -> if pkg.Dictionary#?s and pkg#"reverse dictionary"#?x and pkg#"reverse dictionary"#x === X then (
	       remove(pkg#"reverse dictionary",x); 
	       break)))

toString Dictionary := d -> (
     if ReverseDictionary#?d then return toString ReverseDictionary#d;
     if PrintNames#?d then return PrintNames#d;
     if length d == 0 then "Dictionary{}" else "Dictionary{..." | toString length d | "...}"
     )

globalAssignFunction = (X,x) -> (
     ReverseDictionary#x = X;
     use x;
     )

globalReleaseFunction = (X,x) -> (
     remove(ReverseDictionary,x);
     )

scan(
     {Type,ScriptedFunctor}, 
     X -> (
	  X.GlobalAssignHook = globalAssignFunction; 
	  X.GlobalReleaseHook = globalReleaseFunction;
	  )
     )

installMethod(GlobalAssignHook,Package,globalAssignFunction)
installMethod(GlobalReleaseHook,Package,globalReleaseFunction)

removePackage = method()
removePackage Package := p -> (
     hide p.Dictionary;
     packages = select(packages, q -> q =!= p);
     stderr << "--previous definitions removed for package " << p << endl;
     )
removePackage String := s -> scan(packages, p -> if p.name == s then removePackage p)

currentPackageS := getGlobalSymbol(PackagesDictionary,"currentPackage")

newPackage = method( Options => { Using => {}, Version => "0.0", WritableSymbols => {}, DebuggingMode => false } )
newPackage(Package) := opts -> p -> (
     hide p.Dictionary;		    -- hide the old dictionary
     newPackage(p.name,opts))
newPackage(String) := opts -> (title) -> (
     if not match("^[a-zA-Z0-9]+$",title) then error( "package title not alphanumeric: ",title);
     stderr << "--package " << p << " loading" << endl;
     removePackage title;
     saveD := globalDictionaries;
     hook := () -> globalDictionaries = saveD;
     newdict := (
	  if title === "Main" then first globalDictionaries
	  else (
	       d := new Dictionary;
	       loadErrorHooks = prepend(hook, loadErrorHooks);
	       globalDictionaries = {d,Main.Dictionary,PackagesDictionary};    -- implement Using, too
	       d));
     if class opts.WritableSymbols =!= List then error "option WritableSymbols: expected a list";
     if not all(opts.WritableSymbols, s -> class s === Symbol) then error "option WritableSymbols: expected a list of symbols";
     p := currentPackageS <- new Package from {
          symbol name => title,
     	  symbol Dictionary => newdict,
	  symbol Version => opts.Version,
	  symbol WritableSymbols => opts.WritableSymbols,
	  "load error hook" => hook,
	  "previous package" => currentPackage,
	  "previous dictionaries" => saveD,
	  "old debugging mode" => debuggingMode,
	  "test inputs" => new MutableHashTable,
	  "reverse dictionary" => new MutableHashTable,
	  "print names" => new MutableHashTable,
	  "raw documentation" => new MutableHashTable,
	  "documentation" => new MutableHashTable,
	  "example inputs" => new MutableHashTable,
	  "example results" => new MutableHashTable,
	  "edited documentation" => new MutableHashTable,
	  "html documentation" => new MutableHashTable,
	  "source directory" => currentFileDirectory,
	  "package prefix" => if title === "Main" then prefixDirectory else (
	       m := matches("(/|^)" | LAYOUT#"packages" | "$", currentFileDirectory);
	       if m#?1 then substring(currentFileDirectory,0,m#1#0 + m#1#1)
	       ),
	  };
     PrintNames#newdict = title | ".Dictionary";
     debuggingMode = opts.DebuggingMode;
     packages = prepend(p,packages);
     p)

addStartFunction( () -> if prefixDirectory =!= null then Main#"package prefix" = prefixDirectory )

newPackage("Main",
     DebuggingMode => debuggingMode,
     Version => version#"VERSION",
     WritableSymbols => {
	  symbol oooo, symbol ooo, symbol oo, symbol path, symbol currentDirectory, symbol fullBacktrace, symbol backtrace,
	  symbol DocDatabase, symbol currentFileName, symbol compactMatrixForm, symbol gbTrace, symbol encapDirectory, 
	  symbol buildHomeDirectory, symbol sourceHomeDirectory, symbol prefixDirectory, symbol currentPrompts, symbol currentPackage,
	  symbol packages, symbol notify, symbol loadDepth, symbol printingPrecision, symbol loadErrorHooks,
	  symbol errorDepth, symbol recursionLimit, symbol globalDictionaries, symbol debuggingMode, 
	  symbol stopIfError, symbol debugLevel, symbol lineNumber, symbol debuggerHook, symbol printWidth
	  })

Command.GlobalAssignHook = 
Function.GlobalAssignHook = 
Manipulator.GlobalAssignHook = (X,x) -> ReverseDictionary#X = x

Function.GlobalReleaseHook = 
Manipulator.GlobalReleaseHook = (X,x) -> remove(ReverseDictionary,x)

Command.GlobalReleaseHook = (X,x) -> (
     stderr << "warning: " << toString X << " redefined" << endl;
     remove(ReverseDictionary,x);
     )

closePackage = method()
closePackage String := title -> (
     if title =!= currentPackage.name then error ("package not the current package");
     p := currentPackage;
     sym := getGlobalSymbol(PackagesDictionary,title);
     p.Symbol = sym;
     globalAssignFunction(sym,p);
     sym <- p;
     scan(p.WritableSymbols, s -> if value s === s then stderr << "warning: unused writable symbol '" << s << "'" << endl);
     ws := set p.WritableSymbols;
     d := p.Dictionary;
     loadErrorHooks = select(loadErrorHooks, f -> f =!= p#"load error hook");
     remove(p,"load error hook");
     scan(values d,
	  s -> (
	       if not ws#?s then protect s;
	       if value s =!= s and not ReverseDictionary#?(value s) then ReverseDictionary#(value s) = s;
	       ));
     if p =!= Main then (			    -- protect it later
	  protect p.Dictionary;
	  );
     if first globalDictionaries =!= p.Dictionary then error ("another dictionary is open");
     if p.name =!= "Main" then globalDictionaries = prepend(d,p#"previous dictionaries");
     currentPackage = p#"previous package";
     debuggingMode = p#"old debugging mode";
     stderr << "--package " << p << " installed" << endl;
     p)

pushDictionary = () -> (
     d := new Dictionary;
     globalDictionaries = prepend(d,globalDictionaries);
     d)

popDictionary = d -> (
     if d =!= first globalDictionaries then error "expected argument to be current dictionary";
     globalDictionaries = drop(globalDictionaries,1);
     d)

dictionary = method()
dictionary Symbol := s -> (				    -- eventually every symbol will know what dictionary it's in, perhaps
     n := toString s;
     scan(globalDictionaries, d -> if d#?n and d#n === s then break d))
dictionary Thing := x -> if ReverseDictionary#?x then dictionary ReverseDictionary#x

package = method ()
package Dictionary := d -> scan(packages, pkg -> if pkg.Dictionary === d then break pkg)
package Thing := x -> (
     d := dictionary x;
     if d =!= null then package d)
package Symbol := s -> (
     d := dictionary s;
     if d =!= null then package d)
package HashTable := package Function := x -> if ReverseDictionary#?x then package ReverseDictionary#x

warned := new MutableHashTable

package TO := x -> (
     key := normalizeDocumentTag x#0;
     pkg := packageTag key;
     fkey := formatDocumentTag key;
     p := select(packages, P -> P#"documentation"#?fkey);
     if #p == 1 then (
	  p = p#0;
	  if pkg =!= p then stderr << "warning: documentation for \"" << fkey << "\" found in package " << p << ", but it seems to belong in " << pkg << endl;
	  p)
     else if #p > 1 then (
	  error("documentation for ",fkey," occurs in multiple packages: ", concatenate between(", ",apply(p,P -> P.name)));
	  )
     else (
	  if not warned#?key then stderr << "warning: documentation for \"" << fkey << "\" not found, assuming it will be found in package " << pkg << " eventually" << endl;
	  warned#key = true;
     	  pkg))

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
