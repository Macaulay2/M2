--		Copyright 1993-2003 by Daniel R. Grayson

addStartFunction(
     () -> (
	  path = prepend("./",path);
	  if sourceHomeDirectory =!= null then path = prepend(sourceHomeDirectory|"packages/",path);
	  if prefixDirectory =!= null then path = prepend(prefixDirectory|LAYOUT#"packages",path);
	  ))

Package = new Type of MutableHashTable
Package.synonym = "package"

hide := d -> (
     globalDictionaries = select(globalDictionaries, x -> x =!= d);
     )

reverseDictionary = x -> scan(values PackageDictionary, 
     p -> (
	  pkg := value p;
	  d := pkg#"reverse dictionary";
	  if d#?x then break d#x))
reverseDictionaryRecord = (X,x) -> if X =!= x then (
     s := toString X;
     scan(values PackageDictionary, 
     	  p -> (
	       pkg := value p;
	       if pkg.Dictionary#?s and pkg.Dictionary#s === X and not pkg#"reverse dictionary"#?x 
	       then (	-- too bad a symbol doesn't know what dictionary it's in...
	       	    pkg#"reverse dictionary"#x = X; 
	       	    break))))
reverseDictionaryRemove = (X,x) -> (
     s := toString X;
     scan(values PackageDictionary, 
     	  p -> (
	       pkg := value p;
	       if pkg.Dictionary#?s and pkg#"reverse dictionary"#?x and pkg#"reverse dictionary"#x === X 
	       then (
	       	    remove(pkg#"reverse dictionary",x); 
	       	    break))))

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
     stderr << "--previous definitions removed for package " << p << endl;
     )
removePackage String := n -> if PackageDictionary#?n and class value PackageDictionary#n === Package then removePackage value PackageDictionary#n

currentPackageS := getGlobalSymbol(PackageDictionary,"currentPackage")

newPackage = method( 
     Options => { 
	  Using => {}, 
	  Version => "0.0", 
	  WritableSymbols => {}, 
	  DebuggingMode => false
	  }
     )
newPackage(Package) := opts -> p -> (
     hide p.Dictionary;		    -- hide the old dictionary
     newPackage(p.name,opts))
newPackage(String) := opts -> (title) -> (
     if not match("^[a-zA-Z0-9]+$",title) then error( "package title not alphanumeric: ",title);
     if not all(opts.Using, pkg -> class pkg === Package) then error "expected 'Using' option to be a list of loaded packages";
     stderr << "--package " << title << " loading" << endl;
     removePackage title;
     saveD := globalDictionaries;
     hook := haderror -> if haderror then globalDictionaries = saveD else closePackage title;
     newdict := (
	  if title === "Main" then first globalDictionaries
	  else (
	       d := new Dictionary;
	       fileExitHooks = prepend(hook, fileExitHooks);
	       globalDictionaries = join({d,PackageDictionary,Main.Dictionary},apply(opts.Using,pkg->pkg.Dictionary));
	       d));
     if class opts.WritableSymbols =!= List then error "option WritableSymbols: expected a list";
     if not all(opts.WritableSymbols, s -> class s === Symbol) then error "option WritableSymbols: expected a list of symbols";
     p := currentPackageS <- new Package from {
          symbol name => title,
     	  symbol Dictionary => newdict,
	  symbol Options => opts,
     	  "close hook" => hook,
	  "previous package" => currentPackage,
	  "previous dictionaries" => saveD,
	  "old debugging mode" => debuggingMode,
	  "test inputs" => new MutableHashTable,
	  "reverse dictionary" => new MutableHashTable,
	  "print names" => new MutableHashTable,
	  "raw documentation" => new MutableHashTable,
	  "documentation" => new MutableHashTable,
	  "example inputs" => new MutableHashTable,
	  "exported symbols" => {},
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
     sym := getGlobalSymbol(PackageDictionary,title);
     p.Symbol = sym;
     globalAssignFunction(sym,p);
     sym <- p;
     p)

export = method(SingleArgumentDispatch => true)
export Symbol := x -> singleton x
export Sequence := v -> export toList v
export List := v -> (
     if not all(v, x -> class x === Symbol) then error "expected a list of symbols";
     if currentPackage === null then error "no current package";
     currentPackage#"exported symbols" = join(currentPackage#"exported symbols",v);
     v)

addStartFunction( () -> if prefixDirectory =!= null then Main#"package prefix" = prefixDirectory )

newPackage("Main",
     DebuggingMode => debuggingMode,
     Version => version#"VERSION",
     WritableSymbols => {
	  symbol oooo, symbol ooo, symbol oo, symbol path, symbol currentDirectory, symbol fullBacktrace, symbol backtrace,
	  symbol DocDatabase, symbol currentFileName, symbol compactMatrixForm, symbol gbTrace, symbol encapDirectory, 
	  symbol buildHomeDirectory, symbol sourceHomeDirectory, symbol prefixDirectory, symbol currentPrompts, symbol currentPackage,
	  symbol notify, symbol loadDepth, symbol printingPrecision, symbol fileExitHooks,
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
     if currentPackage === null or title =!= currentPackage.name then error ("package not current: ",title);
     p := currentPackage;
     scan(p.Options.WritableSymbols, s -> if value s === s then stderr << "warning: unused writable symbol '" << s << "'" << endl);
     ws := set p.Options.WritableSymbols;
     d := p.Dictionary;
     scan(values d,
	  s -> (
	       if not ws#?s then protect s;
	       if value s =!= s and not ReverseDictionary#?(value s) then ReverseDictionary#(value s) = s;
	       ));
     if p =!= Main then (			    -- protect it later
	  protect p.Dictionary;
	  );
     if p.name =!= "Main" then globalDictionaries = prepend(d,p#"previous dictionaries");
     hook := p#"close hook";
     fileExitHooks = select(fileExitHooks, f -> f =!= hook);
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
package Dictionary := d -> (
     if currentPackage =!= null and currentPackage.Dictionary === d then currentPackage else
     scan(values PackageDictionary, pkg -> if (value pkg).Dictionary === d then break (value pkg))
     )
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
     p := select(value \ values PackageDictionary, P -> P#"documentation"#?fkey); -- speed this up by implementing break for scanValues
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

use Package := pkg -> (
     if not member(pkg.Dictionary,globalDictionaries) then globalDictionaries = prepend(pkg.Dictionary,globalDictionaries);
     )

needsPackage = method()
needsPackage String := s -> (
     if PackageDictionary#?s then use value PackageDictionary#s
     else load (s | ".m2")
     )

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
