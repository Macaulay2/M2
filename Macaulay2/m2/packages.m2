--		Copyright 1993-2003 by Daniel R. Grayson

addStartFunction(
     () -> (
	  path = prepend("./",path);
	  if sourceHomeDirectory =!= null then path = prepend(sourceHomeDirectory|"packages/",path);
	  if prefixDirectory =!= null then path = prepend(prefixDirectory|LAYOUT#"packages",path);
	  ))

currentPackage = null

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
     r := reverseDictionary d;
     if r =!= null then return toString r;
     if length d == 0 then "Dictionary{}" else "Dictionary{..." | toString length d | "...}"
     )

globalAssignFunction = (X,x) -> (
     reverseDictionaryRecord (X,x);
     if not x#?(symbol name) then (			    -- phase this out
	  x.Symbol = X;
	  x.name = toString X;
	  );
     use x;
     )

globalReleaseFunction = (X,x) -> (
     reverseDictionaryRemove (X,x);
     if x.?Symbol and X === x.Symbol
     then (
	  remove(x,symbol name);
	  remove(x,symbol symbol);
	  )
     )

Type.GlobalAssignHook = globalAssignFunction
Type.GlobalReleaseHook = globalReleaseFunction
ScriptedFunctor.GlobalAssignHook = globalAssignFunction
ScriptedFunctor.GlobalReleaseHook = globalReleaseFunction
installMethod(GlobalAssignHook,Package,globalAssignFunction)
installMethod(GlobalReleaseHook,Package,globalReleaseFunction)

removePackage = method()
removePackage Package := p -> (
     hide p.Dictionary;
     packages = select(packages, q -> q =!= p);
     stderr << "--previous definitions removed for package " << p << endl;
     )
removePackage String := s -> scan(packages, p -> if p.name == s then removePackage p)

globalDictionaries = append(globalDictionaries,OutputDictionary)

newPackage = method( Options => { Using => {}, Version => "0.0", WritableSymbols => {}, DebuggingMode => false } )
newPackage(Package) := opts -> p -> (
     hide p.Dictionary;		    -- hide the old dictionary
     newPackage(p.name,opts))
newPackage(Symbol) := opts -> p -> newPackage(toString p,opts)
newPackage(String) := opts -> (title) -> (
     if not match("^[a-zA-Z0-9]+$",title) then error( "package title not alphanumeric: ",title);
     sym := value ("symbol " | title);
     removePackage title;
     newdict := (
	  if title === "Main" then first globalDictionaries
	  else (
	       d := new Dictionary;
	       globalDictionaries = prepend(d,globalDictionaries);
	       d));
     p := global currentPackage <- new Package from {
          symbol name => title,
	  symbol Symbol => sym,
     	  symbol Dictionary => newdict,
	  symbol Version => opts.Version,
	  symbol WritableSymbols => opts.WritableSymbols,
	  "previous package" => currentPackage,
	  "old debugging mode" => debuggingMode,
	  "test inputs" => new MutableHashTable,
	  "reverse dictionary" => new MutableHashTable,
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
     debuggingMode = opts.DebuggingMode;
     globalAssignFunction(sym,p);
     sym <- p;
     packages = prepend(p,packages);
     p)

addStartFunction( () -> if prefixDirectory =!= null then Main#"package prefix" = prefixDirectory )

newPackage("Main",
     DebuggingMode => debuggingMode,
     Version => version#"VERSION",
     WritableSymbols => {
	  symbol oooo, symbol ooo, symbol oo, symbol path, symbol currentDirectory, symbol fullBacktrace, symbol backtrace,
	  symbol DocDatabase, symbol currentFileName, symbol compactMatrixForm, symbol gbTrace, symbol encapDirectory, symbol User,
	  symbol buildHomeDirectory, symbol sourceHomeDirectory, symbol prefixDirectory, symbol currentPrompts, symbol currentPackage,
	  symbol packages, symbol currentDictionary, symbol notify, symbol loadDepth, symbol printingPrecision,
	  symbol errorDepth, symbol recursionLimit, symbol globalDictionaries, symbol debuggingMode, 
	  symbol stopIfError, symbol debugLevel, symbol lineNumber, symbol debuggerHook, symbol printWidth
	  })

Command.GlobalAssignHook = 
Function.GlobalAssignHook = 
Manipulator.GlobalAssignHook = (X,x) -> reverseDictionaryRecord(X,x)

Function.GlobalReleaseHook = 
Manipulator.GlobalReleaseHook = (X,x) -> reverseDictionaryRemove(X,x)

Command.GlobalReleaseHook = (X,x) -> (
     stderr << "warning: " << toString X << " redefined" << endl;
     reverseDictionaryRemove(X,x);
     )

closePackage = p -> (
     if p =!= currentPackage then error ("package not open");
     ws := set p.WritableSymbols;
     scan(values p.Dictionary,
	  s -> (
	       if not ws#?s then protect s;
	       if value s =!= s and not p#"reverse dictionary"#?(value s) then p#"reverse dictionary"#(value s) = s;
	       ));
     if p =!= Main then (			    -- protect it later, after package User is open
	  protect p.Dictionary;
	  );
     if first globalDictionaries =!= p.Dictionary then error ("another dictionary is open");
     currentPackage = p#"previous package"; remove(p,"previous package");
     debuggingMode = p#"old debugging mode"; remove(p,"old debugging mode");
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
dictionary Thing := x -> (
     s := reverseDictionary x;
     if s === null then null else dictionary s
     )

package = method ()
package Dictionary := d -> scan(packages, pkg -> if pkg.Dictionary === d then break pkg)
package Symbol := s -> (
     d := dictionary s;
     if d === null then return null;
     package d)
package HashTable := package Function := x -> (
     X := reverseDictionary x;
     if X =!= null then package X)
package Thing := x -> (
     d := dictionary x;
     if d =!= null then package d)

warned := new MutableHashTable

package TO := x -> (
     key := normalizeDocumentTag x#0;
     pkg := packageTag key;
     fkey := formatDocumentTag key;
     pkgs := select(packages, P -> P =!= User);
     p := select(pkgs, P -> P#"documentation"#?fkey);
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
