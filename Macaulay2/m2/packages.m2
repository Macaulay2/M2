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

toString Dictionary := d -> if length d == 0 then "Dictionary{}" else "Dictionary{..." | toString length d | "...}"

installMethod(GlobalAssignHook,Package,globalAssignFunction)
installMethod(GlobalReleaseHook,Package,globalReleaseFunction)

hide := d -> (
     globalDictionaries = select(globalDictionaries, x -> x =!= d);
     )

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
	  if title === "Main"
	  then first globalDictionaries
	  else if title === "Output" then OutputDictionary
	  else first (globalDictionaries = prepend(new Dictionary,globalDictionaries))
	  );
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
	  "example inputs" => new MutableHashTable,
	  "example outputs" => new MutableHashTable,
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

newPackage("Main",
     DebuggingMode => debuggingMode,
     Version => version#"VERSION",
     WritableSymbols => {
	  symbol oooo, symbol ooo, symbol oo, symbol path, symbol phase, symbol currentDirectory, symbol fullBacktrace, symbol backtrace,
	  symbol DocDatabase, symbol currentFileName, symbol compactMatrixForm, symbol gbTrace, symbol encapDirectory, symbol User,
	  symbol buildHomeDirectory, symbol sourceHomeDirectory, symbol prefixDirectory, symbol currentPrompts, symbol currentPackage,
	  symbol packages, symbol currentDictionary, symbol notify, symbol loadDepth, symbol printingPrecision,
	  symbol errorDepth, symbol recursionLimit, symbol globalDictionaries, symbol Output, symbol debuggingMode, 
	  symbol stopIfError, symbol debugLevel, symbol lineNumber, symbol debuggerHook--, symbol printWidth
	  })
reverseDictionary = x -> scan(packages, pkg -> (
	  d := pkg#"reverse dictionary";
	  if d#?x then break d#x))
reverseDictionaryRecord = (X,x) -> if X =!= x then (
     s := toString X;
     scan(packages, 
     	  pkg -> if pkg.Dictionary#?s and pkg.Dictionary#s === X then (	-- too bad a symbol doesn't know what dictionary it's in...
	       pkg#"reverse dictionary"#x = X; 
	       break)))
reverseDictionaryRemove = (X,x) -> (
     s := toString X;
     scan(packages, 
     	  pkg -> if pkg.Dictionary#?s and pkg#"reverse dictionary"#?x and pkg#"reverse dictionary"#x === X then (
	       remove(pkg#"reverse dictionary",x); 
	       break)))

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
	       if value s =!= s then p#"reverse dictionary"#(value s) = s;
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

dictionary := s -> (
     n := toString s;
     r := select(globalDictionaries, d -> d#?n and d#n === s);
     if #r === 0 then null else first r
     )

package = method ()
package Symbol := s -> (
     d := dictionary s;
     if d === null then return(null);
     r := select(packages,p -> p.Dictionary === d);
     if #r > 0 then first r)
package Thing := x -> (					    -- try hard
     X := reverseDictionary x;
     if X =!= null then package X
     )


-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2"
-- End:
