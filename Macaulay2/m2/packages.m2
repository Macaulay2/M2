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

hide := d -> (
     globalDictionaries = select(globalDictionaries, x -> x =!= d);
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
     hide p.Dictionary;
     stderr << "--previous definitions removed for package " << p << endl;
     )
removePackage String := n -> if PackageDictionary#?n and class value PackageDictionary#n === Package then removePackage value PackageDictionary#n

currentPackageS := getGlobalSymbol(PackageDictionary,"currentPackage")

newPackage = method( 
     Options => { 
	  Using => {}, 
	  Version => "0.0", 
	  DebuggingMode => false
	  }
     )
newPackage(Package) := opts -> p -> (
     hide p.Dictionary;		    -- hide the old dictionary
     newPackage(p#"title",opts))
newPackage(String) := opts -> (title) -> (
     if not match("^[a-zA-Z0-9]+$",title) then error( "package title not alphanumeric: ",title);
     if class opts.Using =!= List or not all(opts.Using, pkg -> class pkg === Package) then error "expected 'Using' option to be a list of loaded packages";
     stderr << "--package " << title << " loading" << endl;
     removePackage title;
     saveD := globalDictionaries;
     hook := haderror -> if haderror then globalDictionaries = saveD else closePackage title;
     newdict := (
	  if title === "Main" then first globalDictionaries
	  else (
	       d := new Dictionary;
	       fileExitHooks = prepend(hook, fileExitHooks);
	       globalDictionaries = join({d,Main.Dictionary,PackageDictionary},apply(opts.Using,pkg->pkg.Dictionary));
	       d));
     p := currentPackageS <- new Package from {
          "title" => title,
     	  "private dictionary" => newdict,			    -- how do I make a synonym in a *different* dictionary (make a synonym dictionary closure, with the same frame?)
	  symbol Options => opts,
     	  symbol Dictionary => if title === "Main" then newdict else new Dictionary,
     	  "close hook" => hook,
	  "previous package" => currentPackage,
	  "previous dictionaries" => saveD,
	  "mutable symbols" => {},
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
     PrintNames#(p.Dictionary) = title | ".Dictionary";
     debuggingMode = opts.DebuggingMode;
     sym := getGlobalSymbol(PackageDictionary,title);
     ReverseDictionary#p = sym;
     sym <- p;
     p)

export = method(SingleArgumentDispatch => true)
export Symbol := x -> export singleton x
export Sequence := v -> export toList v
export List := v -> (
     if not all(v, x -> class x === Symbol) then error "expected a list of symbols";
     if currentPackage === null then error "no current package";
     currentPackage#"exported symbols" = join(currentPackage#"exported symbols",v);
     if currentPackage =!= Main then scan(v, s -> (
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

addStartFunction( () -> if prefixDirectory =!= null then Main#"package prefix" = prefixDirectory )

newPackage("Main", DebuggingMode => debuggingMode, Version => version#"VERSION" )

exportMutable {
	  symbol oooo, symbol ooo, symbol oo, symbol path, symbol currentDirectory, symbol fullBacktrace, symbol backtrace,
	  symbol DocDatabase, symbol currentFileName, symbol compactMatrixForm, symbol gbTrace, symbol encapDirectory, 
	  symbol buildHomeDirectory, symbol sourceHomeDirectory, symbol prefixDirectory, symbol currentPrompts, symbol currentPackage,
	  symbol notify, symbol loadDepth, symbol printingPrecision, symbol fileExitHooks, symbol doExamples,
	  symbol errorDepth, symbol recursionLimit, symbol globalDictionaries, symbol debuggingMode, 
	  symbol stopIfError, symbol debugLevel, symbol lineNumber, symbol debuggerHook, symbol printWidth
	  }

findSynonyms = method()
findSynonyms Symbol := x -> (
     r := {};
     scan(globalDictionaries, d -> scan(pairs d, (nam,sym) -> if x === sym then r = append(r,nam)));
     sort r)

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
	       if #w > 0 then stderr << "--   synonym(s) for " << nam << ": " << concatenate between(", ",w) << endl
	       else if member(UserDictionary,globalDictionaries) then for i from 0 do (
		    newsyn := nam | "$" | toString i;
		    if not isGlobalSymbol newsyn then (
			 UserDictionary#newsyn = sym;
			 stderr << "--   new synonym provided for '" << nam << "': " << newsyn << endl;
			 break)))))

closePackage = method()
closePackage String := title -> (
     if currentPackage === null or title =!= currentPackage#"title" then error ("package not current: ",title);
     pkg := currentPackage;
     scan(pkg#"mutable symbols", s -> if value s === s then stderr << "warning: unused writable symbol '" << s << "'" << endl);
     ws := set pkg#"mutable symbols";
     dict := pkg.Dictionary;
     scan(values dict,
	  s -> (
	       if not ws#?s then protect s;
	       if value s =!= s and not ReverseDictionary#?(value s) then ReverseDictionary#(value s) = s;
	       ));
     exportDict := pkg.Dictionary;
     if pkg =!= Main then (			    -- protect it later
	  protect dict;					    -- maybe don't do this, as it will be private
	  protect exportDict;
	  );
     if pkg#"title" =!= "Main" then (
	  globalDictionaries = prepend(exportDict,pkg#"previous dictionaries");
     	  checkShadow();
	  );
     hook := pkg#"close hook";
     fileExitHooks = select(fileExitHooks, f -> f =!= hook);
     currentPackage = pkg#"previous package";
     debuggingMode = pkg#"old debugging mode";
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
     if currentPackage =!= null and (currentPackage.Dictionary === d or currentPackage#"private dictionary" === d)
     then currentPackage 
     else scan(values PackageDictionary, pkg -> if (value pkg).Dictionary === d then break (value pkg))
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
	  error("documentation for ",fkey," occurs in multiple packages: ", concatenate between(", ",apply(p,P -> P#"title")));
	  )
     else (
	  if not warned#?key then stderr << "warning: documentation for \"" << fkey << "\" not found, assuming it will be found in package " << pkg << " eventually" << endl;
	  warned#key = true;
     	  pkg))

Package.GlobalAssignHook = (X,x) -> (
     if not ReverseDictionary#?x then ReverseDictionary#x = X;
     -- use x;
     )
Package.GlobalReleaseHook = globalReleaseFunction

use Package := pkg -> (
     if not member(pkg.Dictionary,globalDictionaries) then globalDictionaries = prepend(pkg.Dictionary,globalDictionaries);
     )


needsPackage = method()
needsPackage String := s -> (
     if PackageDictionary#?s then use value PackageDictionary#s
     else load (s | ".m2")
     )

beginDocumentation = () -> (
     stderr << "beginDocumentation: currentFileDirectory = " << currentFileDirectory << endl;
     )

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
