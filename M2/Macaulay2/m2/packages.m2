--		Copyright 1993-2003 by Daniel R. Grayson

addStartFunction( () -> path = join({"./", sourceHomeDirectory|"packages/"},path) )

currentPackage = null

packages = {}

Package = new Type of MutableHashTable
Package.synonym = "package"

net Dictionary := toString Dictionary := toExternalString Dictionary := d -> if Symbols#?d then toString Symbols#d else "--dictionary--"

installMethod(GlobalAssignHook,Package,globalAssignFunction)
installMethod(GlobalReleaseHook,Package,globalReleaseFunction)

M2title := "Macaulay2"

record := (sym,val) -> (
     if Symbols#?(value sym) then remove(Symbols,value sym);
     sym <- val;
     Symbols#val = sym;
     )

hide := d -> (
     globalDictionaries select(globalDictionaries(), x -> x =!= d);
     )

removePackage = method()
removePackage Package := p -> (
     hide p#"dictionary";
     packages = select(packages, q -> q =!= p);
     stderr << "--previous definitions removed for package " << p << endl;
     )
removePackage String := s -> scan(packages, p -> if p.name == s then removePackage p)

newPackage = method( Options => { Using => {}, Version => "0.0" } )
newPackage(Package) := opts -> p -> (
     hide p#"dictionary";		    -- hide the old dictionary
     newPackage(p.name,opts))
newPackage(Symbol) := opts -> p -> newPackage(toString p,opts)
newPackage(String) := opts -> (title) -> (
     if not match("^[a-zA-Z0-9]+$",title) then error( "package title not alphanumeric: ",title);
     sym := value ("symbol " | title);
     removePackage title;
     newdict := if title === M2title then first globalDictionaries() else first globalDictionaries prepend(newDictionary(),globalDictionaries());
     p := global currentPackage <- new Package from {
          symbol name => title,
	  symbol Symbol => sym,
	  "outerPackage" => currentPackage,
     	  "dictionary" => newdict,
	  "version" => opts.Version,
	  "test inputs" => new MutableHashTable,
	  "raw documentation" => new MutableHashTable,
	  "example inputs" => new MutableHashTable,
	  "example outputs" => new MutableHashTable,
	  "edited documentation" => new MutableHashTable,
	  "html documentation" => new MutableHashTable,
	  "file directory" => currentFileDirectory
	  };
     record(value ("symbol " | title | "Dictionary"), p#"dictionary");
     globalAssignFunction(sym,p);
     sym <- p;
     packages = append(packages,p);
     p)

newPackage(M2title,Version => version#"VERSION")

closePackage = p -> (
     if p =!= currentPackage then error ("package not open");
     if p =!= Macaulay2 then (			    -- protect it later, after package User is open
	  protect p#"dictionary";
	  );
     if first globalDictionaries() =!= p#"dictionary" then error ("another dictionary is open");
     currentPackage = p#"outerPackage";
     remove(p,"outerPackage");
     stderr << "--package " << p << " installed" << endl;
     p)

pushDictionary = () -> (
     d := newDictionary();
     globalDictionaries prepend(d,globalDictionaries());
     d)

popDictionary = d -> (
     if d =!= first globalDictionaries() then error "expected argument to be current dictionary";
     globalDictionaries drop(globalDictionaries(),1);
     d)

dictionary := s -> (
     n := toString s;
     r := select(globalDictionaries(), d -> d#?n and d#n === s);
     if #r === 0 then null else first r
     )

package = method ()
package Symbol := s -> (
     d := dictionary s;
     if d === null then return(null);
     r := select(packages,p -> p#"dictionary" === d);
     if #r > 0 then first r)
package Thing := x -> if Symbols#?x then package Symbols#x
