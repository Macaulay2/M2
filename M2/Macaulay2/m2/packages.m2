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

newPackage = method( Options => { Using => {}, Version => "0.0" } )
newPackage(Package) := opts -> p -> newPackage(p.name,opts)
newPackage(Symbol) := opts -> p -> newPackage(toString p,opts)
newPackage(String) := opts -> (title) -> (
     if not match("^[a-zA-Z0-9]+$",title) then error( "package title not alphanumeric: ",title);
     sym := value ("symbol " | title);
     newdict := if title === M2title then first dictionaries() else first dictionaries prepend(newDictionary(),dictionaries());
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

endPackage = p -> (
     if p =!= currentPackage then error ("package not open");
     if p =!= Macaulay2 then (
	  if first dictionaries() =!= p#"dictionary" then error ("another dictionary is open");
	  dictionaries rotate(dictionaries(),1);
	  );
     currentPackage = null;
     p)

pushDictionary = () -> (
     d := newDictionary();
     dictionaries prepend(d,dictionaries());
     d)

popDictionary = d -> (
     if d =!= first dictionaries() then error "expected argument to be current dictionary";
     dictionaries drop(dictionaries(),1);
     d)

     