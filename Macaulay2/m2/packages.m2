--		Copyright 1993-2003 by Daniel R. Grayson

addStartFunction( () -> path = join({"./", sourceHomeDirectory|"packages/"},path) )

currentPackage = null

packages = new VerticalList from {}

Package = new Type of MutableHashTable
Package.synonym = "package"

net Dictionary := toString Dictionary := toExternalString Dictionary := d -> if Symbols#?d then toString Symbols#d else "--dictionary--"

installMethod(GlobalAssignHook,Package,globalAssignFunction)
installMethod(GlobalReleaseHook,Package,globalReleaseFunction)

M2title := "Macaulay2"

record := (sym,val) -> (
     sym <- val;
     Symbols#val = sym;
     )

newPackage = method( Options => { Using => {} } )
newPackage(String,String) := opts -> (title,vers) -> (
     if not match("^[a-zA-Z0-9]+$",title) then error( "package title not alphanumeric: ",title);
     sym := value ("symbol " | title);
     p := global currentPackage <- new Package from {
          symbol name => title,
	  symbol Symbol => sym,
	  "outerPackage" => currentPackage,
     	  "dictionary" => if title === M2title then first dictionaries() else first dictionaries prepend(newDictionary(),dictionaries()),
	  "package title" => title,
	  "package version" => vers,
	  "test inputs" => new MutableHashTable,
	  "raw documentation" => new MutableHashTable,
	  "example inputs" => new MutableHashTable,
	  "example outputs" => new MutableHashTable,
	  "edited documentation" => new MutableHashTable,
	  "html documentation" => new MutableHashTable,
	  "options" => opts,
	  "file directory" => currentFileDirectory
	  };
     record(value ("symbol " | title | "Dictionary"), p#"dictionary");
     globalAssignFunction(sym,p);
     sym <- p;
     packages = append(packages,p);
     p)

Package _ Symbol := (p,s) -> value p#"dictionary"#(toString s)

newPackage(M2title,version#"VERSION")

end Package := p -> (
     if p =!= currentPackage then error ("package not open");
     if not p.?name then p.name = p#"package title";
     if p =!= Macaulay2 then (
	  d := dictionaries();
	  assert( d#0 === p#"dictionary" );
	  dictionaries append(drop(d,1),d);		    -- move this dictionary to the end
	  );
     p)
