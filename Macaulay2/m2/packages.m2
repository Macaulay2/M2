--		Copyright 1993-2003 by Daniel R. Grayson

addStartFunction( () -> path = join({"./", sourceHomeDirectory|"packages/"},path) )

currentPackage = null

packages = new VerticalList from {}

Package = new Type of MutableHashTable
Package.synonym = "package"

installMethod(GlobalAssignHook,Package,globalAssignFunction)
installMethod(GlobalReleaseHook,Package,globalReleaseFunction)

net Package := p -> p#"package title" | " version " | p#"package version";

M2title := "Macaulay 2"

newPackage = method( Options => { Using => {} } )
newPackage(String,String) := opts -> (title,vers) -> (
     global currentPackage <- new Package from {
	  "outerPackage" => currentPackage,
     	  "dictionary" => if title === M2title then globalDictionary() else pushDictionary(),
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
	  }
     )

end Package := p -> (
     if p =!= currentPackage then error ("package not open");
     packages = append(packages,p);
     if not p.?name then p.name = p#"package title";
     p )

Macaulay2 = newPackage(M2title,version#"VERSION")
