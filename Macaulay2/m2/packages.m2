--		Copyright 1993-2003 by Daniel R. Grayson

saveValues = varlist -> (
     valuelist := apply(varlist, x -> value x);
     () -> apply(varlist, valuelist, (x,n) -> x <- n))

addStartFunction( 
     () -> (
	  home := getenv "M2HOME";
	  path = append(path, home | "/packages/");
	  ))

load "layout.m2"					    -- defines LAYOUT
PREFIX := ""
addStartFunction(
     () -> (
          PREFIX = getenv "M2PREFIX";			    -- usually /usr or /sw or /usr/local
	  if PREFIX =!= "" then path = append( path, 
	       minimizeFilename ( PREFIX | "/" | LAYOUT#"packages" )
	       )))

currentPackage = null
writableGlobals.currentPackage = true

packages = {}
writableGlobals.packages = true

Dictionary = new Type of MutableHashTable		    -- temporary fiction

Package = new Type of MutableHashTable
Package.synonym = "package"

installMethod(GlobalAssignHook,Package,globalAssignFunction)
installMethod(GlobalReleaseHook,Package,globalReleaseFunction)

writableGlobals.currentDictionary = true

net Package := p -> p#"package title" | " version " | p#"package version";

newPackage = method( Options => { Using => {} } )
newPackage(String,String) := opts -> (title,vers) -> (
     doctable := new MutableHashTable;
     documentationPath = append(documentationPath,doctable);
     restore := saveValues { global currentPackage, global currentDictionary };
     global currentPackage <- new Package from {
	  "restore" => restore,
	  "package title" => title,
	  "package version" => vers,
     	  "dictionary" => currentDictionary = new Dictionary, -- ! make dictionaries first class objects
	  "test inputs" => new MutableHashTable,
	  "raw documentation" => doctable,
	  "example inputs" => new MutableHashTable,
	  "example outputs" => new MutableHashTable,
	  "edited documentation" => new MutableHashTable,
	  "html documentation" => new MutableHashTable,
	  "options" => opts,
	  "initial global symbols" => new MutableList from values symbolTable(),
	  "file directory" => currentFileDirectory
	  }
     )

end Package := p -> (
     if p =!= currentPackage then error ("package not open");
     p#"dictionary" = new Dictionary from (
	  apply(keys (set values symbolTable() - set p#"initial global symbols"), 
	       s -> toString s => s));
     remove(p,"initial global symbols");
     packages = append(packages,p);
     p#"restore"();
     remove(p,"restore");
     p )

Macaulay2 = newPackage("Macaulay 2",version#"VERSION")
Macaulay2#"initial global symbols" = {}
