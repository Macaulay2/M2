--		Copyright 1993-2003 by Daniel R. Grayson

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

packagesLoaded = {}
writableGlobals.packagesLoaded = true

Dictionary = new Type of MutableHashTable		    -- temporary fiction

Package = new Type of MutableHashTable
newPackage = method( Options => { Using => {} } )
newPackage(String,String) := opts -> (pkgname,vers) -> (
     if currentPackage =!= null then error("the package ",currentPackage.name," is already open");
     erase getGlobalSymbol pkgname;
     sym := getGlobalSymbol pkgname;
     sym <- currentPackage = new Package from {
	  global name => pkgname,
	  global version => vers,
	  global symbol => sym,
     	  "dictionary" => currentDictionary = new Dictionary, -- ! make dictionaries first class objects
	  "test inputs" => new MutableHashTable,
	  "raw documentation" => new MutableHashTable,
	  "example inputs" => new MutableHashTable,
	  "example outputs" => new MutableHashTable,
	  "edited documentation" => new MutableHashTable,
	  "html documentation" => new MutableHashTable,
	  "options" => opts,
	  "initial global symbols" => values symbolTable(),
	  "file directory" => currentFileDirectory,
	  "file name" => currentFileName
	  };
     protect sym;
     currentPackage
     )

addEndFunction(
     () -> if currentPackage =!= null then error("the package ",currentPackage.name," is still open")
     )

endPackage = () -> (
     p := currentPackage;
     if p === null then error "no package currently open";
     if p#"file name" =!= currentFileName then error "'endPackage' after 'newPackage', but in different file";
     p#"dictionary" = new Dictionary from (
	  apply(keys (set values symbolTable() - set p#"initial global symbols"), 
	       s -> toString s => s));
     remove(p,"initial global symbols");
     packagesLoaded = append(packagesLoaded,p);
     currentPackage = null;
     currentDictionary = null;
     p )
