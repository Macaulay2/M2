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

Package = new Type of MutableHashTable
newPackage = method( Options => { Using => {} } )
newPackage(String,String) := options -> (pkgname,vers) -> (
     if currentPackage =!= null then error("the package ",currentPackage.name," is already open");
     erase getGlobalSymbol pkgname;
     sym := getGlobalSymbol pkgname;
     sym <- currentPackage = new Package from {
	  global name => pkgname,
	  global version => vers,
	  global symbol => sym,
	  Using => options.Using,
	  "initial global symbols" => keys symbolTable(),
	  "initial docs" => keys Documentation,
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
     p#"symbols" = sort keys (set keys symbolTable() - set p#"initial global symbols");
     remove(p,"initial global symbols");
     p#"docs" = sort keys (set keys Documentation - set p#"initial docs");
     remove(p,"initial docs");
     currentPackage = null;
     packagesLoaded = append(packagesLoaded,p);
     p
     )
