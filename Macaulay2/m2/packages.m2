--		Copyright 1993-2003 by Daniel R. Grayson

currentPackage = null
writableGlobals.currentPackage = true

packagesLoaded = {}
writableGlobals.packagesLoaded = true

Package = new Type of MutableHashTable
newPackage = method()
newPackage(String,String) := (pkgname,vers) -> (
     if currentPackage =!= null then error("the package ",currentPackage.name," is already open");
     erase getGlobalSymbol pkgname;
     sym := getGlobalSymbol pkgname;
     sym <- currentPackage = new Package from {
	  global name => pkgname,
	  global version => vers,
	  global symbol => sym,
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

makeHTMLPages = method()
makeHTMLPages Package := p -> (
     currentHTMLDirectory = concatenate(
	  p.name,"-",p.version,"/share/doc/Macaulay2/packages/StateTables/html/"
	  );
     keys := unique join(p#"symbols",p#"docs");
     ret := makeHtmlNode \ keys;
     "created " | toString( #ret ) | " html pages: " | stack keys
     )
