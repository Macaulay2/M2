--		Copyright 1993-2003 by Daniel R. Grayson

PREFIX := ""
addStartFunction(
     () -> (
          PREFIX = getenv "M2PREFIX";			    -- usually /usr or /sw or /usr/local
	  if PREFIX =!= "" then path = append( path, 
	       minimizeFilename ( PREFIX | "/share/Macaulay2/packages/" )
	       )))

addStartFunction( 
     () -> (
	  home := getenv "M2HOME";
	  path = append(path, home | "/packages/");
	  ))

isAbsolute := url -> (					    -- drg: replace with regexp after merging the branch
     "#" == substring(url,0,1) or
     "http://" == substring(url,0,7) or
     "ftp://" == substring(url,0,6) or
     "mailto:" == substring(url,0,7)
     )

buildDirectory = "tmp/"					    -- buildDirectory is one possible PREFIX
     	       	    	      	   	     	       	    -- set, for example, in packages.m2

buildPackage = ""					    -- name of the package currently being built


rel := url -> (
     if isAbsolute url 
     then url
     else relativizeFilename(htmlDirectory, url)
     )

htmlFilename = (nodename) -> (	-- returns the path from the PREFIX to the file
     basename := toFilename nodename | ".html";
     if buildPackage === "" 
     then "share/doc/Macaulay2/" | version#"VERSION" | "/html/"|basename
     else (
     	  fn0 := "share/doc/Macaulay2/currentVersion/html/"|basename;
	  if PREFIX =!= "" and fileExists (PREFIX|"/"|fn0)
     	  then fn0
     	  else concatenate("share/doc/Macaulay2/packages/", buildPackage, "/html/", basename)
	  )
     )

html IMG  := x -> "<IMG src=\"" | rel first x | "\">"
text IMG  := x -> ""
tex  IMG  := x -> ""

html HREF := x -> (
     "<A HREF=\"" 					    -- "
     | rel first x 
     | "\">" 						    -- "
     | html last x 
     | "</A>"
     )
text HREF := x -> "\"" | last x | "\""
tex HREF := x -> (
     concatenate(
	  ///\special{html:<A href="///, 		    -- "
	       texLiteral rel first x,
	       ///">}///,				    -- "
	  tex last x,
	  ///\special{html:</A>}///
	  )
     )

html TO := x -> (
     key := x#0;
     formattedKey := formatDocumentTag key;
     concatenate ( 
     	  ///<A HREF="///,				    -- "
	  rel htmlFilename formattedKey,
	  ///">///, 					    -- "
     	  htmlExtraLiteral formattedKey,
     	  "</A>",
     	  drop(toList x,1) 
     	  )
     )
html BASE := x -> concatenate("<BASE HREF=\"",rel first x,"\">")

-----------------------------------------------------------------------------

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

makeHTMLPages = method(Options => { TemporaryDirectory => "tmp/" })
makeHTMLPages(Package) := o -> pkg -> (
     buildPackage = pkg.name;
     buildDirectory = o.TemporaryDirectory | pkg.name | "-" | pkg.version;
     htmlDirectory = "share/doc/Macaulay2/packages/" | pkg.name | "/html/";
     keys := unique join(pkg#"symbols",pkg#"docs");
     ret := makeHtmlNode \ keys;
     "pages " | stack keys | " in " | htmlDirectory
     )
