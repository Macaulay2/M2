-- -*- fill-column: 107 -*-
--		Copyright 1993-2002 by Daniel R. Grayson

-- maybe we should rename this file to "packages2.m2" after the merge.

-- we've turned off checking for existence of files...

local prefix, local topNodeButton
local haderror, local nullButton, local masterIndexButton
local NEXT, local PREV, local UP
local nextButton, local prevButton, local upButton
local lastKey, local thisKey
local linkFollowedTable, local masterIndex
local docdatabase

buildPackage := null					    -- name of the package currently being built
topNodeName := null					    -- name of the top node of this package
topFileName := "index.html"				    -- top node file name
masterFileName := "master.html";			    -- master index file of all topics
finalDirectory := "/usr/local/"
buildDirectory := "/tmp/"				    -- the root of the relative paths:
htmlDirectory := ""					    -- relative path to the html directory

isAbsolute := url -> (				       -- drg: replace with regexp after merging the branch
     "#" == substring(url,0,1) or
     "http://" == substring(url,0,7) or
     "ftp://" == substring(url,0,6) or
     "mailto:" == substring(url,0,7)
     )


rel := url -> (
     if isAbsolute url 
     then url
     else relativizeFilename(htmlDirectory, url)
     )

htmlFilename = (nodename) -> (	-- returns the relative path from the PREFIX to the file
     if nodename === topNodeName then (
	  if buildPackage === null
	  then LAYOUT#"htmldoc" | topFileName
	  else LAYOUT#"packagehtml" buildPackage | topFileName
	  )
     else (
	  basename := toFilename nodename | ".html";
	  if buildPackage === null
	  or finalDirectory =!= "" 
	  and fileExists minimizeFilename(finalDirectory | "/" | LAYOUT#"htmldoc" | basename)
	  then LAYOUT#"htmldoc" | basename
	  else LAYOUT#"packagehtml" buildPackage | basename
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



-- produce html form of documentation, for Macaulay 2 and for packages

documentationMemo := memoize documentation		    -- for speed

BUTTON := (s,alt) -> (
     -- if not fileExists s then error ("file ", s, " doesn't exist");
     s = relativizeFilename(htmlDirectory,s);
     if alt === null
     then LITERAL concatenate("<IMG src=\"",s,"\" border=0 align=center>")
     else LITERAL concatenate("<IMG src=\"",s,"\" border=0 align=center alt=\"[", alt, "]\">")
     )

upAncestors := key -> reverse (
     n := 0;
     while UP#?key and n < 20 list (n = n+1; key = UP#key)
     )

next := key -> if NEXT#?key then HREF { htmlFilename NEXT#key, nextButton } else nullButton
prev := key -> if PREV#?key then HREF { htmlFilename PREV#key, prevButton } else nullButton
up   := key -> if   UP#?key then HREF { htmlFilename   UP#key,   upButton } else nullButton

scope := method(SingleArgumentDispatch => true)
scope2 := method(SingleArgumentDispatch => true)
scope1 := method(SingleArgumentDispatch => true)

follow := key -> (
     fkey := formatDocumentTag key;
     -- stderr << "key     = " << key << endl;
     -- stderr << "fkey    = " << fkey << endl; 
     -- stderr << "prefix  = " << prefix << endl; 
     -- stderr << "linkFollowedTable#?fkey  = " << linkFollowedTable#?fkey << endl; 
     if not linkFollowedTable#?fkey then (
	  fn := htmlFilename fkey;
     	  -- stderr << "fn      = " << fn << endl;
	  if true or prefix == substring(fn,0,#prefix) then (	    -- don't stray outside this package???
	       linkFollowedTable#fkey = true;
	       if class key =!= Option and class key =!= Sequence then masterIndex#(fkey,key) = true;
	       saveThisKey := thisKey;
	       saveLastKey := lastKey;
	       thisKey = fkey;
	       lastKey = null;
	       scope documentationMemo key;
	       thisKey = saveThisKey;
	       lastKey = saveLastKey;
	       )
	  else (
	       linkFollowedTable#fkey = false;
	       )
	  )
     )

-- scanning at top level
scope Thing := x -> null
scope Sequence := scope BasicList := x -> scan(x,scope)
scope SHIELD := x -> scan(x,scope1)
scope MENU := x -> scan(x,scope2)
scope TO := scope TOH := x -> follow x#0

-- scanning inside a SHIELD
scope1 Thing := x -> null
scope1 Sequence := scope1 BasicList := x -> scan(x,scope1)
scope1 TO := scope1 TOH := x -> follow x#0

-- scanning inside a MENU not inside a SHIELD
scope2 Thing := scope
scope2 SEQ := x -> scan(x,scope2)
scope2 SHIELD := x -> scan(x,scope1)
scope2 TO := scope2 TOH := x -> (
     -- here we construct the ordered tree needed for presentation in book format,
     -- with UP, NEXT, and PREVIOUS pointers.
     key := formatDocumentTag x#0;
     if UP#?key 
     then (
	  stderr << "error: links to '" << key << "' from two nodes: '"
	  << UP#key << "' and '" << thisKey << "'" << endl
	  )
     else if key == thisKey then stderr << "error: node " << key << " links to itself" << endl
     else (
	  UP#key = thisKey;
	  if lastKey =!= null then (
	       PREV#key = lastKey;
	       NEXT#lastKey = key;
	       );
	  lastKey = key;
	  );
     follow x#0;
     )

buttonBar := (key) -> CENTER {
     next key,
     prev key, 
     up key,
     if key =!= topNodeName then topNodeButton else nullButton,
     masterIndexButton,
     LITERAL concatenate (///
     <form action="///,					    -- "
     if getenv "SEARCHENGINE" === "" 
     then "http://rhenium.math.uiuc.edu:7003/" else getenv "SEARCHENGINE",
     ///">
	search:
	<input type="text"   name="words">
	<input type="hidden" name="method"   value="boolean">
	<input type="hidden" name="format"   value="builtin-short">
	<input type="hidden" name="sort"     value="score">
	<input type="hidden" name="config"   value="htdig-M2">
     </form>
     ///)						    -- "
     }

pass1 := () -> (
     << "pass 1, finding undocumented symbols" << endl;
     scan(undocumentedSymbols(), 
	  x -> (
	       haderror = true;
	       follow toString x;
	       )
	  )
     )

pass2 := () -> (
     << "pass 2, descending through documentation tree" << endl;
     follow topNodeName;
     )

pass3 := () -> (
     << "pass 3, checking for unreachable documentation nodes" << endl;
     scan(keys docdatabase,
	  key -> (
	       if not linkFollowedTable#?key then (
		    haderror = true;
		    stderr << "error: documentation node '" << key << "' not reachable" << endl ) ) ) )

fakeMenu := x -> (
     --   o  item 1
     --     o  item 2
     --       o  item 3
     SEQ { BR,
	  SEQ for i from 0 to #x-1 list SEQ {
	       LITERAL ( 3*i+2 : "&nbsp;", "&#149;", 2 : "&nbsp;"), x#i, BR
	       }
	  }
     )

makeHtmlNode = fkey -> (
     fn := buildDirectory | htmlFilename fkey;
     stderr << "--making html page for " << fkey << endl;
     fn << html HTML { 
	  HEAD TITLE {fkey, headline fkey},
	  BODY { 
	       buttonBar fkey,
	       if UP#?fkey then SEQ {
		    "Parent headings:",
		    fakeMenu apply(upAncestors fkey, i -> TOH i)
		    },
	       HR{}, 
	       documentationMemo fkey,
	       }
	  }
     << endl << close)

pass4 := () -> (
     << "pass 4, writing html files" << endl;
     scan(keys linkFollowedTable, fkey -> if linkFollowedTable#fkey then makeHtmlNode fkey))

-----------------------------------------------------------------------------

alpha := characters "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
anchorPoint := 0
anchor := entry -> if alpha#?anchorPoint and entry >= alpha#anchorPoint then (
     s := select(drop(alpha,anchorPoint), c -> entry >= c);
     anchorPoint = anchorPoint + #s;
     SEQ apply(s, c -> ANCHOR {c, ""})
     )

pass5 := () -> (
     fn := buildDirectory | htmlDirectory | masterFileName;
     << "pass 5, creating the master index in " << fn << endl;
     masterNodeName := topNodeName | " Index";
     fn << html HTML {
	  HEAD { TITLE masterNodeName },
	  BODY {
	       HEADER2 masterNodeName, PARA,
	       CENTER topNodeButton, PARA,
	       CENTER between(LITERAL "&nbsp;&nbsp;&nbsp;",apply(alpha, c -> HREF {"#"|c, c})), PARA,
	       MENU apply(sort keys masterIndex, (fkey,key) -> SEQ { anchor fkey, TOH key })
	       }
	  } << endl << close
     )

-----------------------------------------------------------------------------
-- making the html pages
-----------------------------------------------------------------------------

makeHTML = (builddir,finaldir) -> (
     buildDirectory = minimizeFilename(builddir | "/");
     finalDirectory = minimizeFilename(finaldir | "/");
     docdatabase = DocDatabase;				    -- used to be an argument
     topNodeName = "Macaulay 2";			    -- used to be an argument
     htmlDirectory = LAYOUT#"htmldoc";
     buildPackage = null;
     gifpath := LAYOUT#"images";
     prefix = htmlDirectory;
     topNodeButton = HREF { topFileName, BUTTON (checkFile(gifpath|"top.gif"),"top") };
     nullButton = BUTTON(checkFile(gifpath|"null.gif"),null);
     masterIndexButton = HREF { masterFileName, BUTTON(checkFile(gifpath|"index.gif"),"index") };
     nextButton = BUTTON(checkFile(gifpath|"next.gif"),"next");
     prevButton = BUTTON(checkFile(gifpath|"previous.gif"),"previous");
     upButton = BUTTON(checkFile(gifpath|"up.gif"),"up");
     sav := if htmlDefaults#?"BODY" then htmlDefaults#"BODY";
     htmlDefaults#"BODY" = concatenate(
	  "BACKGROUND=\"",				    -- "
	  relativizeFilename(htmlDirectory,checkFile(gifpath|"recbg.jpg")),
	  "\""						    -- "
	  );
     lastKey = null;
     thisKey = null;
     linkFollowedTable = new MutableHashTable;
     masterIndex = new MutableHashTable;
     NEXT = new MutableHashTable;
     PREV = new MutableHashTable;
     UP   = new MutableHashTable;
     haderror = false;
     setrecursionlimit first (
	  setrecursionlimit 4000,
     	  time pass1(),
     	  time pass2(),
     	  time pass3(),
     	  time pass4(),
     	  time pass5()
	  );
     if sav =!= null then htmlDefaults#"BODY" = sav;
     if haderror then (
	  stderr << "error: ignoring documentation errors" << endl;
	  -- error "documentation errors occurred";
	  );
     )     

-----------------------------------------------------------------------------
-- assembling packages -- eventually to be merged with 
-- the code above for making html for Macaulay 2 itself
-----------------------------------------------------------------------------

assemble = method(Options => { 
	  TemporaryDirectory => "tmp/", 
	  FinalDirectory => "tmp/",
	  Encapsulate => true
	  })
assemble Package := o -> pkg -> (
     topNodeName = pkg.name;
     buildPackage = pkg.name;
     buildDirectory = o.TemporaryDirectory | "/";
     if o.Encapsulate then buildDirectory = buildDirectory|buildPackage|"-"|pkg#"package version"|"/";
     buildPackage = minimizeFilename buildPackage;
     finalDirectory = minimizeFilename(o.FinalDirectory | "/");
     stderr << "--assembling package " << pkg#"package title" << " in " << buildDirectory << endl;

     currentSourceDir := pkg#"file directory";
     stderr << "--using package sources found in " << currentSourceDir << endl;

     -- copy source file
     pkgDirectory := LAYOUT#"packages";
     makeDirectory (buildDirectory|pkgDirectory);
     bn := buildPackage | ".m2";
     fn := currentSourceDir|bn;
     if not fileExists fn then error("file ", fn, " not found");
     copyFile(fn, buildDirectory|pkgDirectory|bn, Verbose=>true);

     -- copy source subdirectory
     srcDirectory := LAYOUT#"packagesrc" buildPackage;
     makeDirectory (buildDirectory|srcDirectory);
     dn := currentSourceDir|buildPackage;
     stderr << "--copying auxiliary source files from " << dn << endl;
     if fileExists dn
     then copyDirectory(dn, buildDirectory|srcDirectory, Verbose=>true, Exclude => {"CVS"});

     -- make html files
     htmlDirectory = LAYOUT#"packagehtml" buildPackage;
     makeDirectory (buildDirectory|htmlDirectory);     
     nodes := unique join(keys pkg#"dictionary",keys pkg#"raw documentation",{topNodeName});
     stderr << "--making html pages in " << buildDirectory|htmlDirectory << endl;
     ret := makeHtmlNode \ toString \ nodes;

     -- make example input files
     exampleDir := buildDirectory|LAYOUT#"packageexamples" buildPackage;
     infn := nodename -> exampleDir|toFilename nodename|".m2";
     outfn := nodename -> exampleDir|toFilename nodename|".out";
     stderr << "--making example files in " << exampleDir << endl;
     makeDirectory exampleDir;
     scan(pairs pkg#"example inputs", (nodename,inputs) -> (
	       inf := infn nodename;
	       val := concatenate apply(values inputs, s -> s|"\n");
	       if fileExists inf and get inf === val
	       then stderr << "--leaving example input file for " << nodename << endl
	       else (
		    stderr << "--making example input file for " << nodename << endl;
		    inf << val << close;
		    )));

     -- make example output files
     haderror := false;
     scan(pairs pkg#"example inputs", (nodename,inputs) -> (
	       inf := infn nodename;
	       outf := outfn nodename;
	       if fileExists outf and fileTime outf >= fileTime inf
	       then stderr << "--leaving example output file for " << nodename << endl
	       else (
		    stderr << "--making example output file for " << nodename << endl;
		    -- later we'll figure out how to start *this* version of M2!!!
		    cmd := "M2 -silent -q -s -x -e'load \""|buildPackage|".m2\"' <"|inf|" >"|outf;
		    stderr << cmd << endl;
		    r := run cmd;
		    if r != 0 then (
			 stderr << "--error return code: " << r << endl;
			 haderror = true;
			 ))));
     if haderror then error "error occurred running example files";
     )

check = method()
check Package := pkg -> (
     scan(values pkg#"test inputs", t -> (
	       stderr << "--testing " << net t << endl;
	       -- later we'll figure out how to start *this* version of M2!!!
	       cmd := "M2 -silent -q -s -e'load \""|pkg.name|".m2\"'";
	       stderr << cmd << endl;
	       "!" | cmd << t << endl << close;
	       << endl;
	       )))
