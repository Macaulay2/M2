-- -*- fill-column: 107 -*-
--		Copyright 1993-2002 by Daniel R. Grayson

-----------------------------------------------------------------------------
-- html output
-----------------------------------------------------------------------------

-- maybe we should rename this file to "packages2.m2" after the merge.

-- we've turned off checking for existence of files...

local prefix, local topNodeButton
local haderror, local nullButton, local masterIndexButton, local tocButton, local homeButton
local NEXT, local PREV, local UP, local CONT
local nextButton, local prevButton, local upButton
local masterIndex

buildPackage := null					    -- name of the package currently being built
topNodeName := null					    -- name of the top node of this package
topFileName := "index.html"				    -- top node's file name, constant
indexFileName := "master.html"  			    -- file name for master index of topics in a package
tocFileName := "toc.html"       			    -- file name for the table of contents of a package
buildDirectory := "/tmp/"				    -- the root of the relative paths:
htmlDirectory := ""					    -- relative path to the html directory

isAbsolute := url -> match( "^(#|mailto:|[a-z]+://)", url )

rel := url -> (
     if isAbsolute url 
     then url
     else relativizeFilename(htmlDirectory, url)
     )

htmlFilename = key -> (				   -- returns the relative path from the PREFIX to the file
     key = normalizeDocumentTag key;
     fkey := formatDocumentTag key;
     pkg := package TO key;
     if pkg === null then toFilename fkey|".html"
     else LAYOUT#"packagehtml" pkg#"title" | if fkey === pkg#"top node name" then topFileName else toFilename fkey|".html" )

html IMG  := x -> "<IMG src=\"" | rel first x | "\">"
net IMG := tex IMG  := x -> ""

html HREF := x -> (
     "<A HREF=\"" 					    -- "
     | rel first x 
     | "\">" 						    -- "
     | html last x 
     | "</A>"
     )
net HREF := x -> net last x
tex HREF := x -> (
     concatenate(
	  ///\special{html:<A href="///, 		    -- "
	       texLiteral rel first x,
	       ///">}///,				    -- "
	  tex last x,
	  ///\special{html:</A>}///
	  )
     )

html LABEL := x -> LITERAL concatenate(
     "<label title=\"", x#0, "\">",
     html x#1,
     "</label>"
     )

html TO := x -> (
     key := normalizeDocumentTag x#0;
     formattedKey := formatDocumentTag key;
     concatenate ( 
     	  ///<A HREF="///,				    -- "
	  rel htmlFilename key,
	  ///">///, 					    -- "
     	  htmlExtraLiteral formattedKey,
     	  "</A>",
     	  drop(toList x,1) 
     	  )
     )

net TO2 := x -> concatenate drop(toList x,1)
html TO2 := x -> (
     key := normalizeDocumentTag x#0;
     formattedKey := formatDocumentTag key;
     concatenate ( 
     	  ///<A HREF="///,				    -- "
	  rel htmlFilename key,
	  ///">///, 					    -- "
     	  drop(toList x,1),
     	  "</A>"
     	  )
     )

html BASE := x -> concatenate("<BASE HREF=\"",rel first x,"\">")

--

encoding := ///<?xml version="1.0" encoding="us-ascii"?>///
doctype := ///<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">///

style := () -> LITERAL {///
<style type="text/css">
   @import "/// | rel ( LAYOUT#"style" | "doc.css" ) | ///";
</style>
/// }

links := () -> ""
-- LITERAL ///
--     <link rel="Home" title="Advanced Bash-Scripting Guide" href="index.html">
--     <link rel="Up" title="Advanced Topics" href="part4.html">
--     <link rel="Previous" title="Advanced Topics" href="part4.html">
--     <link rel="Next" title="Globbing" href="globbingref.html">
--     <link rel="Index" title="Globbing" href="index.html">
--     <link rel="stylesheet" href="common/kde-common.css" type="text/css">
-- ///

-- validate := LITERAL ///
-- <a href="http://validator.w3.org/check/referer">Validate</a> the html on this page, or <a href="http://jigsaw.w3.org/css-validator/check/referer">validate</a> the css on this page.
-- ///

-- produce html form of documentation, for Macaulay 2 and for packages

next := key -> if NEXT#?key then LABEL { "Next node",     HREF { htmlFilename NEXT#key, nextButton } } else nullButton
prev := key -> if PREV#?key then LABEL { "Previous node", HREF { htmlFilename PREV#key, prevButton } } else nullButton
up   := key -> if   UP#?key then LABEL { "Parent node",   HREF { htmlFilename   UP#key,   upButton } } else nullButton

buttonBar := (key) -> TABLE { { 

	  LITERAL concatenate (///
	       <form class="search" action="///,					    -- "
	       if getenv "SEARCHENGINE" === "" then "http://rhenium.math.uiuc.edu:7003/" else getenv "SEARCHENGINE",
	       ///">
	       <p>
	       <label title="Macaulay2 term to search for">Search: 
	         <input type="text" name="words">
	       </label>
	       <input type="hidden" name="method"   value="boolean">
	       <input type="hidden" name="format"   value="builtin-short">
	       <input type="hidden" name="sort"     value="score">
	       <input type="hidden" name="config"   value="htdig-M2">
	       </p>
	       </form>
	       ///),						    -- "

	  SEQ {
	       LITERAL ///<p class="buttonbar">///,
	       next key, prev key, up key,
     	       if key =!= topNodeName then topNodeButton else nullButton,
     	       masterIndexButton,
     	       tocButton,
     	       homeButton,
     	       LITERAL ///</p>///
	       },

	  } }

BUTTON := (s,alt) -> (
     s = rel s;
     if alt === null
     then error "required attribute: ALT"
     else LITERAL concatenate("<IMG class=\"button\" src=\"",s,"\" alt=\"[", alt, "]\">\n")
     )

upAncestors := key -> reverse (
     n := 0;
     prepend(key, while UP#?key and n < 20 list (n = n+1; key = UP#key)))

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

makeHtmlNode = key -> (
     fn := buildDirectory | htmlFilename key;
     stderr << "--making html page for " << key << endl;
     fn
     << encoding << endl
     << doctype << endl
     << html HTML { 
	  HEAD {
	       TITLE {key, headline key},
	       style(), links()
	       },
	  BODY { 
	       buttonBar key,
	       if UP#?key then SEQ between(" > ", apply(upAncestors key, i -> TO i)),
	       HR{}, 
	       documentation key,
	       }
	  }
     << endl << close)

-----------------------------------------------------------------------------

alpha := characters "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
anchorPoint := 0
anchor := entry -> if alpha#?anchorPoint and entry >= alpha#anchorPoint then (
     s := select(drop(alpha,anchorPoint), c -> entry >= c);
     anchorPoint = anchorPoint + #s;
     SEQ apply(s, c -> ANCHOR {c, ""})
     )

packageNodes := (pkg,topNodeName) -> unique join(
     select(keys pkg.Dictionary,s -> not match ( "\\$" , s )),
     keys pkg#"documentation",{topNodeName})

-----------------------------------------------------------------------------
-- constructing the tree-structure for the documentation nodes in a package
-----------------------------------------------------------------------------

-- make this first:
linkTable := new MutableHashTable			    -- keys are fkeys for a node, values are ordinary lists of descendents

-- assemble this next
ForestNode = new Type of BasicList			    -- list of formatted keys for descendents
TreeNode = new Type of BasicList			    -- first entry is formatted key for this node, second entry is a descendent list

net ForestNode := x -> stack apply(toList x,net)
net TreeNode := x -> (
     y := net x#1;
     net x#0 || (stack (height y + depth y : " |  ")) | y)

toDoc := method()
toDoc ForestNode := x -> if #x>0 then UL apply(toList x, y -> toDoc y)
toDoc TreeNode := x -> SEQ { TOH x#0, toDoc x#1 }

local visitCount
local missingReferences
showExternalReferences = () -> sort keys missingReferences
local duplicateReferences
local nodesToScope

makeTree := x -> (
     visits := if visitCount#?x then visitCount#x else 0;
     visitCount#x = visits + 1;
     if linkTable#?x 
     then (
	  if visits > 0
     	  then new TreeNode from { x | " -- repeated reference" , new ForestNode from { }  }
     	  else new TreeNode from { x, new ForestNode from apply(linkTable#x,makeTree) }
	  )
     else (
	  if missingReferences#?x
     	  then new TreeNode from { x | " -- missing reference", new ForestNode from { } }
	  else new TreeNode from { x | " -- internal error", new ForestNode from { } }
	  )
     )

makeForest := x -> new ForestNode from makeTree \ x

leaves := () -> keys set flatten values linkTable
roots := () -> (
     x := keys ( set keys linkTable - set leaves() );
     if not member(topNodeName,x) then stderr << "--warning: top node name " << topNodeName << " not a root" << endl;
     x = select(x,k -> k =!= topNodeName);
     prepend(topNodeName, sort x))
getTrees := topNode -> (
     visitCount = new MutableHashTable;
     return makeForest roots())

-----------------------------------------------------------------------------

markLinks := method()
markLinks ForestNode := x -> (
     for i from 0 to #x-2 do ( NEXT#(x#i#0) = x#(i+1)#0; PREV#(x#(i+1)#0) = x#i#0; );
     scan(x,markLinks))
markLinks TreeNode   := x -> (
     scan(x#1, i -> UP#(i#0) = x#0);
     markLinks x#1)

buildLinks := method()
buildLinks ForestNode := x -> (
     UP = new MutableHashTable;
     NEXT = new MutableHashTable;
     PREV = new MutableHashTable;
     markLinks x)

-----------------------------------------------------------------------------

assembleTree = method()
assembleTree Package := pkg -> (
     oldpkg := currentPackage;
     currentPackage = pkg;
     missingReferences = new MutableHashTable;
     duplicateReferences = new MutableHashTable;
     topNodeName = pkg#"top node name";
     key := normalizeDocumentTag topNodeName;
     fkey := formatDocumentTag key;			    -- same as topNodeName
     nodes := packageNodes(pkg,topNodeName);
     linkTable = new HashTable from apply(
	  pairs pkg#"documentation", (fkey,doc) -> fkey => (
	       if doc.?Menu
	       then formatDocumentTag \ normalizeDocumentTag \ first \ select(doc.Menu, x -> class x === TO)
	       else {}
	       ));
     CONT = getTrees();
     buildLinks CONT;
     )

-----------------------------------------------------------------------------
-- making the html pages
-----------------------------------------------------------------------------

setupButtons := () -> (
     gifpath := LAYOUT#"images";
     topNodeButton = LABEL { "top node", HREF { htmlDirectory|topFileName, BUTTON (gifpath|"top.gif","top") } };
     tocButton = LABEL { "table of contents", HREF { htmlDirectory|tocFileName, BUTTON (gifpath|"toc.gif","toc") } };
     homeButton = LABEL { "table of contents", HREF { "http://www.math.uiuc.edu/Macaulay2/" , BUTTON (gifpath|"home.gif","home") } };
     nullButton = BUTTON(gifpath|"null.gif","null");
     masterIndexButton = LABEL { "index", HREF { htmlDirectory|indexFileName, BUTTON(gifpath|"index.gif","index") } };
     nextButton = BUTTON(gifpath|"next.gif","next");
     prevButton = BUTTON(gifpath|"previous.gif","previous");
     upButton = BUTTON(gifpath|"up.gif","up");
     )

separateRegexp = method()
separateRegexp(String,String) := (re,s) -> separateRegexp(re,0,s)
separateRegexp(String,ZZ,String) := (re,n,s) -> (
     m := matches(re,s);
     if m#?n then prepend(substring(s,0,m#n#0), separateRegexp(re,n,substring(m#n#0+m#n#1,s))) else {s})
separateExampleOutput = s -> (
     r := capture s;
     while r#0 == "\n" do r = substring(1,r);
     while r#-1 == "\n" do r = substring(0,#r-1,r);
     separateRegexp("(\n\n)i+[1-9][0-9]* : ",1,r))

-----------------------------------------------------------------------------
-- installing packages -- eventually to be merged with 
-- the code above for making html for Macaulay 2 itself
-----------------------------------------------------------------------------

makeMasterIndex := keylist -> (
     fn := buildDirectory | htmlDirectory | indexFileName;
     title := topNodeName | " Index";
     << "--making  '" << title << "' in " << fn << endl;
     fn
     << encoding << endl
     << doctype << endl     
     << html HTML {
	  HEAD { TITLE title, style(), links() },
	  BODY {
	       HEADER2 title, PARA,
	       topNodeButton, tocButton, homeButton,
	       PARA between(LITERAL "&nbsp;&nbsp;&nbsp;",apply(alpha, c -> HREF {"#"|c, c})), 
	       UL apply(sort keylist, (fkey) -> SEQ { anchor fkey, TOH fkey }),
	       }
	  } << endl << close
     )

makeTableOfContents := () -> (
     fn := buildDirectory | htmlDirectory | tocFileName;
     title := topNodeName | " : Table of Contents";
     << "--making  '" << title << "' in " << fn << endl;
     fn
     << encoding << endl
     << doctype << endl     
     << html HTML {
	  HEAD { TITLE title, style(), links() },
	  BODY {
	       HEADER2 title, PARA,
	       topNodeButton, masterIndexButton, homeButton,
	       HR{},
	       toDoc CONT
	       }
	  } << endl << close
     )

installPackage = method(Options => { 
	  Prefix => "./tmp/",
	  Encapsulate => true,
	  IgnoreExampleErrors => true
	  })

installPackage Symbol := opts -> pkg -> (
     if class value pkg === Package then return installPackage(value pkg, opts);
     fn := toString pkg | ".m2";
     load fn;
     if class value pkg === Package then return installPackage(value pkg, opts);
     error ("loading file '",fn,"' failed to create a package named '",toString pkg,"'");
     )

installPackage Package := o -> pkg -> (
     oldpkg := currentPackage;
     currentPackage = pkg;
     topNodeName = pkg#"top node name";
     nodes := packageNodes(pkg,topNodeName);
     buildPackage = if pkg === Main then "Macaulay2" else pkg#"title";
     buildDirectory = minimizeFilename(o.Prefix | "/");
     if o.Encapsulate then buildDirectory = buildDirectory|buildPackage|"-"|pkg.Options.Version|"/";
     buildPackage = minimizeFilename buildPackage;
     stderr << "--installing package " << pkg << " in " << buildDirectory << endl;

     if pkg =!= Main then (				    -- Main sources are handled separately

	  currentSourceDir := pkg#"source directory";
	  stderr << "--using package sources found in " << currentSourceDir << endl;

	  -- copy source file
	  pkgDirectory := LAYOUT#"packages";
	  makeDirectory (buildDirectory|pkgDirectory);
	  bn := buildPackage | ".m2";
	  fn := currentSourceDir|bn;
	  if not fileExists fn then error("file ", fn, " not found");
	  copyFile(fn, buildDirectory|pkgDirectory|bn, Verbose=>true);

	  -- copy source subdirectory
	  srcDirectory := LAYOUT#"packagesrc" pkg#"title";
	  dn := realpath(currentSourceDir|buildPackage);
	  if fileExists dn
	  then (
	       stderr << "--copying auxiliary source files from " << dn << endl;
	       makeDirectory (buildDirectory|srcDirectory);
	       copyDirectory(dn, buildDirectory|srcDirectory, Verbose=>true, Exclude => {"CVS"});
	       );

     	  );

     -- This is a bit of a fiction: we've copied the files for our package into the build directory,
     -- so let's pretend we loaded the package from there in the first place, thereby allowing "documentation()"
     -- to find the example output files the same way it would if the package had been loaded from there.
     pkg#"package prefix" = buildDirectory;

     -- make example input files
     exampleDir := buildDirectory|LAYOUT#"packageexamples" pkg#"title";
     infn := nodename -> exampleDir|toFilename nodename|".m2";
     outfn := nodename -> exampleDir|toFilename nodename|".out";
     tmpfn := nodename -> exampleDir|toFilename nodename|".tmp";
     stderr << "--making example files in " << exampleDir << endl;
     makeDirectory exampleDir;
     scan(pairs pkg#"example inputs", (nodename,inputs) -> (
	       inf := infn nodename;
	       val := concatenate apply(inputs, s -> s|"\n");
	       if fileExists inf and get inf === val
	       then (
		    -- stderr << "--leaving example input file for " << nodename << endl;
		    )
	       else (
		    stderr << "--making example input file for " << nodename << endl;
		    inf << val << close;
		    )));

     -- make example output files
     haderror := false;
     scan(pairs pkg#"example inputs", (nodename,inputs) -> (
	       inf := infn nodename;
	       outf := outfn nodename;
	       tmpf := tmpfn nodename;
	       if fileExists outf and fileTime outf >= fileTime inf
	       then (
		    -- stderr << "--leaving example results file for " << nodename << endl
		    )
	       else (
		    stderr << "--making example results file for " << nodename << endl;
		    loadargs := if pkg === Main then "" else "-e 'load \""|fn|"\"'";
		    cmd := "ulimit -t 20 -v 60000; " | commandLine#0 | " --silent --print-width 80 --stop --int -e errorDepth=0 -q " | loadargs | " <" | inf | " >" | tmpf;
		    stderr << cmd << endl;
		    r := run cmd;
		    if r != 0 then (
			 if o.IgnoreExampleErrors then (
			      unlink outf;
			      link(tmpf,outf);
			      unlink tmpf
			      )
			 else (
			      unlink tmpf;
			      );
			 stderr << "--error return code: (" << r//256 << "," << r%256 << ")" << endl;
			 if r == 131 then (
			      stderr << "subprocess terminated abnormally, exiting" << endl;
			      exit r;
			      );
			 if r == 2 then (
			      stderr << "subprocess interrupted with INT, exiting, too" << endl;
			      exit r;
			      );
			 haderror = true;
			 )
		    else (
			 if fileExists outf then unlink outf;
			 link(tmpf,outf);
			 unlink tmpf;
			 ));
	       -- read, separate, and store example output
	       if fileExists outf then pkg#"example results"#nodename = drop(separateM2output get outf,-1)
	       else stderr << "warning: missing file " << outf << endl;
	       ));
     if haderror and not o.IgnoreExampleErrors then error "error(s) occurred running example files";

     -- make table of contents, including next, prev, and up links
     stderr << "--assembling table of contents" << endl;
     assembleTree pkg;
     pkg#"table of contents" = new Bag from {CONT}; -- we bag it because it might be big!
     pkg#"links up" = UP;
     pkg#"links next" = NEXT;
     pkg#"links prev" = PREV;

     -- make html files
     htmlDirectory = LAYOUT#"packagehtml" pkg#"title";
     setupButtons();
     makeDirectory (buildDirectory|htmlDirectory);     
     stderr << "--making html pages in " << buildDirectory|htmlDirectory << endl;
     ret := makeHtmlNode \ toString \ nodes;

     -- make master.html with master index of all the html files
     makeMasterIndex nodes;

     -- make table of contents
     makeTableOfContents();

     stderr << "--installed package " << pkg << " in " << buildDirectory << endl;
     currentPackage = oldpkg;
     )

check = method()
check Package := pkg -> (
     logfile := "Macaulay2-test.log";
     scan(pairs pkg#"test inputs", 
	  (i,t) -> (
	       stderr << "--------------------------------------------" << endl;
	       if class t === String then (
	       	    cmd := commandLine#0 | " --silent -q -e 'load \""|pkg#"title"|".m2\"'";
	       	    stderr << "-- test " << i << ": " << cmd << endl;
	       	    "!" | cmd << t << endl << close;
		    )
	       else if class t === Function then (
		    stderr << "-- test " << i << ":" << endl << code t << endl;
		    t()))))

htmlDebug = () -> commandInterpreter local x

relativizeFilename2 = on relativizeFilename

makePackageIndex = method(SingleArgumentDispatch => true)
makePackageIndex Sequence := () -> makePackageIndex prefixDirectory
makePackageIndex String := prefixDirectory -> (
     htmlDirectory = LAYOUT#"docm2";
     p := prefixDirectory | htmlDirectory;
     setupButtons();
     r := readDirectory p;
     r = select(r, fn -> fn != "." and fn != "..");
     r = select(r, pkg -> fileExists (prefixDirectory | LAYOUT#"packagehtml" pkg | "index.html"));
     key := "package index";
     p | "index.html"
     << encoding << endl
     << doctype << endl
     << html HTML { 
	  HEAD {
	       TITLE {key, headline key},
	       style(), links()
	       },
	  BODY { 
	       buttonBar key,
	       HR{},
	       PARA BOLD "Index of installed packages:",
	       UL apply(r, pkg -> HREF { LAYOUT#"packagehtml" pkg | "index.html", pkg })
	       }
	  } << endl << close
     )

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
