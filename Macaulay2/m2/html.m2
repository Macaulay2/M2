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
topDocumentTag := null
topFileName := "index.html"				    -- top node's file name, constant
indexFileName := "master.html"  			    -- file name for master index of topics in a package
tocFileName := "toc.html"       			    -- file name for the table of contents of a package
buildDirectory := "/tmp/"				    -- the root of the relative paths:
htmlDirectory := ""					    -- relative path to the html directory, depends on the package

-----------------------------------------------------------------------------
-- relative URLs and filenames
-----------------------------------------------------------------------------

isAbsolute := url -> match( "^(#|mailto:|[a-z]+://)", url )

rel := url -> (
     if isAbsolute url 
     then url
     else relativizeFilename(htmlDirectory, url))

htmlFilename = method(SingleArgumentDispatch => true)
htmlFilename DocumentTag := tag -> (
     fkey := DocumentTag.FormattedKey tag;
     pkg := DocumentTag.Package tag;
     if pkg === null then toFilename fkey|".html"
     else LAYOUT#"packagehtml" pkg#"title" | if fkey === pkg#"top node name" then topFileName else toFilename fkey|".html" )

html IMG  := x -> concatenate("<IMG src=\"", rel first x, "\">")
html HREF := x -> concatenate("<A HREF=\"", rel first x, "\">", html last x, "</A>")
tex  HREF := x -> concatenate("\special{html:<A href=\"", texLiteral rel first x, "\">}", tex last x, "\special{html:</A>}")
html LABEL:= x -> concatenate("<label title=\"", x#0, "\">", html x#1, "</label>")
html TO   := x -> concatenate("<A HREF=\"", rel htmlFilename x#0, "\">", htmlExtraLiteral DocumentTag.FormattedKey x#0, "</A>", if x#?1 then x#1)
html TO2  := x -> concatenate("<A HREF=\"", rel htmlFilename x#0, "\">", htmlExtraLiteral                          x#1, "</A>")
html BASE := x -> concatenate("<BASE HREF=\"",rel first x,"\">")

next := tag -> if NEXT#?tag then LABEL { "Next node",     HREF { htmlFilename NEXT#tag, nextButton } } else nullButton
prev := tag -> if PREV#?tag then LABEL { "Previous node", HREF { htmlFilename PREV#tag, prevButton } } else nullButton
up   := tag -> if   UP#?tag then LABEL { "Parent node",   HREF { htmlFilename   UP#tag,   upButton } } else nullButton

style := () -> LITERAL {///
<style type="text/css">
   @import "/// | rel ( LAYOUT#"style" | "doc.css" ) | ///";
</style>
/// }

BUTTON := (s,alt) -> (
     s = rel s;
     if alt === null
     then error "required attribute: ALT"
     else LITERAL concatenate("<IMG class=\"button\" src=\"",s,"\" alt=\"[", alt, "]\">\n")
     )

-----------------------------------------------------------------------------

encoding := ///<?xml version="1.0" encoding="us-ascii"?>///
doctype := ///<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">///


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

buttonBar := (tag) -> TABLE { { 

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
	       next tag, prev tag, up tag,
     	       if tag =!= topDocumentTag then topNodeButton else nullButton,
     	       masterIndexButton,
     	       tocButton,
     	       homeButton,
     	       LITERAL ///</p>///
	       },

	  } }

upAncestors := tag -> reverse (
     n := 0;
     prepend(tag, while UP#?tag and n < 20 list (n = n+1; tag = UP#tag)))

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

commentize := s -> if s =!= null then concatenate(" -- ",s)

-----------------------------------------------------------------------------

checkIsTag := tag -> ( assert(class tag === DocumentTag); tag )

alpha := characters "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
anchorPoint := 0
anchor := entry -> (
     checkIsTag entry;
     if alpha#?anchorPoint and entry >= alpha#anchorPoint then (
     	  s := select(drop(alpha,anchorPoint), c -> entry >= c);
     	  anchorPoint = anchorPoint + #s;
     	  SEQ apply(s, c -> ANCHOR {c, ""})
     	  ))

packageTagList := (pkg,topDocumentTag) -> checkIsTag \ unique join(
     apply(
     	  select(pairs pkg.Dictionary,(nam,sym) -> not match ( "\\$" , nam )),
	  (nam,sym) -> makeDocumentTag(sym, Package => pkg)),
     apply(
	  values pkg#"raw documentation",
	  doc -> doc.DocumentTag),
     { topDocumentTag }
     )

-----------------------------------------------------------------------------
-- constructing the tree-structure for the documentation nodes in a package
-----------------------------------------------------------------------------

-- make this first:
linkTable := new MutableHashTable			    -- keys are DocumentTags for a node, values are lists of DocumentTags of descendents

-- assemble this next
ForestNode = new Type of BasicList			    -- list of tree nodes, the descendent list
TreeNode = new Type of BasicList			    -- first entry is DocumentTag for this node, second entry is a forest node

traverse := method()
traverse(ForestNode,Function) := (n,f) -> scan(n,t -> traverse(t,f))
traverse(TreeNode,Function) := (t,f) -> (f t#0, traverse(t#1,f))

net ForestNode := x -> stack apply(toList x,net)
net TreeNode := x -> (
     y := net x#1;
     net x#0 || (stack (height y + depth y : " |  ")) | y)

toDoc := method()
toDoc ForestNode := x -> if #x>0 then UL apply(toList x, y -> toDoc y)
toDoc TreeNode := x -> SEQ { TOH checkIsTag x#0, toDoc x#1 }

local visitCount
local duplicateReferences
local nodesToScope
local missingReferences
local repeatedReferences

makeTree := x -> (
     visits := if visitCount#?x then visitCount#x else 0;
     visitCount#x = visits + 1;
     if linkTable#?x then (
	  if visits > 0
     	  then (
	       if not repeatedReferences#?x then (
		    repeatedReferences#x = true;
		    stderr << "--error: repeated reference(s) to documentation as subnode: " << x << endl;
		    );
	       new TreeNode from { x , new ForestNode}	    -- repeated reference
	       )
     	  else new TreeNode from { x, new ForestNode from apply(linkTable#x,makeTree)})
     else (
	  if not missingReferences#?x then (
	       missingReferences#x = true;
	       stderr << "--error: missing reference to documentation as subnode: " << x << endl;
	       );
	  new TreeNode from { x , new ForestNode}	    -- missing reference
	  ))
makeForest := x -> new ForestNode from makeTree \ x

leaves := () -> keys set flatten values linkTable
roots := () -> (
     x := keys ( set keys linkTable - set leaves() );
     if not member(topDocumentTag,x) then stderr << "--warning: top node name " << topDocumentTag << " not a root" << endl;
     x = select(x,k -> k =!= topDocumentTag);
     prepend(topDocumentTag, sort x))
getTrees := topNode -> (
     visitCount = new MutableHashTable;
     return makeForest roots())

-----------------------------------------------------------------------------

markLinks := method()
markLinks ForestNode := x -> (
     for i from 0 to #x-2 do ( 
	  NEXT#(x#i#0) = checkIsTag x#(i+1)#0;
	  PREV#(x#(i+1)#0) = checkIsTag x#i#0;
	  );
     scan(x,markLinks))
markLinks TreeNode   := x -> (
     scan(x#1, i -> UP#(i#0) = checkIsTag x#0);
     markLinks x#1)

buildLinks := method()
buildLinks ForestNode := x -> (
     UP = new MutableHashTable;
     NEXT = new MutableHashTable;
     PREV = new MutableHashTable;
     markLinks x)

-----------------------------------------------------------------------------

assembleTree := (pkg,nodes) -> (
     missingReferences = new MutableHashTable;
     repeatedReferences = new MutableHashTable;
     duplicateReferences = new MutableHashTable;
     linkTable = new HashTable from apply(nodes, tag -> (   -- collect links from each tag to its subnodes
	       checkIsTag tag;
	       fkey := DocumentTag.FormattedKey tag;
	       if pkg#"raw documentation"#?fkey then (
		    doc := pkg#"raw documentation"#fkey;
		    tag => first \ select(if doc.?Subnodes then toList doc.Subnodes else {}, x -> class x === TO))
	       else (
		    tag => {}
		    )
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
     title := "Combined Index";
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
	       UL apply(sort keylist, (tag) -> (
			 checkIsTag tag;
			 SEQ { anchor tag, TOH tag }
			 )),
	       }
	  } << endl << close
     )

makeTableOfContents := () -> (
     fn := buildDirectory | htmlDirectory | tocFileName;
     title := DocumentTag.FormattedKey topDocumentTag | " : Table of Contents";
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
	  IgnoreExampleErrors => true,
	  MakeInfo => true
	  })

installPackage String := opts -> pkg -> (
     if isGlobalSymbol pkg and class value getGlobalSymbol pkg === Package then return installPackage(value getGlobalSymbol pkg, opts);
     needsPackage pkg;
     if class value pkg === Package then return installPackage(value pkg, opts);
     error ("can't locate package '",pkg,"'");
     )

installPackage Package := o -> pkg -> (
     oldpkg := currentPackage;
     currentPackage = pkg;
     topDocumentTag = makeDocumentTag(pkg#"top node name", Package => pkg);
     rawDoc := pkg#"raw documentation";

     -- check that we've read the raw documentation
     if #rawDoc > 0 or pkg#?"raw documentation database" and isOpen pkg#"raw documentation database" then null
     else (
	  if pkg === Macaulay2 then (
     	       currentPackage = Macaulay2;
     	       stderr << "--loading Macaulay2-doc.m2" << endl;
	       load "Macaulay2-doc.m2";
     	       currentPackage = null;
	       )
	  else error "raw documentation not loaded";
	  );

     -- here's where we get the list of nodes from the raw documentation
     nodes := packageTagList(pkg,topDocumentTag);

     buildPackage = if pkg === Macaulay2 then "Macaulay2" else pkg#"title";
     buildDirectory = minimizeFilename(o.Prefix | "/");
     if o.Encapsulate then buildDirectory = buildDirectory|buildPackage|"-"|pkg.Options.Version|"/";
     buildPackage = minimizeFilename buildPackage;
     stderr << "--installing package " << pkg << " in " << buildDirectory << endl;

     if pkg =!= Macaulay2 then (				    -- Macaulay2 sources are handled separately

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
	       copyDirectory(dn, buildDirectory|srcDirectory, Verbose => debugLevel > 0, Exclude => {"CVS"});
	       );

     	  );

     -- This is a bit of a fiction: we've copied the files for our package into the build directory,
     -- so let's pretend we loaded the package from there in the first place, thereby allowing "documentation()"
     -- to find the example output files the same way it would if the package had been loaded from there.
     pkg#"package prefix" = buildDirectory;

     -- make example input files
     exampleDir := buildDirectory|LAYOUT#"packageexamples" pkg#"title";
     infn := fkey -> exampleDir|toFilename fkey|".m2";
     outfn := fkey -> exampleDir|toFilename fkey|".out";
     tmpfn := fkey -> exampleDir|toFilename fkey|".tmp";
     stderr << "--making example input files in " << exampleDir << endl;
     makeDirectory exampleDir;
     scan(pairs pkg#"example inputs", (fkey,inputs) -> (
	       inf := infn fkey;
	       val := concatenate apply(inputs, s -> s|"\n");
	       if fileExists inf and get inf === val
	       then (
		    -- stderr << "--leaving example input file for " << fkey << endl;
		    )
	       else (
		    stderr << "--making example input file for " << fkey << endl;
		    inf << val << close;
		    )));

     -- cache raw documentation in database, and check for changes
     rawDocUnchanged := new MutableHashTable;
     docDir := buildDirectory | LAYOUT#"packagedoc" pkg#"title";
     rawdbname := docDir | "rawdocumentation.db";
     rawdbnametmp := rawdbname | ".tmp";
     stderr << "--storing raw documentation in " << rawdbname << endl;
     makeDirectory docDir;
     if fileExists rawdbnametmp then unlink rawdbnametmp;
     if fileExists rawdbname then (
	  tmp := openDatabaseOut rawdbname;   -- just to make sure the database file isn't open for writing
	  copyFile(rawdbname,rawdbnametmp);
	  close tmp;
	  );
     rawdocDatabase := openDatabaseOut rawdbnametmp;
     scan(nodes, tag -> (
	       fkey := DocumentTag.FormattedKey tag;
	       if rawDoc#?fkey then (
	       	    v := toExternalString rawDoc#fkey;
		    if rawdocDatabase#?fkey then (
     	       	    	 if rawdocDatabase#fkey === v 
			 then rawDocUnchanged#fkey = true
			 else rawdocDatabase#fkey = v
			 )
		    else rawdocDatabase#fkey = v
		    )
	       else (
		    if rawdocDatabase#?fkey then (
			 stderr << "--warning: documentation for " << fkey << " is no longer present" << endl;
			 )
		    else rawDocUnchanged#fkey = true
		    )));
     close rawdocDatabase;
     moveFile(rawdbnametmp,rawdbname);
     rawkey := "raw documentation database";
     pkg#rawkey = openDatabase rawdbname;
     addEndFunction(() -> if pkg#?rawkey and isOpen pkg#rawkey then close pkg#rawkey);

     -- make example output files
     stderr << "--making example result files in " << exampleDir << endl;
     haderror := false;
     scan(pairs pkg#"example inputs", (fkey,inputs) -> (
	       inf := infn fkey;
	       outf := outfn fkey;
	       tmpf := tmpfn fkey;
	       if fileExists outf and fileTime outf >= fileTime inf
	       then (
		    -- stderr << "--leaving example results file for " << fkey << endl
		    )
	       else (
		    remove(rawDocUnchanged,fkey);
		    stderr << "--making example results file for " << fkey << endl;
		    loadargs := if pkg === Macaulay2 then "" else "-e 'load \""|fn|"\"'";
		    cmd := "ulimit -t 20 -v 60000; " | commandLine#0 | " --silent --print-width 80 --stop --int -e errorDepth=0 -q " | loadargs | " <" | inf | " >" | tmpf | " 2>&1";
		    stderr << cmd << endl;
		    r := run cmd;
		    if r != 0 then (
			 stderr << "--error return code: (" << r//256 << "," << r%256 << ")" << endl;
			 stderr << "--example error output visible in file: " << tmpf << endl;
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
			 moveFile(tmpf,outf);
			 ));
	       -- read, separate, and store example output
	       if fileExists outf then pkg#"example results"#fkey = drop(separateM2output get outf,-1)
	       else stderr << "--warning: missing file " << outf << endl;
	       ));
     if haderror and not o.IgnoreExampleErrors then error "error(s) occurred running example files";

     -- process documentation
     stderr << "--processing documentation nodes..." << endl;
     scan(nodes, tag -> (
	       fkey := DocumentTag.FormattedKey tag;
	       if rawDocUnchanged#?fkey then (
	       	    -- stderr << "--skipping   " << tag << endl;
		    )
	       else (
	       	    stderr << "--processing " << tag << endl;
	       	    pkg#"processed documentation"#fkey = documentation tag;
		    );
	       ));

     -- cache processed documentation in database
     dbname := docDir | "documentation.db";
     dbnametmp := dbname | ".tmp";
     if fileExists dbnametmp then unlink dbnametmp;
     if fileExists dbname then (
	  tmp2 := openDatabaseOut dbname;   -- just to make sure the database file isn't open for writing
	  copyFile(dbname,dbnametmp);
	  close tmp2;
	  );
     stderr << "--storing processed documentation in " << dbname << endl;
     prockey := "processed documentation database";
     if pkg#?prockey and isOpen pkg#prockey then close pkg#prockey;
     docDatabase := openDatabaseOut dbname;
     scan(pairs pkg#"processed documentation", (k,v) -> docDatabase#k = toExternalString v);
     close docDatabase;
     moveFile(dbnametmp,dbname);
     pkg#prockey = openDatabase dbname;
     addEndFunction(() -> if pkg#?prockey and isOpen pkg#prockey then close pkg#prockey);

     -- make table of contents, including next, prev, and up links
     stderr << "--assembling table of contents" << endl;
     assembleTree(pkg,nodes);
     pkg#"table of contents" = new Bag from {CONT}; -- we bag it because it might be big!
     pkg#"links up" = UP;
     pkg#"links next" = NEXT;
     pkg#"links prev" = PREV;

     -- make info file
     if o.MakeInfo then (
	  savePW := printWidth;
	  printWidth = 79;
	  infodir := buildDirectory|LAYOUT#"info";
	  makeDirectory infodir;
	  infotitle := pkg#"title";
	  infobasename := infotitle|".info";
	  tmpinfobasename := infobasename|".tmp";
	  infofile := openOut (infodir|tmpinfobasename);
	  stderr << "--making info file in " << infofile << endl;
	  upto30 := t -> concatenate(t,30-#t:" ");
	  infofile << "This is " << infobasename << ", produced by Macaulay 2, version " << version#"VERSION" << endl << endl;
	  infofile << "INFO-DIR-SECTION " << pkg.Options.InfoDirSection << endl;
	  infofile << "START-INFO-DIR-ENTRY" << endl;
	  infofile << upto30 concatenate( "* ", infotitle, ": (", infotitle, ").") << "  ";
	  infofile << (if pkg.Options.Headline =!= null then pkg.Options.Headline else infotitle | ", a Macaulay 2 package") << endl;
	  infofile << "END-INFO-DIR-ENTRY" << endl << endl;
	  byteOffsets := new MutableHashTable;
	  topNodeName := DocumentTag.FormattedKey topDocumentTag;
	  chk := if topNodeName === "Top" then identity else n -> if n === "Top" then error "encountered a documentation node named 'Top'";
	  infoTagConvert' := n -> if n === topNodeName then "Top" else infoTagConvert n;
	  getPDoc := fkey -> (
	       if pkg#"processed documentation"#?fkey then pkg#"processed documentation"#fkey else
	       if pkg#"processed documentation database"#?fkey then value pkg#"processed documentation database"#fkey else (
		    stderr << "--warning: missing documentation node: " << fkey << endl;
		    ));
	  traverse(unbag pkg#"table of contents", tag -> (
		    key := DocumentTag.Key tag;
		    fkey := DocumentTag.FormattedKey tag;
		    chk fkey;
		    byteOffsets# #byteOffsets = concatenate("Node: ",infoTagConvert' fkey,"\177",toString length infofile);
		    infofile << "\037" << endl << "File: " << infobasename << ", Node: " << infoTagConvert' fkey;
		    if NEXT#?tag then infofile << ", Next: " << infoTagConvert' DocumentTag.FormattedKey NEXT#tag;
		    if PREV#?tag then infofile << ", Prev: " << infoTagConvert' DocumentTag.FormattedKey PREV#tag;
		    if UP#?tag   then infofile << ", Up: " << infoTagConvert' DocumentTag.FormattedKey UP#tag;
		    infofile << endl << endl << info getPDoc fkey << endl));
	  infofile << "\037" << endl << "Tag Table:" << endl;
	  scan(values byteOffsets, b -> infofile << b << endl);
	  infofile << "\037" << endl << "End Tag Table" << endl;
	  infofile << close;
	  moveFile(infodir|tmpinfobasename,infodir|infobasename);
	  stderr << "--completed info file moved to " << infodir|infobasename << endl;
	  printWidth = savePW;
	  )
     else (
	  stderr << "--not making info file, as instructed" << endl;
	  );

     -- make postinstall and preremove files, if encap
     if o.Encapsulate then (
	  octal := s -> (n := 0 ; z := first ascii "0"; scan(ascii s, i -> n = 8*n + i - z); n);
	  stderr << "--making postinstall, preremove, and encapinfo files in " << buildDirectory << endl;
	  f := buildDirectory | "postinstall" 
	  << ///#! /bin/sh -e/// << endl
	  << ///cd "$ENCAP_SOURCE/$ENCAP_PKGNAME/info" || exit 0/// << endl
	  << ///for i in *.info/// << endl
	  << ///do (set -x ; install-info --dir-file="$ENCAP_TARGET/info/dir" "$i")/// << endl
	  << ///done/// << endl;
	  fileChangeMode(f,octal "755");
	  f << close;
     	  f = buildDirectory | "preremove"
	  << ///#! /bin/sh -x/// << endl
	  << ///cd "$ENCAP_SOURCE/$ENCAP_PKGNAME/info" || exit 0/// << endl
	  << ///for i in *.info/// << endl
	  << ///do (set -x ; install-info --dir-file="$ENCAP_TARGET/info/dir" --delete "$i")/// << endl
	  << ///done/// << endl;
	  fileChangeMode(f,octal "755");
 	  f << close;
	  f = buildDirectory | "encapinfo"
	  << ///encap 2.0/// << endl
	  << ///contact dan@math.uiuc.edu/// << endl;
	  removeLastSlash := s -> if s#?0 and s#-1 === "/" then substring(s,0,#s-1) else s;
	  scan(("libm2","packagedoc","packageexamples","packagehtml","packageimages","packagesrc","packagetests"),
	       k -> f << "linkdir " << (if class LAYOUT#k === Function then removeLastSlash LAYOUT#k "*" else removeLastSlash LAYOUT#k) << endl);
	  f << close;
	  );

     -- make html files
     htmlDirectory = LAYOUT#"packagehtml" pkg#"title";
     setupButtons();
     makeDirectory (buildDirectory|htmlDirectory);     
     stderr << "--making html pages in " << buildDirectory|htmlDirectory << endl;
     scan(nodes, tag -> (
	  key := DocumentTag.Key tag;
	  fkey := DocumentTag.FormattedKey tag;
	  fn := buildDirectory | htmlFilename tag;
	  if fileExists fn and rawDocUnchanged#?fkey then return;
	  stderr << "--making html page for " << tag << endl;
	  fn
	  << encoding << endl
	  << doctype << endl
	  << html HTML { 
	       HEAD {
		    TITLE {fkey, commentize headline key},
		    style(), links()
		    },
	       BODY { 
		    buttonBar tag,
		    if UP#?tag then SEQ between(" > ", apply(upAncestors tag, i -> TO i)),
		    HR{}, 
		    getPDoc fkey
		    }
	       }
	  << endl << close));

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
     tag := makeDocumentTag key;
     p | "index.html"
     << encoding << endl
     << doctype << endl
     << html HTML { 
	  HEAD {
	       TITLE {key, commentize headline key},
	       style(), links()
	       },
	  BODY { 
	       buttonBar tag,
	       HR{},
	       PARA BOLD "Index of installed packages:",
	       UL apply(r, pkg -> HREF { LAYOUT#"packagehtml" pkg | "index.html", pkg })
	       }
	  } << endl << close
     )

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
