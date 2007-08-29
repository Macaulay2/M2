-- -*- fill-column: 107 -*-
--		Copyright 1993-2002 by Daniel R. Grayson

Macaulay2HomePage := () -> "http://www.math.uiuc.edu/Macaulay2/index-" | version#"VERSION" | ".html"

-----------------------------------------------------------------------------
-- html output
-----------------------------------------------------------------------------

-- maybe we should rename this file to "packages2.m2" after the merge.

-- we've turned off checking for existence of files...

local prefix; local topNodeButton
local nullButton; local masterIndexButton; local tocButton; local homeButton
local NEXT; local PREV; local UP; local tableOfContents; local linkTable; local SRC
local nextButton; local prevButton; local upButton; local backwardButton; local forwardButton
local masterIndex

hadExampleError := false
numExampleErrors := 0;

hadDocumentationWarning := false
numDocumentationWarnings := 0;

hadDocumentationError := false

seenit := new MutableHashTable

chkdoc := true
signalDocError = tag -> (				    -- also called from document.m2, temporarily
     not seenit#?tag
     and chkdoc
     and (numDocumentationWarnings = numDocumentationWarnings + 1;
	  seenit#tag = hadDocumentationWarning = true))

buildPackage := null					    -- name of the package currently being built
topDocumentTag := null
topFileName := "index.html"				    -- top node's file name, constant
indexFileName := "master.html"  			    -- file name for master index of topics in a package
tocFileName := "toc.html"       			    -- file name for the table of contents of a package
buildDirectory := "/tmp/"				    -- the root of the relative paths:
htmlDirectory := ""					    -- relative path to the html directory, depends on the package
installDirectory := ""					    -- absolute path to the install directory

runfun := o -> if instance(o, Function) then o() else o
initInstallDirectory := o -> installDirectory = minimizeFilename(runfun o.InstallPrefix | "/")

-----------------------------------------------------------------------------
-- relative URLs and filenames
-----------------------------------------------------------------------------

absoluteLinks := false

isAbsolute := url -> match( "^(#|/|mailto:|[a-z]+://)", url )

rel := url -> (
     if isAbsolute url 
     then url
     else (
	  -- stderr << "rel : url = " << url << endl
	  -- << "    (prefixDirectory | url) = " << (prefixDirectory | url) << endl
	  -- << "     fileExists (prefixDirectory | url) = " << fileExists (prefixDirectory | url) << endl;
	  if absoluteLinks and class prefixDirectory === String and fileExists (prefixDirectory | url) then (prefixDirectory | url)
     	  else relativizeFilename(htmlDirectory, url)))

htmlFilename = method(Dispatch => Thing)
htmlFilename Thing := x -> htmlFilename makeDocumentTag x
htmlFilename DocumentTag := tag -> (
     fkey := DocumentTag.FormattedKey tag;
     pkgtitle := DocumentTag.Title tag;
     LAYOUT#"packagehtml" pkgtitle | if fkey === pkgtitle then topFileName else toFilename fkey|".html" )
htmlFilename FinalDocumentTag := tag -> (
     fkey := FinalDocumentTag.FormattedKey tag;
     pkgtitle := FinalDocumentTag.Title tag;
     LAYOUT#"packagehtml" pkgtitle | if fkey === pkgtitle then topFileName else toFilename fkey|".html" )

html IMG  := x -> (
     (o,cn) := override(IMG.Options,toSequence x);
     if o#"alt" === null then error ("IMG item is missing alt attribute");
     concatenate("<img src=", format rel o#"src", " alt=", format o#"alt", "/>"))

html HREF := x -> (
     r := html last x;
     if match("^ +$",r) then r = #r : "&nbsp;&nbsp;";
     concatenate("<a href=\"", rel first x, "\">", r, "</a>")
     )
tex  HREF := x -> concatenate("\\special{html:<a href=\"", texLiteral rel first x, "\">}", tex last x, "\\special{html:</a>}")
html TO   := x -> (
     r := htmlLiteral DocumentTag.FormattedKey x#0;
     if match("^ +$",r) then r = #r : "&nbsp;&nbsp;";
     concatenate( "<a href=\"", rel htmlFilename getPrimary x#0, "\" title=\"", headline x#0, "\">", r, "</a>", if x#?1 then x#1))
html TO2  := x -> concatenate("<a href=\"", rel htmlFilename getPrimary x#0, "\">", htmlLiteral x#1, "</a>")

next := tag -> ( if NEXT#?tag then HREF { htmlFilename NEXT#tag, nextButton } else nextButton, " | ")
prev := tag -> ( if PREV#?tag then HREF { htmlFilename PREV#tag, prevButton } else prevButton, " | ")
up   := tag -> ( if   UP#?tag then HREF { htmlFilename   UP#tag,   upButton } else upButton  , " | ")

FIRST := tag -> (while PREV#?tag do tag = PREV#tag; tag)
LAST  := tag -> (while NEXT#?tag do tag = NEXT#tag; tag)

FORWARD0  := tag -> if NEXT#?tag then NEXT#tag else if UP#?tag then FORWARD0 UP#tag
FORWARD   := tag -> if linkTable#?tag and length linkTable#tag > 0 then          first linkTable#tag else FORWARD0 tag
BACKWARD0 := tag -> if linkTable#?tag and length linkTable#tag > 0 then BACKWARD0 last linkTable#tag else tag
BACKWARD  := tag -> if PREV#?tag then BACKWARD0 PREV#tag else if UP#?tag then UP#tag

forward  := tag -> ( f := FORWARD  tag; ( if f =!= null then HREF { htmlFilename f, forwardButton } else forwardButton , " | "))
backward := tag -> ( b := BACKWARD tag; ( if b =!= null then HREF { htmlFilename b, backwardButton} else backwardButton, " | "))

linkTitle := s -> concatenate( " title=\"", s, "\"" )
linkTitleTag := tag -> "title" => concatenate(DocumentTag.FormattedKey tag, commentize headline tag)
links := tag -> (
     f := FORWARD tag;
     b := BACKWARD tag;
     nonnull splice (
	  if topDocumentTag =!= null then LINK { "href" => rel htmlDirectory|topFileName, "rel" =>"Top", linkTitleTag topDocumentTag },
	  LINK { "href" => rel htmlDirectory|indexFileName, "rel" => "Index" },
	  LINK { "href" => rel htmlDirectory|tocFileName,   "rel" => "Table-of-Contents" },
	  LINK { "href" => rel Macaulay2HomePage(), "rel" => "Macaulay-2-Home-Page" },
	  if f =!= null then LINK { "href" => rel htmlFilename f, "rel" => "Next", linkTitleTag f},
	  if b =!= null then LINK { "href" => rel htmlFilename b, "rel" => "Previous", linkTitleTag b},
	  if NEXT#?tag then (
	       LINK { "href" => rel htmlFilename NEXT#tag, "rel" => "Forward", linkTitleTag NEXT#tag},
	       LINK { "href" => rel htmlFilename LAST tag, "rel" => "Last", linkTitleTag LAST tag}
	       ),
	  if PREV#?tag then (
	       LINK { "href" => rel htmlFilename PREV#tag, "rel" => "Backward", linkTitleTag PREV#tag},
	       LINK { "href" => rel htmlFilename FIRST tag, "rel" => "First", linkTitleTag FIRST tag},
	       ),
	  if UP#?tag then LINK { "href" => rel htmlFilename UP#tag, "rel" => "Up", linkTitleTag UP#tag},
	  LINK { "href" => rel LAYOUT#"packagesrc" "Style" | "doc.css", "rel" => "stylesheet", "type" => "text/css" },
	  LINK { "href" => rel LAYOUT#"packagesrc" "Style" | "doc-no-buttons.css", "rel" => "alternate stylesheet", "title" => "no buttons", "type" => "text/css" },
	  if SRC#?tag then (
     	       LINK { 
		    "href" => concatenate("file://", toAbsolutePath SRC#tag#0), 
		    "rel" => concatenate("Source (see text above line ", toString SRC#tag#1, ")"),
		    "type" => "text/plain" } ) ) )

BUTTON := (s,alt) -> (
     s = rel s;
     if alt === null
     then error "required attribute: ALT"
     else IMG("src" => s, "alt" => concatenate("[",alt,"]")))

html HTML := t -> concatenate(
///<?xml version="1.0" encoding="us-ascii" ?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1 plus MathML 2.0 plus SVG 1.1//EN"	 "http://www.w3.org/Math/DTD/mathml2/xhtml-math11-f.dtd" >
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en">
///,
     apply(t,html), 
     "</html>\n"
     )

-- produce html form of documentation, for Macaulay 2 and for packages

buttonBar := (tag) -> ButtonTABLE {{ 
	  DIV splice {
     	       forward tag,
	       backward tag,
	       next tag,
	       prev tag,
	       up tag,
     	       (if tag =!= topDocumentTag then topNodeButton else topNodeButton#-1, " | "),
     	       masterIndexButton, " | ",
     	       tocButton, " | ",
     	       homeButton
	       }}}

upAncestors := tag -> reverse (
     n := 0;
     prepend(tag, while UP#?tag and n < 20 list (n = n+1; tag = UP#tag)))

commentize := s -> if s =!= null then concatenate(" -- ",s)

-----------------------------------------------------------------------------

checkIsTag := tag -> ( assert(class tag === DocumentTag); tag )

alpha := characters "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
indAl := new HashTable from apply(#alpha, i -> alpha#i => i)
numAnchorsMade := 0
makeAnchors := n -> (
     ret := SPAN apply(take(alpha,{numAnchorsMade,n-1}), c -> ANCHOR{ "id" => c, ""});
     numAnchorsMade = n;
     ret)
anchorsUpTo := entry -> if alpha#?numAnchorsMade and entry >= alpha#numAnchorsMade then makeAnchors length select(alpha, c -> entry >= c)
remainingAnchors := () -> makeAnchors (#alpha)

packageTagList := (pkg,topDocumentTag) -> checkIsTag \ unique join(
     apply(
     	  select(pairs pkg.Dictionary,(nam,sym) -> not match ( "\\$" , nam )),
	  (nam,sym) -> makeDocumentTag(sym, Package => pkg)),
     select(
	  apply(
	       values pkg#"raw documentation",
	       doc -> doc.DocumentTag),
	  x -> x =!= null),
     { topDocumentTag }
     )

-----------------------------------------------------------------------------
-- constructing the tree-structure for the documentation nodes in a package
-----------------------------------------------------------------------------

-- make this first:
linkTable = new MutableHashTable			    -- keys are DocumentTags for a node, values are lists of DocumentTags of descendents

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
toDoc TreeNode := x -> SPAN { TOH checkIsTag x#0, toDoc x#1 }

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
		    -- this kind of structural problem is bad because it can make circular structures in the NEXT and PREV links
		    hadDocumentationError = true
		    );
	       new TreeNode from { x , new ForestNode}
	       )
     	  else new TreeNode from { x, new ForestNode from apply(linkTable#x,makeTree)})
     else (
	  if not missingReferences#?x then (
	       missingReferences#x = true;
	       if chkdoc then stderr << "--warning: missing reference to documentation as subnode: " << x << endl;
	       );
	  new TreeNode from { x , new ForestNode}
	  ))
makeForest := x -> new ForestNode from makeTree \ x

leaves := () -> keys set flatten values linkTable
roots := () -> (
     x := keys ( set keys linkTable - set leaves() );
     if not member(topDocumentTag,x) then stderr << "--warning: top node " << topDocumentTag << " not a root" << endl;
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
		    tag => getPrimary \ first \ select(if doc.?Subnodes then toList doc.Subnodes else {}, x -> class x === TO))
	       else (
		    tag => {}
		    )
	       ));
     tableOfContents = getTrees();
     if hadDocumentationError then error ("documentation errors occurred");
     buildLinks tableOfContents;
     )

-----------------------------------------------------------------------------
-- making the html pages
-----------------------------------------------------------------------------
setupButtons := () -> (
     topNodeButton = HREF {htmlDirectory|topFileName, "top" };
     tocButton = HREF {htmlDirectory|tocFileName, "toc"};
     homeButton = HREF {Macaulay2HomePage (), "home"};
     nullButton = "";
     masterIndexButton = HREF {htmlDirectory|indexFileName,"index"};
     forwardButton = "next";
     backwardButton = "previous";
     nextButton = "forward";
     prevButton = "backward";
     upButton = "up";
     )

separateExampleOutput = r -> (
     while r#0 == "\n" do r = substring(1,r);
     while r#-1 == "\n" do r = substring(0,#r-1,r);
     separateRegexp("(\n\n)i+[1-9][0-9]* : ",1,r))

capture = method()
capture String := s -> (
     (err,out) := internalCapture s;
     (err,out,separateExampleOutput out))

-----------------------------------------------------------------------------
-- installing packages -- eventually to be merged with 
-- the code above for making html for Macaulay 2 itself
-----------------------------------------------------------------------------

makeMasterIndex := keylist -> (
     numAnchorsMade = 0;
     fn := buildDirectory | htmlDirectory | indexFileName;
     title := "Symbol Index";
     stderr << "--making  '" << title << "' in " << fn << endl;
     r := HTML {
	  HEAD splice { TITLE title, links() },
	  BODY {
	       DIV { topNodeButton, " | ", tocButton, " | ", homeButton },
	       HR{},
	       HEADER1 title,
	       DIV between(LITERAL "&nbsp;&nbsp;&nbsp;",apply(alpha, c -> HREF {"#"|c, c})), 
	       UL apply(sort keylist, (tag) -> (
			 checkIsTag tag;
			 anch := anchorsUpTo tag;
			 if anch === null then LI TOH tag else LI {anch, TOH tag})),
	       DIV remainingAnchors()
	       }};
     validate r;
     fn << html r << endl << close
     )

maketableOfContents := () -> (
     fn := buildDirectory | htmlDirectory | tocFileName;
     title := DocumentTag.FormattedKey topDocumentTag | " : Table of Contents";
     stderr << "--making  '" << title << "' in " << fn << endl;
     fn
     << html HTML {
	  HEAD splice { TITLE title, links() },
	  BODY {
	       DIV { topNodeButton, " | ", masterIndexButton, " | ", homeButton },
	       HR{},
	       HEADER1 title,
	       toDoc tableOfContents
	       }
	  } << endl << close
     )

utest := opt -> (
     cmd := "ulimit " | opt | "; ";
     if run("2>/dev/null >/dev/null "|cmd) == 0 then cmd else ""
     )
ulimit := null

M2errorRegexp := "^[^:\n]+:[0-9]+:[0-9]+:\\([0-9]+\\): "
aftermatch := (pat,str) -> (
     m := regex(pat,str);
     if m === null then "" else substring(m#0#0,str))

runFile := (inf,outf,tmpf,desc,pkg,announcechange,rundir,usermode) -> ( -- return false if error
     announcechange();
     stderr << "--making " << desc << " in file " << outf << endl;
     if fileExists outf then removeFile outf;
     pkgname := toString pkg;
     ldpkg := if pkgname != "Macaulay2" then concatenate("-e 'loadPackage(\"",pkgname,"\", FileName => \"",pkg#"source file","\")'") else "";
     args := "--silent --print-width 77 --stop --int" | (if usermode then "" else " -q") | " " | ldpkg;
     cmdname := commandLine#0;
     if ulimit === null then (
	  ulimit = utest " -t 80" | utest " -m 200000"| utest " -v 200000";
	  );
     tmpf << "-- -*- M2-comint -*-" << endl << close;
     cmd := ulimit | "cd " | rundir | "; time " | cmdname | " " | args | " <" | format inf | " >>" | format tmpf | " 2>&1";
     stderr << cmd << endl;
     r := run cmd;
     if r == 0 then (
	  moveFile(tmpf,outf);
	  return true;
	  )
     else (
	  if r == 2 then (
	       stderr << "subprocess interrupted with INT, exiting, too" << endl;
	       removeFile tmpf;
	       exit r;
	       );
	  stderr << tmpf << ":0: (output file) error return code: (" << r//256 << "," << r%256 << ")" << endl;
	  stderr << inf  << ":0: (input file)" << endl;
	  stderr << aftermatch(M2errorRegexp,get tmpf);
	  if r == 131 then (
	       stderr << "subprocess terminated abnormally, exiting" << endl;
	       exit r;
	       );
	  hadExampleError = true;
	  numExampleErrors = numExampleErrors + 1;
	  return false;
	  ))

runString := (x,pkg,rundir,usermode) -> (
     tfn := temporaryFileName();
     inf := tfn | ".m2";
     tmpf := tfn | ".tmp";
     outf := tfn | ".out";
     rm := fn -> if fileExists fn then removeFile fn;
     rmall := () -> rm \ {inf, tmpf, outf};
     inf << x << endl << close;
     ret := runFile(inf,outf,tmpf,"test results",pkg,t->t,rundir,usermode);
     if ret then (rm inf; rm outf;);
     ret)

check = method()
prep = pkg -> (
     use pkg;
     if pkg#?"documentation not loaded" then pkg = loadPackage(pkg#"title", LoadDocumentation => true);
     hadExampleError = false;
     numExampleErrors = 0;
     )
onecheck = (seqno,pkg) -> (
     usermode := false;					    -- fix this later as an option to "check" or something!
     (filename,lineno,s) := pkg#"test inputs"#seqno;
     stderr << "--running test " << seqno << " of package " << pkg << " on line " << lineno << " in file " << filename << endl;
     stderr << "--    rerun with: check_" << seqno << " \"" << pkg << "\"" << endl;
     runString(s,pkg,".",usermode);
     )
check(ZZ,Package) := (seqno,pkg) -> (
     prep pkg;
     onecheck(seqno,pkg);
     if hadExampleError then error("error occurred running test for package ", toString pkg, ": ", toString seqno);
     )
check(ZZ,String) := (seqno,pkg) -> check(seqno, needsPackage (pkg, LoadDocumentation => true))
check Package := pkg -> (
     prep pkg;
     scan(keys pkg#"test inputs", seqno -> onecheck(seqno,pkg));
     if hadExampleError then error(toString numExampleErrors, " error(s) occurred running tests for package ", toString pkg);
     )
check String := pkg -> check needsPackage (pkg, LoadDocumentation => true)

setupNames := (opts,pkg) -> (
     buildPackage = pkg#"title";
     buildDirectory = minimizeFilename(runfun opts.PackagePrefix | "/");
     if opts.Encapsulate then buildDirectory = buildDirectory|buildPackage|"-"|pkg.Options.Version|"/";
     )

installPackage = method(Options => { 
	  PackagePrefix => () -> applicationDirectory() | "encap/",
          InstallPrefix => () -> applicationDirectory() | "local/",
	  UserMode => false,
	  Encapsulate => true,
	  IgnoreExampleErrors => false,
	  FileName => null,
	  CheckDocumentation => true,
	  MakeDocumentation => true,
	  MakeInfo => true,
	  RemakeAllDocumentation => true,		    -- until we get better dependency graphs between documentation nodes, "false" here will confuse users
	  RerunExamples => false,
	  AbsoluteLinks => false,
	  MakeLinks => true,
	  DebuggingMode => false
	  })
uninstallPackage = method(Options => { 
	  PackagePrefix => () -> applicationDirectory() | "encap/",
          InstallPrefix => () -> applicationDirectory() | "local/",
	  Encapsulate => true,
	  MakeLinks => true
	  })
uninstallPackage String := opts -> pkg -> (
     if not opts.Encapsulate then error "uninstallPackage: can't uninstall with Encapsulate => false";
     if not match("^[a-zA-Z0-9]+$",pkg) then error( "package title not alphanumeric: ",pkg);
     buildDirectory := minimizeFilename(runfun opts.PackagePrefix | "/");
     installDirectory := minimizeFilename(runfun opts.InstallPrefix | "/");
     rex := "^" | pkg | "-";
     scan(readDirectory buildDirectory, dir -> if match(rex,dir) then (
	       dir = buildDirectory|dir|"/";
	       enc := dir|"encapinfo";
	       if not fileExists enc then error ("expected package to contain file: ",enc);
	       symlinkDirectory(dir, installDirectory, Verbose => debugLevel > 0, Undo => true);
	       scan(reverse findFiles dir, fn -> if isDirectory fn then removeDirectory fn else removeFile fn);
	       ));
     )

installPackage String := opts -> pkg -> (
     if pkg =!= "Macaulay2" then needsPackage "Macaulay2";  -- load the core documentation
     -- we load the package even if it's already been loaded, because even if it was loaded with
     -- its documentation the first time, it might have been loaded at a time when the core documentation
     -- in the "Macaulay2" package was not yet loaded
     pkg = loadPackage(pkg, DebuggingMode => opts.DebuggingMode, LoadDocumentation => opts.MakeDocumentation, FileName => opts.FileName);
     installPackage(pkg, opts);
     )

dispatcherMethod := m -> m#-1 === Sequence and (
     f := lookup m;
     any(dispatcherFunctions, g -> functionBody f === functionBody g))

installPackage Package := opts -> pkg -> (
     use pkg;
     chkdoc = opts.CheckDocumentation;			    -- oops, this will have a lingering effect...

     if opts.MakeDocumentation and pkg#?"documentation not loaded"
     then pkg = loadPackage(pkg#"title", DebuggingMode => opts.DebuggingMode, LoadDocumentation => true, FileName => opts.FileName);

     absoluteLinks = opts.AbsoluteLinks;
     if class absoluteLinks =!= Boolean then error "expected true or false for option AbsoluteLinks"; 
     oldpkg := currentPackage;
     currentPackage = pkg;
     topDocumentTag = makeDocumentTag(pkg#"title", Package => pkg);
     
     -- here's where we get the list of nodes from the raw documentation
     nodes := if opts.MakeDocumentation then packageTagList(pkg,topDocumentTag) else {};
     
     setupNames(opts,pkg);
     initInstallDirectory opts;
     
     stderr << "--installing package " << pkg << " in " << buildDirectory << endl;
     
     currentSourceDir := pkg#"source directory";
     stderr << "--using package sources found in " << currentSourceDir << endl;

     -- copy package source file
     pkgDirectory := LAYOUT#"packages";
     makeDirectory (buildDirectory|pkgDirectory);
     bn := buildPackage | ".m2";
     fn := currentSourceDir|bn;
     if not fileExists fn then error("file ", fn, " not found");
     copyFile(fn, buildDirectory|pkgDirectory|bn, Verbose => debugLevel > 5);

     excludes := Exclude => {"^CVS$", "^\\.svn$"};

     if pkg === Core then (
	  ) else (
     	  
	  -- copy package source subdirectory
	  srcDirectory := LAYOUT#"packagesrc" pkg#"title";
	  dn := realpath(currentSourceDir|buildPackage);
	  if fileExists dn
	  then (
	       stderr << "--copying auxiliary source files from " << dn << endl;
	       makeDirectory (buildDirectory|srcDirectory);
	       buildDirectory|srcDirectory|".linkdir" << close;
	       copyDirectory(dn, buildDirectory|srcDirectory, UpdateOnly => true, Verbose => debugLevel > 0, excludes);
	       );
     	  );

     -- copy package source subdirectory examples
     exampleDir := buildDirectory|LAYOUT#"packageexamples" pkg#"title";

     if opts.MakeDocumentation then (
	  pkg#"package prefix" = buildDirectory;

	  -- copy package doc subdirectory if we loaded the package from a distribution
     	  -- ... to be implemented, but we seem to be copying the examples already, but only partially

	  -- make example input files
	  infn := fkey -> exampleDir|toFilename fkey|".m2";
	  outfn := fkey -> exampleDir|toFilename fkey|".out";
	  tmpfn := fkey -> exampleDir|toFilename fkey|".errors";
	  stderr << "--making example input files in " << exampleDir << endl;
	  makeDirectory exampleDir;
	  exampleDir|".linkdir" << close;
	  exampleInputFiles := new MutableHashTable;
	  scan(pairs pkg#"example inputs", (fkey,inputs) -> (
		    inf := infn fkey;
		    exampleInputFiles#inf = true;
		    val := concatenate apply(inputs, s -> s|"\n");
		    if fileExists inf and get inf === val
		    then (
			 if debugLevel > 0 then stderr << "--leaving example input file for " << fkey << endl;
			 )
		    else (
			 if debugLevel > 0 then stderr << "--making example input file for " << fkey << endl;
			 inf << val << close;
			 )));

	  -- check for obsolete example input and output files and remove them
	  if opts.CheckDocumentation then
	  scan(readDirectory exampleDir, fn -> (
		    fn = exampleDir | fn;
		    if match("\\.out$",fn) and not exampleInputFiles#?(replace("\\.out$",".m2",fn)) then (
			 stderr << "--warning: removing obsolete example output file: " <<  fn << endl;
			 removeFile fn;
			 );
		    if match("\\.m2$",fn) and not exampleInputFiles#?fn then (
			 stderr << "--warning: removing obsolete example input file: " <<  fn << endl;
			 removeFile fn;
			 );
		    ));

	  -- cache raw documentation in database, and check for changes
	  rawDocUnchanged := new MutableHashTable;
	  docDir := pkg#"package prefix" | LAYOUT#"packagecache" pkg#"title";
	  rawdbname := docDir | "rawdocumentation" | databaseSuffix;
	  rawdbnametmp := rawdbname | ".tmp";
	  stderr << "--storing raw documentation in " << rawdbname << endl;
	  makeDirectory docDir;
	  docDir|".linkdir" << close;
	  if fileExists rawdbnametmp then removeFile rawdbnametmp;
	  if fileExists rawdbname then (
	       tmp := openDatabase rawdbname;   -- just to make sure the database file isn't open for writing
	       copyFile(rawdbname,rawdbnametmp);
	       close tmp;
	       );
	  rawdocDatabase := openDatabaseOut rawdbnametmp;
	  rawDoc := pkg#"raw documentation";
	  -- remove any keys from the processed database no longer used
	  scan(keys rawdocDatabase - set keys rawDoc, key -> remove(rawdocDatabase,key));
	  scan(nodes, tag -> (
		    fkey := DocumentTag.FormattedKey tag;
		    if rawDoc#?fkey then (
			 v := toExternalString rawDoc#fkey;
			 if rawdocDatabase#?fkey then (
			      if rawdocDatabase#fkey === v 
			      then rawDocUnchanged#fkey = true
			      else rawdocDatabase#fkey = v
			      )
			 else (
			      if debugLevel > 0 then stderr << "--new raw documentation, not already in database, for " << fkey << endl;
			      rawdocDatabase#fkey = v;
			      )
			 )
		    else (
			 if rawdocDatabase#?fkey then (
			      stderr << "--warning: raw documentation for " << fkey << ", in database, is no longer present" << endl;
			      )
			 else (
			      rawDocUnchanged#fkey = true;
			      )
			 )));
	  close rawdocDatabase;

	  -- run tests that are functions
	  stderr << "--running tests that are functions " << exampleDir << endl;
	  scan(pairs pkg#"test inputs", (key,str) -> if instance(str, Function) then (
		    stderr << "--  running test " << key << ", function " << str << endl;
		    str();
		    ));

     	  -- Make sure the processed documentation database exists, even if empty, so when running the examples,
	  -- M2 doesn't read in all the documentation sources each time.  For the package Macaulay2 this makes a big
	  -- difference.
	  dbname := docDir | "documentation" | databaseSuffix;
     	  if opts.RemakeAllDocumentation and fileExists dbname then removeFile dbname;
     	  if not fileExists dbname then (
	       stderr << "--creating empty database for processed documentation in " << dbname << endl;
	       close openDatabaseOut dbname;
	       );

	  -- make example output files, or else copy them from old package directory tree
	  exampleDir' := realpath(currentSourceDir|buildPackage|"/examples") | "/";
	  infn' := fkey -> exampleDir'|toFilename fkey|".m2";
	  outfn' := fkey -> exampleDir'|toFilename fkey|".out";
	  stderr << "--making example result files in " << exampleDir << endl;
	  hadExampleError = false;
	  numExampleErrors = 0;
	  scan(pairs pkg#"example inputs", (fkey,inputs) -> (
		    -- args:
		    inf := infn fkey;
		    outf := outfn fkey;
		    inf' := infn' fkey;
		    outf' := outfn' fkey;
		    tmpf := tmpfn fkey;
		    desc := "example results for " | fkey;
		    changefun := () -> remove(rawDocUnchanged,fkey);
		    if not opts.RerunExamples and fileExists outf and fileTime outf >= fileTime inf then (
			 -- do nothing
			 )
		    else if (
			 not opts.RerunExamples 
			 and inf != inf' 
			 and fileExists inf' 
			 and fileExists outf' 
			 and fileTime outf' >= fileTime inf' 
			 and get inf == get inf'
			 and not fileExists tmpf
			 )
		    then (
			 copyFile(outf',outf);
			 )
		    else (
			 runFile(inf,outf,tmpf,desc,pkg,changefun,".",opts.UserMode);
			 );
		    -- read, separate, and store example output
		    if fileExists outf then pkg#"example results"#fkey = drop(separateM2output get outf,-1)
		    else (
			 if debugLevel > 1 then stderr << "--warning: missing file " << outf << endl;
			 )
		    ));

 	  if not opts.IgnoreExampleErrors 
	  then if hadExampleError then error(toString numExampleErrors, " error(s) occurred running example files");

	  -- process documentation
	  stderr << "--processing documentation nodes..." << endl;
     	  SRC = new MutableHashTable;
	  scan(nodes, 
	       tag -> if isUndocumented tag then (
		    if debugLevel > 0 then stderr << "--undocumented " << tag << endl;
		    )
	       else if isSecondary tag then (
		    if debugLevel > 0 then stderr << "--is secondary " << tag << endl;
		    )
	       else (
		    fkey := DocumentTag.FormattedKey tag;
		    if not opts.RemakeAllDocumentation and rawDocUnchanged#?fkey then (
			 if debugLevel > 0 then stderr << "--skipping     " << tag << endl;
			 )
		    else (
			 if debugLevel > 0 then stderr << "--processing   " << tag << endl;
			 pkg#"processed documentation"#fkey = help tag;
			 -- get source filename and linenum, too:
			 if pkg#"raw documentation"#?fkey then (
			      doc := pkg#"raw documentation"#fkey;
			      SRC#tag = (doc#"filename",doc#"linenum");
			      );
			 );
		    )
	       );

	  -- cache processed documentation in database
	  dbnametmp := dbname | ".tmp";
	  if fileExists dbnametmp then removeFile dbnametmp;
	  if fileExists dbname then (
	       tmp2 := openDatabase dbname;   -- just to make sure the database file isn't open for writing
	       copyFile(dbname,dbnametmp);
	       close tmp2;
	       );
	  stderr << "--storing processed documentation in " << dbname << endl;
	  prockey := "processed documentation database";
	  if pkg#?prockey and isOpen pkg#prockey then close pkg#prockey;
	  docDatabase := openDatabaseOut dbnametmp;
	  scan(pairs pkg#"processed documentation", (k,v) -> docDatabase#k = toExternalString v);
	  close docDatabase;
	  shield (
	       moveFile(dbnametmp,dbname);
	       moveFile(rawdbnametmp,rawdbname);
	       );
	  rawkey := "raw documentation database";
	  pkg#prockey = openDatabase dbname;
	  pkg#rawkey = openDatabase rawdbname;
	  addEndFunction(() -> if pkg#?rawkey and isOpen pkg#rawkey then close pkg#rawkey);
	  addEndFunction(() -> if pkg#?prockey and isOpen pkg#prockey then close pkg#prockey);

	  -- make table of contents, including next, prev, and up links
	  stderr << "--assembling table of contents" << endl;
	  assembleTree(pkg,getPrimary \ select(nodes,tag -> not isUndocumented tag)); -- sets tableOfContents
	  -- if chkdoc then stderr << "+++++" << endl << "table of contents, in tree form:" << endl << tableOfContents << endl << "+++++" << endl;
	  pkg#"table of contents" = Bag {tableOfContents}; -- we bag it because it might be big!
	  pkg#"links up" = UP;
	  pkg#"links next" = NEXT;
	  pkg#"links prev" = PREV;

     	  -- check that everything is documented
	  if opts.CheckDocumentation then (
	       seenit = new MutableHashTable;
	       hadDocumentationWarning = false;
	       numDocumentationWarnings = 0;
	       scan((if pkg#"title" == "Macaulay2" then Core else pkg)#"exported symbols", s -> (
			 tag := makeDocumentTag s;
			 if not isUndocumented tag and not hasDocumentation s and signalDocError tag then stderr << "--warning: symbol has no documentation: " << tag << endl;
			 f := value s;
			 if instance(f, Function) then (
			      scan(methods f, m -> if isDocumentableMethod m then (
					tag := makeDocumentTag m;
					if not isUndocumented tag and not dispatcherMethod m and not hasDocumentation m and signalDocError tag
					then stderr << "--warning: method has no documentation: " << tag << ", key: " << DocumentTag.Key tag << endl;
					));
			      ))));

	  if hadDocumentationWarning then
	  stderr << "--warning: " << numDocumentationWarnings << " warning" 
	  << (if numDocumentationWarnings > 1 then "(s)" else "")
     	  << " occurred in documentation for package " << pkg << endl;

	  -- helper routine
	  getPDoc := fkey -> (
	       if pkg#"processed documentation"#?fkey then pkg#"processed documentation"#fkey else
	       if pkg#"processed documentation database"#?fkey then value pkg#"processed documentation database"#fkey else (
		    if debugLevel > 0 then stderr << "--warning: missing documentation node: " << fkey << endl;
		    ));

	  -- make info file
	  if opts.MakeInfo then (
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
	       traverse(unbag pkg#"table of contents", tag -> (
			 fkey := DocumentTag.FormattedKey tag;
			 chk fkey;
			 byteOffsets# #byteOffsets = concatenate("Node: ",infoTagConvert' fkey,"\177",toString fileLength infofile);
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
	       stderr << "--not making info file" << endl;
	       );

	  -- make html files
	  htmlDirectory = LAYOUT#"packagehtml" pkg#"title";
	  setupButtons();
	  makeDirectory (buildDirectory|htmlDirectory);
	  buildDirectory|htmlDirectory|".linkdir" << close;
	  stderr << "--making html pages in " << buildDirectory|htmlDirectory << endl;
	  scan(nodes, tag -> if not isUndocumented tag then (
	       -- key := DocumentTag.Key tag;
	       fkey := DocumentTag.FormattedKey tag;
	       fn := buildDirectory | htmlFilename tag;
	       if fileExists fn and not opts.RemakeAllDocumentation and rawDocUnchanged#?fkey then return;
	       if isSecondary tag then return;
	       if debugLevel > 0 then stderr << "--making html page for " << tag << endl;
	       fn
	       << html HTML { 
		    HEAD splice {
			 TITLE {fkey, commentize headline fkey}, -- I hope this works...
			 links tag
			 },
		    BODY { 
			 buttonBar tag,
			 if UP#?tag then DIV between(" > ", apply(upAncestors tag, i -> TO i)),
			 HR{}, 
			 getPDoc fkey
			 }
		    }
	       << endl << close));

	  -- make master.html with master index of all the html files
	  makeMasterIndex select(nodes,tag -> not isUndocumented tag and instance(DocumentTag.Key tag,Symbol));

	  -- make table of contents
	  maketableOfContents();

     	  );						    -- end if opts.MakeDocumentation

     -- make postinstall and preremove files, if encap
     if opts.Encapsulate then (
	  octal := s -> (n := 0 ; z := first ascii "0"; scan(ascii s, i -> n = 8*n + i - z); n);
	  stderr << "--making INSTALL, postinstall, preremove, and encapinfo files in " << buildDirectory << endl;
     	  fix := s -> (
	       s = replace("info/", LAYOUT#"info", s);
	       s = replace("bin/", LAYOUT#"bin", s);
	       s);
	  -- postinstall
	  f := buildDirectory | "postinstall" 
	  << ///#! /bin/sh -e/// << endl
	  << fix ///cd "$ENCAP_SOURCE/$ENCAP_PKGNAME/info/" || exit 0/// << endl
	  << ///for i in *.info/// << endl
	  << fix ///do (set -x ; install-info --info-dir="$ENCAP_TARGET/info/" "$i")/// << endl
	  << ///done/// << endl;
	  if version#"dumpdata" and pkg#"title" == "Macaulay2" then (
	       f << endl << fix "(set -x ; \"$ENCAP_TARGET\"/bin/" << version#"M2 name" << " --stop --dumpdata)" << endl;
	       );
	  fileMode(octal "755",f);
	  f << close;
	  -- preremove
     	  f = buildDirectory | "preremove"
	  << ///#! /bin/sh -x/// << endl
	  << fix ///cd "$ENCAP_SOURCE/$ENCAP_PKGNAME/info/" || exit 0/// << endl
	  << ///for i in *.info/// << endl
	  << fix ///do (set -x ; install-info --info-dir="$ENCAP_TARGET/info/" --remove "$i")/// << endl
	  << ///done/// << endl;
	  fileMode(octal "755",f);
 	  f << close;
	  -- encapinfo
	  f = buildDirectory | "encapinfo"
	  << ///encap 2.0/// << endl
	  << ///contact dan@math.uiuc.edu/// << endl;
	  removeLastSlash := s -> if s#?0 and s#-1 === "/" then substring(s,0,#s-1) else s;
	  scan(("libm2","packagedoc","packageexamples","packagehtml","packageimages","packagesrc","packagetests","libraries"),
	       k -> f << "linkdir " << (if instance(LAYOUT#k, Function) then removeLastSlash LAYOUT#k "*" else removeLastSlash LAYOUT#k) << endl);
	  fileMode(octal "644",f);
	  f << close;
	  -- INSTALL
	  if pkg#"title" == "Macaulay2" then (
	       assert( class installFile === String );
	       f = buildDirectory | "INSTALL"
	       << installFile;
	       fileMode(octal "644",f);
	       f << close;
	       );
	  );

     -- make symbolic links
     if opts.Encapsulate and opts.MakeLinks then (
     	  stderr << "--making symbolic links from \"" << installDirectory << "\" to \"" << buildDirectory << "\"" << endl;
	  symlinkDirectory(buildDirectory, installDirectory, Verbose => debugLevel > 0, Exclude => {"encapinfo", "postinstall", "preremove"})
	  );

     -- all done
     SRC = null;
     stderr << "--installed package " << pkg << " in " << buildDirectory << endl;
     currentPackage = oldpkg;
     if not noinitfile and prefixDirectory =!= null then makePackageIndex();
     )

userMacaulay2Directory := () -> (
     dir := applicationDirectory();
     if not isDirectory dir then (
	  stderr << "--initializing user Macaulay 2 directory \"" << dir << "\"" << endl;
	  makeDirectory(dir);
	  makeDirectory(dir|"encap/");
	  makeDirectory(dir|"local/");
	  makeDirectory(dir|"code/");
	  -- make sample init.m2 file
	  dir|"init.m2" << ///-- This is a sample init.m2 file provided with Macaulay2.
-- It contains Macaulay 2 code and is automatically loaded upon
-- startup of Macaulay2, unless you use the "-q" option.

-- Uncomment the following line to cause Macaulay2 to load "start.m2" in the current working directory upon startup.
-- if fileExists "start.m2" then load(currentDirectory|"start.m2")

-- Uncomment and edit the following lines to add your favorite directories containing Macaulay 2
-- source code files to the load path.  Terminate each directory name with a "/".
-- path = join( { homeDirectory | "src/singularities/", "/usr/local/src/M2/" }, path )

-- Uncomment the following line if you prefer Macaulay2's larger 2-dimensional display form for matrices.
-- compactMatrixForm = false

-- Uncomment and edit the following line if you would like to set the variable kk to your favorite field.
-- kk = ZZ/101

-- Uncomment and edit the following line if you don't need to be informed of the class of a sequence 
-- after it is printed by M2.  This works for other classes, too.
-- Sequence#{Standard,AfterPrint} = Sequence#{Standard,AfterNoPrint} = identity

-- Uncomment and edit the following line to set a default printwidth for situations where M2 doesn't know the width
-- of your terminal.
-- if printWidth == 0 then printWidth = 100

-- Uncomment and edit the following line to preload your favorite package.
-- needsPackage "StateTables"

/// << close;	  
     	  --
     	  absPrefixDirectory := "/" | relativizeFilename ("/", prefixDirectory);
	  -- make dot-emacs file
	  dir|"dot-emacs" << ///
(setq load-path 
      (append
       '( "/// << absPrefixDirectory << ///share/emacs/site-lisp/" )
       load-path
       ))

(load "M2-init" t)

; comment out the following line with an initial semicolon if you want to use your f12 key for something else
(global-set-key [ f12 ] 'M2)

/// << close;
	  -- make dot-profile file
          dir|"dot-profile" << ///
PATH=/// << absPrefixDirectory << ///bin:$PATH
export PATH

MANPATH=/// << absPrefixDirectory << ///man:$MANPATH
export MANPATH

MANPATH=/// << absPrefixDirectory << ///share/man:$MANPATH
export MANPATH

LD_LIBRARY_PATH=/// << absPrefixDirectory << ///lib:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH
/// << close;
	  -- make dot-cshrc file
     	  dir|"dot-cshrc" << ///
setenv PATH /// << absPrefixDirectory << ///bin:$PATH
setenv MANPATH /// << absPrefixDirectory << ///bin:$MANPATH
setenv LD_LIBRARY_PATH /// << absPrefixDirectory << ///lib:$LD_LIBRARY_PATH
/// << close;
	  -- make README file
	  dir|"README" << ///Welcome to Macaulay2!
			     
This directory is used to contain data and code specific to Macaulay2.  For
example, your initialization file, init.m2, is in this directory, and is
automatically loaded upon startup of Macaulay2, unless you use the "-q" option.
You may edit it to meet your needs.

The web browser file "index.html" in this directory contains a list of links to
the documentation of Macaulay2 and its installed packages and is updated every
time you start Macaulay2 (unless you use the "-q" option).  To update it
manually, use "makePackageIndex()".  Point your web browser at that file and
bookmark it.

You may place Macaulay 2 source files in the subdirectory "code/".  It's on
your "path", so Macaulay2's "load" and "input" commands will automatically look
there for your files.

You may obtain source code for Macaulay 2 packages and install them yourself
with the function "installPackage".  Behind the scenes, Macaulay 2 will use the
subdirectory "encap/" to house the code for those packages in separate
subdirectories.  The subdirectory "local/" will hold a single merged directory
tree for those packages, with symbolic links to the files of the packages.

Sometimes Macaulay 2 has been installed in a nonstandard location, and you
will need to set up your environment correctly so the program can be found and
will run, and so the documentation can be found.  The files "dot-profile" and
"dot-cshrc" in this directory contain commands you can incorporate into your
files ".profile" or ".cshrc" in your home directory.  Users of "bash" or "sh"
will want to modify ".profile", and users of "csh" or "tcsh" will want to
modify ".chsrc".  The file "dot-emacs" in this directory contains code you
can incorporate into your file ".emacs" in your home directory.

Good luck!

http://www.math.uiuc.edu/Macaulay2/

Daniel R. Grayson <dan@math.uiuc.edu>,
Michael R. Stillman <mike@math.cornell.edu>
/// << close;
	  );
     dir)

makePackageIndex = method(Dispatch => Thing)
makePackageIndex Sequence := x -> (
     if #x > 0 then error "expected 0 arguments";
     makePackageIndex path    -- this might get too many files (formerly we used packagePath)
     )
makePackageIndex List := path -> (
     initInstallDirectory options installPackage;
     if prefixDirectory === null then (
	  stderr << "warning: can't make package index" << endl;
	  return;
	  );
     absoluteLinks = true;
     key := "Macaulay 2";
     htmlDirectory = userMacaulay2Directory();		    -- links are relative to this directory
     fn := htmlDirectory | "index.html";
     if notify then stderr << "--making index of installed packages in " << fn << endl;
     fn << html HTML { 
	  HEAD splice {
	       TITLE {key, commentize headline key},
	       links()
	       },
	  BODY { 
	       -- buttonBar tag, HR{},
	       PARA {
		    "This is the top level documentation page for Macaulay 2 and its packages.
		    Bookmark this page for future reference."
		    },
	       HEADER3 "Documentation",
	       UL splice {
               	    if prefixDirectory =!= null then HREF { prefixDirectory | LAYOUT#"packagehtml" "Macaulay2" | "index.html", "Macaulay 2" },
		    apply(toSequence unique path, pkgdir -> (
			      prefixDirectory := minimizeFilename(pkgdir | relativizeFilename(LAYOUT#"packages",""));
			      p := prefixDirectory | LAYOUT#"docm2";
			      if isDirectory p then (
				   r := readDirectory p;
				   r = select(r, fn -> fn != "." and fn != ".."
					-- and fn != "Macaulay2"
					);
				   r = select(r, pkg -> fileExists (prefixDirectory | LAYOUT#"packagehtml" pkg | "index.html"));
				   r = sort r;
				   DIV {
					HEADER3 {"Packages in ", toAbsolutePath prefixDirectory},
					if #r > 0 then UL apply(r, pkg -> HREF { prefixDirectory | LAYOUT#"packagehtml" pkg | "index.html", pkg }) 
					}
				   )
			      )
			 )
		    }
	       }
	  } << endl
     << close;
     )

runnable := fn -> 0 < # select(1,apply(separate(":", getenv "PATH"),p -> p|"/"|fn),fileExists)
chk := ret -> if ret != 0 then error "external command failed"
browserMethods := hashTable {
     "firefox" => "firefox \"%s\"&",
     "open" => "open \"%s\"",
     "netscape" => "netscape -remote \"openURL(%s)\""
     }
URL = new SelfInitializingType of BasicList
new URL from String := (URL,str) -> new URL from {str}
show URL := x -> (
     url := x#0;
     browser := getenv "WWWBROWSER";
     if version#"operating system" === "Darwin" and runnable "open" then browser = "open"; -- should ignore WWWBROWSER, according to Mike
     if browser === "" then (
	  if runnable "firefox" then browser = "firefox"
	  else if runnable "netscape" then browser = "netscape"
	  else error "no browser found, and none specified in $WWWBROWSER"
	  );
     if browserMethods#?browser then browser = browserMethods#browser;
     chk run if match("%s",browser) then replace("%s",url,browser) else browser | " " | url
     )

fix := fn -> "file://" | replace(" ","%20",fn) 		    -- might want to replace more characters
showHtml = show Hypertext := x -> (
     fn := temporaryFileName() | ".html";
     fn << html HTML {
	  HEAD {
	       TITLE "Macaulay 2 Output"
	       },
     	  BODY {
	       x
	       }} << endl << close;
     show new URL from { fix fn };
     addEndFunction( () -> if fileExists fn then removeFile fn );
     )

show TEX := x -> showTex x

viewHelp = key -> (
     -- we have to rewrite this to check for secondary keys
     checkLoadDocumentation();
     if key === () then {
	  show new URL from { fix (applicationDirectory() | "index.html") }
	  }
     else (
     	  prefixes := nonnull {
	       if not member("-q",commandLine) then applicationDirectory()|"local/",
	       prefixDirectory
	       };
	  fn := htmlFilename DocumentTag.FormattedKey getPrimary makeDocumentTag key;
	  p := null;
	  scan(prefixes, dir -> if fileExists (dir|fn) then p = dir|fn);
	  if p === null then error("html file not found: ",fn)
	  else show new URL from { fix p }
	  );
     )
viewHelp = new Command from viewHelp

indexHtml = dir -> (
     -- experimental
     if not isDirectory dir then error "expected a directory";
     title := baseFilename dir;
     ind := minimizeFilename (dir|"/index.html");
     if fileExists ind then (
	  if not match("generated by indexHtml",get ind) then error("file not made by indexHtml already present: ",ind);
	  );
     ind = openOut ind;
     ind << ///<?xml version="1.0" encoding="us-ascii"?>
<!-- generated by indexHtml -->
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">
<html>
<head>
<title>///
<< title
<< ///</title>
</head>

<body>
<h1>/// 
     << title 
     << ///</h1>
<ul>
///;
     scan(readDirectory dir, fn -> (
	       if fn == "." or fn == ".." then return;
	       fn2 := minimizeFilename(dir|"/"|fn);
	       if isDirectory fn2 then indexHtml fn2
	       else (
		    ind << ///<li><A HREF="///
		    << fn
		    << ///">///
		    << fn
		    << ///</A>///;
		    if isRegularFile fn2 then (
			 ind << " (" << fileLength fn2 << " bytes)";
			 );
		    ind << "</li>" << endl)));
     ind << ///
</ul>
<hr>
<ul>
  <li><a href="http://validator.w3.org/check/referer">Validate</a> the html on this page.</li>
</ul>
</body>
/// 
     << close;
     )

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
