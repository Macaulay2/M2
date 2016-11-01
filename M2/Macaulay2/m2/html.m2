-- -*- fill-column: 107 -*-
--		Copyright 1993-2002 by Daniel R. Grayson

fixtitle = method()
fixtitle Nothing := identity
fixtitle String := htmlLiteral

Macaulay2HomePage := () -> "http://www.math.uiuc.edu/Macaulay2/"

-----------------------------------------------------------------------------
-- html output
-----------------------------------------------------------------------------

-- maybe we should rename this file to "packages2.m2" after the merge.

-- we've turned off checking for existence of files...

local prefix; local topNodeButton
local nullButton; local masterIndexButton; local tocButton; local homeButton; {* local directoryButton; *}
local NEXT; local PREV; local UP; local tableOfContents; local linkTable; local SRC
local nextButton; local prevButton; local upButton; local backwardButton; local forwardButton
local masterIndex
installationLayout = null
addStartFunction(() -> installationLayout = currentLayout)

hadExampleError := false
numExampleErrors := 0;

hadDocumentationWarning := false
numDocumentationWarnings := 0;

makingPackageIndex := false;

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
buildPrefix := null	   				    -- the root of the relative paths:
installPrefix := null	  	       	    	      	    -- the value of the option InstallPrefix; search also here for links
htmlDirectory := ""					    -- relative path to the html directory, depends on the package
installDirectory := ""					    -- absolute path to the install directory

runfun := o -> if instance(o, Function) then o() else o
initInstallDirectory := o -> installDirectory = minimizeFilename(runfun o.InstallPrefix | "/")

-----------------------------------------------------------------------------
-- relative URLs and filenames
-----------------------------------------------------------------------------

absoluteLinks := false

-- withAbsolutelinks := f -> (
--      t := absoluteLinks;
--      absoluteLinks = true;
--      r := f();
--      absoluteLinks = t;
--      r)

isAbsoluteURL := url -> match( "^(#|mailto:|[a-z]+://)", url )

searchPrefixPath = url -> (
     -- we also search the current InstallPrefix, first
     -- entries on the prefixPath may have different layouts
     pth := if buildPrefix === null then prefixPath else prepend(buildPrefix,prefixPath); -- what if buildPrefix has a left-over value from a previous invocation???
     for i in pth do (
	  p := i|url;
	  if fileExists p then (
	       if debugLevel > 5 then stderr << "--file " << url << " found in " << i << endl;
	       return p;
	       )
	  );
     if debugLevel > 5 then stderr << "--file " << url << " not found in " << prefixPath << endl;
     )

toURL := pth -> (
     if isAbsolutePath pth then concatenate(rootURI,
	  if fileExists pth then realpath pth 
	  else (
	       stderr << "-- *** warning: file needed for URL not found: " << pth << endl;
	       pth))
     else if isAbsoluteURL pth then pth
     else if absoluteLinks then (
	  p := searchPrefixPath pth;
	  if p =!= null
	  then concatenate(rootURI, realpath p)
	  else (
	       error("needed file not found on prefixPath: ",format pth,".\n    Try: (1) add a scheme, such as http://, to make an absolute URL;\n         (2) install the package that provides the file;\n         (3) use option AbsoluteLinks=>false");
	       -- relativizeFilename(htmlDirectory, pth)
	       ))
     else (
	  r := relativizeFilename(htmlDirectory, pth);
	  if debugLevel == 121 then (
	       stderr << "--toURL: htmlDirectory   = " << htmlDirectory << endl;
	       stderr << "--       pth             = " << pth << endl;
	       stderr << "--       relative result = " << r << endl;
	       );
	  r))

htmlFilename = method(Dispatch => Thing)
htmlFilename Thing := x -> htmlFilename makeDocumentTag x
htmlFilename2 = (tag,layout) -> (
     fkey := (class tag).FormattedKey tag;
     pkgtitle := (class tag).Title tag;
     replace("PKG",pkgtitle,layout#"packagehtml") | if fkey === pkgtitle then topFileName else toFilename fkey|".html" )

htmlFilename DocumentTag := tag -> (
     -- this one is used for storing the file, hence "installationLayout"
     htmlFilename2(tag,installationLayout))

-- this was the old thinking:
-- htmlFilename FinalDocumentTag := tag -> (
--      -- this one is used for creating links to the file, hence "currentLayout", since the bifurcation of the layout
--      -- into common and exec halves is not retained in the final installation; it's just a convenience while assembling
--      -- the distribution, so common files can be shared among build trees
--      htmlFilename2(tag,currentLayout))
htmlFilename FinalDocumentTag := tag -> (
     -- this one is used for creating links to the file
     htmlFilename2(tag,installationLayout))

html IMG  := x -> (
     (o,cn) := override(IMG.Options,toSequence x);
     if o#"alt" === null then error ("IMG item is missing alt attribute");
     concatenate("<img src=", format toURL o#"src", " alt=", format o#"alt", "/>"))

html HREF := x -> (
     r := html last x;
     if match("^ +$",r) then r = #r : "&nbsp;&nbsp;";
     concatenate("<a href=\"", toURL first x, "\">", r, "</a>")
     )
tex  HREF := x -> concatenate("\\special{html:<a href=\"", texLiteral toURL first x, "\">}", tex last x, "\\special{html:</a>}")

html TO   := x -> (
     tag := x#0;
     d := fetchPrimaryRawDocumentation tag;
     r := htmlLiteral DocumentTag.FormattedKey tag;
     if match("^ +$",r) then r = #r : "&nbsp;&nbsp;";
     if d#?"undocumented" and d#"undocumented" === true then (
	  if signalDocError tag then (
	       stderr << "--warning: tag cited also declared as undocumented: " << tag << endl;
	       warning();
	       );
	  concatenate( "<tt>", r, "</tt>", if x#?1 then x#1, " (missing documentation<!-- tag: ",toString DocumentTag.Key tag," -->)")
	  )
     else if d === null					    -- isMissingDoc
     then (
	  warning("missing documentation: "|toString tag);
	  concatenate( "<tt>", r, "</tt>", if x#?1 then x#1, " (missing documentation<!-- tag: ",toString DocumentTag.Key tag," -->)")
	  )
     else concatenate( "<a href=\"", toURL htmlFilename toFinalDocumentTag getPrimary x#0, "\" title=\"", fixtitle headline x#0, "\">", r, "</a>", if x#?1 then x#1))
html TO2  := x -> (
     tag := x#0;
     headline tag;		   -- this is a kludge, just to generate error messages about missing links 
     d := fetchPrimaryRawDocumentation tag;
     if d#?"undocumented" and d#"undocumented" === true then (
	  if signalDocError tag then (
	       stderr << "--warning: tag cited also declared as undocumented: " << tag << endl;
	       warning();
	       );
	  concatenate("<tt>", htmlLiteral x#1, "</tt> (missing documentation<!-- tag: ",DocumentTag.FormattedKey tag," -->)")
	  )
     else if d === null					    -- isMissingDoc
     then (
	  warning("missing documentation: "|toString tag);
	  concatenate("<tt>", htmlLiteral x#1, "</tt> (missing documentation<!-- tag: ",DocumentTag.FormattedKey tag," -->)"))
     else concatenate("<a href=\"", toURL htmlFilename toFinalDocumentTag getPrimary x#0, "\">", htmlLiteral x#1, "</a>"))

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

linkTitle := s -> concatenate( " title=\"", fixtitle s, "\"" )
linkTitleTag := tag -> "title" => fixtitle concatenate(DocumentTag.FormattedKey tag, commentize headline tag)
links := tag -> (
     -- we used to have several!
     doccss := replace("PKG","Style",installationLayout#"package") | "doc.css";
     if null === searchPrefixPath doccss
     then error(
	  if makingPackageIndex
	  then "style file not found on prefixPath (install Style package or run 'M2 -q' to avoid making the package index) : "
	  else "style file not found on prefixPath (install Style package) : ",
	  doccss);
     LINK { "href" => toURL doccss, "rel" => "stylesheet", "type" => "text/css" }
     )

-- Also set the character encoding with a meta http-equiv statement. (Sometimes XHTML
-- is parsed as HTML, and then the HTTP header or a meta tag is used to determine the
-- character encoding.  Locally-stored documentation does not have an HTTP header.)
defaultCharSet := () -> META { "http-equiv" => "Content-Type", "content" => "text/html; charset=utf-8" }

BUTTON := (s,alt) -> (
     s = toURL s;
     if alt === null
     then error "required attribute: ALT"
     else IMG("src" => s, "alt" => concatenate("[",alt,"]")))

html HTML := t -> concatenate(
///<?xml version="1.0" encoding="utf-8" ?>  <!-- for emacs: -*- coding: utf-8 -*- -->
<!-- Apache may like this line in the file .htaccess: AddCharset utf-8 .html -->
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1 plus MathML 2.0 plus SVG 1.1//EN"	 "http://www.w3.org/2002/04/xhtml-math-svg/xhtml-math-svg.dtd" >
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en">
///,
     apply(t,html), 
     "</html>\n"
     )

-- produce html form of documentation, for Macaulay2 and for packages

buttonBar := (tag) -> ButtonTABLE {{ 
	  DIV splice {
     	       forward tag, backward tag, next tag, prev tag, up tag,
     	       (if tag =!= topDocumentTag then topNodeButton else topNodeButton#-1, " | "),
     	       masterIndexButton, " | ", tocButton, {* " | ", directoryButton, *} " | ", homeButton
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
toDoc TreeNode := x -> DIV { TOH checkIsTag x#0, toDoc x#1 }

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
	       if chkdoc then (
	       	    -- stderr << "--warning: missing reference to documentation as subnode: " << x << endl;
		    -- warning();
		    error("missing reference to documentation as subnode: ", toString x);
		    );
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
     homeButton = HREF {Macaulay2HomePage (), "Macaulay2 web site"};
     -- directoryButton = HREF { rootURI | applicationDirectory() | "index.html", "directory"};
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
-- the code above for making html for Macaulay2 itself
-----------------------------------------------------------------------------

makeMasterIndex := (keylist,verbose) -> (
     numAnchorsMade = 0;
     fn := buildPrefix | htmlDirectory | indexFileName;
     title := DocumentTag.FormattedKey topDocumentTag | " : Index";
     if verbose then stderr << "--making '" << title << "' in " << fn << endl;
     r := HTML {
	  HEAD splice { TITLE title, defaultCharSet(), links() },
	  BODY nonnull {
	       DIV { topNodeButton, " | ", tocButton, {* " | ", directoryButton, *} " | ", homeButton },
	       HR{},
	       HEADER1 title,
	       DIV between(LITERAL "&nbsp;&nbsp;&nbsp;",apply(alpha, c -> HREF {"#"|c, c})), 
	       if #keylist > 0 then UL apply(sort keylist, (tag) -> (
			 checkIsTag tag;
			 anch := anchorsUpTo tag;
			 if anch === null then LI TOH tag else LI {anch, TOH tag})),
	       DIV remainingAnchors()
	       }};
     validate r;
     fn << html r << endl << close
     )

maketableOfContents := (verbose) -> (
     fn := buildPrefix | htmlDirectory | tocFileName;
     title := DocumentTag.FormattedKey topDocumentTag | " : Table of Contents";
     if verbose then stderr << "--making  " << title << "' in " << fn << endl;
     fn
     << html HTML {
	  HEAD splice { TITLE title, defaultCharSet(), links() },
	  BODY {
	       DIV { topNodeButton, " | ", masterIndexButton, {* " | ", directoryButton, *} " | ", homeButton },
	       HR{},
	       HEADER1 title,
	       toDoc tableOfContents
	       }
	  } << endl << close
     )

utest := opt -> (
     cmd := "ulimit " | opt | "; ";
     if chkrun("2>/dev/null >/dev/null "|cmd) == 0 then cmd else ""
     )
ulimit := null

M2statusRegexp := "^--status:"
statusLines := file -> select(lines file, s -> match(M2statusRegexp,s))

M2errorRegexp := "^[^:\n]+:[0-9]+:[0-9]+:(\\([0-9]+\\)):\\[[0-9]+\\]: "
aftermatch := (pat,str) -> (
     m := regex(pat,str);
     if m === null then "" else substring(m#0#0,str))

describeReturnCode = r -> (
     if r % 256 == 0 then "exited with status code " | toString (r // 256)
     else "killed by signal " | toString (r % 128) | if r & 128 =!= 0 then " (core dumped)" else ""
     )

runFile := (inf,inputhash,outf,tmpf,desc,pkg,announcechange,usermode,examplefiles) -> ( -- return false if error
     announcechange();
     stderr << "--making " << desc << " in file " << outf << endl;
     if fileExists outf then removeFile outf;
     pkgname := toString pkg;
     setseed := " --no-randomize";
     ldpkg := if pkgname != "Macaulay2Doc" then concatenate(" -e 'needsPackage(\"",pkgname,"\", FileName => \"",pkg#"source file","\")'") else "";
     src := concatenate apply(srcdirs, d -> (" --srcdir ",format d));
     -- we specify --no-readline because the readline library catches SIGINT:
     args := "--silent --print-width 77 --stop --int --no-readline" | (if usermode then "" else " -q") | src | setseed | ldpkg;
     cmdname := commandLine#0;
     -- must convert a relative path to an absolute path so we can run the same M2 from another directory while
     -- running the examples:
     if match("/",cmdname) then cmdname = toAbsolutePath cmdname;
     if ulimit === null then (
	  ulimit = utest "-t 700" | utest "-m 850000"| utest "-v 850000" | utest "-s 8192";
	  );
     tmpf << "-- -*- M2-comint -*- {* hash: " << inputhash << " *}" << endl << close;
     rundir := temporaryFileName() | "-rundir/";
     cmd := ulimit | "cd " | rundir | "; " | cmdname | " " | args | " <" | format inf | " >>" | format toAbsolutePath tmpf | " 2>&1";
     stderr << cmd << endl;
     makeDirectory rundir;
     for fn in examplefiles do copyFile(fn,rundir | baseFilename fn);
     r := run cmd;
     if r == 0 then (
	  scan(reverse findFiles rundir, f -> if isDirectory f then (
		    -- under cygwin, it seems to take a random amount of time before the system knows the directory is no longer in use:
		    try removeDirectory f
		    else (
			 stderr << "--warning: *** removing a directory failed, waiting..." << endl;
			 sleep 1;
		    	 try removeDirectory f
			 else (
			      stderr << "--warning: *** removing a directory failed again, waiting..." << endl;
			      sleep 4;
			      removeDirectory f
			      )
			 )
		    ) else removeFile f);
	  moveFile(tmpf,outf);
	  return true;
	  );
     stderr << tmpf << ":0:1: (output file) error: Macaulay2 " << describeReturnCode r << endl;
     stderr << aftermatch(M2errorRegexp,get tmpf);
     stderr << inf  << ":0:1: (input file)" << endl;
     scan(statusLines get inf, x -> stderr << x << endl);
     if # findFiles rundir == 1
     then removeDirectory rundir
     else stderr << rundir << ": error: files remain in temporary run directory after program exits abnormally" << endl;
     stderr << "M2: *** Error " << (if r<256 then r else r//256) << endl;
     if r == 2 then error "interrupted";
     hadExampleError = true;
     numExampleErrors = numExampleErrors + 1;
     return false;
     )

runString := (x,pkg,usermode) -> (
     tfn := temporaryFileName();
     inf := tfn | ".m2";
     tmpf := tfn | ".tmp";
     outf := tfn | ".out";
     rm := fn -> if fileExists fn then removeFile fn;
     rmall := () -> rm \ {inf, tmpf, outf};
     inf << x << endl << close;
     ret := runFile(inf,hash x,outf,tmpf,"test results",pkg,t->t,usermode,{});
     if ret then (rm inf; rm outf;);
     ret)

check = method(Options => {
	  UserMode => null
	  })
prep := pkg -> (
     use pkg;
     if pkg#?"documentation not loaded" then pkg = loadPackage(pkg#"title", LoadDocumentation => true, Reload => true);
     hadExampleError = false;
     numExampleErrors = 0;
     pkg)
onecheck = (seqno,pkg,usermode) -> (
     (filename,lineno,s) := pkg#"test inputs"#seqno;
     stderr << "--running test " << seqno << " of package " << pkg << " on line " << lineno << " in file " << filename << endl;
     stderr << "--    rerun with: check_" << seqno << " \"" << pkg << "\"" << endl;
     runString(s,pkg,usermode);
     )
check(ZZ,Package) := opts -> (seqno,pkg) -> (
     pkg = prep pkg;
     onecheck(seqno,pkg,if opts.UserMode === null then not noinitfile else opts.UserMode);
     if hadExampleError then error("error occurred running test for package ", toString pkg, ": ", toString seqno);
     )
check(ZZ,String) := opts -> (seqno,pkg) -> check(seqno, needsPackage (pkg, LoadDocumentation => true), opts)
check Package := opts -> pkg -> (
     pkg = prep pkg;
     scan(keys pkg#"test inputs", seqno -> onecheck(seqno,pkg,if opts.UserMode === null then not noinitfile else opts.UserMode));
     if hadExampleError then error(toString numExampleErrors, " error(s) occurred running tests for package ", toString pkg);
     )
check String := opts -> pkg -> check(needsPackage (pkg, LoadDocumentation => true), opts)

setupNames := (opts,pkg) -> (
     installPrefix = minimizeFilename(runfun opts.InstallPrefix | "/");
     buildPackage = pkg#"title";
     buildPrefix = 
     if opts.Encapsulate then (
	  minimizeFilename(runfun opts.PackagePrefix | "/") | (
	       if instance(opts.EncapsulateDirectory,Function) then opts.EncapsulateDirectory pkg
	       else if instance(opts.EncapsulateDirectory,String) then opts.EncapsulateDirectory
	       else error "expected EncapsulateDirectory option to be a function or a string"))
     else installPrefix)
unsetupNames := () -> buildPrefix = installPrefix = buildPackage = null


installPackage = method(Options => {
	  SeparateExec => false,
	  PackagePrefix => () -> applicationDirectory() | "encap/",
          InstallPrefix => () -> applicationDirectory() | "local/",
	  UserMode => null,
	  Encapsulate => false,
	  EncapsulateDirectory => pkg -> pkg#"title"|"-"|pkg.Options.Version|"/",
	  IgnoreExampleErrors => false,
	  FileName => null,
	  CacheExampleOutput => null,			    -- overrides the value specified by newPackage if true or false
	  CheckDocumentation => true,
	  MakeDocumentation => true,
	  MakeInfo => true,
	  RemakeAllDocumentation => true,		    -- until we get better dependency graphs between documentation nodes, "false" here will confuse users
	  RerunExamples => false,
	  RunExamples => true,
	  AbsoluteLinks => true,
	  MakeLinks => true,
	  Verbose => true,
	  DebuggingMode => null
	  })
uninstallPackage = method(Options => { 
	  PackagePrefix => () -> applicationDirectory() | "encap/",
          InstallPrefix => () -> applicationDirectory() | "local/"
	  })

removeFiles = p -> scan(reverse findFiles p, fn -> if fileExists fn or readlink fn =!= null then (
	  if isDirectory fn then (
	       -- we silently ignore nonempty directories, which could result from
	       -- removing an open file on an NFS file system.  Such files get renamed
	       -- to something beginning with ".nfs".
	       if length readDirectory fn == 2 then removeDirectory fn)
	  else removeFile fn))

uninstallPackage String := opts -> pkg -> (
     checkPackageName pkg;
     buildDirectory := minimizeFilename(runfun opts.PackagePrefix | "/");
     scan(readDirectory buildDirectory, dir -> if match("^" | pkg | "-",dir) then removeFiles (buildDirectory|dir|"/"));
     installDirectory := minimizeFilename(runfun opts.InstallPrefix | "/");
     apply(findFiles apply({1,2},
	       i -> apply(flatten {
	       		 Layout#i#"packages"|pkg|".m2", Layout#i#"info"|pkg|".info",
			 apply({"package","packagelib","packagedoc"}, f -> replace("PKG", pkg, Layout#i#f))
	       		 }, 
	  	    p -> installDirectory|p)),
	  removeFiles);
     )

installPackage String := opts -> pkg -> (
     if pkg =!= "Macaulay2Doc" then needsPackage "Macaulay2Doc";  -- load the core documentation
     -- we load the package even if it's already been loaded, because even if it was loaded with
     -- its documentation the first time, it might have been loaded at a time when the core documentation
     -- in the "Macaulay2Doc" package was not yet loaded
     pkg = loadPackage(pkg, DebuggingMode => opts.DebuggingMode, LoadDocumentation => opts.MakeDocumentation, FileName => opts.FileName, Reload => true);
     installPackage(pkg, opts);
     )

dispatcherMethod := m -> m#-1 === Sequence and (
     f := lookup m;
     any(dispatcherFunctions, g -> functionBody f === functionBody g))

installPackage Package := opts -> pkg -> (
     verbose := opts.Verbose or debugLevel > 0;
     oldlayout := installationLayout;
     installationLayout = if opts.SeparateExec then Layout#2 else Layout#1;

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
     
     if (options pkg).Headline === null then error ("no Headline option provided to newPackage for ",pkg#"title");
     if (options pkg).Headline === ""   then error ("empty string given as Headline option to newPackage for ",pkg#"title");

     if verbose then stderr << "--installing package " << pkg << " in " << buildPrefix << endl;
     
     currentSourceDir := pkg#"source directory";
     if verbose then stderr << "--using package sources found in " << currentSourceDir << endl;

     -- copy package source file
     pkgDirectory := installationLayout#"packages";
     makeDirectory (buildPrefix|pkgDirectory);
     bn := buildPackage | ".m2";
     fn := currentSourceDir|bn;
     if not fileExists fn then error("file ", fn, " not found");
     copyFile(fn, buildPrefix|pkgDirectory|bn, Verbose => debugLevel > 5);

     excludes := Exclude => {
	  "^CVS$", 
	  "^\\.svn$", 
	  -- The package Style has a read-only file "Makefile", made from "Makefile.in", that doesn't need to be distributed.
	  -- Better would be to fix copyDirectory so it manages to copy even when the target file is read-only
	  "Makefile"
	  };

     if pkg === Core then (
	  ) else (
     	  
	  -- copy package source subdirectory
	  srcDirectory := replace("PKG",pkg#"title",installationLayout#"package");
	  auxiliaryFilesDirectory := realpath currentSourceDir | buildPackage;
	  if isDirectory auxiliaryFilesDirectory
	  then (
	       if not (options pkg).AuxiliaryFiles
	       then error ("package ",toString pkg," has auxiliary files in \"",auxiliaryFilesDirectory,"\", but newPackage wasn't given AuxiliaryFiles=>true");
	       if verbose then stderr << "--copying auxiliary source files from " << auxiliaryFilesDirectory << endl;
	       makeDirectory (buildPrefix|srcDirectory);
	       copyDirectory(auxiliaryFilesDirectory, buildPrefix|srcDirectory, UpdateOnly => true, Verbose => verbose, excludes);
	       )
	  else (
	       if (options pkg).AuxiliaryFiles
	       then error ("package ",toString pkg," has no directory of auxiliary files, but newPackage was given AuxiliaryFiles=>true");
	       )
     	  );

     -- copy package source subdirectory examples
     exampleOutputDir := buildPrefix|replace("PKG",pkg#"title",installationLayout#"packageexampleoutput");

     if opts.MakeDocumentation then (
	  pkg#"package prefix" = buildPrefix;

	  -- copy package doc subdirectory if we loaded the package from a distribution
     	  -- ... to be implemented, but we seem to be copying the examples already, but only partially

     	  fnbase := temporaryFileName ();
	  infn := fkey -> fnbase|toFilename fkey|".m2";
	  outfn := fkey -> exampleOutputDir|toFilename fkey|".out";
	  tmpfn := fkey -> exampleOutputDir|toFilename fkey|".errors";
	  makeDirectory exampleOutputDir;

	  -- check for obsolete example output files and remove them
	  if opts.CheckDocumentation then (
	       exampleOutputFiles := set apply(keys pkg#"example inputs", outfn);
	       scan(readDirectory exampleOutputDir, fn -> (
			 fn = exampleOutputDir | fn;
			 if match("\\.out$",fn) and not exampleOutputFiles#?fn then (
			      -- stderr << "--warning: removing obsolete example output file: " <<  fn << endl;
			      removeFile fn;
			      );
			 )));

	  -- cache raw documentation in database, and check for changes
	  rawDocUnchanged := new MutableHashTable;
	  libDir := pkg#"package prefix" | replace("PKG",pkg#"title",installationLayout#"packagelib");
	  cacheDir := pkg#"package prefix" | replace("PKG",pkg#"title",installationLayout#"packagecache");
	  rawdbname := cacheDir | "rawdocumentation" | databaseSuffix;
	  rawdbnametmp := rawdbname | ".tmp";
	  if verbose then stderr << "--storing raw documentation in " << rawdbname << endl;
	  makeDirectory cacheDir;
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
			 v := toExternalStringWithText rawDoc#fkey;
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
	  if verbose then stderr << "--running tests that are functions" << endl;
	  scan(pairs pkg#"test inputs", (key,str) -> if instance(str, Function) then (
		    if verbose then stderr << "--  running test " << key << ", function " << str << endl;
		    str();
		    ));

	  -- make example output files, or else copy them from old package directory tree
	  exampleDir' := realpath currentSourceDir | buildPackage | "/examples" | "/";
	  outfn' := fkey -> exampleDir'|toFilename fkey|".out";
	  gethash := outf -> (
	       f := get outf;
	       m := regex("\\`.*\\{\\*.* hash: *(-?[0-9]+).*\\*\\}",f);
	       if m =!= null then value substring(f,m#1#0,m#1#1));
	  if verbose then stderr << "--making example result files in " << exampleOutputDir << endl;
	  hadExampleError = false;
	  numExampleErrors = 0;
	  scan(pairs pkg#"example inputs", (fkey,inputs) -> (
		    examplefiles := if pkg#"example data files"#?fkey then pkg#"example data files"#fkey else {};
		    -- args:
		    inf := infn fkey;
		    outf := outfn fkey;
		    outf' := outfn' fkey;
		    tmpf := tmpfn fkey;
		    desc := "example results for " | fkey;
		    changefun := () -> remove(rawDocUnchanged,fkey);
		    inputhash := hash inputs;
	  	    possiblyCache := () -> (
			 if opts.CacheExampleOutput =!= false and (options pkg).CacheExampleOutput === true 
			 and ( not fileExists outf' or fileExists outf' and fileTime outf > fileTime outf' )
			 then (
			      if verbose then stderr << "--caching example output for " << fkey << " in " << outf' << endl;
			      if not isDirectory exampleDir' then makeDirectory exampleDir';
			      copyFile(outf,outf',Verbose=>true);
			      ));
		    if not opts.RunExamples
		    or not opts.RerunExamples and fileExists outf and gethash outf === inputhash then (
			 possiblyCache();
			 )
		    else if (
			 not opts.RerunExamples 
			 and fileExists outf' 
			 and gethash outf' === inputhash
			 )
		    then (
			 if fileExists tmpf then removeFile tmpf;
			 copyFile(outf',outf);
			 )
		    else (
			 inf << concatenate apply(inputs, s -> s|"\n") << close;
			 if runFile(inf,inputhash,outf,tmpf,desc,pkg,changefun,if opts.UserMode === null then not noinitfile else opts.UserMode,examplefiles)
			 then (
			      removeFile inf;
			      possiblyCache();
			      )
			 );
		    -- read, separate, and store example output
		    if fileExists outf then pkg#"example results"#fkey = drop(separateM2output get outf,-1)
		    else (
			 if debugLevel > 1 then stderr << "--warning: missing file " << outf << endl;
			 )
		    ));

 	  if not opts.IgnoreExampleErrors 
	  then if hadExampleError then error(toString numExampleErrors, " error(s) occurred running examples for package ", pkg#"title");

	  -- process documentation
	  rawkey := "raw documentation database";
	  if verbose then stderr << "--processing documentation nodes..." << endl;
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
		    if not opts.MakeInfo 		    -- when making the info file, we need to process all the documentation
		    and not opts.RemakeAllDocumentation
		    and rawDocUnchanged#?fkey then (
			 if debugLevel > 0 then stderr << "--skipping     " << tag << endl;
			 )
		    else (
			 if debugLevel > 0 then stderr << "--processing   " << tag << endl;
			 pkg#"processed documentation"#fkey = (
			      processExamplesStrict = not opts.IgnoreExampleErrors;
			      -- sort of a kludge: what if an error occurs and the variable isn't reset?
			      first(
				   help tag,
				   processExamplesStrict = true));
			 -- get source filename and linenum, too:
			 if pkg#"raw documentation"#?fkey then (
			      doc := pkg#"raw documentation"#fkey;
			      SRC#tag = (doc#"filename",doc#"linenum");
			      );
			 );
		    )
	       );

          if pkg#?rawkey and isOpen pkg#rawkey then close pkg#rawkey;
	  shield (
	       moveFile(rawdbnametmp,rawdbname);
	       );

	  pkg#rawkey = openDatabase rawdbname;
	  addEndFunction(() -> if pkg#?rawkey and isOpen pkg#rawkey then close pkg#rawkey);

	  -- make table of contents, including next, prev, and up links
	  if verbose then stderr << "--assembling table of contents" << endl;
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
	       scan((if pkg#"title" == "Macaulay2Doc" then Core else pkg)#"exported symbols", s -> (
			 tag := makeDocumentTag s;
			 if not isUndocumented tag and not hasDocumentation s and signalDocError tag
			 then stderr << "--warning: symbol has no documentation: " << tag << endl;
			 f := value s;
			 if instance(f, Function) then (
			      scan(methods f, m -> if isDocumentableMethod m then (
					tag := makeDocumentTag m;
					if not isUndocumented tag and not dispatcherMethod m and not hasDocumentation m and signalDocError tag
					then stderr << "--warning: method has no documentation: " << tag << ", key: " << toString DocumentTag.Key tag << endl;
					));
			      ))));

	  if hadDocumentationWarning then
	  stderr << "--warning: " << numDocumentationWarnings << " warning" 
	  << (if numDocumentationWarnings > 1 then "(s)" else "")
     	  << " occurred in documentation for package " << pkg << endl;

	  -- helper routine
	  getPDoc := fkey -> (
	       if pkg#"processed documentation"#?fkey then pkg#"processed documentation"#fkey
	       else error("internal error: documentation node not processed yet: ",fkey)
	       );

	  -- make info file
	  if opts.MakeInfo then (
	       savePW := printWidth;
	       printWidth = 79;
	       infodir := buildPrefix|installationLayout#"info";
	       makeDirectory infodir;
	       infotitle := pkg#"title";
	       infobasename := infotitle|".info";
	       tmpinfobasename := infobasename|".tmp";
	       infofile := openOut (infodir|tmpinfobasename);
	       if verbose then stderr << "--making info file in " << infofile << endl;
	       upto30 := t -> concatenate(t,30-#t:" ");
	       infofile << " -*- coding: utf-8 -*- This is " << infobasename << ", produced by Macaulay2, version " << version#"VERSION" << endl << endl;
	       infofile << "INFO-DIR-SECTION " << pkg.Options.InfoDirSection << endl;
	       infofile << "START-INFO-DIR-ENTRY" << endl;
	       infofile << upto30 concatenate( "* ", infotitle, ": (", infotitle, ").") << "  ";
	       infofile << (if pkg.Options.Headline =!= null then pkg.Options.Headline else infotitle | ", a Macaulay2 package") << endl;
	       infofile << "END-INFO-DIR-ENTRY" << endl << endl;
	       byteOffsets := new MutableHashTable;
	       topNodeName := DocumentTag.FormattedKey topDocumentTag;
	       chk := if topNodeName === "Top" then identity else n -> if n === "Top" then error "encountered a documentation node named 'Top'";
	       infoTagConvert' := n -> if n === topNodeName then "Top" else infoTagConvert n;
	       traverse(unbag pkg#"table of contents", tag -> (
			 if DocumentTag.Title tag =!= pkg#"title" then (
			      error("internal error: alien entry in table of contents: ",toString tag);
			      );
			 fkey := DocumentTag.FormattedKey tag;
			 chk fkey;
			 byteOffsets# #byteOffsets = concatenate("Node: ",infoTagConvert' fkey,"\177",toString fileLength infofile);
			 infofile << "\037" << endl << "File: " << infobasename << ", Node: " << infoTagConvert' fkey;
			 if NEXT#?tag then infofile << ", Next: " << infoTagConvert' DocumentTag.FormattedKey NEXT#tag;
			 if PREV#?tag then infofile << ", Prev: " << infoTagConvert' DocumentTag.FormattedKey PREV#tag;
			 -- nodes without an Up: link tend to make the emacs info reader unable to construct the table of contents,
			 -- and the display isn't as nice after that error occurs
			 infofile << ", Up: " << if UP#?tag then infoTagConvert' DocumentTag.FormattedKey UP#tag else "Top";
			 infofile << endl << endl << info getPDoc fkey << endl));
	       infofile << "\037" << endl << "Tag Table:" << endl;
	       scan(values byteOffsets, b -> infofile << b << endl);
	       infofile << "\037" << endl << "End Tag Table" << endl;
	       infofile << close;
	       moveFile(infodir|tmpinfobasename,infodir|infobasename);
	       if verbose then stderr << "--completed info file moved to " << infodir|infobasename << endl;
	       printWidth = savePW;
	       )
	  else (
	       if verbose then stderr << "--not making info file" << endl;
	       );

	  -- make html files
	  htmlDirectory = replace("PKG",pkg#"title",installationLayout#"packagehtml"); -- if installationLayout =!= currentLayout, this may cause problems
	  setupButtons();
	  makeDirectory (buildPrefix|htmlDirectory);
	  if verbose then stderr << "--making empty html pages in " << buildPrefix|htmlDirectory << endl;
	  if pkg.Options.Headline =!= null then (
	       buildPrefix|htmlDirectory|".Headline" << pkg.Options.Headline << close;
	       );
	  if pkg.Options.Certification =!= null then (
	       buildPrefix|htmlDirectory|".Certification" << toExternalString pkg.Options.Certification << close;
	       );
	  scan(nodes, tag -> if not isUndocumented tag then (
		    fkey := DocumentTag.FormattedKey tag;
		    fn := buildPrefix | htmlFilename tag;
		    if fileExists fn then return;
		    if isSecondary tag then return;
		    if debugLevel > 0 then stderr << "--creating empty html page for " << tag << " in " << fn << endl;
		    fn << close));
	  for n in (topFileName, indexFileName, tocFileName) do (
	       fn := buildPrefix | replace("PKG",pkg#"title",installationLayout#"packagehtml") | n;
	       if not fileExists fn then (
		    if debugLevel > 0 then stderr << "--creating empty html page " << fn << endl;
		    fn << close)
	       else (
		    if debugLevel > 0 then stderr << "--html page exists: " << fn << endl;
		    ));
	  if verbose then stderr << "--making html pages in " << buildPrefix|htmlDirectory << endl;
     	  scan(nodes, tag -> if not isUndocumented tag then (
	       -- key := DocumentTag.Key tag;
	       fkey := DocumentTag.FormattedKey tag;
	       fn := buildPrefix | htmlFilename tag;
	       if fileExists fn and fileLength fn > 0 and not opts.RemakeAllDocumentation and rawDocUnchanged#?fkey then return;
	       if isSecondary tag then return;
	       if debugLevel > 0 then stderr << "--making html page for " << tag << endl;
	       fn
	       << html HTML { 
		    HEAD splice {
			 TITLE {fkey, commentize headline fkey}, -- I hope this works...
			 defaultCharSet(),
			 links tag
			 },
		    BODY { 
			 buttonBar tag,
			 if UP#?tag
			 then DIV between(" > ", apply(upAncestors tag, i -> TO i))
			 else DIV (TO topDocumentTag, " :: ", TO tag),
			 HR{}, 
			 getPDoc fkey
			 }
		    }
	       << endl << close));

	  -- make master.html with master index of all the html files
	  makeMasterIndex(select(nodes,tag -> not isUndocumented tag {* and instance(DocumentTag.Key tag,Symbol) *} ), verbose);

	  -- make table of contents
	  maketableOfContents verbose;

     	  );						    -- end if opts.MakeDocumentation

     -- make postinstall and preremove files, if encap
     if opts.Encapsulate then (
	  octal := s -> (n := 0 ; z := first ascii "0"; scan(ascii s, i -> n = 8*n + i - z); n);
	  if verbose then stderr << "--making INSTALL, postinstall, preremove, and encapinfo files in " << buildPrefix << endl;
     	  fix := s -> (
	       s = replace("info/", installationLayout#"info", s);
	       s = replace("bin/", installationLayout#"bin", s);
	       s);
	  -- postinstall
	  f := buildPrefix | "postinstall" 
	  << ///#! /bin/sh -e/// << endl
	  << fix ///cd "$ENCAP_SOURCE/$ENCAP_PKGNAME/info/" || exit 0/// << endl
	  << ///for i in *.info/// << endl
	  << fix ///do (set -x ; install-info --info-dir="$ENCAP_TARGET/info/" "$i")/// << endl
	  << ///done/// << endl;
	  if version#"dumpdata" and pkg#"title" == "Macaulay2Doc" then (
	       f << endl << fix "(set -x ; \"$ENCAP_TARGET\"/bin/" << version#"M2 name" << " --stop --dumpdata)" << endl;
	       );
	  fileMode(octal "755",f);
	  f << close;
	  -- preremove
     	  f = buildPrefix | "preremove"
	  << ///#! /bin/sh -x/// << endl
	  << fix ///cd "$ENCAP_SOURCE/$ENCAP_PKGNAME/info/" || exit 0/// << endl
	  << ///for i in *.info/// << endl
	  << fix ///do (set -x ; install-info --info-dir="$ENCAP_TARGET/info/" --remove "$i")/// << endl
	  << ///done/// << endl;
	  fileMode(octal "755",f);
 	  f << close;
	  -- encapinfo
	  f = buildPrefix | "encapinfo"
	  << ///encap 2.0/// << endl
	  << ///contact dan@math.uiuc.edu/// << endl;
	  removeLastSlash := s -> if s#?0 and s#-1 === "/" then substring(s,0,#s-1) else s;
	  scan(("packagecache","packagedoc","package","libraries"),
	       k -> f << "linkdir" << " " << removeLastSlash replace("PKG","*",installationLayout#k) << endl);
	  fileMode(octal "644",f);
	  f << close;
	  );

     -- make symbolic links
     if opts.Encapsulate and opts.MakeLinks then (
     	  if verbose then stderr << "--making symbolic links from \"" << installDirectory << "\" to \"" << buildPrefix << "\"" << endl;
	  scan(unique {installationLayout#"common",installationLayout#"exec"}, d ->
	       symlinkDirectory(buildPrefix|d, installDirectory,
		    Verbose => debugLevel > 0, 
		    Exclude => {
			 "^encapinfo$", "^postinstall$", "^preremove$", -- configuration files for epkg
			 "^\\.nfs"				    -- removed open files on NFS mounted file systems
			 })));

     -- all done
     SRC = null;
     if not hadExampleError then (
	  iname := libDir|".installed";
	  iname << close;
	  if verbose then stderr << "--file created: " << iname << endl;
	  );
     if verbose then stderr << "--installed package " << pkg << " in " << buildPrefix << endl;
     currentPackage = oldpkg;
     if not noinitfile then (
	  userMacaulay2Directory();
	  makePackageIndex();
	  );
     installationLayout = oldlayout;
     unsetupNames();
     )

sampleInitFile = ///-- This is a sample init.m2 file provided with Macaulay2.
-- It contains Macaulay2 code and is automatically loaded upon
-- startup of Macaulay2, unless you use the "-q" option.

-- Uncomment the following line to cause Macaulay2 to load "start.m2" in the current working directory upon startup.
-- if fileExists "start.m2" then load(currentDirectory()|"start.m2")

-- Uncomment and edit the following lines to add your favorite directories containing Macaulay2
-- source code files to the load path.  Terminate each directory name with a "/".
-- (To see your current load path, display the value of the variable "path".)
-- path = join( { "~/" | "src/singularities/", "/usr/local/src/M2/" }, path )

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

///

readmeFile = ///Welcome to Macaulay2!
			     
This directory is used to contain data and code specific to Macaulay2.  For
example, your initialization file, init.m2, is in this directory, and is
automatically loaded upon startup of Macaulay2, unless you use the "-q" option.
You may edit it to meet your needs.

The web browser file "index.html" in this directory contains a list of links to
the documentation of Macaulay2 and its installed packages and is updated every
time you start Macaulay2 (unless you use the "-q" option).  To update it
manually, use "makePackageIndex()".  Point your web browser at that file and
bookmark it.

You may place Macaulay2 source files in the subdirectory "code/".  It's on
your "path", so Macaulay2's "load" and "input" commands will automatically look
there for your files.

You may obtain source code for Macaulay2 packages and install them yourself
with the function "installPackage".  Behind the scenes, Macaulay2 will use the
subdirectory "encap/" to house the code for those packages in separate
subdirectories.  The subdirectory "local/" will hold a single merged directory
tree for those packages, with symbolic links to the files of the packages.

Good luck!

http://www.math.uiuc.edu/Macaulay2/

Daniel R. Grayson <dan@math.uiuc.edu>,
Michael R. Stillman <mike@math.cornell.edu>
///

userMacaulay2Directory = () -> (
     dir := applicationDirectory();
     makeDirectory(dir);
     makeDirectory(dir|"encap/");
     makeDirectory(dir|"local/");
     makeDirectory(dir|"code/");
     f := (n,c) -> (n = dir|n; if not fileExists n then n << c << close);
     f("init.m2", sampleInitFile);
     f("README", readmeFile);
     )

endswith = (suff,str) -> substring(str,-#suff) == suff

makePackageIndex = method(Dispatch => Thing)
makePackageIndex Sequence := x -> (
     if #x > 0 then error "expected 0 arguments";
     makePackageIndex path    -- this might get too many files (formerly we used packagePath)
     )
makePackageIndex List := path -> (
     makingPackageIndex = true;
     initInstallDirectory options installPackage;
     absoluteLinks = true;
     key := "Macaulay2";
     star := IMG {
	  "src" => replace("PKG","Style",currentLayout#"package") | "GoldStar.png",
	  "alt" => "a gold star"
	  };
     htmlDirectory = applicationDirectory();
     fn := htmlDirectory | "index.html";
     if notify then stderr << "--making index of installed packages in " << fn << endl;
     if debugLevel > 10 then stderr << "--path = " << stack path << endl;
     docdirdone := new MutableHashTable;
     fn << html HTML { 
	  HEAD splice {
	       TITLE {key, commentize headline key},
	       defaultCharSet(),
	       links()
	       },
	  BODY { 
	       -- buttonBar tag, HR{},
	       PARA {
		    "This is the directory for Macaulay2 and its packages.
		    Bookmark this page for future reference, or run the viewHelp command in Macaulay2
		    to open up your browser on this page."
		    },
	       HEADER3 "Documentation",
	       ul nonnull splice {
               	    if prefixDirectory =!= null 
		    then (
			 m2doc := prefixDirectory | replace("PKG","Macaulay2Doc",currentLayout#"packagehtml") | "index.html";
			 if fileExists m2doc then HREF { m2doc, "Macaulay2" }
			 ),
		    splice apply(toSequence unique apply(select(path,isDirectory),realpath), pkgdir -> (
			      if debugLevel > 10 then stderr << "--checking package directory " << pkgdir << endl;
			      apply(toSequence values Layout, layout -> (
				   packagelayout := layout#"packages";
				   if not endswith(packagelayout,pkgdir) then (
					if debugLevel > 10 then stderr << "--package directory " << format pkgdir << " does not end with " << format packagelayout << endl;
					return;
					);
				   prefixDirectory := substring(pkgdir,0,#pkgdir-#packagelayout);
				   p := prefixDirectory | layout#"docdir";
				   if docdirdone#?p then (
					if debugLevel > 10 then stderr << "--documentation directory already checked: " << p << endl;
					)
				   else if isDirectory p then (
					p = realpath p;
					docdirdone#p = true;
					if debugLevel > 10 then stderr << "--checking documentation directory " << p << endl;
					r := readDirectory p;
					r = select(r, fn -> fn != "." and fn != ".." );
					pkghtmldir := pkg -> prefixDirectory | replace("PKG",pkg,layout#"packagehtml");
					r = select(r, pkg -> fileExists (pkghtmldir pkg | "index.html"));
					r = sort r;
					DIV {
					     HEADER3 {"Packages in ", toAbsolutePath prefixDirectory},
					     if #r > 0 then UL apply(r, pkg -> LI splice {
						       if fileExists (pkghtmldir pkg | ".Certification") then (
							    star, " "
							    ),
						       HREF { pkghtmldir pkg | "index.html", pkg },
						       if fileExists (pkghtmldir pkg | ".Headline") then (
							    " -- ", get (pkghtmldir pkg | ".Headline")
							    )})})
				   else (
					if debugLevel > 10 then stderr << "--documentation directory does not exist: " << p << endl;
					)))))
		    }
	       }
	  } << endl
     << close;
     makingPackageIndex = false;
     )

runnable := fn -> (
     if isAbsolutePath fn then (
	  fileExists fn
	  )
     else (
     	  0 < # select(1,apply(separate(":", getenv "PATH"),p -> p|"/"|fn),fileExists)
	  )
     )
chk := ret -> if ret != 0 then (
     if version#"operating system" === "MicrosoftWindows" and ret == 256 then return;     
     error "external command failed"
     )
browserMethods := hashTable {
     "firefox" => url -> {"firefox", url},
     "open" => url -> {"open", url},
     "cygstart" => url -> {"cygstart", url},
     "netscape" => url -> {"netscape", "-remote",  "openURL(" | url | ")" },
     "windows firefox" => url -> { "/cygdrive/c/Program Files/Mozilla Firefox/firefox", "-remote", "openURL(" | url | ")" }
     }
URL = new SelfInitializingType of BasicList
new URL from String := (URL,str) -> new URL from {str}
show URL := x -> (
     url := x#0;
     browser := getenv "WWWBROWSER";
     if version#"operating system" === "Darwin" and runnable "open" then browser = "open" -- should ignore WWWBROWSER, according to Mike
     else
     if version#"issue" === "Cygwin" then browser = "cygstart";
     if browser === "" then (
	  if runnable "firefox" then browser = "firefox"
	  else if runnable "netscape" then browser = "netscape"
	  else error "no browser found, and none specified in $WWWBROWSER"
	  );
     cmd := if browserMethods#?browser then browserMethods#browser url else { browser, url };
     if fork() == 0 then (
	  setGroupID(0,0);
     	  try exec cmd;
     	  stderr << "exec failed: " << toExternalString cmd << endl;
	  exit 1
	  )
     )

fix := fn -> (
     r := rootURI | replace(" ","%20",realpath fn); 		    -- might want to replace more characters
     if debugLevel > 0 then stderr << "--fixed URL: " << r << endl;
     r)
showHtml = show Hypertext := x -> (
     fn := temporaryFileName() | ".html";
     fn << html HTML {
	  HEAD {
	       TITLE "Macaulay2 Output",
	       defaultCharSet()
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
	  i := applicationDirectory() | "index.html";
	  if not fileExists i then error("missing file (run makePackageIndex() or start M2 without -q): ",i);
	  show new URL from { "file://" | i }		    -- formerly (for cygwin): fix i
	  }
     else (
	  fn := htmlFilename2(toFinalDocumentTag getPrimary makeDocumentTag key,Layout#1);
	  p := null;
	  scan(prefixPath, dir -> if fileExists (dir|fn) then (p = dir|fn;break));
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
     ind << ///<?xml version="1.0" encoding="utf-8"?>  <!-- for emacs: -*- coding: utf-8 -*- -->
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
