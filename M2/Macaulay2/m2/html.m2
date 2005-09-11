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
local NEXT; local PREV; local UP; local CONT; local linkTable
local nextButton; local prevButton; local upButton
local masterIndex

haderror := false
numerrors := 0;

buildPackage := null					    -- name of the package currently being built
topDocumentTag := null
topFileName := "index.html"				    -- top node's file name, constant
indexFileName := "master.html"  			    -- file name for master index of topics in a package
tocFileName := "toc.html"       			    -- file name for the table of contents of a package
buildDirectory := "/tmp/"				    -- the root of the relative paths:
htmlDirectory := ""					    -- relative path to the html directory, depends on the package
installDirectory := ""					    -- absolute path to the install directory

runfun := o -> if class o === Function then o() else o
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

htmlFilename = method(SingleArgumentDispatch => true)
htmlFilename Thing := x -> htmlFilename makeDocumentTag x
htmlFilename DocumentTag := tag -> (
     fkey := DocumentTag.FormattedKey tag;
     pkgtitle := DocumentTag.Title tag;
     LAYOUT#"packagehtml" pkgtitle | if fkey === pkgtitle then topFileName else toFilename fkey|".html" )
htmlFilename FinalDocumentTag := tag -> (
     fkey := FinalDocumentTag.FormattedKey tag;
     pkgtitle := FinalDocumentTag.Title tag;
     LAYOUT#"packagehtml" pkgtitle | if fkey === pkgtitle then topFileName else toFilename fkey|".html" )

html IMG  := x -> concatenate("<img src=\"", rel x#0, "\" alt=\"", x#1, "\"/>")
html LINK := x -> concatenate("<link href=\"", rel first x, "\"", concatenate drop(x,1), "/>",newline)
html HREF := x -> concatenate("<a href=\"", rel first x, "\">", html last x, "</a>")
tex  HREF := x -> concatenate("\\special{html:<a href=\"", texLiteral rel first x, "\">}", tex last x, "\\special{html:</a>}")
html LABEL:= x -> concatenate("<label title=\"", x#0, "\">", html x#1, "</label>")
html TO   := x -> concatenate(
     "<a href=\"",
     rel htmlFilename x#0,
     "\" title=\"",
     headline x#0,
     "\">",
     htmlLiteral DocumentTag.FormattedKey x#0,
     "</a>",
     if x#?1 then x#1)
html TO2  := x -> concatenate("<a href=\"", rel htmlFilename x#0, "\">", htmlLiteral x#1, "</a>")

next := tag -> if NEXT#?tag then ( HREF { htmlFilename NEXT#tag, nextButton }, " | ")
prev := tag -> if PREV#?tag then ( HREF { htmlFilename PREV#tag, prevButton }, " | ")
up   := tag -> if   UP#?tag then ( HREF { htmlFilename   UP#tag,   upButton }, " | ")

FIRST := tag -> (while PREV#?tag do tag = PREV#tag; tag)
LAST  := tag -> (while NEXT#?tag do tag = NEXT#tag; tag)

FORWARD0  := tag -> if NEXT#?tag then NEXT#tag else if UP#?tag then FORWARD0 UP#tag
FORWARD   := tag -> if linkTable#?tag and length linkTable#tag > 0 then          first linkTable#tag else FORWARD0 tag
BACKWARD0 := tag -> if linkTable#?tag and length linkTable#tag > 0 then BACKWARD0 last linkTable#tag else tag
BACKWARD  := tag -> if PREV#?tag then BACKWARD0 PREV#tag else if UP#?tag then UP#tag

forward  := tag -> ( f := FORWARD  tag; if f =!= null then ( HREF { htmlFilename f, forwardButton }, " | "))
backward := tag -> ( b := BACKWARD tag; if b =!= null then ( HREF { htmlFilename b, backwardButton}, " | "))

linkTitle := s -> concatenate( " title=\"", s, "\"" )
linkTitleTag := tag -> concatenate( " title=\"", DocumentTag.FormattedKey tag, commentize headline tag, "\"" )
links := tag -> (
     f := FORWARD tag;
     b := BACKWARD tag;
     SEQ {
	  if topDocumentTag =!= null then LINK { htmlDirectory|topFileName,   " rel=\"Top\"", linkTitleTag topDocumentTag},
	  LINK { htmlDirectory|indexFileName, " rel=\"Index\""},
	  LINK { htmlDirectory|tocFileName,   " rel=\"Table-of-Contents\""},
	  LINK { Macaulay2HomePage (), " rel=\"Macaulay-2-Home-Page\""},
	  if f =!= null then LINK { htmlFilename f, " rel=\"Next\"", linkTitleTag f},
	  if b =!= null then LINK { htmlFilename b, " rel=\"Previous\"", linkTitleTag b},
	  if NEXT#?tag then SEQ {
	       LINK { htmlFilename NEXT#tag, " rel=\"Forward\"", linkTitleTag NEXT#tag},
	       LINK { htmlFilename LAST tag, " rel=\"Last\"", linkTitleTag LAST tag}
	       },
	  if PREV#?tag then SEQ {
	       LINK { htmlFilename PREV#tag, " rel=\"Backward\"", linkTitleTag PREV#tag},
	       LINK { htmlFilename FIRST tag, " rel=\"First\"", linkTitleTag FIRST tag},
	       },
	  if UP#?tag then LINK { htmlFilename UP#tag, " rel=\"Up\"", linkTitleTag UP#tag},
	  LINK { LAYOUT #"packagesrc" "Style" | "doc.css", " rel=\"stylesheet\" type=\"text/css\"" },
	  LINK { LAYOUT #"packagesrc" "Style" | "doc-no-buttons.css", " rel=\"alternate stylesheet\" title=\"no buttons\" type=\"text/css\"" }
	  }
     )

BUTTON := (s,alt) -> (
     s = rel s;
     if alt === null
     then error "required attribute: ALT"
     else LITERAL concatenate("<img src=\"",s,"\" alt=\"[", alt, "]\"/>\n")
     )

html HTML := t -> concatenate(
     "<?xml version=\"1.0\" encoding=\"us-ascii\"?>", newline,
     "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1 plus MathML 2.0//EN\" \"http://www.w3.org/TR/MathML2/dtd/xhtml-math11-f.dtd\" >", newline,
     "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\">", apply(t,html), "</html>",newline
     )

-- validate := LITERAL ///
-- <a href="http://validator.w3.org/check/referer">Validate</a> the html on this page, or <a href="http://jigsaw.w3.org/css-validator/check/referer">validate</a> the css on this page.
-- ///

-- produce html form of documentation, for Macaulay 2 and for packages

buttonBar := (tag) -> ButtonTABLE {{ 
--	  LITERAL concatenate (
--	       "<form action=\"",
--	       if getenv "SEARCHENGINE" === "" then "http://rhenium.math.uiuc.edu:7003/" else getenv "SEARCHENGINE",
--	       "\">
--     	         <div>Search:
--		   <input type=\"text\"   name=\"words\"  />
--		   <input type=\"hidden\" name=\"method\" value=\"boolean\" />
--		   <input type=\"hidden\" name=\"format\" value=\"builtin-short\" />
--		   <input type=\"hidden\" name=\"sort\"   value=\"score\" />
--		   <input type=\"hidden\" name=\"config\" value=\"htdig-M2\" />
--     	         </div>
--	       </form>
--	       "),
	  DIV splice {
     	       forward tag,
	       backward tag,
	       next tag,
	       prev tag,
	       up tag,
     	       if tag =!= topDocumentTag then (topNodeButton, " | "),
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
		    tag => first \ select(if doc.?Subnodes then toList doc.Subnodes else {}, x -> class x === TO))
	       else (
		    tag => {}
		    )
	       ));
     CONT = getTrees();
     if debugLevel > 0 then (
	  n := net CONT;
	  f := (ht,dp) -> (stack (ht + dp : "** "^-1)) ^ ht;
	  n = f(height n, depth n) | n;
	  stderr << "tree structure:" << endl << n << endl;
	  );
     buildLinks CONT;
     )

-----------------------------------------------------------------------------
-- making the html pages
-----------------------------------------------------------------------------

-- setupButtons := () -> (
--      gifpath := LAYOUT#"images";
--      topNodeButton = LABEL { "top node", HREF { htmlDirectory|topFileName, BUTTON (gifpath|"top.gif","top") } };
--      tocButton = LABEL { "table of contents", HREF { htmlDirectory|tocFileName, BUTTON (gifpath|"toc.gif","toc") } };
--      homeButton = LABEL { "table of contents", HREF { Macaulay2HomePage, BUTTON (gifpath|"home.gif","home") } };
--      nullButton = BUTTON(gifpath|"null.gif","null");
--      masterIndexButton = LABEL { "index", HREF { htmlDirectory|indexFileName, BUTTON(gifpath|"index.gif","index") } };
--      nextButton = BUTTON(gifpath|"next.gif","next");
--      prevButton = BUTTON(gifpath|"previous.gif","previous");
--      upButton = BUTTON(gifpath|"up.gif","up");
--      )
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

separateRegexp = method()
separateRegexp(String,String) := (re,s) -> separateRegexp(re,0,s)
separateRegexp(String,ZZ,String) := (re,n,s) -> (
     m := regex(re,s);
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
     anchorPoint = 0;
     fn := buildDirectory | htmlDirectory | indexFileName;
     title := "Symbol Index";
     stderr << "--making  '" << title << "' in " << fn << endl;
     fn
     << html HTML {
	  HEAD { TITLE title, links() },
	  BODY {
	       DIV { topNodeButton, " | ", tocButton, " | ", homeButton },
	       HR{},
	       HEADER1 title,
	       DIV between(LITERAL "&nbsp;&nbsp;&nbsp;",apply(alpha, c -> HREF {"#"|c, c})), 
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
     stderr << "--making  '" << title << "' in " << fn << endl;
     fn
     << html HTML {
	  HEAD { TITLE title, links() },
	  BODY {
	       DIV { topNodeButton, " | ", masterIndexButton, " | ", homeButton },
	       HR{},
	       HEADER1 title,
	       toDoc CONT
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
     ldpkg := "-e 'needsPackage \""|toString pkg|"\"'";
     args := "--silent --print-width 80 --stop --int -e errorDepth=0" | (if usermode then "" else " -q") | " " | ldpkg;
     cmdname := commandLine#0;
     if ulimit === null then (
	  ulimit = utest " -t 40" | utest " -m 90000"| utest " -v 90000";
	  );
     tmpf << "-- -*- M2-comint -*-" << endl << close;
     cmd := ulimit | "cd " | rundir | "; " | cmdname | " " | args | " <" | format inf | " >>" | format tmpf | " 2>&1";
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
	  stderr << tmpf << ":0: error output left in this file, return code: (" << r//256 << "," << r%256 << ")" << endl;
	  stderr << aftermatch(M2errorRegexp,get tmpf);
	  if r == 131 then (
	       stderr << "subprocess terminated abnormally, exiting" << endl;
	       exit r;
	       );
	  haderror = true;
	  numerrors = numerrors + 1;
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
     result)

check = method()
check Package := pkg -> (
     usermode := false;					    -- fix this later as an option to "check" or something!
     scan(values pkg#"test inputs", s -> runString(s,pkg,".",usermode));
     if haderror then error(toString numerrors, " error(s) occurred running tests for package ", toString pkg);
     )

setupNames := (opts,pkg) -> (
     buildPackage = pkg#"title";
     buildDirectory = minimizeFilename(runfun opts.PackagePrefix | "/");
     if opts.Encapsulate then buildDirectory = buildDirectory|buildPackage|"-"|pkg.Options.Version|"/";
     )

installPackage = method(Options => { 
	  PackagePrefix => () -> applicationDirectory() | "encap/",
          InstallPrefix => () -> applicationDirectory() | "local/",
	  UserMode => true,
	  Encapsulate => true,
	  IgnoreExampleErrors => false,
	  MakeDocumentation => true,
	  MakeInfo => false,
	  RemakeAllDocumentation => false,
	  RerunExamples => false,
	  AbsoluteLinks => false,
	  MakeLinks => true,
	  RunDirectory => ".",
	  DebuggingMode => false
	  })

uninstallPackage = method(Options => options installPackage)
uninstallPackage String := opts -> pkg -> (
     if isGlobalSymbol pkg and class value getGlobalSymbol pkg === Package then return uninstallPackage(value getGlobalSymbol pkg, opts);
     needsPackage pkg;
     if class value pkg === Package then return uninstallPackage(value pkg, opts);
     error ("can't locate package '",pkg,"'");
     )

uninstallPackage Package := o -> pkg -> (
     setupNames(o,pkg);
     initInstallDirectory o;
     stderr << "--uninstalling package " << pkg << " in " << buildDirectory << endl;
     -- unmake symbolic links
     if o.Encapsulate and o.MakeLinks then (
	  symlinkDirectory(buildDirectory, installDirectory, Verbose => debugLevel > 0, Undo => true);
	  );
     )

reloadPackage := (pkg,opts) -> (
     stderr << "--reloading package \"" << pkg << "\"" << endl;
     fl := forceLoadDocumentation;
     forceLoadDocumentation = true;
     dismiss pkg;
     p := loadPackage(pkg, DebuggingMode => opts.DebuggingMode);
     forceLoadDocumentation = fl;
     p)

installPackage String := opts -> pkg -> (
     if PackageDictionary#?pkg and class value PackageDictionary#pkg === Package then (
	  PKG := value PackageDictionary#pkg;
	  if not opts.MakeDocumentation or PKG#?"processed documentation database" and isOpen PKG#"processed documentation database"
     	  then return installPackage(PKG, opts);
	  );
     installPackage(reloadPackage(pkg,opts), opts))

installPackage Package := opts -> pkg -> (
     if pkg =!= Macaulay2Core then (
     	  pkg = reloadPackage(pkg#"title",opts);
     	  rawDoc := pkg#"raw documentation";
     	  if #rawDoc == 0 then stderr << "--warning: package seems to have no documentation" << endl;
	  );

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
     copyFile(fn, buildDirectory|pkgDirectory|bn, Verbose => debugLevel > 0);

     if pkg === Macaulay2Core then (
	  ) else (
     	  
	  -- copy package source subdirectory
	  srcDirectory := LAYOUT#"packagesrc" pkg#"title";
	  dn := realpath(currentSourceDir|buildPackage);
	  if fileExists dn
	  then (
	       stderr << "--copying auxiliary source files from " << dn << endl;
	       makeDirectory (buildDirectory|srcDirectory);
	       buildDirectory|srcDirectory|".linkdir" << close;
	       copyDirectory(dn, buildDirectory|srcDirectory, UpdateOnly => true, Verbose => debugLevel > 0, Exclude => {"^CVS$"});
	       );
     	  );

     -- copy package source subdirectory examples
     exampleDir := buildDirectory|LAYOUT#"packageexamples" pkg#"title";
     en := realpath(currentSourceDir|buildPackage|"/examples/");
     if fileExists en then (
	  stderr << "--copying example files from " << en << endl;
	  copyDirectory(en, exampleDir, Verbose => debugLevel > 0, Exclude => {"^CVS$"}, UpdateOnly => true);
	  );

     if opts.MakeDocumentation then (
	  -- This is a bit of a fiction: we've copied the files for our package into the build directory,
	  -- so let's pretend we loaded the package from there in the first place, thereby allowing "documentation()"
	  -- to find the example output files the same way it would if the package had been loaded from there.
	  oldPackagePrefix := pkg#"package prefix";
	  if oldPackagePrefix === null then oldPackagePrefix = buildDirectory;
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

	  -- check for obsolete example input files and remove them
	  scan(readDirectory exampleDir, fn -> (
		    fn = exampleDir | fn;
		    if match("\\.m2$",fn) and not exampleInputFiles#?fn then (
			 stderr << "--warning: removing obsolete example input file: " <<  fn << endl;
			 removeFile fn;
			 )));

     --     -- make test input files
     --     testsDir := buildDirectory|LAYOUT#"packagetests" pkg#"title";
     --     infn2  := n -> testsDir|toString n|".m2";
     --     outfn2 := n -> testsDir|toString n|".out";
     --     tmpfn2 := n -> testsDir|toString n|".errors";
     --     stderr << "--making test input files in " << testsDir << endl;
     --     makeDirectory testsDir;
     --     testsDir|".linkdir" << close;
     --     scan(pairs pkg#"test inputs", (key,str) -> if class str === String then (
     --	       (n,fn) := key;
     --	       inf := infn2 n;
     --	       val := str | "\n";
     --	       if fileExists inf and get inf === val
     --	       then (
     --		    if debugLevel > 1 then stderr << "--leaving test input file: " << key << endl;
     --		    )
     --	       else (
     --		    if debugLevel > 1 then stderr << "--making test input file: " << key << endl;
     --		    inf << val << close;
     --		    )));

	  -- cache raw documentation in database, and check for changes
	  rawDocUnchanged := new MutableHashTable;
	  docDir := pkg#"package prefix" | LAYOUT#"packagedoc" pkg#"title";
	  rawdbname := docDir | "rawdocumentation.db";
	  rawdbnametmp := rawdbname | ".tmp";
	  stderr << "--storing raw documentation in " << rawdbname << endl;
	  makeDirectory docDir;
	  docDir|".linkdir" << close;
	  if fileExists rawdbnametmp then unlinkFile rawdbnametmp;
	  if fileExists rawdbname then (
	       tmp := openDatabase rawdbname;   -- just to make sure the database file isn't open for writing
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
	  moveFile(rawdbnametmp,rawdbname);
	  rawkey := "raw documentation database";
	  pkg#rawkey = openDatabase rawdbname;
	  addEndFunction(() -> if pkg#?rawkey and isOpen pkg#rawkey then close pkg#rawkey);

	  -- run tests that are functions
	  stderr << "--running tests that are functions " << exampleDir << endl;
	  scan(pairs pkg#"test inputs", (key,str) -> if class str === Function then (
		    stderr << "--  running test " << key << ", function " << str << endl;
		    str();
		    ));

     	  -- Make sure the processed documentation database exists, even if empty, so when running the examples,
	  -- M2 doesn't read in all the documentation sources each time.  For the package Macaulay2 this makes a big
	  -- difference.
	  dbname := docDir | "documentation.db";
     	  if opts.RemakeAllDocumentation and fileExists dbname then removeFile dbname;
     	  if not fileExists dbname then (
	       stderr << "--creating empty database for processed documentation in " << dbname << endl;
	       close openDatabaseOut dbname;
	       );

	  -- make example output files, or else copy them from old package directory tree
	  exampleDir' := oldPackagePrefix|LAYOUT#"packageexamples" pkg#"title";
	  infn' := fkey -> exampleDir'|toFilename fkey|".m2";
	  outfn' := fkey -> exampleDir'|toFilename fkey|".out";
	  stderr << "--making example result files in " << exampleDir << endl;
	  haderror = false;
	  numerrors = 0;
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
		    else if not opts.RerunExamples and inf != inf' and fileExists inf' and fileExists outf' and fileTime outf' >= fileTime inf' and get inf == get inf'
		    then copyFile(outf',outf)
		    else runFile(inf,outf,tmpf,desc,pkg,changefun,opts.RunDirectory,opts.UserMode);
		    -- read, separate, and store example output
		    if fileExists outf then pkg#"example results"#fkey = drop(separateM2output get outf,-1)
		    else (
			 if debugLevel > 1 then stderr << "--warning: missing file " << outf << endl;
			 )
		    ));
 	  if not opts.IgnoreExampleErrors then if haderror then error(toString numerrors, " error(s) occurred running example files");

     --      -- make test output files, or else copy them from the old package directory tree
     --      oldTestsDir := oldPackagePrefix|LAYOUT#"packagetests" pkg#"title";
     --      infn2'  := n -> oldTestsDir|toString n|".m2";
     --      outfn2' := n -> oldTestsDir|toString n|".out";
     --      stderr << "--making test result files in " << testsDir << endl;
     --      haderror = false;
     --      scan(pairs pkg#"test inputs", (key,inputs) -> (
     --      	       -- args:
     -- 	       (n,fn) := key;
     -- 	       inf := infn2 n;
     -- 	       outf := outfn2 n;
     -- 	       tmpf := tmpfn2 n;
     -- 	       inf' := infn2' n;
     -- 	       outf' := outfn2' n;
     -- 	       desc := "test results for " | toString key;
     --      	       if fileExists outf and fileTime outf >= fileTime inf then (
     -- 		    -- do nothing
     -- 		    )
     -- 	       else if inf != inf' and fileExists inf' and fileExists outf' and fileTime outf' >= fileTime inf' and get inf == get inf'
     -- 	       then copyFile(outf',outf)
     -- 	       else (
     -- 		    -- error "debug me";
     -- 		    runFile(inf,outf,tmpf,desc,pkg,identity,".");
     -- 		    );
     -- 	       ));
     --      if haderror then error "error(s) occurred running test files";

	  -- process documentation
	  stderr << "--processing documentation nodes..." << endl;
	  scan(nodes, tag -> if not isUndocumented tag then (
		    fkey := DocumentTag.FormattedKey tag;
		    if not opts.RemakeAllDocumentation and rawDocUnchanged#?fkey then (
			 if debugLevel > 0 then stderr << "--skipping   " << tag << endl;
			 )
		    else (
			 if debugLevel > 0 then stderr << "--processing " << tag << endl;
			 pkg#"processed documentation"#fkey = documentation tag;
			 );
		    ));

	  -- cache processed documentation in database
	  dbnametmp := dbname | ".tmp";
	  if fileExists dbnametmp then unlinkFile dbnametmp;
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
	       stderr << "--not making info file" << endl;
	       );

	  -- make html files
	  htmlDirectory = LAYOUT#"packagehtml" pkg#"title";
	  setupButtons();
	  makeDirectory (buildDirectory|htmlDirectory);
	  buildDirectory|htmlDirectory|".linkdir" << close;
	  stderr << "--making html pages in " << buildDirectory|htmlDirectory << endl;
	  scan(nodes, tag -> (
	       -- key := DocumentTag.Key tag;
	       fkey := DocumentTag.FormattedKey tag;
	       fn := buildDirectory | htmlFilename tag;
	       if fileExists fn and not opts.RemakeAllDocumentation and rawDocUnchanged#?fkey then return;
	       if isSecondary tag then return;
	       if debugLevel > 0 then stderr << "--making html page for " << tag << endl;
	       fn
	       << html HTML { 
		    HEAD {
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
	  makeMasterIndex select(nodes,tag -> instance(DocumentTag.Key tag,Symbol));

	  -- make table of contents
	  makeTableOfContents();

     	  );						    -- end if opts.MakeDocumentation

     -- make postinstall and preremove files, if encap
     if opts.Encapsulate then (
	  octal := s -> (n := 0 ; z := first ascii "0"; scan(ascii s, i -> n = 8*n + i - z); n);
	  stderr << "--making postinstall, preremove, and encapinfo files in " << buildDirectory << endl;
	  f := buildDirectory | "postinstall" 
	  << ///#! /bin/sh -e/// << endl
	  << ///cd "$ENCAP_SOURCE/$ENCAP_PKGNAME/info" || exit 0/// << endl
	  << ///for i in *.info/// << endl
	  << ///do (set -x ; install-info --dir-file="$ENCAP_TARGET/info/dir" "$i")/// << endl
	  << ///done/// << endl;
	  if version#"dumpdata" and pkg#"title" == "Macaulay2" then (
	       f << endl << "(set -x ; \"$ENCAP_SOURCE\"/\"$ENCAP_PKGNAME\"/bin/" << version#"M2 name" << " --stop --dumpdata)" << endl;
	       );
	  fileMode(f,octal "755");
	  f << close;
     	  f = buildDirectory | "preremove"
	  << ///#! /bin/sh -x/// << endl
	  << ///cd "$ENCAP_SOURCE/$ENCAP_PKGNAME/info" || exit 0/// << endl
	  << ///for i in *.info/// << endl
	  << ///do (set -x ; install-info --dir-file="$ENCAP_TARGET/info/dir" --delete "$i")/// << endl
	  << ///done/// << endl;
	  fileMode(f,octal "755");
 	  f << close;
	  f = buildDirectory | "encapinfo"
	  << ///encap 2.0/// << endl
	  << ///contact dan@math.uiuc.edu/// << endl;
	  removeLastSlash := s -> if s#?0 and s#-1 === "/" then substring(s,0,#s-1) else s;
	  scan(("libm2","packagedoc","packageexamples","packagehtml","packageimages","packagesrc","packagetests"),
	       k -> f << "linkdir " << (if class LAYOUT#k === Function then removeLastSlash LAYOUT#k "*" else removeLastSlash LAYOUT#k) << endl);
	  f << close;
	  );

     -- make symbolic links
     if opts.Encapsulate and opts.MakeLinks then (
     	  stderr << "--making symbolic links from \"" << installDirectory << "\" to \"" << buildDirectory << "\"" << endl;
	  symlinkDirectory(buildDirectory, installDirectory, Verbose => debugLevel > 0)
	  );

     -- all done
     stderr << "--installed package " << pkg << " in " << buildDirectory << endl;
     currentPackage = oldpkg;
     if prefixDirectory =!= null then makePackageIndex();
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
-- Sequence.AfterPrint = Sequence.AfterNoPrint = identity

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

(load "M2-init.el" t)

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

makePackageIndex = method(SingleArgumentDispatch => true)
makePackageIndex Sequence := () -> makePackageIndex packagePath
makePackageIndex List := packagePath -> (
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
	  HEAD {
	       TITLE {key, commentize headline key},
	       links()
	       },
	  BODY { 
	       -- buttonBar tag, HR{},
	       PARA {
		    "This is the top level documentation page for Macaulay 2 and its packages."
		    },
	       HEADER3 "Documentation",
	       UL splice {
               	    if prefixDirectory =!= null then HREF { prefixDirectory | LAYOUT#"packagehtml" "Macaulay2" | "index.html", "Macaulay 2" },
		    apply(toSequence packagePath, prefixDirectory -> (
			      p := prefixDirectory | LAYOUT#"docm2";
			      if isDirectory p then (
				   r := readDirectory p;
				   r = select(r, fn -> fn != "." and fn != ".."
					-- and fn != "Macaulay2"
					);
				   r = select(r, pkg -> fileExists (prefixDirectory | LAYOUT#"packagehtml" pkg | "index.html"));
				   r = sort r;
				   SEQ {
					HEADER3 {"Packages in ", prefixDirectory},
					UL apply(r, pkg -> HREF { prefixDirectory | LAYOUT#"packagehtml" pkg | "index.html", pkg }) 
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
check := ret -> if ret != 0 then error "--error: external command failed"

URL = new Type of BasicList
show = method()
show URL := x -> (
     url := x#0;
     if runnable "firefox" then check run("firefox "|url|"&") -- big problem: we can't predict whether firefox will exit immediately, so we have to run in the backgroun always, sigh
     else if runnable "open" then (
	  check run("open \""|url|"\"")
	  )
     else if runnable "netscape" then check run("netscape -remote \"openURL("|url|")\"") 
     else error "can't find firefox, open, or netscape"
     )

fix := fn -> "file://" | replace(" ","%20",fn) 		    -- might want to replace more characters
showHtml = show Hypertext := x -> (
     fn := temporaryFileName() | ".html";
     fn << "<title>Macaulay 2 Output</title>" << endl << html x << endl << close;
     show new URL from { fix fn };
     addEndFunction( () -> run ( "rm " | fn ) );
     )

show TEX := x -> showTex x

viewHelp = key -> (
     if prefixDirectory === null then error "can't run viewHelp from build tree";
     show new URL from { fix if key === () then applicationDirectory() | "index.html" else prefixDirectory | htmlFilename key }
     )
viewHelp = new Command from viewHelp

indexHtml = dir -> (
     if not isDirectory dir then error "expected a directory";
     title := baseFilename dir;
     ind := minimizeFilename (dir|"/index.html");
     if fileExists ind then (
	  if not match("generated by indexHtml",get ind) then error("file not made by indexHtml already present: ",ind);
	  );
     ind = openOut ind;
     ind << ///
<?xml version="1.0" encoding="us-ascii"?>
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
     << ///</h1>///;
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
