-- -*- fill-column: 107 -*-
--		Copyright 1993-2002 by Daniel R. Grayson
-- TODO: rename this file

-----------------------------------------------------------------------------
-- Generate documentation
-----------------------------------------------------------------------------

Macaulay2HomePage := () -> "http://www.math.uiuc.edu/Macaulay2/"

-----------------------------------------------------------------------------
-- html output
-----------------------------------------------------------------------------

endswith := (suff,str) -> substring(str,-#suff) == suff

-- we've turned off checking for existence of files...

local prefix; local topNodeButton
local nullButton; local masterIndexButton; local tocButton; local homeButton; -* local directoryButton; *-
local NEXT; local PREV; local UP; local tableOfContents; local linkTable; local SRC
local nextButton; local prevButton; local upButton; local backwardButton; local forwardButton
local masterIndex

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
indexFileName = "master.html"  			    	    -- file name for master index of topics in a package
tocFileName = "toc.html"       			    	    -- file name for the table of contents of a package
installPrefix = null	  	       	    	      	    -- the installation prefix
installLayout = null		     	       	    	    -- the layout of the installPrefix, global for communication to document.m2
installLayoutIndex = null				    -- the layout index of the installPrefix, equal to 1 or 2
htmlDirectory = ""					    -- relative path to the html directory, depends on the package

runfun := o -> if instance(o, Function) then o() else o
initInstallDirectory := o -> (
     installPrefix = realpath toAbsolutePath(runfun o.InstallPrefix);
     if not match("/$",installPrefix) then installPrefix = installPrefix | "/";
     installLayoutIndex = detectCurrentLayout installPrefix;
     if installLayoutIndex === null then installLayoutIndex = if o.SeparateExec then 2 else 1;
     installLayout = Layout#installLayoutIndex;
     )

-----------------------------------------------------------------------------
-- relative URLs and filenames
-----------------------------------------------------------------------------

isAbsoluteURL := url -> match( "^(#|mailto:|[a-z]+://)", url )

-- TODO: phase this one out eventually
toURL = method()
toURL String := pth -> (
     if isAbsolutePath pth then concatenate(rootURI,
	  if fileExists pth then realpath pth 
	  else (
	       stderr << "-- *** warning: file needed for URL not found: " << pth << endl;
	       pth))
     else if isAbsoluteURL pth then pth
     else (
	  r := if htmlDirectory === null then pth else relativizeFilename(htmlDirectory, pth);
	  if debugLevel == 121 then (
	       stderr << "--toURL String: htmlDirectory   = " << htmlDirectory << endl;
	       stderr << "--              pth             = " << pth << endl;
	       stderr << "--              relative result = " << r << endl;
	       );
	  r))

toURL (String,String) := (prefix,tail) -> (		    -- this is the good one
     -- we assume we are installing an html file in the directory installPrefix|htmlDirectory
     r := if prefix === installPrefix    -- note: installPrefix might be null, if we aren't installing a package
          and htmlDirectory =!= null
          then relativizeFilename(htmlDirectory,tail) 
          else prefix|tail;
     if debugLevel == 121 then (
	  stderr << "--toURL(String,String): htmlDirectory = " << htmlDirectory << endl;
	  stderr << "--                      prefix        = " << prefix << endl;
	  stderr << "--                      result        = " << r << endl;
	  );
     r)

htmlFilename1 = (fkey,pkgname,layout) -> (
     basefilename := if fkey === pkgname then topFileName else toFilename fkey|".html";
     replace("PKG",pkgname,layout#"packagehtml") | basefilename)

htmlFilename2 = (tag,layout) -> (
     fkey := formattedKey tag;
     pkgname := packageName tag;
     htmlFilename1(fkey,pkgname,layout))

htmlFilename = method(Dispatch => Thing)
htmlFilename DocumentTag := tag -> (
     pkgname := packageName tag;
     fkey := formattedKey tag;
     basefilename := if fkey === pkgname then topFileName else toFilename fkey|".html";
     if currentPackage#"pkgname" === pkgname
     then (
	  layout := installLayout;
	  prefix := installPrefix;
	  )
     else (
	  pkginfo := getPackageInfo pkgname;
	  if pkginfo === null 
	  then (
	       layout = installLayout; -- we assume, perhaps mistakenly, that this uninstalled package will be installed under the same prefix eventually
     	       prefix = installPrefix;
	       )
	  else (
	       layout = Layout#(pkginfo#"layout index");
	       prefix = pkginfo#"prefix";
	       ));
     if layout === null then error ("package ",pkgname," is not installed on the prefixPath");
     tail := replace("PKG",pkgname,layout#"packagehtml") | basefilename;
     assert isAbsolutePath prefix;
     (prefix,tail)					    -- this pair will be processed by toURL(String,String)
     )
htmlFilename Thing := x -> htmlFilename makeDocumentTag x

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


linkTitle := s -> concatenate( " title=\"", htmlLiteral s, "\"" )
linkTitleTag := tag -> "pkgname" => htmlLiteral concatenate(DocumentTag.FormattedKey tag, commentize headline tag)

-- produce html form of documentation, for Macaulay2 and for packages

buttonBar := tag -> TABLE { "class" => "buttons",
    TR { TD { DIV splice {
		forward tag, backward tag, next tag, prev tag, up tag,
		(if tag =!= topDocumentTag then topNodeButton else topNodeButton#-1, " | "),
		masterIndexButton, " | ", tocButton, -* " | ", directoryButton, *- " | ", homeButton
		}}}}

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
     fn := installPrefix | htmlDirectory | indexFileName;
     title := DocumentTag.FormattedKey topDocumentTag | " : Index";
     if verbose then stderr << "--making '" << title << "' in " << fn << endl;
     r := HTML {
	  defaultHEAD title,
	  BODY nonnull {
	       DIV { topNodeButton, " | ", tocButton, -* " | ", directoryButton, *- " | ", homeButton },
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
     fn := installPrefix | htmlDirectory | tocFileName;
     title := DocumentTag.FormattedKey topDocumentTag | " : Table of Contents";
     if verbose then stderr << "--making  " << title << "' in " << fn << endl;
     fn
     << html HTML {
	  defaultHEAD title,
	  BODY {
	       DIV { topNodeButton, " | ", masterIndexButton, -* " | ", directoryButton, *- " | ", homeButton },
	       HR{},
	       HEADER1 title,
	       toDoc tableOfContents
	       }
	  } << endl << close
     )



setupNames := (opts,pkg) -> (
     installPrefix = minimizeFilename(runfun opts.InstallPrefix | "/");
     buildPackage = pkg#"pkgname";
     )
unsetupNames := () -> installPrefix = installLayout = buildPackage = null


installPackage = method(
     TypicalValue => Package,
     Options => {
	  SeparateExec => false,
          InstallPrefix => () -> applicationDirectory() | "local/",
	  UserMode => null,
	  IgnoreExampleErrors => false,
	  FileName => null,
	  CacheExampleOutput => null,			    -- overrides the value specified by newPackage if true or false
	  CheckDocumentation => true,
	  MakeDocumentation => true,
	  MakeInfo => true,
	  RemakeAllDocumentation => true,		    -- until we get better dependency graphs between documentation nodes, "false" here will confuse users
	  RerunExamples => false,
	  RunExamples => true,
	  MakeLinks => true,
	  Verbose => true,
	  DebuggingMode => null
	  })
uninstallPackage = method(Options => { 
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
     installPrefix := minimizeFilename(runfun opts.InstallPrefix | "/");
     apply(findFiles apply({1,2},
	       i -> apply(flatten {
	       		 Layout#i#"packages"|pkg|".m2", Layout#i#"info"|pkg|".info",
			 apply({"package","packagelib","packagedoc"}, f -> replace("PKG", pkg, Layout#i#f))
	       		 }, 
	  	    p -> installPrefix|p)),
	  removeFiles);
     tallyInstalledPackages();
     )

installPackage String := opts -> pkg -> (
     -- if pkg =!= "Macaulay2Doc" then needsPackage "Macaulay2Doc";  -- load the core documentation
     -- -- we load the package even if it's already been loaded, because even if it was loaded with
     -- -- its documentation the first time, it might have been loaded at a time when the core documentation
     -- -- in the "Macaulay2Doc" package was not yet loaded
     -- ... but we want to build the package Style without loading any other packages
     pkg = loadPackage(pkg, DebuggingMode => opts.DebuggingMode, LoadDocumentation => opts.MakeDocumentation, FileName => opts.FileName, Reload => true);
     installPackage(pkg, opts))

dispatcherMethod := m -> m#-1 === Sequence and (
     f := lookup m;
     any(dispatcherFunctions, g -> functionBody f === functionBody g))

reproduciblePaths = outstr -> (
     if topSrcdir === null then return outstr;
     srcdir := regexQuote toAbsolutePath topSrcdir;
     prefixdir := regexQuote prefixDirectory;
     builddir := replace("usr-dist/?$", "", prefixdir);
     homedir := replace("/$", "", regexQuote homeDirectory);
     if any({srcdir, builddir, homedir}, dir -> match(dir, outstr))
     then (
	 -- .m2 files in source directory
	 outstr = replace(srcdir | "Macaulay2/\\b(m2|Core)\\b",
	     finalPrefix | Layout#1#"packages" | "Core", outstr);
	 outstr = replace(srcdir | "Macaulay2/packages/",
	     finalPrefix | Layout#1#"packages", outstr);
	 -- generated .m2 files in build directory (tvalues.m2)
	 outstr = replace(builddir | "Macaulay2/m2",
	     finalPrefix | Layout#1#"packages" | "Core", outstr);
	 -- everything in staging area
	 scan({"bin", "data", "lib", "program licenses", "programs"}, key ->
	     outstr = replace(prefixdir | Layout#2#key,
		 finalPrefix | Layout#1#key, outstr));
	 outstr = replace(prefixdir, finalPrefix, outstr);
	 -- usr-build/bin is in PATH during build
	 outstr = replace(builddir | "usr-build/", finalPrefix, outstr);
	 -- home directory
	 outstr = replace(homedir, "/home/m2user", outstr);
	 );
     outstr
    )

installPackage Package := opts -> pkg -> (
     tallyInstalledPackages();
     verbose := opts.Verbose or debugLevel > 0;

     use pkg;
     chkdoc = opts.CheckDocumentation;			    -- oops, this will have a lingering effect...

     if opts.MakeDocumentation and pkg#?"documentation not loaded"
     then pkg = loadPackage(pkg#"pkgname", DebuggingMode => opts.DebuggingMode, LoadDocumentation => true, FileName => opts.FileName);

     oldpkg := currentPackage;
     currentPackage = pkg;
     topDocumentTag = makeDocumentTag(pkg#"pkgname", Package => pkg);
     
     -- here's where we get the list of nodes from the raw documentation
     nodes := if opts.MakeDocumentation then packageTagList(pkg,topDocumentTag) else {};
     
     setupNames(opts,pkg);
     initInstallDirectory opts;				    -- initializes installPrefix

     if (options pkg).Headline === null then error ("no Headline option provided to newPackage for ",pkg#"pkgname");
     if (options pkg).Headline === ""   then error ("empty string given as Headline option to newPackage for ",pkg#"pkgname");

     if verbose then stderr << "--installing package " << pkg << " in " << installPrefix << " with layout " << installLayoutIndex << endl;
     
     currentSourceDir := pkg#"source directory";

     if currentSourceDir === installPrefix | installLayout#"packages" then error "the package is already installed there, and has been loaded from there";

     if verbose then stderr << "--using package sources found in " << currentSourceDir << endl;

     -- copy package source file
     pkgDirectory := installLayout#"packages";
     makeDirectory (installPrefix|pkgDirectory);
     bn := buildPackage | ".m2";
     fn := currentSourceDir|bn;
     if not fileExists fn then error("file ", fn, " not found");
     copyFile(fn, installPrefix|pkgDirectory|bn, Verbose => debugLevel > 5);

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
	  srcDirectory := replace("PKG",pkg#"pkgname",installLayout#"package");
	  auxiliaryFilesDirectory := realpath currentSourceDir | buildPackage;
	  if isDirectory auxiliaryFilesDirectory
	  then (
	       if not (options pkg).AuxiliaryFiles
	       then error ("package ",toString pkg," has auxiliary files in \"",auxiliaryFilesDirectory,"\", but newPackage wasn't given AuxiliaryFiles=>true");
	       if verbose then stderr << "--copying auxiliary source files from " << auxiliaryFilesDirectory << endl;
	       makeDirectory (installPrefix|srcDirectory);
	       copyDirectory(auxiliaryFilesDirectory, installPrefix|srcDirectory, UpdateOnly => true, Verbose => verbose, excludes);
	       )
	  else (
	       if (options pkg).AuxiliaryFiles
	       then error ("package ",toString pkg," has no directory of auxiliary files, but newPackage was given AuxiliaryFiles=>true");
	       )
     	  );

     -- copy package source subdirectory examples
     exampleOutputDir := installPrefix|replace("PKG",pkg#"pkgname",installLayout#"packageexampleoutput");

     if opts.MakeDocumentation then (
	  pkg#"package prefix" = installPrefix;

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
	  rawdbname := databaseFilename(installLayout,pkg#"package prefix",pkg#"pkgname");
	  rawdbnametmp := rawdbname | ".tmp";
	  if verbose then stderr << "--storing raw documentation in " << rawdbname << endl;
	  makeDirectory databaseDirectory(installLayout,pkg#"package prefix",pkg#"pkgname");
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
	  if verbose then stderr << "--closed the database" << endl;

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
	       m := regex("\\`.* hash: *(-?[0-9]+)",f);    -- this regular expression must detect the format used above
	       if m =!= null then value substring(f,m#1#0,m#1#1));
	  if verbose then stderr << "--making example result files in " << exampleOutputDir << endl;
	  hadError = false;
	  numErrors = 0;
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
			 and pkg.Options.UseCachedExampleOutput
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
		    if fileExists outf then (
			 outstr := reproduciblePaths get outf;
			 outf << outstr << close;
			 pkg#"example results"#fkey = drop(separateM2output outstr,-1)
		    )
		    else (
			 if debugLevel > 1 then stderr << "--warning: missing file " << outf << endl;
			 )
		    ));

	  if not opts.IgnoreExampleErrors
	  then if hadError then error(toString numErrors,
	       " error(s) occurred running examples for package ",
	       pkg#"pkgname",
	       if verbose then ":" | newline | newline |
	            concatenate apply(select(readDirectory(exampleOutputDir),
		         file -> match("\\.errors$", file)), err ->
		              err | newline |
			      concatenate(width err : "*") | newline |
			      get("!tail " | exampleOutputDir | err))
	       else "");

	  -- if no examples were generated, then remove the directory
	  if length readDirectory exampleOutputDir == 2 then
	  	  removeDirectory exampleOutputDir;

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
	       moveFile(rawdbnametmp,rawdbname,Verbose=>debugLevel>0);
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
	       scan((if pkg#"pkgname" == "Macaulay2Doc" then Core else pkg)#"exported symbols", s -> (
			 tag := makeDocumentTag s;
			 if not isUndocumented tag and not hasDocumentation s and signalDocError tag
			 then stderr << "--warning: symbol has no documentation: " << tag << ", package " << packageName tag << endl;
			 f := value s;
			 if instance(f, Function) then (
			      scan(methods f, m -> if isDocumentableMethod m then (
					tag := makeDocumentTag m;
					if not isUndocumented tag and not dispatcherMethod m and not hasDocumentation m and signalDocError tag
					then stderr << "--warning: method has no documentation: " << tag << ", key " << toExternalString DocumentTag.Key tag << ", package " << packageName tag << endl;
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
	       infodir := installPrefix|installLayout#"info";
	       makeDirectory infodir;
	       infotitle := pkg#"pkgname";
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
			 if DocumentTag.PackageName tag =!= pkg#"pkgname" then (
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
	  htmlDirectory = replace("PKG",pkg#"pkgname",installLayout#"packagehtml");
	  setupButtons();
	  makeDirectory (installPrefix|htmlDirectory);
	  if verbose then stderr << "--making empty html pages in " << installPrefix|htmlDirectory << endl;
	  if pkg.Options.Headline =!= null then (
	       installPrefix|htmlDirectory|".Headline" << pkg.Options.Headline << close;
	       );
	  if pkg.Options.Certification =!= null then (
	       installPrefix|htmlDirectory|".Certification" << toExternalString pkg.Options.Certification << close;
	       );
	  scan(nodes, tag -> if not isUndocumented tag then (
		    fkey := DocumentTag.FormattedKey tag;
		    (prefix,tail) := htmlFilename tag;
		    fn := prefix|tail;
		    if fileExists fn then return;
		    if isSecondary tag then return;
		    if debugLevel > 0 then stderr << "--creating empty html page for " << tag << " in " << fn << endl;
		    fn << close));
	  for n in (topFileName, indexFileName, tocFileName) do (
	       fn := installPrefix | replace("PKG",pkg#"pkgname",installLayout#"packagehtml") | n;
	       if not fileExists fn then (
		    if debugLevel > 0 then stderr << "--creating empty html page " << fn << endl;
		    fn << close)
	       else (
		    if debugLevel > 0 then stderr << "--html page exists: " << fn << endl;
		    ));
	  if verbose then stderr << "--making html pages in " << installPrefix|htmlDirectory << endl;
     	  scan(nodes, tag -> if not isUndocumented tag then (
	       -- key := DocumentTag.Key tag;
	       fkey := DocumentTag.FormattedKey tag;
	       (prefix,tail) := htmlFilename tag;
	       fn := prefix|tail;
	       if fileExists fn and fileLength fn > 0 and not opts.RemakeAllDocumentation and rawDocUnchanged#?fkey then return;
	       if isSecondary tag then return;
	       if debugLevel > 0 then stderr << "--making html page for " << tag << endl;
	       fn
	       << html HTML { 
		    defaultHEAD {fkey, commentize headline fkey},
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
	  makeMasterIndex(select(nodes,tag -> not isUndocumented tag -* and instance(DocumentTag.Key tag,Symbol) *- ), verbose);

	  -- make table of contents
	  maketableOfContents verbose;

     	  );						    -- end if opts.MakeDocumentation

     -- all done
     SRC = null;
     if not hadError then (
 	  libDir := pkg#"package prefix" | replace("PKG",pkg#"pkgname",installLayout#"packagelib");
	  iname := libDir|".installed";
	  iname << close;
	  if verbose then stderr << "--file created: " << iname << endl;
	  );
     if verbose then stderr << "--installed package " << pkg << " in " << installPrefix << endl;
     currentPackage = oldpkg;
     if not noinitfile then (
	  setUpApplicationDirectory();
	  makePackageIndex();
	  );
     unsetupNames();
     htmlDirectory = null;
     tallyInstalledPackages();
     pkg)

makePackageIndex = method(Dispatch => Thing)
makePackageIndex Sequence := x -> if x === () then makePackageIndex path else error "expected no arguments"
-- this might get too many files (formerly we used packagePath)
makePackageIndex List := path -> (
    -- TO DO : rewrite this function to use the results of tallyInstalledPackages
     tallyInstalledPackages();
     makingPackageIndex = true;
     initInstallDirectory options installPackage;
     key := "Macaulay2";
     star := IMG {
	  "src" => prefixDirectory | replace("PKG","Style",currentLayout#"package") | "GoldStar.png",
	  "alt" => "a gold star"
	  };
     htmlDirectory = applicationDirectory();
     fn := htmlDirectory | topFileName;
     if notify then stderr << "--making index of installed packages in " << fn << endl;
     if debugLevel > 10 then (
	  stderr << "--prefixPath = " << stack prefixPath << endl
	      << "--path = " << stack path << endl
	      << "--prefixDirectory = " << prefixDirectory << endl;
	  );
     docdirdone := new MutableHashTable;
     fn << html HTML { 
	  defaultHEAD {key},
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
			 m2doc := prefixDirectory | replace("PKG","Macaulay2Doc",currentLayout#"packagehtml") | topFileName;
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
					r = select(r, pkg -> fileExists (pkghtmldir pkg | topFileName));
					r = sort r;
					DIV {
					     HEADER3 {"Packages in ", toAbsolutePath prefixDirectory},
					     if #r > 0 then UL apply(r, pkg -> LI splice {
						       if fileExists (pkghtmldir pkg | ".Certification") then (
							    star, " "
							    ),
						       HREF { pkghtmldir pkg | topFileName, pkg },
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
     htmlDirectory = null;
     )

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
