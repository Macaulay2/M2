-- -*- fill-column: 107 -*-
--		Copyright 1993-2002 by Daniel R. Grayson
-- TODO: add regex option to readDirectory
-- TODO: add relative directory to minimizeFilename
-- TODO: generate parent nodes for orphan nodes based on their type
-- TODO: make orphan overview nodes subnodes of the top node
-- TODO: not reentrant yet, see resetCounters

-----------------------------------------------------------------------------
-- Generate the html documentation
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- Local variables
-----------------------------------------------------------------------------

chkdoc := true
seenErrors   := new MutableHashTable
seenWarnings := new MutableHashTable
hadDocumentationError    := false
numDocumentationErrors   := 0
hadDocumentationWarning  := false
numDocumentationWarnings := 0

-- The default values are set so "(html, Hypertext)" works before Macaulay2Doc is installed.
-- TODO: They should be functions rather than global values
topDocumentTag := null
installPrefix   = applicationDirectory() | "local/"  -- default the installation prefix
installLayout   = Layout#2			     -- the layout of the installPrefix, global for communication to document.m2
htmlDirectory   = ""	      -- relative path to the html directory, depends on the package

indexFileName  := "master.html"   -- file name for master index of topics in a package
tocFileName    := "toc.html"      -- file name for the table of contents of a package

-----------------------------------------------------------------------------
-- Local utilities
-----------------------------------------------------------------------------

resetCounters := () -> (
    seenErrors   = new MutableHashTable;
    seenWarnings = new MutableHashTable;
    hadDocumentationError  = hadDocumentationWarning  = false;
    numDocumentationErrors = numDocumentationWarnings = 0)

signalDocumentationError   = tag -> not seenErrors#?tag and chkdoc and (
    numDocumentationErrors = numDocumentationErrors + 1;
    hadDocumentationError  = seenErrors#tag = true)

-- also called from document.m2 and html.m2, temporarily
signalDocumentationWarning   = tag -> not seenWarnings#?tag and chkdoc and (
    numDocumentationWarnings = numDocumentationWarnings + 1;
    hadDocumentationWarning  = seenWarnings#tag = true)

runfun := f -> if instance(f, Function) then f() else f

-- returns the Layout index of the installPrefix, equal to 1 or 2
initInstallDirectory := opts -> (
    installPrefix = toAbsolutePath opts.InstallPrefix;
    if not fileExists installPrefix then makeDirectory installPrefix;
    installPrefix = realpath installPrefix;
    installLayoutIndex := detectCurrentLayout installPrefix;
    if installLayoutIndex === null then installLayoutIndex = if opts.SeparateExec then 2 else 1;
    installLayout = Layout#installLayoutIndex;
    installLayoutIndex)

-----------------------------------------------------------------------------
-- htmlFilename
-----------------------------------------------------------------------------
-- determines the normalized filename of a key or tag
htmlFilename = method(Dispatch => Thing)
htmlFilename Thing       := key -> htmlFilename makeDocumentTag key
htmlFilename DocumentTag := tag -> (
    fkey := format tag;
    pkgname := tag.Package;
    basefilename := if fkey === pkgname then topFileName else toFilename fkey | ".html";
    if currentPackage#"pkgname" === pkgname then (layout, prefix) := (installLayout, installPrefix)
    else (
	pkginfo := getPackageInfo pkgname;
	-- we assume, perhaps mistakenly, that this package
	-- will be installed under the same prefix eventually
	(layout, prefix) = if pkginfo === null then (installLayout, installPrefix)
	else (Layout#(pkginfo#"layout index"), pkginfo#"prefix"));
    if layout === null then error("package ", pkgname, " is not installed on the prefixPath");
    tail := replace("PKG", pkgname, layout#"packagehtml") | basefilename;
    assert isAbsolutePath prefix;
    (prefix, tail))					    -- this pair will be processed by toURL(String,String)

-----------------------------------------------------------------------------
-- produce html form of documentation, for Macaulay2 and for packages
-----------------------------------------------------------------------------

checkIsTag := tag -> ( assert(instance(tag, DocumentTag)); tag )

alpha := characters "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
numAnchorsMade := 0
makeAnchors := n -> (
     ret := SPAN apply(take(alpha, {numAnchorsMade, n-1}), c -> ANCHOR{ "id" => c, ""});
     numAnchorsMade = n;
     ret)
anchorsUpTo := entry -> if alpha#?numAnchorsMade and entry >= alpha#numAnchorsMade then makeAnchors length select(alpha, c -> entry >= c)
remainingAnchors := () -> makeAnchors (#alpha)

packageTagList := (pkg, topDocumentTag) -> checkIsTag \ unique nonnull join(
    apply(pairs pkg.Dictionary, (name, sym) ->
	if not match("\\$", name) then makeDocumentTag(sym, Package => pkg)),
    apply(values pkg#"raw documentation", rawdoc -> rawdoc.DocumentTag),
    { topDocumentTag })

-----------------------------------------------------------------------------
-- helper functions for assembleTree
-----------------------------------------------------------------------------

-- assemble this next
ForestNode = new Type of BasicList			    -- list of tree nodes, the descendent list
TreeNode   = new Type of BasicList			    -- first entry is DocumentTag for this node, second entry is a forest node

net ForestNode := x -> stack apply(toList x,net)
net TreeNode   := x -> (
    y := net x#1;
    net x#0 || (stack (height y + depth y : " |  ")) | y)

toDoc := method()
toDoc ForestNode := x -> if #x>0 then UL apply(toList x, y -> toDoc y)
toDoc TreeNode   := x -> DIV nonnull { TOH checkIsTag x#0, toDoc x#1 }

traverse := method()
traverse(ForestNode, Function) := (n, f) -> scan(n, t -> traverse(t, f))
traverse(TreeNode,   Function) := (t, f) -> (f t#0, traverse(t#1, f))

-----------------------------------------------------------------------------

-- a directed acyclic graph of nodes -> subnodes
-- TODO: make this functional
graph := new MutableHashTable

-- a depth-first search
makeTree := (parent, graph, visits, node) -> (
    visits#"parents"#node = if visits#"parents"#?node then append(visits#"parents"#node, parent) else {parent};
    tree := if graph#?node then (
	if #visits#"parents"#node == 1 then new ForestNode from apply(graph#node, child -> makeTree(node, graph, visits, child))
	else if not visits#"repeated"#?node and signalDocumentationError node then visits#"repeated"#node = true)
    else if not visits#"missing"#?node and signalDocumentationError node then visits#"missing"#node = true;
    if tree === null then tree = new ForestNode;
    new TreeNode from {node, tree})

-- pre-compute the list of orphan nodes in a graph, with parent
-- node (typcally topDocumentTag) appearing in the beginning
orphanNodes := (parent, graph) -> (
    nonLeaves := set keys graph - set flatten values graph;
    if not nonLeaves#?parent and signalDocumentationWarning parent then printerr("warning: top node ", parent, " not a root");
    if #nonLeaves > 1 then printerr("warning: found ", toString(#nonLeaves - 1), " documentation node(s) not listed as a subnode");
    unique prepend(parent, sort keys nonLeaves))

makeForest := (graph, visits) -> (
    new ForestNode from apply(orphanNodes(topDocumentTag, graph),
	node -> makeTree(topDocumentTag, graph, visits, node)))

-----------------------------------------------------------------------------

local NEXT
local PREV
local UP

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
-- constructs the tree-structure for the Subnodes of each node
-----------------------------------------------------------------------------

assembleTree := (pkg, nodes) -> (
    resetCounters();
    -- keep track of various possible issues with the nodes
    visits := new HashTable from {
	"parents"  => new MutableHashTable,
	"missing"  => new MutableHashTable,
	"repeated" => new MutableHashTable,
	};
    -- collect links from each tag to its subnodes
    graph = new HashTable from apply(nodes, tag -> (
	    checkIsTag tag;
	    fkey := format tag;
	    if  pkg#"raw documentation"#?fkey
	    and pkg#"raw documentation"#fkey.?Subnodes then (
		subnodes := pkg#"raw documentation"#fkey.Subnodes;
		subnodes  = select(deepApply(subnodes, identity), DocumentTag);
		subnodes  = select(subnodes, node -> package node === pkg);
		tag => getPrimaryTag \ subnodes)
	    else tag => {}));
    -- build the forest
    tableOfContents := makeForest(graph, visits);
    -- signal errors
    if chkdoc and hadDocumentationError then (
	scan(keys visits#"missing",
	    node -> (
		printerr("error: missing reference(s) to subnode documentation: ", format node);
		printerr("  Parent nodes: ", demark_", " (format \ unique visits#"parents"#node))));
	scan(keys visits#"repeated",
	    node -> (
		printerr("error: repeated references to subnode documentation: ", format node);
		printerr("  Parent nodes: ", demark_", " (format \ unique visits#"parents"#node))));
	error("installPackage: error in assembling the documentation tree"));
    -- build the navigation links
    buildLinks tableOfContents;
    tableOfContents)

-----------------------------------------------------------------------------
-- Buttons for the top
-----------------------------------------------------------------------------

FORWARD0  := tag -> if NEXT#?tag then NEXT#tag else if UP#?tag then FORWARD0 UP#tag
FORWARD   := tag -> if graph#?tag and length graph#tag > 0 then          first graph#tag else FORWARD0 tag
BACKWARD0 := tag -> if graph#?tag and length graph#tag > 0 then BACKWARD0 last graph#tag else tag
BACKWARD  := tag -> if PREV#?tag then BACKWARD0 PREV#tag else if UP#?tag then UP#tag

-- buttons at the top
topNodeButton  := (htmlDirectory, topFileName)   -> HREF {htmlDirectory | topFileName,   "top" };
indexButton    := (htmlDirectory, indexFileName) -> HREF {htmlDirectory | indexFileName, "index"};
tocButton      := (htmlDirectory, tocFileName)   -> HREF {htmlDirectory | tocFileName,   "toc"};
homeButton     := HREF {"http://macaulay2.com/", "Macaulay2 website"};

nextButton     := tag -> if NEXT#?tag then HREF { htmlFilename NEXT#tag, "next" }     else "next"
prevButton     := tag -> if PREV#?tag then HREF { htmlFilename PREV#tag, "previous" } else "previous"
forwardButton  := tag -> ( f :=  FORWARD tag; if f =!= null then HREF { htmlFilename f, "forward" }  else "forward" )
backwardButton := tag -> ( b := BACKWARD tag; if b =!= null then HREF { htmlFilename b, "backward" } else "backward" )
upButton       := tag -> if   UP#?tag then HREF { htmlFilename   UP#tag, "up" } else "up"
topButton      := tag -> if tag =!= topDocumentTag then topNodeButton(htmlDirectory, topFileName) else "top"

-- TODO: revamp this using Bootstrap
buttonBar := tag -> TABLE { "class" => "buttons", TR { TD { DIV splice between_" | " {
		nextButton tag, prevButton tag, forwardButton tag, backwardButton tag, upButton tag, topButton tag,
		indexButton(htmlDirectory, indexFileName), tocButton(htmlDirectory, tocFileName), homeButton}}}}

-----------------------------------------------------------------------------
-- makePackageIndex
-----------------------------------------------------------------------------

star := IMG {
    "src" => prefixDirectory | replace("PKG", "Style", currentLayout#"package") | "GoldStar.png",
    "alt" => "a gold star"}

findDocumentationPaths := path -> (
    unique nonnull flatten apply(path, dir -> (
	    dir = if isDirectory dir then realpath dir else return;
	    apply(keys Layout, i -> (
		    packagesdir := Layout#i#"packages";
		    if not match(packagesdir | "$", dir) then return;
		    prefix := substring(dir, 0, #dir - #packagesdir);
		    (prefix, i))))))

-- TODO: this function runs on startup, unless -q is given, and takes about 0.1~0.2s
makePackageIndex = method(Dispatch => Thing)
makePackageIndex Sequence := x -> if x === () then makePackageIndex path else error "expected no arguments"
-- this might get too many files (formerly we used packagePath)
makePackageIndex List := path -> (
    verboseLog := if debugLevel > 10 then printerr else identity;
    -- TO DO : rewrite this function to use the results of tallyInstalledPackages
    tallyInstalledPackages();
    initInstallDirectory options installPackage;
    docdirs := toSequence findDocumentationPaths path;
    htmlDirectory = applicationDirectory();
    indexFilename := htmlDirectory | topFileName;
    verboseLog("making index of installed packages in ", indexFilename);
    indexFilename << html validate HTML {
	defaultHEAD { "Macaulay2" },
	BODY {
	    PARA {"This is the directory for Macaulay2 and its packages. Bookmark this page for future reference,
		or run the ", TT "viewHelp", " command in Macaulay2 to open up your browser on this page.
		See the ", homeButton, " for the latest version."},
	    HEADER3 "Documentation",
	    UL nonnull splice {
		if prefixDirectory =!= null then (
		    m2doc := prefixDirectory | replace("PKG", "Macaulay2Doc", currentLayout#"packagehtml") | topFileName;
		    if fileExists m2doc then LI HREF { m2doc, "Macaulay2 documentation" }),
		apply(docdirs, dirs -> (
			prefixDirectory := first dirs;
			layout := Layout#(last dirs);
			docdir := prefixDirectory | layout#"docdir";
			verboseLog("checking documentation directory ", docdir);
			pkghtmldir := pkgname -> prefixDirectory | replace("PKG", pkgname, layout#"packagehtml");
			contents := select(readDirectory docdir, fn -> fn != "." and fn != ".." );
			contents  = select(contents, pkg -> fileExists(pkghtmldir pkg | topFileName));
			if #contents > 0 then LI {
			    HEADER3 {"Packages in ", TT toAbsolutePath prefixDirectory},
			    UL apply(sort contents, pkgname -> (
				    pkgopts := readPackage pkgname;
				    LI nonnull splice {
					HREF { pkghtmldir pkgname | topFileName, pkgname }, -- TO (pkgname | "::" | pkgname),
					if pkgopts.Certification =!= null then (" ", star),
					if pkgopts.Headline      =!= null then commentize pkgopts.Headline}
				    ))
			    }))
		}}} << endl << close;
    htmlDirectory = null;)

-----------------------------------------------------------------------------
-- install PDF documentation for package
-----------------------------------------------------------------------------
-- see book.m2

-----------------------------------------------------------------------------
-- install info documentation for package
-----------------------------------------------------------------------------

upto30 := t -> concatenate(t, 30-#t : " ");

installInfo := (pkg, installPrefix, installLayout, verboseLog) -> (
    topNodeName := format makeDocumentTag(pkg#"pkgname", Package => pkg);

    infoTagConvert' := n -> if topNodeName === n     then "Top" else infoTagConvert n;
    chkInfoKey      := n -> if topNodeName === "Top" then n else if n === "Top" then error "installPackage: encountered a documentation node named 'Top'";
    chkInfoTag      := t -> if package t =!= pkg     then error("installPackage: alien entry in table of contents: ", toString t);

    pushvar(symbol printWidth, 79);

    infotitle       := pkg#"pkgname";
    infobasename    := infotitle | ".info";
    infodir         := installPrefix | installLayout#"info";

    verboseLog("making info file ", infodir | infobasename);
    makeDirectory infodir;
    infofile := openOut(infodir | infobasename);

    infofile << " -*- coding: utf-8 -*- This is " << infobasename << ", produced by Macaulay2, version " << version#"VERSION" << endl << endl;
    infofile << "INFO-DIR-SECTION " << pkg.Options.InfoDirSection << endl;
    infofile << "START-INFO-DIR-ENTRY" << endl;
    infofile << upto30 concatenate( "* ", infotitle, ": (", infotitle, ").") << "  " << pkg.Options.Headline << endl;
    infofile << "END-INFO-DIR-ENTRY" << endl << endl;

    byteOffsets := new MutableHashTable;
    traverse(unbag pkg#"table of contents", tag -> (
	    currentDocumentTag = tag; -- for debugging purposes
	    fkey := format tag;
	    chkInfoTag tag;
	    chkInfoKey fkey;
	    byteOffsets# #byteOffsets = concatenate("Node: ", infoTagConvert' fkey, "\177", toString fileLength infofile);
	    infofile << "\037" << endl << "File: " << infobasename << ", Node: " << infoTagConvert' fkey;
	    if NEXT#?tag then infofile << ", Next: " << infoTagConvert' format NEXT#tag;
	    if PREV#?tag then infofile << ", Prev: " << infoTagConvert' format PREV#tag;
	    -- nodes without an Up: link tend to make the emacs info reader unable to construct the table of contents,
	    -- and the display isn't as nice after that error occurs
	    infofile << ", Up: " << if UP#?tag then infoTagConvert' format UP#tag else "Top";
	    infofile << endl << endl << info fetchProcessedDocumentation(pkg, fkey) << endl));
    infofile << "\037" << endl << "Tag Table:" << endl;
    scan(values byteOffsets, b -> infofile << b << endl);
    infofile << "\037" << endl << "End Tag Table" << endl;
    infofile << close;

    popvar(symbol printWidth))

-----------------------------------------------------------------------------
-- install HTML documentation for package
-----------------------------------------------------------------------------

upAncestors := tag -> reverse(
    n := 0; prepend(tag, while UP#?tag and n < 20 list (n = n+1; tag = UP#tag)))

makeSortedIndex := (nodes, verboseLog) -> (
     numAnchorsMade = 0;
     fn := installPrefix | htmlDirectory | indexFileName;
     title := format topDocumentTag | " : Index";
     verboseLog("making ", format title, " in ", fn);
     fn << html validate HTML {
	  defaultHEAD title,
	  BODY nonnull {
	       DIV { topNodeButton(htmlDirectory, topFileName), " | ", tocButton(htmlDirectory, tocFileName), -* " | ", directoryButton, *- " | ", homeButton },
	       HR{},
	       HEADER1 title,
	       DIV between(LITERAL "&nbsp;&nbsp;&nbsp;",apply(alpha, c -> HREF {"#"|c, c})),
	       if #nodes > 0 then UL apply(sort select(nodes, tag -> not isUndocumented tag), (tag) -> (
			 checkIsTag tag;
			 anch := anchorsUpTo tag;
			 if anch === null then LI TOH tag else LI {anch, TOH tag})),
	       DIV remainingAnchors()
	       }} << endl << close)

makeTableOfContents := (pkg, verboseLog) -> (
     fn := installPrefix | htmlDirectory | tocFileName;
     title := format topDocumentTag | " : Table of Contents";
     verboseLog("making  ", format title, " in ", fn);
     fn << html validate HTML {
	  defaultHEAD title,
	  BODY {
	       DIV { topNodeButton(htmlDirectory, topFileName), " | ", indexButton(htmlDirectory, indexFileName), -* " | ", directoryButton, *- " | ", homeButton },
	       HR{},
	       HEADER1 title,
	       toDoc unbag pkg#"table of contents"
	       }
	  } << endl << close)

installHTML := (pkg, installPrefix, installLayout, verboseLog, rawDocumentationCache, opts) -> (
    topDocumentTag := makeDocumentTag(pkg#"pkgname", Package => pkg);
    nodes := packageTagList(pkg, topDocumentTag);

    htmlDirectory = replace("PKG", pkg#"pkgname", installLayout#"packagehtml");

    makeDirectory(installPrefix | htmlDirectory);
    verboseLog("making html pages in ", minimizeFilename installPrefix | htmlDirectory);
    if pkg.Options.Certification =!= null then
    (installPrefix | htmlDirectory | ".Certification") << toExternalString pkg.Options.Certification << close;
    (installPrefix | htmlDirectory | ".Headline") << pkg.Options.Headline << close;
    for n in (topFileName, indexFileName, tocFileName) do (
	fn := installPrefix | htmlDirectory | n;
	if fileExists fn then (
	    verboseLog("creating empty html page ", minimizeFilename fn);
	    fn << close)
	else verboseLog("html page exists: ", minimizeFilename fn));
    scan(nodes, tag -> if not isUndocumented tag then (
	    currentDocumentTag = tag; -- for debugging purposes
	    fkey := format tag;
	    fn := concatenate htmlFilename tag;
	    if isSecondaryTag tag
	    or fileExists fn and fileLength fn > 0 and not opts.RemakeAllDocumentation and rawDocumentationCache#?fkey then return;
	    verboseLog("making html page for ", toString tag);
	    fn << html validate HTML {
		defaultHEAD {fkey, commentize headline fkey},
		BODY {
		    buttonBar tag,
		    if UP#?tag
		    then DIV between(" > ", apply(upAncestors tag, i -> TO i))
		    else DIV (TO topDocumentTag, " :: ", TO tag),
		    HR{},
		    fetchProcessedDocumentation(pkg, fkey)
		    }
		} << endl << close));

    -- make the alphabetical index
    makeSortedIndex(nodes, verboseLog);

    -- make the table of contents
    makeTableOfContents(pkg, verboseLog);
    )

-----------------------------------------------------------------------------
-- helper functions for installPackage
-----------------------------------------------------------------------------

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

generateExampleResults := (pkg, rawDocumentationCache, exampleDir, exampleOutputDir, verboseLog, pkgopts, opts) -> (
    fnbase := temporaryFileName();
    inpfn  := fkey ->           fnbase | toFilename fkey | ".m2";
    outfn' := fkey ->       exampleDir | toFilename fkey | ".out";
    outfn  := fkey -> exampleOutputDir | toFilename fkey | ".out";
    errfn  := fkey -> exampleOutputDir | toFilename fkey | ".errors";
    gethash := outf -> (
	f := get outf;
	-- this regular expression detects the format used in runFile
	m := regex("\\`.* hash: *(-?[0-9]+)", f);
	if m =!= null then value substring(m#1, f));
    changeFunc := fkey -> () -> remove(rawDocumentationCache, fkey);

    possiblyCache := (outf, outf') -> fkey -> (
	if opts.CacheExampleOutput =!= false and pkgopts.CacheExampleOutput === true
	and ( not fileExists outf' or fileExists outf' and fileTime outf > fileTime outf' ) then (
	    verboseLog("caching example results for ", fkey, " in ", outf');
	    if not isDirectory exampleDir then makeDirectory exampleDir;
	    copyFile(outf, outf', Verbose => true)));

    scan(pairs pkg#"example inputs", (fkey, inputs) -> (
	    inpf  := inpfn  fkey; -- input file
	    outf' := outfn' fkey; -- cached file
	    outf  := outfn  fkey; -- output file
	    errf  := errfn  fkey; -- error file
	    inputhash := hash inputs;
	    -- use cached example results
	    if  not opts.RunExamples
	    or  not opts.RerunExamples and fileExists outf  and gethash outf  === inputhash then (
		(possiblyCache(outf, outf'))(fkey))
	    -- use distributed example results
	    else if pkgopts.UseCachedExampleOutput
	    and not opts.RerunExamples and fileExists outf' and gethash outf' === inputhash then (
		if fileExists errf then removeFile errf; copyFile(outf', outf))
	    -- run and capture example results
	    else elapsedTime captureExampleOutput(
		pkg, fkey, demark_newline inputs,
		possiblyCache(outf, outf'),
		inpf, outf, errf,
		inputhash, changeFunc,
		if opts.UserMode === null then not noinitfile else opts.UserMode, verboseLog);
	    storeExampleOutput(pkg, fkey, outf, verboseLog)));

    -- check for obsolete example output files and remove them
    if chkdoc then (
	exampleOutputFiles := set apply(keys pkg#"example inputs", outfn);
	scan(readDirectory exampleOutputDir, fn -> (
		fn = exampleOutputDir | fn;
		if match("\\.out$", fn) and not exampleOutputFiles#?fn then removeFile fn)));
    )

-----------------------------------------------------------------------------
-- installPackage
-----------------------------------------------------------------------------

installPackage = method(
    TypicalValue => Package,
    Options => {
	-- overrides the value specified by newPackage if true or false
	CacheExampleOutput     => null,
	CheckDocumentation     => true,
	DebuggingMode          => null,
	FileName               => null,
	IgnoreExampleErrors    => false,
	InstallPrefix          => applicationDirectory() | "local/",
	MakeDocumentation      => true,
	MakeHTML               => true,
	MakeInfo               => true,
	MakePDF                => false,
	MakeLinks              => true,
	-- until we get better dependency graphs between documentation
	-- nodes, "false" here will confuse users
	RemakeAllDocumentation => true,
	RerunExamples          => false,
	RunExamples            => true,
	SeparateExec           => false,
	UserMode               => null,
	Verbose                => false
	})

installPackage String := opts -> pkg -> (
    -- if pkg =!= "Macaulay2Doc" then needsPackage "Macaulay2Doc";  -- load the core documentation
    -- -- we load the package even if it's already been loaded, because even if it was loaded with
    -- -- its documentation the first time, it might have been loaded at a time when the core documentation
    -- -- in the "Macaulay2Doc" package was not yet loaded
    -- ... but we want to build the package Style without loading any other packages
    pkg = loadPackage(pkg,
	FileName          => opts.FileName,
	DebuggingMode     => opts.DebuggingMode,
	LoadDocumentation => opts.MakeDocumentation,
	Reload            => true);
    installPackage(pkg, opts))

installPackage Package := opts -> pkg -> (
    tallyInstalledPackages();
    verboseLog := if opts.Verbose or debugLevel > 0 then printerr else identity;

    use pkg;
    -- TODO: make this more functional
    chkdoc = opts.CheckDocumentation;			    -- oops, this will have a lingering effect...

    if opts.MakeDocumentation and pkg#?"documentation not loaded"
    then pkg = loadPackage(pkg#"pkgname",
	FileName          => opts.FileName,
	DebuggingMode     => opts.DebuggingMode,
	LoadDocumentation => true);
    pkgopts := options pkg;
    if pkgopts.Headline === null or pkgopts.Headline === ""
    then error("installPackage: expected non-empty Headline in package ", toString pkg);

    pushvar(symbol currentPackage, pkg);
    topDocumentTag = makeDocumentTag(pkg#"pkgname", Package => pkg);

    -- set installPrefix, installLayout, and installLayoutIndex
    installLayoutIndex := initInstallDirectory opts; -- TODO: make installPrefix and installLayout non-global
    verboseLog("installing package ", toString pkg, " in ", minimizeFilename installPrefix, " with layout #", toString installLayoutIndex);

    currentSourceDir := pkg#"source directory";

    if currentSourceDir === installPrefix | installLayout#"packages"
    then error("installPackage: the package is already installed and loaded from ", minimizeFilename currentSourceDir);

    verboseLog("using package sources found in ", minimizeFilename currentSourceDir);

    -- copy package source file
    makeDirectory(installPrefix | installLayout#"packages");
    bn := pkg#"pkgname" | ".m2";
    fn := currentSourceDir | bn;
    if not fileExists fn then error("installPackage: file ", fn, " not found");
    copyFile(fn, installPrefix | installLayout#"packages" | bn,
	Verbose => debugLevel > 5);

    -- copy package auxiliary files
    if pkg =!= Core then (
	srcDirectory := replace("PKG", pkg#"pkgname", installLayout#"package");
	auxiliaryFilesDirectory := realpath currentSourceDir | pkg#"pkgname";
	if isDirectory auxiliaryFilesDirectory then (
	    if not pkgopts.AuxiliaryFiles
	    then error("installPackage: package ", toString pkg," has auxiliary files in ",
		format auxiliaryFilesDirectory, ", but newPackage wasn't given AuxiliaryFiles => true");
	    verboseLog("copying auxiliary source files from ", auxiliaryFilesDirectory);
	    makeDirectory(installPrefix | srcDirectory);
	    copyDirectory(auxiliaryFilesDirectory, installPrefix | srcDirectory,
		Exclude    => {"^CVS$", "^\\.svn$", "Makefile"},
		UpdateOnly => true,
		Verbose    => opts.Verbose or debugLevel > 0))
	else if pkgopts.AuxiliaryFiles
	then error("installPackage: package ", toString pkg, " has no directory of auxiliary files, but newPackage was given AuxiliaryFiles => true"));

    if opts.MakeDocumentation then (
	-- here's where we get the list of nodes from the raw documentation
	nodes := packageTagList(pkg, topDocumentTag);

	pkg#"package prefix" = installPrefix;

	-- copy package doc subdirectory if we loaded the package from a distribution
	-- ... to be implemented, but we seem to be copying the examples already, but only partially

	-- cache raw documentation in database, and check for changes
	rawDocumentationCache := new MutableHashTable;
	rawdbname    := databaseFilename(installLayout, pkg#"package prefix", pkg#"pkgname");
	rawdbnametmp := rawdbname | ".tmp";
	verboseLog("storing raw documentation in ", minimizeFilename rawdbname);
	makeDirectory databaseDirectory(installLayout, pkg#"package prefix", pkg#"pkgname");
	if fileExists rawdbnametmp then removeFile rawdbnametmp;
	if fileExists rawdbname then (
	    tmp := openDatabase rawdbname;   -- just to make sure the database file isn't open for writing
	    copyFile(rawdbname, rawdbnametmp);
	    close tmp);
	rawdocDatabase := openDatabaseOut rawdbnametmp;
	rawDoc := pkg#"raw documentation";
	-- remove any keys from the processed database no longer used
	scan(keys rawdocDatabase - set keys rawDoc, key -> remove(rawdocDatabase, key));
	scan(nodes, tag -> (
		fkey := format tag;
		if rawDoc#?fkey then (
		    v := evaluateWithPackage(getpkg "Text", rawDoc#fkey, toExternalString);
		    if rawdocDatabase#?fkey
		    then if rawdocDatabase#fkey === v then rawDocumentationCache#fkey = true else rawdocDatabase#fkey = v
		    else (
			rawdocDatabase#fkey = v;
			verboseLog("new raw documentation, not already in database, for ", fkey)))
		else if rawdocDatabase#?fkey
		then printerr("warning: raw documentation for ", fkey, ", in database, is no longer present")
		else rawDocumentationCache#fkey = true;
		));
	close rawdocDatabase;
	verboseLog "closed the database";

	-- run tests that are functions
	-- TODO: is this used anywhere?
	verboseLog "running tests that are functions";
	scan(pairs pkg#"test inputs", (key, str) -> if instance(str, Function) then (
		verboseLog("  running test ", key, ", function ", str);
		str()));

	-- directories for cached and generated example outputs
	exampleDir := realpath currentSourceDir | pkg#"pkgname" | "/examples" | "/";
	exampleOutputDir := installPrefix | replace("PKG", pkg#"pkgname", installLayout#"packageexampleoutput");
	makeDirectory exampleOutputDir;

	-- make example output files, or else copy them from old package directory tree
	verboseLog("making example result files in ", minimizeFilename exampleOutputDir);
	(hadError, numErrors) = (false, 0); -- declared in run.m2
	generateExampleResults(pkg, rawDocumentationCache, exampleDir, exampleOutputDir, verboseLog, pkgopts, opts);

	if not opts.IgnoreExampleErrors and hadError
	then error("installPackage: ", toString numErrors, " error(s) occurred running examples for package ", pkg#"pkgname",
	    if opts.Verbose or debugLevel > 0 then ":" | newline | newline |
	    concatenate apply(select(readDirectory exampleOutputDir, file -> match("\\.errors$", file)), err ->
		err | newline |	concatenate(width err : "*") | newline | get("!tail " | exampleOutputDir | err)) else "");

	-- if no examples were generated, then remove the directory
	if length readDirectory exampleOutputDir == 2 then removeDirectory exampleOutputDir;

	-- process documentation
	verboseLog "processing documentation nodes...";
	-- ~50s -> ~100s for Macaulay2Doc
	scan(nodes, tag ->
	    if      isUndocumented tag              then verboseLog("undocumented ", toString tag)
	    else if isSecondaryTag tag              then verboseLog("is secondary ", toString tag)
	    else if not opts.RemakeAllDocumentation
	    and     not opts.MakeInfo -- when making the info file, we need to process all the documentation
	    and rawDocumentationCache#?(format tag) then verboseLog("skipping     ", toString tag)
	    else storeProcessedDocumentation(pkg, tag, opts, verboseLog));

	-- should this be here, or farther up? Note: assembleTree resets the counters, so stay above that.
	if chkdoc and hadDocumentationError then error(
	    toString numDocumentationErrors, " errors(s) occurred in processing documentation for package ", toString pkg);

	if pkg#?rawKeyDB and isOpen pkg#rawKeyDB then close pkg#rawKeyDB;

	shield ( moveFile(rawdbnametmp, rawdbname, Verbose => debugLevel > 1); );

	pkg#rawKeyDB = openDatabase rawdbname;
	addEndFunction(() -> if pkg#?rawKeyDB and isOpen pkg#rawKeyDB then close pkg#rawKeyDB);

	-- make table of contents, including next, prev, and up links
	verboseLog("assembling table of contents");
	tableOfContents := assembleTree(pkg, getPrimaryTag \ select(nodes, tag -> not isUndocumented tag));
	-- if chkdoc then stderr << "+++++" << endl << "table of contents, in tree form:" << endl << tableOfContents << endl << "+++++" << endl;
	pkg#"table of contents" = Bag {tableOfContents}; -- we bag it because it might be big!
	pkg#"links up"   = UP;
	pkg#"links next" = NEXT;
	pkg#"links prev" = PREV;

	-- check that everything is documented
	-- ~22s for Macaulay2Doc
	if chkdoc then (
	    resetCounters();
	    srcpkg := if pkg#"pkgname" == "Macaulay2Doc" then Core else pkg;
	    scan(join (srcpkg#"exported symbols", srcpkg#"exported mutable symbols"), s -> (
		    tag := makeDocumentTag s;
		    if  not isUndocumented tag
		    and not hasDocumentation tag
		    and signalDocumentationWarning tag then printerr(
			"warning: symbol has no documentation: ", toString tag, ", package ", toString package tag);
		    f := value s;
		    if instance(f, Function) then (
			scan(methods f, m -> if isDocumentableMethod m then (
				tag := makeDocumentTag m;
				if  not isUndocumented tag
				and not hasDocumentation tag
				and not dispatcherMethod m
				and signalDocumentationWarning tag then printerr(
				    "warning: method has no documentation: ", toString tag,
				    ", key ", toExternalString tag.Key,
				    ", package ", toString package tag);
				));
			))));

	if chkdoc and hadDocumentationError then error(
	    toString numDocumentationErrors, " errors(s) occurred in documentation for package ", toString pkg);

	-- make info documentation
	-- ~60 -> ~70s for Macaulay2Doc
	if opts.MakeInfo then installInfo(pkg, installPrefix, installLayout, verboseLog)
	else verboseLog("not making documentation in info format");

	-- make html documentation
	-- ~50 -> ~80s for Macaulay2Doc
	if opts.MakeHTML then installHTML(pkg, installPrefix, installLayout, verboseLog, rawDocumentationCache, opts)
	else verboseLog("not making documentation in HTML format");

	-- make pdf documentation
	if opts.MakePDF then installPDF(pkg, installPrefix, installLayout, verboseLog)
	else verboseLog("not making documentation in PDF format");

	if chkdoc and hadDocumentationWarning then printerr("warning: ",
	    toString numDocumentationWarnings, " warning(s) occurred in documentation for package ", toString pkg);

	); -- end of opts.MakeDocumentation

    -- touch .installed if no errors occurred
    if not hadError then (
	libDir := pkg#"package prefix" | replace("PKG", pkg#"pkgname", installLayout#"packagelib");
	iname := libDir|".installed";
	iname << close;
	verboseLog("file created: ", minimizeFilename iname);
	);

    verboseLog("installed package ", toString pkg, " in ", installPrefix);
    popvar(symbol currentPackage);
    if not noinitfile then makePackageIndex();
    htmlDirectory = null;
    installLayout = null;
    installPrefix = null;
    tallyInstalledPackages();
    pkg)

-----------------------------------------------------------------------------
-- installedPackages
-----------------------------------------------------------------------------

installedPackages = () -> (
    prefix := applicationDirectory() | "local/";
    layout := Layout#(detectCurrentLayout prefix);
    packages := prefix | layout#"packages";
    if isDirectory packages then for pkg in readDirectory packages list (
	if match("\\.m2$", pkg) then replace("\\.m2$", "", pkg) else continue) else {})

-----------------------------------------------------------------------------
-- uninstallPackage and uninstallAllPackages
-----------------------------------------------------------------------------

removeFiles = p -> scan(reverse findFiles p, fn -> if fileExists fn or readlink fn =!= null then (
	if isDirectory fn then (
	    -- we silently ignore nonempty directories, which could result from
	    -- removing an open file on an NFS file system.  Such files get renamed
	    -- to something beginning with ".nfs".
	    if length readDirectory fn == 2 then removeDirectory fn)
	else removeFile fn))

uninstallPackage = method(Options => { InstallPrefix => applicationDirectory() | "local/" })
uninstallPackage Package := opts -> pkg -> uninstallPackage(toString pkg, opts)
uninstallPackage String  := opts -> pkg -> (
    checkPackageName pkg;
    installPrefix := minimizeFilename opts.InstallPrefix;
    apply(findFiles apply({1, 2},
	    i -> apply(flatten {
		    Layout#i#"packages" | pkg | ".m2", Layout#i#"info" | pkg | ".info",
		    apply({"package", "packagelib", "packagedoc"}, f -> replace("PKG", pkg, Layout#i#f))
		    },
		p -> installPrefix | p)),
	removeFiles);
    tallyInstalledPackages();
    )

uninstallAllPackages = () -> for pkg in installedPackages() do (
    printerr("uninstalling package ", toString pkg); uninstallPackage pkg)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
