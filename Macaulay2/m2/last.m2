--		Copyright 1993-2001 by Daniel R. Grayson

-- this file should be mentioned *last* in dumpseq

recursionLimit = 300

addStartFunction(() -> path = unique apply( path, minimizeFilename))
addEndFunction(() -> scan(openFiles(), f -> if isOutputFile f then flush f))
addEndFunction(() -> path = {})

wr := (sep,x) -> wrap(printWidth, sep, net x)
VisibleList.Wrap = 
Tally.Wrap = RawMatrix.Wrap = Matrix.Wrap = Ideal.Wrap = RingElement.Wrap = VisibleList.Wrap = Sequence.Wrap = x -> wr("-",x)
String.Wrap = x -> ( x = net x; if height x + depth x <= 3 then wr("",x) else x )
Net.Wrap = x -> if height x + depth x <= 3 then wr("-",x) else x
Number.Wrap = x -> wr("",x)

addStartFunction( 
     () -> (
	  if class value getGlobalSymbol "User" =!= Package then (
     	       dismiss "User";
	       newPackage("User", DebuggingMode => true);
	       );
	  if not nobanner then (
	       if topLevelMode === TeXmacs then stderr << TeXmacsBegin << "verbatim:";
	       hd := "with packages: ";
	       stderr << hd << wrap(printWidth-#hd, concatenate between_", " sort apply(loadedPackages,toString)) << endl;
	       if topLevelMode === TeXmacs then stderr << TeXmacsEnd << flush;
	       )
	  )
     )

addStartFunction( () -> (
	  prefixPath = 
	  if prefixDirectory === null 
	  then {} 
	  else nonnull {				    -- detect the layout used and accomodate searches for both layouts
	       prefixDirectory,
	       if isDirectory(prefixDirectory|"common/") then prefixDirectory|"common/",
	       if isDirectory(prefixDirectory|version#"machine") then prefixDirectory|version#"machine"
	       };
	  if not noinitfile then (
	       prefixPath = prepend(applicationDirectory()|"local/", prefixPath);
	       userMacaulay2Directory();
	       makePackageIndex())))

addStartFunction( () -> if not noinitfile and prefixDirectory =!= null then (
	  isprefix := (s,t) -> s === substring(0,#s,t);
	  GLOBAL := prefixDirectory;			    -- installed doc
	  LOCAL := applicationDirectory() | "local/";	    -- user's "local" application directory doc
	  makeDirectory(LOCAL|currentLayout#"docdir");
	  scan(join(
		    {"Macaulay2"},			    -- this is a hold-over : M2 versions before 1.1 had a package called "Macaulay2" !
     	       	    if isDirectory (GLOBAL|currentLayout#"docdir")
		    then readDirectory (GLOBAL|currentLayout#"docdir")
		    else {}
		    ),
	       fn -> if fn =!= "." and fn =!= ".." then (
		    tardir := LOCAL|currentLayout#"docdir";
		    tar := tardir|fn;
		    src := GLOBAL|currentLayout#"docdir"|fn;
		    if readlink tar =!= null and not isprefix(tardir,realpath readlink tar) then removeFile tar;
		    ))))

addStartFunction( () -> if not noinitfile then (
	  -- remove empty directories and dead symbolic links from the local application directory
	  dir := applicationDirectory() | "local/";
	  apply(reverse findFiles dir,
	       fn -> if fn =!= dir then (
		    if isDirectory fn and # readDirectory fn == 2 then removeDirectory fn else
		    if readlink fn =!= null and not fileExists fn then removeFile fn
		    ))))

addStartFunction( () -> if dumpdataFile =!= null and fileExists dumpdataFile then (
	  dumptime := fileTime dumpdataFile;
	  newfiles := select(values loadedFiles, fn -> dumptime < fileTime fn);
	  if #newfiles == 0 then return;
	  stderr << "--warning: old dumpdata file: " << dumpdataFile << endl;
	  stderr << "--         the following source files are newer:" << endl;
	  scan(sort newfiles, fn -> stderr << "--         " << fn << endl)))

addStartFunction( () -> if version#"gc version" < "7.0" then error "expected libgc version 7.0 or larger; perhaps our sharable library is not being found" )

unexportedSymbols = () -> hashTable apply(pairs Core#"private dictionary", (n,s) -> if not Core.Dictionary#?n then (s => class value s => value s))

scan(values Core#"private dictionary" - set values Core.Dictionary,
     s -> if mutable s and value s === s then error("mutable unexported unset symbol in Core: ", s))

Core#"pre-installed packages" = {}			    -- these will loaded before dumping, see below
Core#"base packages" = {}				    -- these will be kept visible with other packages are loaded

-- make sure this is after all public global symbols are defined or erased
endPackage "Core"
-- after this point, private global symbols, such as noinitfile, are no longer visible

flagLookup \ vars (0 .. 51)

load "installedpackages.m2"

scan(Core#"pre-installed packages",	-- initialized in the file installedpackages.m2, which is made from the file installedpackages
     pkg -> needsPackage(pkg, DebuggingMode => not stopIfError))

Core#"base packages" = join(Core#"pre-installed packages",Core#"base packages")

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
