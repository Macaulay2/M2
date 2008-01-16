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
	  hd := "with packages: ";
	  if not nobanner
	  then stderr << hd << wrap(printWidth-#hd, concatenate between_", " sort apply(loadedPackages,toString)) << endl;
	  )
     )

addStartFunction( () -> if not noinitfile and prefixDirectory =!= null then makePackageIndex() )

addStartFunction( () -> if not noinitfile then (
	  -- remove empty directories and dead symbolic links from the local application directory
	  dir := applicationDirectory() | "local/";
	  apply(findFiles dir,
	       fn -> if fn =!= dir then (
		    if isDirectory fn and # readDirectory fn == 2 then removeDirectory fn else
		    if readlink fn =!= null and not fileExists fn then removeFile fn
		    ))))

addStartFunction( () -> if not noinitfile and prefixDirectory =!= null then (
	  ins := prefixDirectory;			    -- installed doc
	  loc := applicationDirectory() | "local/";	    -- user's "local" application directory doc
	  makeDirectory(loc|LAYOUT#"docpackages");
	  scan(readDirectory (ins|LAYOUT#"docpackages"), fn -> if fn =!= "." and fn =!= ".." then (
		    tar := loc|LAYOUT#"docpackages"|fn;
		    src := ins|LAYOUT#"docpackages"|fn;
		    if isDirectory realpath src and readlink tar =!= src then (
			 if readlink tar =!= null then removeFile tar;
			 if not fileExists tar then symlinkFile(src,tar))))))

addStartFunction( () -> if dumpdataFile =!= null and fileExists dumpdataFile then (
	  dumptime := fileTime dumpdataFile;
	  newfiles := select(values loadedFiles, fn -> dumptime < fileTime fn);
	  if #newfiles == 0 then return;
	  stderr << "--warning: old dumpdata file: " << dumpdataFile << endl;
	  stderr << "--         the following source files are newer:" << endl;
	  scan(sort newfiles, fn -> stderr << "--         " << fn << endl)))

unexportedSymbols = () -> hashTable apply(pairs Core#"private dictionary", (n,s) -> if not Core.Dictionary#?n then (s => class value s => value s))

scan(values Core#"private dictionary" - set values Core.Dictionary,
     s -> if mutable s and value s === s then stderr << symbolLocation s << ": warning: mutable unexported unset symbol in Core: " << s << endl)

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
