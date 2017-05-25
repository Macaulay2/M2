--		Copyright 1993-2001 by Daniel R. Grayson

-- this file should be mentioned *last* in dumpseq

recursionLimit = 300

protect Example

degreesRing 0;

setIOUnSynchronized()					    -- try to avoid deadlocks when running examples

(addStartFunction if member("--no-randomize",commandLine) 
   then (() -> setRandomSeed 0)
   else (() -> setRandomSeed((currentTime() << 16) + processID())))
addStartFunction(() -> path = unique apply( path, minimizeFilename))
addEndFunction(() -> scan(openFiles(), f -> if isOutputFile f then flush f))
addEndFunction(() -> path = {})

wr := (sep,x) -> wrap(printWidth, sep, net x)
VisibleList.Wrap = 
Tally.Wrap = RawMatrix.Wrap = Matrix.Wrap = Ideal.Wrap = RingElement.Wrap = VisibleList.Wrap = Sequence.Wrap = x -> wr("-",x)
String.Wrap = x -> ( x = net x; if height x + depth x <= 3 then wr("",x) else x )
Net.Wrap = x -> if height x + depth x <= 3 then wr("-",x) else x
Number.Wrap = x -> wr("",x)
if instance(PythonObject,Type) then PythonObject.Wrap = x -> wr("",x) else protect PythonObject
QQ.Wrap = x -> wr("=",x)

ignoreP := set { "Core", "Classic", "Parsing", "SimpleDoc" }
mentionQ := p -> not ignoreP#?(toString p)

addStartFunction( 
     () -> (
	  if class value getGlobalSymbol "User" =!= Package then (
     	       dismiss "User";
	       newPackage("User", DebuggingMode => true, Reload => true);
	       -- debug:
	       -- protect User#"private dictionary";
	       allowLocalCreation User#"private dictionary";
	       );
	  if not nobanner then (
	       if topLevelMode === TeXmacs then stderr << TeXmacsBegin << "verbatim:";
	       hd := "with packages: ";
	       stderr << hd << wrap(printWidth-#hd, concatenate between_", " sort apply(select(loadedPackages,mentionQ),toString)) << endl;
	       if topLevelMode === TeXmacs then stderr << TeXmacsEnd << flush;
	       );
	  )
     )

addStartFunction( () -> (
	  -- we use "realpath" to produce real paths, because Cygwin-style symbolic links are not understood by native Windows applications
	  prefixPath = 
	  if prefixDirectory === null 
	  then {} 
	  else nonnull {				    -- detect the layout used and accomodate searches for both layouts
	       if isDirectory(prefixDirectory|"common/") then realpath(prefixDirectory|"common/"),
	       if isDirectory prefixDirectory then realpath prefixDirectory,
	       if isDirectory(prefixDirectory|version#"machine") then realpath(prefixDirectory|version#"machine")
	       };
	  if not noinitfile and getenv "HOME" =!= "" then (
	       prefixPath = prepend(applicationDirectory()|"local/", prefixPath);
	       userMacaulay2Directory();
	       makePackageIndex())))

userpath' := userpath = {
	  applicationDirectory() | "code/",
	  d1 := applicationDirectory() | "local/" | Layout#1#"packages", 
	  d2 := applicationDirectory() | "local/" | Layout#2#"packages"
	  }
addStartFunction( () -> if not noinitfile then (
	  -- remove empty directories and dead symbolic links from the local application directory
	  dir := applicationDirectory() | "local/";
	  apply(reverse findFiles dir,
	       fn -> if fn =!= dir then (
		    if isDirectory fn and # readDirectory fn == 2 then removeDirectory fn else
		    if readlink fn =!= null and not fileExists fn then removeFile fn
		    ));
	  -- if isDirectory d1 and isDirectory d2 then stderr << "--warning: both types of layout in use for user-installed packages" << endl
	  ))

addStartFunction( () -> if dumpdataFile =!= null and fileExists dumpdataFile then (
	  dumptime := fileTime dumpdataFile;
	  newfiles := select(values loadedFiles, fn -> dumptime < fileTime fn);
	  if #newfiles == 0 then return;
	  stderr << "--warning: old dumpdata file: " << dumpdataFile << endl;
	  stderr << "--         the following source files are newer:" << endl;
	  scan(sort newfiles, fn -> stderr << "--         " << fn << endl)))
addStartFunction( () -> if version#"gc version" < "7.0" then error "expected libgc version 7.0 or larger; perhaps our sharable library is not being found" )
unexportedSymbols = () -> hashTable apply(pairs Core#"private dictionary", (n,s) -> if not Core.Dictionary#?n then (s => class value s => value s))
noinitfile' := noinitfile
Core#"pre-installed packages" = lines get (currentFileDirectory | "installedpackages")
Core#"base packages" = {}				    -- these will be kept visible while other packages are loaded
path = packagepath
Function.GlobalReleaseHook = (X,x) -> (
     if dictionary X =!= User#"private dictionary" then warningMessage(X," redefined");
     if hasAttribute(x,ReverseDictionary) then removeAttribute(x,ReverseDictionary);
     )
waterMark = serialNumber symbol waterMark      -- used by Serialization package
endPackage "Core" -- after this point, private global symbols, such as noinitfile, are no longer visible, and public symbols have been exported
scan(Core#"pre-installed packages",	-- initialized in the file installedpackages.m2, which is made from the file installedpackages
     needsPackage)
Core#"base packages" = join(Core#"pre-installed packages",Core#"base packages")
if not noinitfile' then path = join(userpath',path)
if #OutputDictionary > 0 then error("symbols entered into OutputDictionary during startup phase: ",toString keys OutputDictionary)
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
