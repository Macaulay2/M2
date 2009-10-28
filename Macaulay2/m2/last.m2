--		Copyright 1993-2001 by Daniel R. Grayson

-- this file should be mentioned *last* in dumpseq

recursionLimit = 300

scan(join(apply(methods baseName,last),{MonoidElement}), BType -> if BType =!= Symbol and BType =!= IndexedVariable and BType =!= Holder then (
	  v := value;
     	  bn := a -> (
	       r := baseName a;
	       if value r =!= a then v = identity;	-- don't take values afterwards if either endpoint is not assigned to its base name
	       r);
	  err1 := lookup(symbol .., Thing, Thing);
	  BType .. Thing := (a,z) -> v \ (( try bn a else err1(a,z) ) .. z);
	  Thing .. BType := (a,z) -> v \ (a .. (try bn z else err1(a,z) ));
	  BType .. BType := (a,z) -> v \ (( try bn a else err1(a,z) ) .. (try bn z else err1(a,z) ));
	  err2 := lookup(symbol ..<, Thing, Thing);
	  BType ..< Thing := (a,z) -> v \ (( try bn a else err2(a,z) ) ..< z);
	  Thing ..< BType := (a,z) -> v \ (a ..< (try bn z else err2(a,z) ));
	  BType ..< BType := (a,z) -> v \ (( try bn a else err2(a,z) ) ..< (try bn z else err2(a,z) ));
	  ))

addStartFunction(() -> setRandomSeed((currentTime() << 16) + processID()))
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
	       newPackage("User", DebuggingMode => true);
	       );
	  if not nobanner then (
	       if topLevelMode === TeXmacs then stderr << TeXmacsBegin << "verbatim:";
	       hd := "with packages: ";
	       stderr << hd << wrap(printWidth-#hd, concatenate between_", " sort apply(select(loadedPackages,mentionQ),toString)) << endl;
	       if topLevelMode === TeXmacs then stderr << TeXmacsEnd << flush;
	       )
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
scan(values Core#"private dictionary" - set values Core.Dictionary,
     s -> if mutable s and value s === s then error("mutable unexported unset symbol in Core: ", s))
noinitfile' := noinitfile
load "installedpackages.m2"
Core#"base packages" = {}				    -- these will be kept visible while other packages are loaded
path = packagepath
Function.GlobalReleaseHook = (X,x) -> (
     stderr << "--warning: function " << toString X << " redefined" << endl;
     if hasAttribute(x,ReverseDictionary) then removeAttribute(x,ReverseDictionary);
     )
endPackage "Core" -- after this point, private global symbols, such as noinitfile, are no longer visible, and public symbols have been exported
flagLookup \ vars (0 .. 51)
scan(Core#"pre-installed packages",	-- initialized in the file installedpackages.m2, which is made from the file installedpackages
     pkg -> needsPackage(pkg, DebuggingMode => not stopIfError))
Core#"base packages" = join(Core#"pre-installed packages",Core#"base packages")
if not noinitfile' then path = join(userpath',path)
if #OutputDictionary > 0 then error("symbols entered into OutputDictionary during startup phase: ",toString keys OutputDictionary)
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
