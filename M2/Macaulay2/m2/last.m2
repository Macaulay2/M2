--		Copyright 1993-2001 by Daniel R. Grayson

-- this file should be mentioned *last* in dumpseq

needs "engine.m2"
needs "methods.m2"
needs "nets.m2"
needs "monoids.m2"
needs "packages.m2"
needs "robust.m2"
needs "varieties.m2"

recursionLimit = 300

-- initialize the trivial monoid, see rawMonoid()
degreesRing 0;

setIOUnSynchronized()					    -- try to avoid deadlocks when running examples

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
	       newPackage("User",
		   Headline       => "default package for interpreter interactions",
		   DebuggingMode  => true,
		   PackageImports => if member("--no-preload", commandLine) then {} else Core#"pre-installed packages");
	       path = prepend("./",path); -- now we search also the user's current directory, since our files have already been loaded
	       path = unique apply( path, minimizeFilename);	    -- beautify
	       allowLocalCreation User#"private dictionary";
	       );
	  if not nobanner then (
	       if topLevelMode === TeXmacs then stderr << TeXmacsBegin << "verbatim:";
	       relevant := select(loadedPackages,mentionQ);
	       if #relevant > 0 then (
	       	    hd := "with packages: ";
	       	    stderr << hd << wrap(printWidth-#hd, concatenate between_", " sort apply(relevant,toString)) << endl;
		    );
	       if topLevelMode === TeXmacs then stderr << TeXmacsEnd << flush;
	       );
	  )
     )

addStartFunction( () -> (
	  prefixPath = if prefixDirectory === null then {} else {prefixDirectory};
	  if not noinitfile and getenv "HOME" =!= "" then (
	       prefixPath = prepend(applicationDirectory()|"local/", prefixPath);
	       setUpApplicationDirectory();
	       makePackageIndex())))

addStartFunction( () -> tallyInstalledPackages() )

addStartFunction( () -> if not noinitfile then (
	  -- remove empty directories and dead symbolic links from the local application directory
	  dir := applicationDirectory() | "local/";
	  apply(reverse findFiles dir,
	       fn -> if fn =!= dir then (
		    if isDirectory fn and # readDirectory fn == 2 then removeDirectory fn else
		    if readlink fn =!= null and not fileExists fn then removeFile fn else
		    if match("\\.info\\.tmp$",fn) then removeFile fn
		    ));
	  ))

addStartFunction( () -> if version#"gc version" < "7.0" then error "expected libgc version 7.0 or larger; perhaps our shareable library is not being found" )
unexportedSymbols = () -> hashTable apply(pairs Core#"private dictionary", (n,s) -> if not Core.Dictionary#?n then (s => class value s => value s))
Function.GlobalReleaseHook = (X,x) -> (
     if dictionary X =!= User#"private dictionary" then warningMessage(X," redefined");
     if hasAttribute(x,ReverseDictionary) and getAttribute(x,ReverseDictionary) === X then removeAttribute(x,ReverseDictionary);
     )
waterMark = serialNumber symbol waterMark      -- used by Serialization package

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
