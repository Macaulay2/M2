--		Copyright 1993-2001 by Daniel R. Grayson

-- this file should be mentioned *last* in dumpseq

recursionLimit = 300

addStartFunction(() -> path = unique apply( path, minimizeFilename))
addEndFunction(() -> scan(openFiles(), f -> if isOutputFile f then flush f))
addEndFunction(() -> path = {})

lastLN := 0
lastWI := 0
promptWidth = () -> (
     if lineNumber === lastLN then lastWI
     else (
	  lastLN = lineNumber;
	  lastWI = max \\ width \ lines ZZ.InputPrompt lineNumber))

wr := (sep,x) -> wrap(printWidth - promptWidth(), sep, net x)
Tally.Wrap = RawMatrix.Wrap = Matrix.Wrap = Ideal.Wrap = RingElement.Wrap = VisibleList.Wrap = Sequence.Wrap = x -> wr("-",x)
String.Wrap = x -> ( x = net x; if height x + depth x <= 3 then wr("",x) else x )
Net.Wrap = x -> if height x + depth x <= 3 then wr("-",x) else x

-- make sure this is after all global symbols are defined or erased
endPackage "Macaulay2Core"

load "installedpackages.m2"
installedPackages := Macaulay2Core#"pre-installed packages"	-- initialized in the file installedpackages.m2, which is made from the file installedpackages
scan(installedPackages, pkg -> loadPackage(pkg,DebuggingMode => not stopIfError))
-- we used to load package "Macaulay2" automatically, but now we (will) load it lazily

addStartFunction( 
     () -> (
	  if class value getGlobalSymbol "User" =!= Package then (
     	       dismiss "User";
	       newPackage("User", DebuggingMode => true);
	       );
	  needsPackage \ installedPackages;
	  << "with packages: " << wrap concatenate between_", " sort installedPackages << endl;
	  )
     )

addStartFunction( () -> if sourceHomeDirectory =!= null then Macaulay2Core#"source directory" = sourceHomeDirectory|"m2/" )

addStartFunction( () -> if not member("-q",commandLine) and prefixDirectory =!= null then makePackageIndex() )

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
