--		Copyright 1993-2001 by Daniel R. Grayson

-- this file should be mentioned *last* in dumpseq

recursionLimit = 300

addStartFunction(() -> path = unique apply( path, minimizeFilename))
addEndFunction(() -> (
	  scan(openFiles(), f -> if isOutputFile f then flush f);
	  path = {};
	  )
     )

addStartFunction(() -> printWidth = width stdio)

lastLN := 0
lastWI := 0
promptWidth = () -> (
     if lineNumber === lastLN then lastWI
     else (
	  lastLN = lineNumber;
	  lastWI = max \\ width \ lines ZZ.InputPrompt lineNumber))

wr := (sep,x) -> wrap(printWidth - promptWidth(), sep, net x)
RawMatrix.Wrap = Matrix.Wrap = RingElement.Wrap = List.Wrap = Sequence.Wrap = x -> wr("-",x)
String.Wrap = x -> if height x + depth x === 1 then wr("",x) else x


-- make sure this is after all global symbols are defined or erased
closePackage "Macaulay2Core"

load "installedpackages.m2"
installedPackages := Macaulay2Core#"pre-installed packages"	-- initialized in the file installedpackages.m2, which is made from the file installedpackages
scan(installedPackages, pkg -> loadPackage(pkg,DebuggingMode => not stopIfError))

addStartFunction( 
     () -> (
     	  dismiss "User";
	  newPackage("User", DebuggingMode => true);
	  needsPackage \ installedPackages;
	  )
     )

addStartFunction( () -> if sourceHomeDirectory =!= null then Macaulay2Core#"source directory" = sourceHomeDirectory|"m2/" )

addStartFunction( () -> if not member("-q",commandLine) and prefixDirectory =!= null then makePackageIndex() )

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
