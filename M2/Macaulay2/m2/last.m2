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
String.Wrap = x -> wr("",x)

-- make sure this is after all global symbols are defined or erased

closePackage "Macaulay2"

installedPackages := {"PrimaryDecomposition"}
loadPackage \ installedPackages 

currentPackage = null					    -- eliminate the phony package we used for collecting test inputs

addStartFunction( 
     () -> (
     	  dismiss "User";
	  newPackage("User", DebuggingMode => true);
	  needsPackage \ installedPackages;
	  )
     )

if not Macaulay2#?"raw documentation database" or not isOpen Macaulay2#"raw documentation database" then (
     currentPackage = Macaulay2;
     stderr << "--loading Macaulay2-doc.m2" << endl;
     notify = true;
     load "Macaulay2-doc.m2";
     currentPackage = null;
     )

addStartFunction( () -> if sourceHomeDirectory =!= null then Macaulay2#"source directory" = sourceHomeDirectory|"m2/" )

addStartFunction( () -> if not member("-q",commandLine) then makePackageIndex() )

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
