--		Copyright 1993-2001 by Daniel R. Grayson

-- this file should be mentioned *last* in dumpseq

recursionLimit = 300

addStartFunction(() -> path = unique apply( path, minimizeFilename))
addEndFunction(() -> (
	  scan(openFiles(), f -> if isOutputFile f then flush f);
	  path = {};
	  )
     )

addStartFunction(() -> (
	  printWidth = width stdio; -- later, we should adjust this when a window resizing interrupt occurs; "wrap" uses 80 if printWidth == 0
	  ))

lastLN := 0
lastWI := 0
promptWidth = () -> (
     if lineNumber === lastLN then lastWI
     else (
	  lastLN = lineNumber;
	  lastWI = max \\ width \ lines ZZ.InputPrompt lineNumber))
wr := (sep,x) -> wrap(printWidth - promptWidth(), sep, net x)
RawMatrix.BeforePrint = Matrix.BeforePrint = RingElement.BeforePrint = List.BeforePrint = Sequence.BeforePrint = x -> wr("-",x)
String.BeforePrint = x -> wr("",x)

-- make sure this is after all global symbols are defined or erased
closePackage "Macaulay2"

currentPackage = Macaulay2
load "Macaulay2-doc.m2"
currentPackage = null

addStartFunction( () -> if sourceHomeDirectory =!= null then Macaulay2#"source directory" = sourceHomeDirectory )

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
