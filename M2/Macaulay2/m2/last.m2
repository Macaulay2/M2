--		Copyright 1993-2001 by Daniel R. Grayson

-- this file should be mentioned *last* in dumpseq

recursionLimit = 300

addStartFunction(() -> path = unique apply( path, minimizeFilename))
addEndFunction(() -> (
	  scan(openFiles(), f -> if isOutputFile f then flush f);
	  path = {};
	  )
     )

-- try to wrap long lines intelligently, could be improved:
RawMatrix.BeforePrint =
Matrix.BeforePrint =
RingElement.BeforePrint =
List.BeforePrint = 
Sequence.BeforePrint = x -> (
     w := if printWidth != 0 then printWidth else if width stdio != 0 then width stdio else 0;
     if w == 0 then x else (
     	  i := - width stack lines ZZ.InputPrompt lineNumber + w;
     	  if i > 20 then wrap(i, net x) else net x))

-- make sure this is after all global symbols are defined or erased
closePackage Main
addStartFunction( () -> if sourceHomeDirectory =!= null then Main#"source directory" = sourceHomeDirectory )
-- we load the documentation afterwards, because closing the package records the reverse dictionary entries
currentPackage = Main
load "Macaulay2-doc.m2"
currentPackage = null

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2"
-- End:
