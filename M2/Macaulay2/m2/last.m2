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
Matrix.BeforePrint =
RingElement.BeforePrint =
List.BeforePrint = 
Sequence.BeforePrint = x -> if width stdio == 0 then x else wrap(- width stack lines ZZ.InputPrompt lineNumber + width stdio, net x)

-- make sure this is after all global symbols are defined or erased
closePackage Main
-- we load the documentation afterwards, because closing the package records the reverse dictionary entries
currentPackage = Main
-- load "Macaulay2-doc.m2"
currentPackage = null
