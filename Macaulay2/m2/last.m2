--		Copyright 1993-2001 by Daniel R. Grayson

-- this file should be mentioned *last* in dumpseq

-- we need some other way to determine whether to load docs
-- shall we still cache them or not?
-- if phase === 2 or phase === 4 or phase == 5 then load "Macaulay2-doc.m2"
-- load "Macaulay2-doc.m2"

setrecursionlimit 300

addStartFunction(() -> path = unique apply( path, minimizeFilename))
addEndFunction(() -> (
	  scan(openFiles(), f -> if isOutputFile f then flush f);
	  path = {};
	  )
     )

erase symbol outputSymbols

-- make sure this is after all global symbols are defined or erased
end Macaulay2
newPackage( "User", "0.0" )
