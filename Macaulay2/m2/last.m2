--		Copyright 1993-2001 by Daniel R. Grayson

-- this file should be mentioned *last* in dumpseq

if phase === 2 or phase === 4 or phase == 5 then load "docloads.m2"

setrecursionlimit 300

addStartFunction(() -> path = unique apply( path, minimizeFilename))
addEndFunction(() -> (
	  scan(openFiles(), f -> if isOutputFile f then flush f);
	  path = {};
     	  phase = 0;
	  )
     )

path = {}


addStartFunction(
     () -> (
	  TeXmacsMode = member("--texmacs",commandLine);
	  if TeXmacsMode then (
	       << TeXmacsBegin << "verbatim:" << " Macaulay 2 starting up " << endl << TeXmacsEnd << flush;
	       );
	  )
     )

addStartFunction( () -> ( loadDepth (1 + loadDepth()); errorDepth (1 + errorDepth()); ) )

lastSystemSymbol = local privateSymbol
if OLDENGINE then (
     erase symbol ZZZ;
     erase symbol NewMonomialOrder;
     erase symbol Component;
     erase symbol GroupLex;
     erase symbol GroupRevLex;
     erase symbol MonomialOrdering;
     erase symbol NCLex;
     erase symbol newDegreesMonoid;
     erase symbol newDegreesRing;
     erase symbol newEngine;
     erase symbol monomialOrdering;
     remove(ZZ,newDegreesRing);
     remove(Sequence,newDegreesRing);
     remove(ZZ,newDegreesMonoid);
     remove(Sequence,newDegreesMonoid);
     erase symbol clone;
     remove(Sequence,clone);
     )
erase symbol OLDENGINE
erase symbol outputSymbols
erase symbol lastSystemSymbol

-- make sure this is after all global symbols are defined or erased
scanPairs(symbolTable(), (name,sym) -> if not writableGlobals#?sym then protect sym)
end Macaulay2
User = newPackage( "User", "0.0" )
