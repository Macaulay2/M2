--		Copyright 1993-1999 by Daniel R. Grayson

-- this file should be mentioned *last* in dumpseq

if phase > 1 then load "docloads.m2"

addStartFunction(
     () -> path = append(path, homeDirectory | "packages" | pathSeparator)
     )

addStartFunction(
     () -> documentationPath = (
     	  { 
	       "", 
	       "cache/doc/" , 
	       homeDirectory | "packages/cache/doc/",
	       homeDirectory | "m2/cache/doc/"
	       }
	  )
     )

setrecursionlimit 300

addEndFunction(() -> scan(openFiles(), f -> if isOutputFile f then flush f))
