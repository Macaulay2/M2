--		Copyright 1993-2001 by Daniel R. Grayson

-- this file should be mentioned *last* in dumpseq

if phase > 1 then load "docloads.m2"

addStartFunction(
     () -> (
	  if getenv "M2HOME" === "" 
	  then error "environment variable M2HOME not set";
	  path = join(path,
	       {
	       	    getenv "M2HOME" | pathSeparator | "m2" | pathSeparator,
	       	    getenv "M2HOME" | pathSeparator | "packages" | pathSeparator
		    }
	       );
	  documentationPath = { 
	       "", 
	       "cache/doc/" , 
	       getenv "M2HOME" | pathSeparator | "packages/cache/doc/",
	       getenv "M2HOME" | pathSeparator | "m2/cache/doc/"
	       };
	  )
     )

setrecursionlimit 300

addEndFunction(() -> scan(openFiles(), f -> if isOutputFile f then flush f))
