--		Copyright 1993-1999 by Daniel R. Grayson

-- this file should be mentioned *last* in dumpseq

if phase > 1 then load "docloads.m2"

addStartFunction(
     () -> path = append(path, getenv "M2HOME" | "/packages")
     )

addStartFunction(
     () -> documentationPath = (
     	  if getenv "M2HOME" === "" 
     	  then { "", "cache/doc/" }
     	  else { "", "cache/doc/" , getenv "M2HOME" | "/packages/cache/doc/", getenv "M2HOME" | "/m2/cache/doc/" } ))

setrecursionlimit 300

addEndFunction(() -> scan(openFiles(), f -> if isOutputFile f then flush f))
