--		Copyright 1993-1999 by Daniel R. Grayson

-- this file should be mentioned *last* in dumpseq

if phase > 1 then load "docloads.m2"

addStartFunction(
     () -> path = (
     	  if getenv "M2HOME" === "" 
     	  then { "." }
     	  else { "." , getenv "M2HOME" | "/packages" } ))

setrecursionlimit 300

addEndFunction(() -> scan(openFiles(), f -> if isOutputFile f then flush f))


