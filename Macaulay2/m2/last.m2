--		Copyright 1993-2001 by Daniel R. Grayson

-- this file should be mentioned *last* in dumpseq

if phase > 1 then load "docloads.m2"

setrecursionlimit 300

addEndFunction(() -> scan(openFiles(), f -> if isOutputFile f then flush f))
