--		Copyright 1993-2001 by Daniel R. Grayson

-- this file should be mentioned *last* in dumpseq

if phase === 2 or phase === 4 or phase == 5 then load "docloads.m2"

setrecursionlimit 300

addStartFunction(() -> path = unique apply( path, minimizeFilename))
addEndFunction(() -> scan(openFiles(), f -> if isOutputFile f then flush f))
