--		Copyright 1993-1999 by Daniel R. Grayson

Option.synonym = "option"
Net.synonym = "net"
Time.synonym = "timing result"
Boolean.synonym = "Boolean value"
MutableHashTable.synonym = "mutable hash table"
CacheTable.synonym = "cache table"
Function.synonym = "function"
Sequence.synonym = "sequence"
VisibleList.synonym = "visible list"
Database.synonym = "database"
Thing.synonym = "thing"
Type.synonym = "type"
String.synonym = "string"
BasicList.synonym = "basic list"
List.synonym = "list"
MutableList.synonym = "mutable list"
File.synonym = "file"
Array.synonym = "array"
Symbol.synonym = "symbol"
Keyword.synonym = "keyword"
RRR.synonym = "big real number"
Dictionary.synonym = "dictionary"
Pseudocode.synonym = "pseudocode"
ZZ.synonym = "integer"
ZZ.tex = ///$\mathbb Z$///
ZZ.texMath = ///{\mathbb Z}///
QQ.synonym = "rational number"
QQ.texMath = ///{\mathbb Q}///
RR.synonym = "real number"
RR.texMath = ///{\mathbb R}///
Ring.synonym = "ring"

uniform = (x) -> same apply(x,class)


-- Now some extra stuff:

Command   \ VisibleList := VisibleList => (f,v) -> apply(v,i -> f i)
Function  \ VisibleList := VisibleList => (f,v) -> apply(v,f)
Command  \\ VisibleList := 
Function \\ VisibleList := VisibleList => (f,v) -> f v
       List /  Function :=        List => (v,f) -> apply(v,f) -- just because of conflict with List / Thing!
       List /  Command  :=        List => (v,f) -> apply(v,i -> f i)
VisibleList /  Command  := VisibleList => (v,f) -> apply(v,i -> f i)
VisibleList /  Function := VisibleList => (v,f) -> apply(v,f)
VisibleList // Command  := 
VisibleList // Function := VisibleList => (v,f) -> f v
-----------------------------------------------------------------------------

-- miscellaneous stuff:

Function @@ Function := Function => (f,g) -> x -> f g x

Function _ Thing := Function => (f,x) -> y -> f splice (x,y)

-----------------------------------------------------------------------------

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
