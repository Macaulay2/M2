--		Copyright 1993-1999 by Daniel R. Grayson

Option.synonym = "option"
Net.synonym = "net"
Time.synonym = "timing result"
Boolean.synonym = "boolean value"
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
SymbolTable.synonym = "symbol table"
BigReal.synonym = "big real"
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

Function \ VisibleList := VisibleList => (f,v) -> apply(v,f)
       List / Function :=        List => (v,f) -> apply(v,f) -- just because of conflict with List / Thing!
VisibleList / Function := VisibleList => (v,f) -> apply(v,f)

use = identity				  -- just temporary, until methods.m2

globalAssignFunction = (X,x) -> (
     if not x#?(symbol name) then (
	  x.Symbol = X;
	  x.name = string X;
	  );
     use x;
     )

globalReleaseFunction = (X,x) -> (
     if x.?Symbol and X === x.Symbol
     then (
	  remove(x,symbol name);
	  remove(x,symbol symbol);
	  )
     )

Type.GlobalAssignHook = globalAssignFunction
Type.GlobalReleaseHook = globalReleaseFunction
-----------------------------------------------------------------------------

-- miscellaneous stuff:

Function @@ Function := Function => (f,g) -> x -> f g x

Function _ Thing := Function => (f,x) -> y -> f splice (x,y)

-----------------------------------------------------------------------------
