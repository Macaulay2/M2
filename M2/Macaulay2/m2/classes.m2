--		Copyright 1993-1999 by Daniel R. Grayson

Option.name = "Option"
Option.synonym = "option"
Net.name = "Net"
Net.synonym = "net"
Time.name = "Time"
Time.synonym = "timing result"
Handle.name = "Handle"
Handle.synonym = "handle"
Handle.synonym = "hash table"
HashTable.name = "HashTable"
Boolean.name = "Boolean"
Boolean.synonym = "boolean value"
MutableHashTable.name = "MutableHashTable"
MutableHashTable.synonym = "mutable hash table"
Function.name = "Function"
Function.synonym = "function"
Sequence.name = "Sequence"
Sequence.synonym = "sequence"
VisibleList.name = "VisibleList"
VisibleList.synonym = "visible list"
Error.name = "Error"
erase symbol Error
Database.name = "Database"
Database.synonym = "database"
Thing.name = "Thing"
Thing.synonym = "thing"
Nothing.name = "Nothing"
Type.name = "Type"
Type.synonym = "type"
String.name = "String"
String.synonym = "string"
BasicList.name = "BasicList"
BasicList.synonym = "basic list"
List.name = "List"
List.synonym = "list"
MutableList.name = "MutableList"
MutableList.synonym = "mutable list"
File.name = "File"
File.synonym = "file"
Array.name = "Array"
Array.synonym = "array"
Symbol.name = "Symbol"
Symbol.synonym = "symbol"
SymbolTable.name = "SymbolTable"
SymbolTable.synonym = "symbol table"
ZZ.name = "ZZ"
ZZ.synonym = "integer"
QQ.name = "QQ"
QQ.synonym = "rational number"
RR.name = "RR"
RR.synonym = "real number"
Ring.name = "Ring"
Ring.synonym = "ring"
Field.name = "Field"
Field.synonym = "field"

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

endFunctions := {}
addEndFunction = g -> (
     endFunctions = append(endFunctions,g);
     g)
runEndFunctions = () -> scan(endFunctions, f -> f())

exit1 := ret -> (runEndFunctions(); exit ret)
erase symbol exit
exit = exit1

erase symbol [

between = (m,v) -> mingle(v,#v-1:m)

-- miscellaneous stuff:

Function @@ Function := Function => (f,g) -> x -> f g x

Function _ Thing := Function => (f,x) -> y -> f(x,y)

-----------------------------------------------------------------------------
