--		Copyright 1994 by Daniel R. Grayson

Option.name = "Option"
Net.name = "Net"
Time.name = "Time"
Handle.name = "Handle"
HashTable.name = "HashTable"
Boolean.name = "Boolean"
MutableHashTable.name = "MutableHashTable"
Function.name = "Function"
Sequence.name = "Sequence"
Error.name = "Error"
erase symbol Error
Database.name = "Database"
Thing.name = "Thing"
Nothing.name = "Nothing"
Type.name = "Type"
String.name = "String"
BasicList.name = "BasicList"
List.name = "List"
MutableList.name = "MutableList"
File.name = "File"
Array.name = "Array"
Symbol.name = "Symbol"
SymbolTable.name = "SymbolTable"
ZZ.name = "ZZ"
QQ.name = "QQ"
RR.name = "RR"
Ring.name = "Ring"
Field.name = "Field"

uniform = (x) -> same apply(x,class)


-- Now some extra stuff:

Function \ Sequence := Sequence => (f,v) -> apply(v,f)
Function \ List     := List     => (f,v) -> apply(v,f)
Sequence / Function := Sequence => (v,f) -> apply(v,f)
    List / Function := List     => (v,f) -> apply(v,f)

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

GlobalAssignHook Type := globalAssignFunction
GlobalReleaseHook Type := globalReleaseFunction
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

-----------------------------------------------------------------------------
