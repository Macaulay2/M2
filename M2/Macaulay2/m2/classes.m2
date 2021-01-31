--		Copyright 1993-1999 by Daniel R. Grayson

Net.synonym = "net"
Time.synonym = "timing result"
Boolean.synonym = "Boolean value"
MutableHashTable.synonym = "mutable hash table"
HashTable.synonym = "hash table"
CacheTable.synonym = "cache table"
Function.synonym = "function"
FunctionClosure.synonym = "function closure"
CompiledFunction.synonym = "compiled function"
CompiledFunctionClosure.synonym = "compiled function closure"
FunctionBody.synonym = "function body"
SymbolBody.synonym = "symbol body"
CompiledFunctionBody.synonym = "compiled function body"
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
AngleBarList.synonym = "angle bar list"
Symbol.synonym = "symbol"
Keyword.synonym = "keyword"
Dictionary.synonym = "dictionary"
-- Pseudocode.synonym = "pseudocode" -- "a pseudocode" doesn't sound so great
ZZ.synonym = "integer"
ZZ.texMath = ///{\mathbb Z}///
QQ.synonym = "rational number"
QQ.texMath = ///{\mathbb Q}///
Ring.synonym = "ring"
Task.synonym = "task"

uniform = (x) -> same apply(x,class)

-- Now some extra stuff:

Command   \ VisibleList := VisibleList => (f,v) -> apply(v,i -> f i)
Function  \ VisibleList := VisibleList => (f,v) -> apply(v,f)
Command  \\ Thing       := 
Function \\ Thing       := VisibleList => (f,v) -> f v
       List /  Function :=        List => (v,f) -> apply(v,f) -- just because of conflict with List / Thing!
       List /  Command  :=        List => (v,f) -> apply(v,i -> f i)
VisibleList /  Command  := VisibleList => (v,f) -> apply(v,i -> f i)
VisibleList /  Function := VisibleList => (v,f) -> apply(v,f)
      Thing // Command  := 
      Thing // Function := VisibleList => (v,f) -> f v
-----------------------------------------------------------------------------

-- miscellaneous stuff:

codeHelper = new MutableHashTable

Function @@ Function := Function => (f,g) -> x -> f g x
codeHelper#(functionBody(identity @@ identity)) = h -> { 
     ("-- function f:", value' (first localDictionaries h)#"f"),
     ("-- function g:", value' (first localDictionaries h)#"g")
     }

Function _ Thing := Function => (f,x) -> y -> f splice (x,y)
codeHelper#(functionBody(identity _ null)) = h -> { 
     ("-- function f:", value' (first localDictionaries h)#"f"),
     ("-- value of x:", value' (first localDictionaries h)#"x")
     }

-----------------------------------------------------------------------------

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
