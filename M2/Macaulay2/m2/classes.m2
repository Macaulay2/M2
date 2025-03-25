--		Copyright 1993-1999 by Daniel R. Grayson

-----------------------------------------------------------------------------
-- Functions dealing with types
-----------------------------------------------------------------------------

-- TODO: is there a better name for ancestors'?
ancestors  = T -> unique join({T}, while (T = parent T) =!= Thing list T, {Thing})
ancestors' = T -> unique join({T}, while (T = class  T) =!= Type  list T, {Type})

-- TODO: make this TT toString X later?
synonym = X -> if X.?synonym then X.synonym else "object of class " | toString X
-- TODO: find a more permanent solution
plurals = new MutableHashTable from {
    "body"       => "bodies",
    "dictionary" => "dictionaries",
    "matrix"     => "matrices",
    "sheaf"      => "sheaves",
    "variety"    => "varieties",
    }
pluralize = s -> demark_" " append(
    drop(ws := separate_" " s, -1),
    if  plurals#?(last ws)
    then plurals#(last ws) else last ws | "s")
pluralsynonym = T -> if T.?synonym then pluralize T.synonym else "objects of class " | toString T

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
Database.synonym = "database"
Thing.synonym = "thing"
Type.synonym = "type"
String.synonym = "string"
File.synonym = "file"
Symbol.synonym = "symbol"
Keyword.synonym = "keyword"
Dictionary.synonym = "dictionary"
Task.synonym = "task"

-----------------------------------------------------------------------------
-- SelfInitializingType
-----------------------------------------------------------------------------

SelfInitializingType = new Type of Type
SelfInitializingType.synonym = "self initializing type"

SelfInitializingType Thing         := (T, z) -> new T from z
SelfInitializingType      \\ Thing := (T, z) -> T z
Thing      // SelfInitializingType := (z, T) -> T z
SelfInitializingType \ VisibleList := (T, z) -> (i -> T i) \ z
       List / SelfInitializingType := -- override List / Thing
VisibleList / SelfInitializingType := (z, T) -> z / (i -> T i)

-----------------------------------------------------------------------------
-- Bags
-----------------------------------------------------------------------------

Bag = new SelfInitializingType of MutableList
Bag.synonym = "bag"
Bag ? Bag := (x,y) -> incomparable -- so we can sort with them

-----------------------------------------------------------------------------
-- Commands
-----------------------------------------------------------------------------

Command = new SelfInitializingType of BasicList
Command.synonym = "command"
globalAssignment Command

new Command from Function := Command => (command, f) -> command {f}
-- new Command from String is defined in system.m2

Command#AfterEval = x -> Thing#AfterEval x#0 ()
Command Thing := (x,y) -> x#0 y

-- Now some extra stuff:

apply(Thing, Command)   := VisibleList => (v,f) -> apply(v, i -> f i)
Command   \ VisibleList :=
Function  \ VisibleList := VisibleList => (f,v) -> apply(v,f)
Command   \ String      :=
Function  \ String      := Sequence    => (f,s) -> apply(s,f)
Command  \\ Thing       := 
Function \\ Thing       := VisibleList => (f,v) -> f v
       List /  Command  :=
       List /  Function :=        List => (v,f) -> apply(v,f) -- just because of conflict with List / Thing!
VisibleList /  Command  :=
VisibleList /  Function := VisibleList => (v,f) -> apply(v,f)
     String /  Command  :=
     String /  Function := Sequence    => (s,f) -> apply(s,f)
VisibleList // Command  := -- here to make documentation easier
VisibleList // Function := -- here to make documentation easier
      Thing // Command  := 
      Thing // Function := VisibleList => (v,f) -> f v

-----------------------------------------------------------------------------
-- Syntactic sugar for function composition and uncurrying the first argument
-----------------------------------------------------------------------------
-- TODO: does this belong in classes.m2?
-- TODO: Function ^ Thing: https://github.com/Macaulay2/M2/issues/1630#issuecomment-735361979

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
-- miscellaneous stuff:

sortBy = f -> v -> last @@ last \ sort \\ (i -> (f i, new Bag from {i})) \ v
sortByName = x -> (sortBy toString) x
sortByHash = sortBy hash

-----------------------------------------------------------------------------

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
