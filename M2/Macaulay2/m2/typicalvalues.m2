--		Copyright 1993-1999 by Daniel R. Grayson

-- These installations are not really methods: we install them just for documentation
-- None of this code will ever get called, because the functions are built-in.

dummy := method(SingleArgumentDispatch=>true)	    -- a compiled function closure is pretty anonymous
installMethod(symbol !, ZZ, ZZ => dummy)
installMethod(symbol !=, Thing, Thing, Boolean => dummy)
installMethod(symbol ##, Function, Sequence, Thing => dummy)
installMethod(symbol #?, HashTable, Thing, Boolean => dummy)
installMethod(symbol #?, Set, Thing, Boolean => dummy)
installMethod(symbol #?, Database, String, Boolean => dummy)
installMethod(symbol #?, BasicList, ZZ, Boolean => dummy)
installMethod(symbol #?, Sequence, ZZ, Boolean => dummy)
installMethod(symbol #?, String, ZZ, Boolean => dummy)
installMethod(symbol #, HashTable, Thing, Thing => dummy)
installMethod(symbol #, Database, String, String => dummy)
installMethod(symbol #, String, ZZ, Thing => dummy)
installMethod(symbol #, BasicList, ZZ, Thing => dummy)
installMethod(symbol #, Sequence, ZZ, Thing => dummy)

installMethod(symbol #, File, ZZ => dummy)
installMethod(symbol #, Set, ZZ => dummy)
installMethod(symbol #, HashTable, ZZ => dummy)
installMethod(symbol #, BasicList, ZZ => dummy)
installMethod(symbol #, Sequence, ZZ => dummy)
installMethod(symbol #, String, ZZ => dummy)

installMethod(symbol <, Thing, Thing, Boolean => dummy)
installMethod(symbol <=, Thing, Thing, Boolean => dummy)
installMethod(symbol =!=, Thing, Thing, Boolean => dummy)
installMethod(symbol ===, Thing, Thing, Boolean => dummy)
installMethod(symbol =>, Thing, Thing, Option => dummy)
installMethod(symbol >, Thing, Thing, Boolean => dummy)
installMethod(symbol >=, Thing, Thing, Boolean => dummy)

acos RR := acos ZZ := RR => acos
asin RR := asin ZZ := RR => asin
atan(RR,RR) := atan RR := atan ZZ := RR => atan
cos RR := cos ZZ := RR => cos
sin RR := sin ZZ := RR => sin
tan RR := tan ZZ := RR => tan
cosh RR := cosh ZZ := RR => cosh
sinh RR := sinh ZZ := RR => sinh
tanh RR := tanh ZZ := RR => tanh
exp RR := exp ZZ := RR => exp
log RR := log ZZ := RR => log
sqrt RR := sqrt ZZ := RR => sqrt
ancestor(Type,Type) := Boolean => ancestor
any(BasicList,Function) := 
any(HashTable,Function) := Boolean => any
append(BasicList,Thing) := BasicList => append
prepend(Thing,BasicList) := BasicList => prepend
apply(BasicList,Function) := BasicList => apply
apply(HashTable,Function) := HashTable => apply
apply(BasicList,BasicList,Function) := BasicList => apply
apply(ZZ,Function) := List => apply
applyKeys(HashTable,Function) := HashTable => applyKeys
applyPairs(HashTable,Function) := HashTable => applyPairs
applyValues(HashTable,Function) := HashTable => applyValues
atEndOfFile(File) := Boolean => atEndOfFile
isInputFile(File) := Boolean => isInputFile
isListener(File) := Boolean => isListener
isOpenFile(File) := Boolean => isOpenFile
isOutputFile(File) := Boolean => isOutputFile
isReady(File) := Boolean => isReady
mutable(Thing) := Boolean => mutable
instance(Thing,Type) := Boolean => instance
matches(String,String) := List => match
basictype Thing := Type => basictype
class Thing := Type => class
parent Thing := Type => parent
characters String := List => characters
concatenate List := String => concatenate
deepSplice BasicList := BasicList => deepSplice
drop(BasicList,ZZ) := drop(BasicList,List) := BasicList => drop
take(BasicList,ZZ) := take(BasicList,List) := BasicList => take
floor RR := ZZ => floor
get File := get String := String => get
getc File := String => getc
getenv String := String => getenv
hashTable List := HashTable => hashTable
typicalValues#horizontalJoin = Net
horizontalJoin BasicList := Net => horizontalJoin
netRows Net := List => netRows
join(BasicList,BasicList) := BasicList => join
keys HashTable := List => keys
pairs HashTable := List => pairs
values HashTable := List => values
merge(HashTable,HashTable,Function) := HashTable => merge
mergePairs(BasicList,BasicList,Function) := BasicList => mergePairs
mingle BasicList := List => mingle
openDatabase String := Database => openDatabase
openDatabaseOut String := Database => openDatabaseOut
openIn String := File => openIn
openOut String := File => openOut
openInOut String := File => openInOut
openListener String := File => openListener
pack(BasicList,ZZ) := List => pack
pack(ZZ,BasicList) := List => pack
reverse BasicList := BasicList => reverse
select(BasicList,Function) := BasicList => select
select(HashTable,Function) := HashTable => select
select(ZZ,BasicList,Function) := BasicList => select
select(ZZ,HashTable,Function) := HashTable => select
sequence Thing := Sequence => singleton
setrecursionlimit ZZ := ZZ => setrecursionlimit
set List := set Sequence := Set => set
tally List := tally Sequence := Tally => tally
splice BasicList := BasicList => splice
typicalValues#stack = Net
stack BasicList := Net => stack
substring(String,ZZ) := String => substring
substring(String,ZZ,ZZ) := String => substring
substring(ZZ,String) := String => substring
substring(ZZ,ZZ,String) := String => substring
toList HashTable := toList Set := toList BasicList := List => toList
toSequence BasicList := toList List := Sequence => toSequence
xor(ZZ,ZZ) := ZZ => xor
ascii String := List => ascii
ascii List := String => ascii
remove(HashTable,Thing) := Nothing => remove
echoOff File := Nothing => echoOff
echoOn File := Nothing => echoOn

-- close File := File =>
--      f -> close f		    -- this code is bypassed by built-in compiled code

-- closeIn File := File =>
--      f -> closeIn f		    -- this code is bypassed by built-in compiled code
-- closeOut File := File =>
--      f -> closeOut f		    -- this code is bypassed by built-in compiled code

kill File := Nothing => kill
read File := String => read
read (File,ZZ) := String => read
read Sequence := String => read
read String := String => read
Function Thing := Thing => dummy
scan(BasicList,Function) := Nothing => scan
scan(ZZ,Function) := Nothing => scan
scanPairs(HashTable,Function) := Nothing => scanPairs
locate Symbol := locate Function := locate Sequence := locate Nothing := Sequence => locate
separate(String,String) := List => separate
lines(String,String) := List => lines
lines String := List => lines
