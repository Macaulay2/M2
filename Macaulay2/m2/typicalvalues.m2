--		Copyright 1993-1999 by Daniel R. Grayson

-- These installations are not really methods: we install them just for documentation
-- None of this code will ever get called, because the functions are built-in.

dummy := handle						    -- this could be any compiled function
installMethod(symbol !, ZZ, ZZ => dummy)
installMethod(symbol !=, Thing, Thing, Boolean => dummy)
installMethod(symbol #?, HashTable, Thing, Boolean => dummy)
installMethod(symbol #?, Set, Thing, Boolean => dummy)
installMethod(symbol #?, Database, String, Boolean => dummy)
installMethod(symbol #?, BasicList, ZZ, Boolean => dummy)
installMethod(symbol #?, String, ZZ, Boolean => dummy)
installMethod(symbol #, File, ZZ => dummy)
installMethod(symbol #, HashTable, Thing, Thing => dummy)
installMethod(symbol #, Database, String, String => dummy)
installMethod(symbol #, String, ZZ, Thing => dummy)
installMethod(symbol #, BasicList, ZZ, Thing => dummy)
installMethod(symbol #, HashTable, ZZ => dummy)
installMethod(symbol #, BasicList, ZZ => dummy)
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
atan RR := atan ZZ := RR => atan
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
any(List,Function) := 
any(HashTable,Function) := Boolean => any
append(BasicList,Thing) := BasicList => append
append(List,Thing) := List => append
prepend(Thing,BasicList) := BasicList => prepend
prepend(Thing,List) := List => prepend
apply(BasicList,Function) := BasicList => apply
apply(List,Function) := List => apply
apply(HashTable,Function) := HashTable => apply
apply(BasicList,BasicList,Function) := List => apply
apply(List,List,Function) := List => apply
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
match(String,String) := Boolean => match
basictype Thing := Type => basictype
class Thing := Type => class
parent Thing := Type => parent
characters String := List => characters
concatenate List := concatenate BasicList := String => concatenate
deepSplice List := List => deepSplice
deepSplice Sequence := Sequence => deepSplice
drop(List,ZZ) := drop(List,List) := List => drop
drop(BasicList,ZZ) := drop(BasicList,BasicList) := BasicList => drop
take(List,ZZ) := take(List,List) := List => take
take(BasicList,ZZ) := take(BasicList,List) := BasicList => take
floor RR := ZZ => floor
get File := get String := String => get
getc File := String => getc
getenv String := String => getenv
hashTable List := HashTable => hashTable
typicalValues#horizontalJoin = Net
typicalValues#verticalJoin = Net
horizontalJoin List := horizontalJoin Sequence := Net => horizontalJoin
netRows Net := List => netRows
join(List,List) := List => join
keys HashTable := List => keys
pairs HashTable := List => pairs
values HashTable := List => values
merge(HashTable,HashTable,Function) := HashTable => merge
mergePairs(List,List,Function) := List => mergePairs
mingle List := mingle Sequence := List => mingle
openDatabase String := Database => openDatabase
openDatabaseOut String := Database => openDatabaseOut
openIn String := File => openIn
openOut String := File => openOut
openInOut String := File => openInOut
openListener String := File => openListener
pack(List,ZZ) := List => pack
reverse BasicList := BasicList => reverse
select(BasicList,Function) := BasicList => select
select(HashTable,Function) := HashTable => select
select(ZZ,BasicList,Function) := BasicList => select
select(ZZ,HashTable,Function) := HashTable => select
sequence Thing := Sequence => singleton
set List := set Sequence := Set => set
tally List := tally Sequence := Tally => tally
splice BasicList := BasicList => splice
typicalValues#stack = Net
stack List := stack Sequence := Net => stack
substring(String,ZZ) := String => substring
substring(String,ZZ,ZZ) := String => substring
toHandle ZZ := Handle => toHandle
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
read Sequence := String => read
read String := String => read
newHandle Thing := Handle => newHandle
toHandle ZZ := Handle => newHandle
handle HashTable := Handle => handle
Function Thing := Thing => dummy
scan(BasicList,Function) := Nothing => scan
scan(ZZ,Function) := Nothing => scan
scanPairs(HashTable,Function) := Nothing => scanPairs
