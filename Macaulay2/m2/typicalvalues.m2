--		Copyright 1993-1999 by Daniel R. Grayson

-- These installations are not really methods: we install them just for documentation
-- None of this code will ever get called, because the functions are built-in.

installMethod(symbol ->, Thing, Thing, Function =>
     (x,y) -> x -> y			    -- this code wouldn't really work
     )
installMethod(symbol !, ZZ, ZZ => 
     n -> n!		    -- this code is never used
     )
installMethod(symbol !=, Thing, Thing, Boolean =>
     (x,y) -> x != y		    -- this code is never used
     )
installMethod(symbol #?, HashTable, Thing, Boolean =>
     (x,y) -> x #? y		    -- this code is never used
     )
installMethod(symbol .?, HashTable, Symbol, Boolean =>
     (x,y) -> x .? y		   -- this code wouldn't really work
     )
installMethod(symbol <, Thing, Thing, Boolean =>
     (x,y) -> x < y		    -- this code is never used
     )
installMethod(symbol <=, Thing, Thing, Boolean =>
     (x,y) -> x <= y		    -- this code is never used
     )
installMethod(symbol =!=, Thing, Thing, Boolean =>
     (x,y) -> x =!= y		    -- this code is never used
     )
installMethod(symbol ===, Thing, Thing, Boolean =>
     (x,y) -> x === y		    -- this code is never used
     )
installMethod(symbol =>, Thing, Thing, Option =>
     (x,y) -> x => y		    -- this code is never used
     )
installMethod(symbol >, Thing, Thing, Boolean =>
     (x,y) -> x > y		    -- this code is never used
     )
f := 
(x,y) -> x >= y		    -- this code is never used
installMethod(symbol >=, Thing, Thing, Boolean => f)

acos RR := acos ZZ := RR => 
     x -> acos x
asin RR := asin ZZ := RR => 
     x -> asin x
atan RR := atan ZZ := RR => 
     x -> atan x
cos RR := cos ZZ := RR => 
     x -> cos x
sin RR := sin ZZ := RR => 
     x -> sin x
tan RR := tan ZZ := RR => 
     x -> tan x
cosh RR := cosh ZZ := RR => 
     x -> cosh x
sinh RR := sinh ZZ := RR => 
     x -> sinh x
tanh RR := tanh ZZ := RR => 
     x -> tanh x
exp RR := exp ZZ := RR => 
     x -> exp x
log RR := log ZZ := RR => 
     x -> log x
sqrt RR := sqrt ZZ := RR => 
     x -> sqrt x

ancestor(Type,Type) := Boolean =>
(x,y) -> ancestor(x,y)

any(BasicList,Function) := 
any(List,Function) := 
any(HashTable,Function) := 
any(Sequence,Function) := Boolean =>
(x,y) -> any(x,y)		    -- this code is never used
     
append(BasicList,Thing) := BasicList =>
(x,i) -> append(x,i)		    -- this code is never used
append(List,Thing) := List =>
(x,i) -> append(x,i)		    -- this code is never used
append(Sequence,Thing) := Sequence =>
(x,i) -> append(x,i)		    -- this code is never used

prepend(Thing,BasicList) := BasicList =>
(i,y) -> prepend(i,y)		    -- this code is never used
prepend(Thing,List) := List =>
(i,y) -> prepend(i,y)		    -- this code is never used
prepend(Thing,Sequence) := Sequence =>
(i,y) -> prepend(i,y)		    -- this code is never used

apply(BasicList,Function) := BasicList =>
(x,f) -> apply(x,f)		    -- this code is never used
apply(List,Function) := List =>
(x,f) -> apply(x,f)		    -- this code is never used
apply(Sequence,Function) := Sequence =>
(x,f) -> apply(x,f)		    -- this code is never used
apply(HashTable,Function) := HashTable =>
(x,f) -> apply(x,f)		    -- this code is never used
apply(List,List,Function) := List =>
(x,y,f) -> apply(x,y,f)		    -- this code is never used
apply(Sequence,Sequence,Function) := Sequence =>
(x,y,f) -> apply(x,y,f)		    -- this code is never used
apply(ZZ,Function) := List =>
(x,f) -> apply(x,f)		    -- this code is never used

applyKeys(HashTable,Function) := HashTable =>
(x,f) -> applyKeys(x,f)		    -- this code is never used
applyPairs(HashTable,Function) := HashTable =>
(x,f) -> applyPairs(x,f)		    -- this code is never used
applyValues(HashTable,Function) := HashTable =>
(x,f) -> applyValues(x,f)		    -- this code is never used

atEndOfFile(File) := Boolean =>
   f -> atEndOfFile f		    -- this code is never used
isInputFile(File) := Boolean =>
   f -> isInputFile f		    -- this code is never used
isListener(File) := Boolean =>
   f -> isListener f		    -- this code is never used
isOpenFile(File) := Boolean =>
   f -> isOpenFile f		    -- this code is never used
isOutputFile(File) := Boolean =>
   f -> isOutputFile f		    -- this code is never used
isReady(File) := Boolean =>
   f -> isReady f		    -- this code is never used
mutable(Thing) := Boolean =>
   f -> mutable f		    -- this code is never used
instance(Thing,Type) := Boolean =>
   (x,X) -> instance(x,X)		    -- this code is never used
match(String,String) := Boolean =>
   (x,X) -> match(x,X)		    -- this code is never used

basictype Thing := Type =>
     x -> basictype x		    -- this code is never used
class Thing := Type =>
     x -> class x		    -- this code is never used
parent Thing := Type =>
     x -> parent x		    -- this code is never used

characters String := List =>
     s -> characters s		    -- this code is never used

-- combine(HashTable, HashTable, Function, Function, Function) := HashTable => 
--      (x,y,f,g,h) -> combine(x,y,f,g,h)		    -- this code is never used

concatenate List := concatenate Sequence := String =>
     x -> concatenate x		    -- this code is never used

deepSplice List := List =>
     x -> deepSplice x		    -- this code is never used
deepSplice Sequence := Sequence =>
     x -> deepSplice x		    -- this code is never used

drop(List,ZZ) := drop(List,List) := List => 
     (x,n) -> drop(x,n)		    -- this code is never used
drop(Sequence,ZZ) := drop(Sequence,List) := Sequence => 
     (x,n) -> drop(x,n)		    -- this code is never used

take(List,ZZ) := take(List,List) := List => 
     (x,n) -> take(x,n)		    -- this code is never used
take(Sequence,ZZ) := take(Sequence,List) := Sequence => 
     (x,n) -> take(x,n)		    -- this code is never used

floor RR := ZZ =>
     x -> floor x		    -- this code is never used

get File := get String := String =>
     file -> get file

getc File := String =>
     file -> getc file

getenv String := String =>
     name -> getenv name

hashTable List := HashTable =>
     w -> hashTable w		    -- this code is never used

horizontalJoin List := horizontalJoin Sequence := Net =>
     w -> net w		    -- this code is never used

join(List,List) := List =>
     (v,w) -> join(v,w)		    -- this code is never used

keys HashTable := List =>
     x -> keys x
pairs HashTable := List =>
     x -> pairs x
values HashTable := List =>
     x -> values x

merge(HashTable,HashTable,Function) := HashTable =>
     (x,y,f) -> merge(x,y,f)
mergePairs(List,List,Function) := List =>
     (x,y,f) -> mergePairs(x,y,f)

mingle List := mingle Sequence := List =>
     x -> mingle x

openDatabase String := Database =>
     filename -> openDatabase filename		    -- this code is never used
openDatabaseOut String := Database =>
     filename -> openDatabaseOut filename		    -- this code is never used
openIn String := File =>
     filename -> openIn filename		    -- this code is never used
openOut String := File =>
     filename -> openOut filename		    -- this code is never used
openInOut String := File =>
     filename -> openInOut filename		    -- this code is never used
openListener String := File =>
     filename -> openListener filename		    -- this code is never used

pack(List,ZZ) := List =>
     (v,n) -> pack(v,n)		    -- this code is never used


reverse List := List =>
     x -> reverse x		    -- this code is never used
reverse Sequence := Sequence =>
     x -> reverse x		    -- this code is never used

select(List,Function) := List =>
     (x,f) -> select(x,f)		    -- this code is never used
select(BasicList,Function) := BasicList =>
     (x,f) -> select(x,f)		    -- this code is never used
select(Sequence,Function) := Sequence =>
     (x,f) -> select(x,f)		    -- this code is never used
select(HashTable,Function) := HashTable =>
     (x,f) -> select(x,f)		    -- this code is never used
select(ZZ,List,Function) := List =>
     (n,x,f) -> select(n,x,f)		    -- this code is never used
select(BasicList,Function) := BasicList =>
     (n,x,f) -> select(n,x,f)		    -- this code is never used
select(Sequence,Function) := Sequence =>
     (n,x,f) -> select(n,x,f)		    -- this code is never used
select(HashTable,Function) := HashTable =>
     (n,x,f) -> select(n,x,f)		    -- this code is never used

sequence Sequence := Sequence =>
     x -> x		    -- this code is never used
sequence Thing := Sequence =>
     x -> singleton x		    -- this code is never used

set List := set Sequence := Set =>
     x -> set x		    -- this code is never used
tally List := tally Sequence := Tally =>
     x -> tally x		    -- this code is never used

splice List := List =>
     x -> splice x		    -- this code is never used
splice BasicList := BasicList =>
     x -> splice x		    -- this code is never used
splice Sequence := Sequence =>
     x -> splice x		    -- this code is never used

stack List := stack Sequence := Net =>
     v -> stack v		    -- this code is never used

substring(String,ZZ) := String =>
     (s,i) -> substring(s,i)		    -- this code is never used
substring(String,ZZ,ZZ) := String =>
     (s,i,n) -> substring(s,i,n)		    -- this code is never used

toHandle ZZ := Handle =>
     i -> toHandle i		    -- this code is never used

toList HashTable := toList BasicList := toList Sequence := List =>
     v -> toList v		    -- this code is never used

toSequence BasicList := toList List := Sequence =>
     x -> toSequence x		    -- this code is never used

xor(ZZ,ZZ) := ZZ =>
     (m,n) -> xor(m,n)		    -- this code is never used

ascii String := List =>
     s -> ascii s		    -- this code is never used
ascii List := String =>
     v -> ascii v		    -- this code is never used
