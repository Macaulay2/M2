--		Copyright 1993-1999 by Daniel R. Grayson

-- These installations are not really methods: we install them just for documentation
-- None of this code will ever get called, because the functions are built-in.

typicalValues#class = Type
typicalValues#parent = Type
typicalValues#(symbol timing) = Time
typicalValues#(symbol local) = Symbol
typicalValues#(symbol global) = Symbol
typicalValues#(symbol symbol) = Symbol
typicalValues#(symbol ?) = Boolean

dummy := x -> error("dummy method function called")
installMethod(symbol #?, HashTable, Thing, Boolean => x -> (dummy x;))
installMethod(symbol #?, Set, Thing, Boolean => x -> (dummy x;))
installMethod(symbol #?, Database, String, Boolean => x -> (dummy x;))
installMethod(symbol #?, BasicList, ZZ, Boolean => x -> (dummy x;))
installMethod(symbol #?, String, ZZ, Boolean => x -> (dummy x;))
installMethod(symbol #, HashTable, Thing, Thing => x -> (dummy x;))
installMethod(symbol #, Database, String, String => x -> (dummy x;))
installMethod(symbol #, String, ZZ, Thing => x -> (dummy x;))
installMethod(symbol #, BasicList, ZZ, Thing => x -> (dummy x;))

-- installMethod(symbol #, File, ZZ => x -> (dummy x;))
installMethod(symbol #, Set, ZZ => x -> (dummy x;))
installMethod(symbol #, HashTable, ZZ => x -> (dummy x;))
installMethod(symbol #, BasicList, ZZ => x -> (dummy x;))
installMethod(symbol #, String, ZZ => x -> (dummy x;))

installMethod(symbol <, Thing, Thing, Boolean => x -> (dummy x;))
installMethod(symbol <=, Thing, Thing, Boolean => x -> (dummy x;))
installMethod(symbol =!=, Thing, Thing, Boolean => x -> (dummy x;))
installMethod(symbol ===, Thing, Thing, Boolean => x -> (dummy x;))
installMethod(symbol =>, Thing, Thing, Option => x -> (dummy x;))
installMethod(symbol >, Thing, Thing, Boolean => x -> (dummy x;))
installMethod(symbol >=, Thing, Thing, Boolean => x -> (dummy x;))

installMethod(symbol .., ZZ, ZZ, Sequence => x -> (dummy x;))
installMethod(symbol ..<, ZZ, ZZ, Sequence => x -> (dummy x;))
ancestor(Type,Type) := Boolean => ancestor
any(ZZ,Function) := any(BasicList,Function) := any(BasicList,BasicList,Function) := any(HashTable,Function) := Boolean => any
append(BasicList,Thing) := BasicList => append
prepend(Thing,BasicList) := BasicList => prepend
apply(BasicList,Function) := BasicList => apply
apply(BasicList,BasicList,Function) := BasicList => apply
apply(ZZ,Function) := List => apply
applyKeys(HashTable,Function) := HashTable => applyKeys
applyKeys(HashTable,Function,Function) := HashTable => applyKeys
applyPairs(HashTable,Function) := HashTable => applyPairs
applyValues(HashTable,Function) := HashTable => applyValues
atEndOfFile(File) := Boolean => atEndOfFile
isInputFile(File) := Boolean => isInputFile
isListener(File) := Boolean => isListener
isOpen(File) := Boolean => isOpen
isOpen(Database) := Boolean => isOpen
isOutputFile(File) := Boolean => isOutputFile
mutable(Thing) := Boolean => mutable
instance(Thing,Type) := Boolean => instance
characters String := List => characters
concatenate Nothing := concatenate String := concatenate Symbol := concatenate ZZ := concatenate BasicList := String => concatenate
deepSplice BasicList := BasicList => deepSplice
drop(BasicList,ZZ) := drop(BasicList,List) := BasicList => drop
take(BasicList,ZZ) := take(BasicList,List) := BasicList => take
get File := get String := String => get
getc File := String => getc
getenv String := String => getenv
hashTable List := HashTable => hashTable
typicalValues#horizontalJoin = Net
horizontalJoin BasicList := Net => horizontalJoin
unstack Net := List => unstack
localDictionaries Function := List => localDictionaries
localDictionaries Symbol := List => localDictionaries
localDictionaries Pseudocode := List => localDictionaries
localDictionaries Dictionary := List => localDictionaries
values HashTable := List => values
merge(HashTable,HashTable,Function) := HashTable => merge
mergePairs(BasicList,BasicList,Function) := BasicList => mergePairs
mingle BasicList := List => mingle
openDatabase String := Database => openDatabase
openDatabaseOut String := Database => openDatabaseOut
openIn String := File => openIn
openOut String := File => openOut
openOutAppend String := File => openOutAppend
openInOut String := openInOut File := File => openInOut
openListener String := File => openListener
pack(BasicList,ZZ) := List => pack
pack(ZZ,BasicList) := List => pack
reverse BasicList := BasicList => reverse
set VisibleList := Set => set
tally VisibleList := Tally => tally
splice BasicList := BasicList => splice
typicalValues#stack = Net
stack BasicList := Net => stack
substring(String,ZZ) := String => substring
substring(String,ZZ,ZZ) := String => substring
substring(ZZ,String) := String => substring
substring(Sequence,String) := String => substring
substring(ZZ,ZZ,String) := String => substring
toList Set := toList BasicList := List => toList
toSequence BasicList := Sequence => toSequence
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
kill ZZ := Nothing => kill
read File := String => read
read (File,ZZ) := String => read
read Sequence := String => read
read String := String => read
Function Thing := Thing => x -> (dummy x;)
scan(BasicList,Function) := Nothing => scan
scan(ZZ,Function) := Nothing => scan
scanPairs(HashTable,Function) := Nothing => scanPairs
locate Symbol := locate Pseudocode := locate Function := locate Sequence := locate Nothing := Sequence => locate
lines(String,String) := List => lines
lines String := List => lines
linkFile(String,String) := Nothing => linkFile
fileMode(String) := ZZ => fileMode
fileMode(ZZ,File) := fileMode
fileMode(File) := fileMode
fileMode(ZZ,String) := fileMode
frames(Sequence) := frames
frames(Symbol) := frames
frames(Function) := frames
frames(Pseudocode) := frames
powermod(ZZ,ZZ,ZZ) := ZZ => powermod

chk := (type,key) -> if type#?key then (
     stderr << "-- method already installed:" << endl
     << "   " << code type#key << endl;
     error("method already installed: ",toString type," # ",toString key))


typval3f-*(Function,Type,Type)*- := (f,X,Z) -> (
     msg := toString f | "(" | toString X | ") => " | toString Z;
     chk(X, f);
     f(X) := Z => x -> (error("dummy method called: ", msg);)
     )
typval4f-*(Function,Type,Type,Type)*- := (f,X,Y,Z) -> (
     chk(youngest (X,Y), (f,X,Y));
     f(X,Y) := Z => x -> (dummy x;)
     )
typval5f-*(Function,Type,Type,Type,Type)*- := (f,X,Y,Z,W) -> (
     chk(youngest (X,Y,Z), (f,X,Y,Z));
     f(X,Y,Z) := W => x -> (dummy x;)
     )
typval3k-*(Keyword,Type,Type)*- := (f,X,Z) -> (
     chk(X, (f,X));
     installMethod(f, X, Z => x -> (dummy x;))
     )
typval4k-*(Keyword,Type,Type,Type)*- := (f,X,Y,Z) -> (
     chk(youngest(X,Y), (f,X,Y));
     installMethod(f, X, Y, Z => x -> (dummy x;))
     )
typval = x -> (
     if #x == 3 then (
	  if instance(x#0,Function) then typval3f x
	  else if instance(x#0,Keyword) then typval3k x
	  else error "typval: expected keyword or function"
	  )
     else if #x == 4 then (
	  if instance(x#0,Function) then typval4f x
	  else if instance(x#0,Keyword) then typval4k x
	  else error "typval: expected keyword or function"
	  )
     else if #x == 5 then typval5f x
     else error "typval: expected 3, 4, or 5 arguments"
     )
if not member("--no-tvalues", commandLine) then load "tvalues.m2"

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
