--		Copyright 1993-1999 by Daniel R. Grayson

new HashTable from List := HashTable => (O,v) -> hashTable v

OptionTable = new Type of HashTable
OptionTable.synonym = "option table"

installMethod(symbol ==>, OptionTable, Function, Function => 
  (defaults,f) -> (
     args -> (
       -- Common code for functions created with ==> to
       -- process options and arguments.
       f ## override (defaults,args)
       )
     )
  )

installMethod(symbol ==>, List, Function, Function =>
     (o,f) -> new OptionTable from o ==> f
     )
