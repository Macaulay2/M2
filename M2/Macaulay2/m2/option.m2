--		Copyright 1993-1999 by Daniel R. Grayson

OptionTable = new Type of HashTable
OptionTable.synonym = "option table"

installMethod(symbol >>, OptionTable, Function, Function => 
  (opts,f) -> args -> (
       -- Common code for functions created with >> to process options and arguments.
       uncurry(f, override (opts,args))
       )
  )

installMethod(symbol >>, List, Function, Function =>
     (o,f) -> new OptionTable from o >> f
     )

codeHelper#(functionBody({} >> identity)) = g -> { 
     ("-- function f:", value (first localDictionaries g)#"f"),
     ("-- option table opts:", value (first localDictionaries g)#"opts")
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
