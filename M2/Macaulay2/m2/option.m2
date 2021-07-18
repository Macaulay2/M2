--		Copyright 1993-1999 by Daniel R. Grayson

needs "classes.m2" -- for codeHelper

Option.synonym = "option"
OptionTable.synonym = "option table"

all' := (L, p) -> not any(L, x -> not p x)

new OptionTable from List := (OptionTable, opts) -> (
    if all'(opts, opt -> opt === null or any({Symbol, String, Type}, T -> instance(opt#0, T)))
    then new HashTable from opts else error("expected option key to be a symbol, a string, or a type"))

installMethod(symbol >>, OptionTable, Function, Function => 
  (opts,f) -> args -> (
       -- Common code for functions created with >> to process options and arguments.
       uncurry(f, override (opts,args))
       )
  )

codeHelper#(functionBody(new OptionTable from {} >> identity)) = g -> { 
     ("-- function f:", value' (first localDictionaries g)#"f"),
     ("-- option table opts:", value' (first localDictionaries g)#"opts")
     }

installMethod(symbol >>, List, Function, Function =>
     (o,f) -> new OptionTable from o >> f
     )

codeHelper#(functionBody({} >> identity)) = g -> { 
     ("-- function f:", value' (first localDictionaries g)#"f"),
     ("-- option table opts:", value' (first localDictionaries g)#"opts")
     }

-- TODO: https://github.com/Macaulay2/M2/issues/1878
installMethod(symbol >>, Boolean, Function, Function => 
  (opts,f) -> args -> (
       -- Common code for functions created with >> to separate options from arguments.
       -- uncurry(f, (new OptionTable from toList select(args, i -> instance(i,Option)), select(args, i -> not instance(i,Option))))
       uncurry(f, override (,args))
       )
  )

codeHelper#(functionBody(true >> identity)) = g -> { 
     ("-- function f:", value' (first localDictionaries g)#"f")
     }

installMethod(symbol ++, OptionTable, OptionTable, OptionTable =>
     (opts1, opts2) -> merge(opts1, opts2, last)
     )

installMethod(symbol ++, OptionTable, List, OptionTable =>
     (opts1, opts2) -> opts1 ++ new OptionTable from opts2
     )

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
