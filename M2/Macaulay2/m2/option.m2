--		Copyright 1993-1999 by Daniel R. Grayson

Option.synonym = "option"
OptionTable.synonym = "option table"

new OptionTable from List := (OptionTable,opts) -> (
     scan(opts, opt -> (
	       if opt === null then error "null entry encountered in option list";
	       if not (instance(opt#0,Symbol) or instance(opt#0,String) or instance(opt#0,Type))
	       then error ("option name ",toString opt#0," should be a symbol, a string, or a type")));
     hashTable opts)

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
