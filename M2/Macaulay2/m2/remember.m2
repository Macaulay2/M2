--		Copyright 1993-1999 by Daniel R. Grayson

needs "methods.m2"

memoize = method()

memoize Function := f -> (
     values := new MutableHashTable;
     x -> (
	  -- This code is common to any function that has been memoized.
	  if not values#?x then values#x = f(x) else values#x
	  )
     )
codeHelper#(functionBody(memoize identity)) = g -> { 
     ("-- function f:", value (first localDictionaries g)#"f")
     }

memoize(Function,List) := (f,initialValues) -> (
     values := new MutableHashTable from initialValues;
     x -> (
	  -- This code is common to any function that has been memoized with initial values.
	  if not values#?x then values#x = f(x) else values#x
	  )
     )
codeHelper#(functionBody(memoize(identity,{}))) = g -> {
     ("-- function f:", value (first localDictionaries g)#"f") ,
     ("-- initialValues:", value (first localDictionaries g)#"initialValues") 
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
