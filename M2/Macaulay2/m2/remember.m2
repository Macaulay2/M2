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

memoizedFunctionBody := functionBody memoize sin

memoizeValues = f -> (
     if not instance(f,Function) then error "expected a function";
     if functionBody f =!= memoizedFunctionBody then error "expected a memoized function";
     values := (frame f)#1;
     assert instance(values, MutableHashTable);
     values
     )

memoizeClear = f -> (
     if not instance(f,Function) then error "expected a function";
     if functionBody f =!= memoizedFunctionBody then error "expected a memoized function";
     assert instance((frame f)#1, MutableHashTable);
     (frame f)#1 = new MutableHashTable;
     )

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
