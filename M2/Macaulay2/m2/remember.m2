--		Copyright 1993-1999 by Daniel R. Grayson

needs "methods.m2"

memoize = method()

memoize(Function,List) := (f,initialValues) -> (
     values := new MutableHashTable from initialValues;
     x -> (
	  -- This code is common to any function that has been memoized with initial values.
	  values#x ??= f(x)
	  )
     )
memoize Function := f -> memoize(f,{})

-- all memoized functions share the same function body
-- (they differ only in the values of the local variables f, values, initivalValues and x)
-- so here we create one to use for checking.  The function "sin" could be any function.
memoizedFunctionBody := functionBody memoize sin

codeHelper#memoizedFunctionBody = g -> {
     ("-- function f:", value (first localDictionaries g)#"f")
     }


memoizeValues = f -> (
     if not instance(f,Function) then error "expected a function";
     if functionBody f =!= memoizedFunctionBody then error "expected a memoized function";
     values := (frame f)#2;
     assert instance(values, MutableHashTable);
     values
     )

memoizeClear = f -> (
     if not instance(f,Function) then error "expected a function";
     if functionBody f =!= memoizedFunctionBody then error "expected a memoized function";
     assert instance((frame f)#2, MutableHashTable);
     (frame f)#2 = new MutableHashTable;
     )

-- values of functions by lookup
lookupfuns = new MutableHashTable
storefuns = new MutableHashTable
lookupfuns#toString = x -> f -> if hasAttribute(x,PrintNames) then getAttribute(x,PrintNames) else f x
storefuns #toString = (x,e) -> (
     if not instance(e,String) then error "expected a string";
     setAttribute(x,PrintNames,e))
Function Thing = (f,x,e) -> if functionBody f === memoizedFunctionBody then (
    values := (frame f)#2;
    assert instance(values, MutableHashTable);
    values#x=e
    ) else (
    if not storefuns#?f then error("no method for storing values of function ", f);
    storefuns#f (x,e)
)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
