--		Copyright 1993-1999 by Daniel R. Grayson

memoize = method()

memoize Function := f -> (
     values := new MutableHashTable;
     x -> (
	  -- This code is common to any function that has been memoized.
	  if not values#?x then values#x = f(x) else values#x
	  )
     )

memoize(Function,List) := (f,initialValues) -> (
     values := new MutableHashTable from initialValues;
     x -> (
	  -- This code is common to any function that has been memoized with initial values.
	  if not values#?x then values#x = f(x) else values#x
	  )
     )
