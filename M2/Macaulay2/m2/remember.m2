--		Copyright 1993-1999 by Daniel R. Grayson

memoize = f -> (
     values := new MutableHashTable;
     x -> (
	  -- This code is common to any function that has been memoized.
	  if not values#?x then values#x = f(x) else values#x
	  )
     )

