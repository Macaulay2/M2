--		Copyright 1994 by Daniel R. Grayson

originalFunction := quote originalFunction
clearTable := quote clearTable
memoize = f -> (
     if class f =!= Function then error "expected a function";
     values := new MutableHashTable;
     g :=
     x -> (
	  -- This code is common to any function that has been memoized.
	  -- Use 'original' to obtain the original function.
	  if not values#?x then values#x = f(x) else values#x
	  );
     values#originalFunction = f;
     values#clearTable = () -> (
	  newvalues := new MutableHashTable;
	  newvalues#clearTable = values#clearTable;
	  newvalues#originalFunction = values#originalFunction;
	  values = newvalues;
	  );
     g)

sample := memoize identity

original = (f) -> (
     if locate f != locate sample then error "expected a memoized function";
     f originalFunction)

