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

document { quote memoize,
     TT "memoize f", " -- produces, from a function f, a new function which
     behaves the same as f, but remembers previous answers to be provided
     the next time the same arguments are presented.",
     PARA,
     EXAMPLE {
	  "fib = n -> if n <= 1 then 1 else fib(n-1) + fib(n-2)",
      	  "time fib 16",
      	  "fib = memoize fib",
      	  "time fib 16",
      	  "time fib 16",
	  },
     PARA,
     "The function ", TT "memoize", " operates by constructing 
     a ", TO "MutableHashTable", " in which the argument sequences are used
     as keys for accessing the return value of the function.",
     PARA,
     "Warning: when the value returned by f is null, it will always be 
     recomputed, even if the same arguments are presented.",
     PARA,
     "Warning: the new function created by ", TT "memoize", " will save
     references to all arguments and values it encounters, and this will
     often prevent those arguments and values from being garbage-collected
     as soon as they might have been.  If the arguments are
     implemented as mutable hash tables (modules, matrices and rings are
     implemented this way) then a viable strategy is to stash computed
     results in the arugments themselves!",
     SEEALSO "original"
     }

document { quote original,
     TT "original f", " -- provides the original function from which the
     memoized function ", TT "f", " was made.",
     SEEALSO "memoize"
     }

TEST "
fib = memoize( n -> if n <= 1 then 1 else fib(n-1) + fib(n-2) )
assert ( fib 10 == 89 )
"

TEST "
a = 0
f = memoize ( x -> ( a = a + 1; true ))
f 1
f 2
f 3
f 1
f 2
f 3
f 1
f 2
f 3
assert( a == 3 )
f = memoize original f
f 3
f 4
f 5
f 6
assert( a == 7 )
"
