--		Copyright 1993-2002 by Daniel R. Grayson

document { override,
     Headline => "override default values for optional arguments",
     TT "override(defaults,args)", " -- overrides default values for
     optional arguments present in the argument sequence ", TT "args", ".",
     PARA,
     "The argument ", TT "defaults", " should be an immutable hash table (
     usually of type ", TO "OptionTable", "), and ", TT "args", " should be
     a sequence of arguments, some of which are optional arguments of
     the form ", TT "x => v", ".  Each such optional argument
     is removed from ", TT "args", ", and the value in ", TT "defaults", "
     corresponding to the key ", TT "x", " is replaced by ", TT "v", ".
     The value returned is the modified pair ", TT "(defaults, args)", ".",
     PARA,
     "This function is intended for internal use only, and is used in the processing
     of optional arguments for method functions which accept them.",
     EXAMPLE {
	  "defs = new HashTable from { a => 1, b => 2 };",
	  "override(defs, (4,b=>6,5))"
	  }
     }

document { userSymbols,
     Headline => "a list of the user's symbols",
     TT "userSymbols ()", " -- provides a list of symbols defined by
     the user.",
     BR,
     NOINDENT, TT "userSymbols X", " -- limits the list to those symbols whose
     values are instances of the class X.",
     PARA,
     "Protected symbols are excluded from the list.",
     SEEALSO "listUserSymbols"
     }


document { listUserSymbols,
     Headline => "display the user's symbols",
     TT "listUserSymbols", " -- a command which returns a display of the variables 
     defined by the user, along with their types.",
     BR,
     NOINDENT, TT "listUserSymbols X", " -- limits the list to those variables whose
     values are instances of X.",
     PARA,
     "This function is useful after using ", TO "loaddata", " to restore 
     a previous session.",
     SEEALSO {"userSymbols"}
     }

document { clearOutput,
     Headline => "forget output values",
     TT "clearOutput", " -- a command which attempts to release memory by 
     clearing the values retained by the output line symbols.",
     PARA,
     SEEALSO { "clearAll" }
     }

document { clearAll,
     Headline => "forget everything",
     TT "clearAll", " -- a command which attempts to release memory by clearing 
     the values retained by the output line symbols and all the user symbols.",
     PARA,
     SEEALSO {"userSymbols", "clearOutput"}
     }

document { exec,
     Headline => "execute another program",
     TT "exec argv", " -- uses the 'exec' operating system call to
     start up another program, replacing the current Macaulay 2 process.
     Here ", TT "argv", " is a string, or a sequence or list of strings
     to be passed as arguments to the new process.  The first string
     is the name of the executable file."
     }

document { restart,
     Headline => "restart Macaulay 2",
     TT "restart", " -- restart Macaulay 2 from the beginning.",
     PARA,
     "Functions previously registered with ", TO "addEndFunction", " will
     be called first."
     }

document { on,
     Headline => "trace a function each time it's run",
     TT "f = on f", " -- replaces the function ", TT "f", " by a version which 
     will print out its arguments and return value each time it's called,
     together with a sequence number so the two reports can be connected.",
     PARA,
     "This function is of only limited utility because it cannot be used
     with write-protected system functions.",
     PARA,
     "The reason we write ", TT "f = on f", " and not something like
     ", TT "f = on(x -> ...)", " is so the function handed to ", TO "on", "
     will know its name.  The name will appear in the display."
     }

document { assert,
     Headline => "assert something is true",
     TT "assert x", " -- prints an error message if x isn't true."
     }

document { notImplemented,
     Headline => "print an 'not implemented' error message",
     TT "notImplemented()", " -- print an error message that 
     says \"not implemented yet\"."
     }

document { "errorDepth",
     Headline => "set the error printing depth",
     TT "errorDepth i", " -- sets the error depth to ", TT "i", ", which should be
     a small integer, returning the old value.",
     BR,
     TT "errorDepth()", " -- returns the current error depth.",
     PARA,
     "During the backtrace after an error message, a position in interpreted
     code is displayed only if the load depth was at least as large at the
     time the code was parsed as the error depth is now.  Typically, the
     error depth is set so that messages from code pre-interpreted and
     reloaded with ", TO "loaddata", " will not appear in the backtrace.",
     PARA,
     "To increase the size of the stack trace, reduce the ", TT "errorDepth", ".",
     SEEALSO { "loadDepth" }
     }

document { "loadDepth",
     Headline => "the load depth",
     TT "loadDepth = i", " -- sets the load depth to ", TT "i", ", which should be
     a small integer, returning the old value.",
     PARA,
     "During the backtrace after an load message, a position in interpreted
     code is displayed only if the load depth was at least as large at the
     time the code was parsed as the error depth is now.  The load depth 
     is incremented each time ", TO "loaddata", " is run.",
     PARA,
     "The load depth also determines which command line arguments are
     heeded.  If the load depth is ", TT "i", " then the arguments between
     the ", TT "i", "-th and the ", TT "i+1", "-st occurence of ", TT "--", "
     are the ones heeded.",
     SEEALSO { "errorDepth" }
     }

document { benchmark,
     Headline => "accurate timing of execution",
     TT "benchmark s", " -- produce an accurate timing for the code contained
     in the string ", TT "s", ".  The value returned is the number of seconds.",
     PARA,
     "The snippet of code provided will be run enough times to register
     meaningfully on the clock, and the garbage collector will be called
     beforehand."
     }

document { memoize,
     Headline => "record results of function evaluation for future use",
     TT "memoize f", " -- produces, from a function ", TT "f", ", a new function which
     behaves the same as ", TT "f", ", but remembers previous answers to be provided
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
     "An optional second argument to memoize provides a list of initial values,
     each of the form ", TT "x => v", ", where ", TT "v", " is the value to
     be provided for the argument ", TT "x", ".",
     PARA,
     "Warning: when the value returned by ", TT "f", " is ", TO "null", ", it will always be 
     recomputed, even if the same arguments are presented.",
     PARA,
     "Warning: the new function created by ", TT "memoize", " will save
     references to all arguments and values it encounters, and this will
     often prevent those arguments and values from being garbage-collected
     as soon as they might have been.  If the arguments are
     implemented as mutable hash tables (modules, matrices and rings are
     implemented this way) then a viable strategy is to stash computed
     results in the arguments themselves.  See also ", TT "CacheTable", "."
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
"

document { (symbol _, Tally, Thing),     
     Headline => "get a count from a tally",
     TT "t_x", " -- returns the number of times ", TT "x", " is counted
     by ", TT "t", ".",
     SEEALSO "Tally"
     }

document { Tally,
     Headline => "the class of all tally results",
     TT "Tally", " -- a class designed to hold tally results, i.e., multisets."
     }

document { (symbol **, Tally, Tally),
     Headline => "Cartesian product of tallies",
     TT "x ** y", " -- produces the Cartesian product of two tallies.",
     PARA,
     "One of the arguments may be a ", TO "Set", ".",
     PARA,
     EXAMPLE {
	  "x = tally {a,a,b}",
      	  "y = tally {1,2,2,2}",
     	  "x ** y",
	  },
     SEEALSO {"Tally", "tally"}
     }

document { (symbol ?, Tally, Tally),
     Headline => "comparison of tallies",
     TT "x ? y", " -- compares two tallies, returning ", TT "symbol <", ", ",
     TT "symbol >", ", ", TT "symbol ==", ", or ", TO "incomparable", ".",
     SEEALSO "Tally"
     }

document { (symbol +, Tally, Tally),
     Headline => "union of tallies",
     TT "x + y", " -- produces the union of two tallies.",
     PARA,
     "One of the arguments may be a ", TO "Set", ".",
     PARA,
     EXAMPLE {
	  "x = tally {a,a,a,b,b,c}",
      	  "y = tally {b,c,c,d,d,d}",
      	  "x + y",
	  },
     SEEALSO {"Tally", "tally"}
     }

document { (symbol -, Tally, Tally),
     Headline => "difference of tallies",
     TT "x - y", " -- produces the difference of two tallies.",
     PARA,
     EXAMPLE "tally {a,a,b,c} - tally {c,d,d}",
     SEEALSO "Tally"
     }

document { tally,
     Headline => "tally the elements of a list",
     TT "tally x", " -- tallies the frequencies of items in a list x.",
     PARA,
     "It produces an hash table (multiset) y which tallies the
     frequencies of occurrences of items in the list x, i.e.,
     y_i is the number of times i appears in x, or is 0 if
     i doesn't appear in the list.",
     PARA,
     EXAMPLE {
	  "y = tally {1,2,3,a,b,1,2,a,1,2,{a,b},{a,b},a}",
      	  "y_{a,b}",
	  },
     PARA,
     SEEALSO "Tally"
     }

TEST ///
assert( toString tally {1,1,1,2,1,3,2} === "new Tally from {1 => 4, 2 => 2, 3 => 1}" )
assert( tally {1,1,1,2,1,3,2} === new Tally from {(1,4),(2,2),(3,1)} )
///

document { Set, 
     Headline => "the class of all sets"
     }

document { (symbol #?, Set, Thing),
     Headline => "test set membership",
     TT "x#?i", " -- tests whether ", TT "i", " is a member of the set ", TT "x", "."
     }

document { (symbol -, Set, Set),
     Headline => "set difference",
     TT "x - y", " -- the difference of two sets.",
     SEEALSO {"Set", "-"}
     }

document { (isSubset,Set,Set), TT "isSubset(X,Y)", " -- tells whether ", TT "X", " is a subset of ", TT "Y", "." }

document { isSubset,
     Headline => "whether something is a subset of another"
     }

document { (symbol ++, Set, Set),
     Headline => "disjoint union of sets",
     EXAMPLE "set {a,b,c} ++ set {b,c,d}"
     }

document { (symbol *, Set, Set),
     Headline => "intersection of sets",
     EXAMPLE "set {1,2,3} * set {2,3,4}"
     }

TEST "
x = set {1,2,3}
y = set {3,4,5}
assert( member(2,x) )
assert( not member(x,x) )
assert( sum y === 12 )
assert( product y === 60 )
assert ( x * y === set {3} )
assert ( x ** y === set {
	  (3, 4), (2, 5), (3, 5), (1, 3), (2, 3), (1, 4), (3, 3), (1, 5), (2, 4)
	  } )
assert ( x - y === set {2, 1} )
assert ( x + y === set {1, 2, 3, 4, 5} )
assert ( toString x === \"set {1, 2, 3}\" )
"

document { (symbol ^**, Module, ZZ),
     Headline => "tensor power",
     Synopsis => {
	  "N = M^**i",
	  "M" => null,
	  "i" => null,
	  "N" => { "the i-th tensor power of M" }
	  }
     }

document { (symbol ^**, CoherentSheaf, ZZ),
     Headline => "tensor power",
     Synopsis => {
	  "N = M^**i",
	  "M" => null,
	  "i" => null,
	  "N" => { "the i-th tensor power of M" }
	  }
     }    

document { setRandomSeed,
     Headline => "set starting point for random number generator"
     }

document { (setRandomSeed, ZZ),
     Synopsis => {
	  "setRandomSeed i",
	  "i" => null
	  },
     "Sets the random number seed to the low-order 32 bits of the integer ", TT "i", ".
     The sequence of future pseudo-random results is determined by the seed.",
     EXAMPLE {
	  "setRandomSeed 123456",
	  "for i to 10 list random 100",
	  "setRandomSeed 123456",
	  "for i to 10 list random 100"
	  }
     }

document { (setRandomSeed, String),
     Synopsis => {
	  ///setRandomSeed s///,
	  "s" => null
	  },
     "Sets the random number seed to an integer computed from ", TT "s", ".  Every character 
     of the string contributes to the seed, but only 32 bits of data are used.
     The sequence of future pseudo-random results is determined by the seed.",
     EXAMPLE {
	  ///setRandomSeed "thrkwjsxz"///,
	  ///for i to 10 list random 100///,
	  ///setRandomSeed "thrkwjsxz"///,
	  ///for i to 10 list random 100///
	  }
     }

document { truncateOutput,
     Synopsis => {
	  "truncateOutput w",
	  "w" => "the maximum output line width to enforce"
	  },
     "If ", TT "w", " is ", TO "infinity", " then truncation is turned off.",
     PARA,
     "This function works by assigning a value to ", TT "Thing.BeforePrint", ", which
     may conflict with other ", TO "BeforePrint", " methods installed by the user.",
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2"
-- End:
