--		Copyright 1993-1999 by Daniel R. Grayson

document { override,
     HEADLINE "override default values for optional arguments",
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
     TT "userSymbols ()", " -- provides a list of variables defined by
     the user.",
     BR,
     NOINDENT, TT "userSymbols X", " -- limits the list to those variables whose
     values are instances of the class X.",
     PARA,
     "Protected variables are excluded from the list.",
     SEEALSO "listUserSymbols"
     }


document { listUserSymbols,
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
     TT "clearOutput", " -- a command which attempts to release memory by 
     clearing the values retained by the output line symbols.",
     PARA,
     SEEALSO { "clearAll" }
     }

document { clearAll,
     TT "clearAll", " -- a command which attempts to release memory by clearing 
     the values retained by the output line symbols and all the user symbols.",
     PARA,
     SEEALSO {"userSymbols", "clearOutput"}
     }

document { ConversionFormat,
     TT "ConversionFormat", " -- a method consulted to provide an engine conversion
     format.",
     SEEALSO {"convert", "pop"}
     }

document { convert,
     TT "convert (fmt,str)", " -- converts a string ", TT "str", " containing data 
     transmitted from the engine in the ", TO "engine communication protocol", ".
     The argument ", TT "fmt", " is a recursive description of the format to
     be used for the conversion.",
     PARA,
     "The method named ", TO "ConversionFormat", " is used to provide
     a conversion format",
     PARA,
     "A format of the form", PRE "          singleton f",
     "specifies that a sequence has been transmitted, each element of
     which should be converted with the format ", TT "f", ".  
     See ", TO "transmitting a sequence", ".",
     PARA,
     "A format of the form", PRE "          (g,x,y,z,...)",
     "where ", TT "g", " is a function, specifies that consecutive items are to
     be converted with the formats x,y,z,..., and the results of conversion
     are to be passed as arguments to the functin ", TT "g", " for processing.",
     PARA,
     "A format of the form", PRE "          (n,x,y,z,...)",
     "where ", TT "n", " is an integer, specifies that consecutive items are to
     be converted wtih the formats x,y,z,..., a total of n times.  The
     results are to be placed in a sequence.",
     PARA,
     "A format consisting of the symbol ", TO "ConvertInteger", "specifies
     that an integer has been transmitted.  See ",
     TO "transmitting an integer", ".",
     PARA,
     "Functions which assemble formats.",
     MENU {
	  TO "ConvertApply",
	  TO "ConvertFixedRepeat",
	  TO "ConvertJoin",
	  TO "ConvertList",
	  TO "ConvertMissing",
	  TO "ConvertRepeat"
	  },
     PARA,
     "A format is usually stored under the key ", TO "ConvertToExpression", "
     in the apprpriate class.",
     PARA,
     SEEALSO {"pop"}
     }

document { ConvertToExpression,
     TT "ConvertToExpression", " -- a key for classes under which a
     conversion format is stored.",
     PARA,
     "See ", TO "convert", "."
     }

document { pop,
     TT "pop", " -- used as a key.  If X is a class, then X.pop will contain a
     routine which uses ", TO "convert", " to pop the top item off the
     engine's stack and return it.",
     SEEALSO "engine communication protocol"
     }

document { ConvertInteger,
     "A format item for communication with the engine that corresponds to
     an integer.  See ", TO "transmitting an integer", "."
     }


document { ConvertApply,
     TT "ConvertApply(f,T1,...,Tm)", " -- a format item for communication with
     the engine that specifies that format items T1, ..., Tm should be 
     applied to the bytes received from the engine, and then the function
     ", TT "f", " should be applied to the sequence of results.",
     PARA,
     "See ", TO "convert", "."
     }


document { ConvertList,
     TT "ConvertList T", " -- a format item for converting data received from the
     ", TO "engine", ", which specifies that format item T be applied to each
     element in the array, returning the results as a list.",
     PARA,
     "See ", TO "convert", "."
     }


document { ConvertRepeat,
     TT "ConvertRepeat T", " -- a format item for converting data received from the
     ", TO "engine", ", which specifies that format item T be applied to each
     element in the array, returning the results as a sequence.",
     PARA,
     "See ", TO "convert", "."
     }


document { ConvertFixedRepeat,
     TT "ConvertFixedRepeat(n,T1,...,Tm)", " -- a format item for converting data
     from the engine that specifies that the format items T1,...Tm be applied
     to the incoming data a total of n times.",
     PARA,
     "See ", TO "convert", "."
     }


document { ConvertJoin,
     TT "ConvertJoin(T1,...,Tm)", " -- a format item for converting data
     from the engine that specifies that format items T1,...,Tm be applied
     to the data received, and the sequence of results returned.",
     PARA,
     "If there is just one format item T1, then its result is returned.",
     PARA,
     "See ", TO "convert", "."
     }

document { sendToEngine,
     TT "sendToEngine s", " -- sends the string ", TT "s", " to the engine and returns the result.",
     PARA,
     "See also ", TO "engine communication protocol", "."
     }

document { ConvertMissing,
     TT "ConvertMissing", " -- a format item for converting data from the engine
     which specifies that the class for which this item has been installed
     has no conversion format specified, presumably because it corresponds
     to a type which the engine doesn't support.",
     PARA,
     "See ", TO "convert", "."
     }

TEST "
     f = (x) -> assert( x == convert(ConvertInteger,gg x) )
     scan(100, i -> (
	       f(2^i);
	       f(2^i-1);
	       f(2^i+1);
	       f(-2^i);
	       f(-2^i+1);
	       ))

     f = x -> assert( x == convert(ConvertRepeat ConvertInteger,gg x) )
     g = i -> f(i .. i+20)
     g(-10)
     g(10)
     g(100)
     g(1000)
     g(1000000)
     g(1000000000000000)
     g(10000000000000000000000000000000000000000000000000)
     "

document { exec,
     TT "exec argv", " -- uses the 'exec' operating system call to
     start up another program, replacing the current Macaulay 2 process.
     Here ", TT "argv", " is a string, or a sequence or list of strings
     to be passed as arguments to the new process.  The first string
     is the name of the executable file."
     }


document { restart,
     TT "restart", " -- restart Macaulay 2 from the beginning.",
     PARA,
     "Functions previously registered with ", TO "addEndFunction", " will
     be called first."
     }
document { on,
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
     TT "assert x", " -- prints an error message if x isn't true."
     }
document { notImplemented,
     TT "notImplemented()", " -- print an error message that 
     says \"not implemented yet\"."
     }

document { errorDepth,
     TT "errorDepth i", " -- sets the error depth to i, which should be
     a small integer, returning the old value.",
     PARA,
     "During the backtrace after an error message, a position in interpreted
     code is displayed only if the value of ", TO "reloaded", " was at least
     as large as the error depth is now.  Typically, the error depth is set
     to 1 so that messages from code pre-interpreted and reloaded with 
     ", TO "loaddata", " will not appear in the backtrace."
     }

document { benchmark,
     TT "benchmark s", " -- produce an accurate timing for the code contained
     in the string ", TT "s", ".  The value returned is the number of seconds.",
     PARA,
     "The snippet of code provided will be run enough times to register
     meaningfully on the clock, and the garbage collector will be called
     beforehand."
     }

document { memoize,
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
     "An optional second argument to memoize provides a list of initial values,
     each of the form ", TT "x => v", ", where ", TT "v", " is the value to
     be provided for the argument ", TT "x", ".",
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
     results in the arugments themselves!"
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
     TT "t_x", " -- returns the number of times ", TT "x", " is counted
     by ", TT "x", ".",
     SEEALSO "Tally"
     }

document { Tally,
     HEADLINE "the class of all tally results",
     TT "Tally", " -- a class designed to hold tally results, i.e., multisets."
     }

document { (symbol **, Tally, Tally),
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
     TT "x ? y", " -- compares two tallies, returning ", TT "symbol <", ", ",
     TT "symbol >", ", ", TT "symbol ==", ", or ", TO "incomparable", ".",
     SEEALSO "Tally"
     }

document { (symbol +, Tally, Tally),
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
     TT "x - y", " -- produces the difference of two tallies.",
     PARA,
     EXAMPLE "tally {a,a,b,c} - tally {c,d,d}",
     SEEALSO "Tally"
     }

document { tally,
     HEADLINE "tally the elements of a list",
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

document { Set, HEADLINE "the class of all sets" }

document { (symbol #?, Set, Thing), HEADLINE "test set membership",
     TT "x#?i", " -- tests whether ", TT "i", " is a member of the set ", TT "x", "."
     }

document { (symbol -, Set, Set),
     TT "x - y", " -- the difference of two sets.",
     SEEALSO {"Set", "-"}
     }

document { (isSubset,Set,Set), TT "isSubset(X,Y)", " -- tells whether ", TT "X", " is a subset of ", TT "Y", "." }
document { isSubset, HEADLINE "whether something is a subset of another" }
document { (symbol ++, Set, Set), HEADLINE "disjoint union of sets", EXAMPLE "set {a,b,c} ++ set {b,c,d}" }
document { (symbol *, Set, Set), HEADLINE "intersection of sets", EXAMPLE "set {1,2,3} * set {2,3,4}" }

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

document { Handle,
     HEADLINE "the class of all handles for engine objects",
     "This concept is mainly for internal use.",
     PARA,
     "A handle is essentially a small integer by means of which the ", TO "engine", "
     refers to the algebraic entities in it.",
     PARA,
     "One advantage of a handle is that it can be registered with the
     Boehm garbage collector for last minute action at the time the handle
     is about to be destroyed.",
     PARA,
     MENU {
	  TO "newHandle"
	  }
     }
document { newHandle,
     TT "newHandle x", " -- passes the commands ", TT "x", " to the engine 
     with ", TO "sendgg", ", pops an object off the engine's stack and 
     produces the handle.",
     SEEALSO {"toHandle", "Handle" }
     }

document { toHandle,
     TT "toHandle i", " -- convert the integer i to a ", TO "Handle", ".",
     PARA,
     "No checking is done to ensure that the integer i actually has
     been assigned by the ", TO "engine", " to one of its objects."
     }

document { handle,
     TT "handle x", " -- produces the ", TO "Handle", " for the object x.",
     PARA,
     "The corresponding symbol is used as a key under which to store
     the handle."
     }

document { look,
     TT "look()", " -- display item on the top of the engine's stack.",
     PARA,
     "It's a ", TO "Command", " so it may be entered simply
     as ", TT "look", " if it's alone on the command line.",
     PARA,
     "Used mainly for debugging the engine.",
     EXAMPLE {
	  "R = ZZ/101[x,y,z]",
      	  "sendgg ggPush R",
	  "look"
	  }
     }

document { callgg,
     TT "callgg(f,x,y,...)", " -- calls the ", TO "engine", " with engine
     command string f, after pushing engine objects corresponding to
     x, y, ... onto the engine's stack."
     }

document { engineStack,
     TT "engineStack()", " -- returns a net containing a display of the contents
     of the engine's stack.",
     PARA,
     "It's a ", TO "Command", " so it may be entered simply
     as ", TT "engineStack", " if it's alone on the command line.",
     PARA,
     "Used mainly for debugging the engine.",
     EXAMPLE {
	  "ZZ/101[x,y,z]",
      	  "f = matrix {{x,y,z}}",
      	  "sendgg ggPush f",
      	  "engineStack"
	  },
     }

document { heap,
     TT "heap()", " -- display the contents of the engine's heap.",
     PARA,
     "It's a ", TO "Command", " so it may be entered simply
     as ", TT "heap", " if it's alone on the command line.",
     PARA,
     "Used mainly for debugging the engine.",
     EXAMPLE {
	  "ZZ/101[x,y,z];",
      	  "matrix {{x,y,z}}",
      	  "heap"
	  },
     }
     
document { engineMemory,
     TT "engineMemory()", " -- display the memory usage of the engine.",
     PARA,
     "It's a ", TO "Command", " so it may be entered simply
     as ", TT "engineMemory", " if it's alone on the command line.",
     PARA,
     "Used mainly for debugging the engine.",
     EXAMPLE {
	  "ZZ/101[x,y,z];",
      	  "matrix {{x,y,z}}",
      	  "engineMemory"
	  }
     }
document { see,
     TT "see i", " -- return a string which displays the engine object whose handle 
     is the integer i.",
     BR,
     TT "see X", " -- return a string which displays the engine object corresponding 
     to the ring, matrix, module, or ring element X.",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[x,y,z]",
      	  "see R",
      	  "see (x+1)^6",
      	  "see handle (x*y*z)",
      	  "see 0"
	  }
     }

document { eePopInt,
     TT "eePopInt()", " -- pop the integer from the top of the engine's stack,
     returning its value.",
     PARA,
     SEEALSO "high level gb engine commands"
     }

document { eePopIntarray,
     TT "eePopIntarray()", " -- pop the array of integers from the top of the engine's 
     stack, returning its value as a list of integers.",
     PARA,
     SEEALSO "high level gb engine commands"
     }

document { eePopBool,
     TT "eePopBool()", " -- pop a boolean value from the top of the engine's stack,
     returning true or false.",
     PARA,
     SEEALSO "high level gb engine commands"
     }

document { eePop,
     TT "eePop f", " -- take an engine conversion format string and use it
     to convert an object popped from the top of the engine's stack.",
     PARA,
     SEEALSO "high level gb engine commands"
     }

document { eePromote,
     TT "eePromote(f,R)", " -- promote a ring element ", TT "f", " to the
     ring ", TT "R", "."
     }

document { eeLift,
     TT "eeLift(f,R)", " -- lift a ring element ", TT "f", " to the
     ring ", TT "R", "."
     }

document { symbol directSum,
     HEADLINE "key for special methods for forming direct sums",
     SEEALSO "directSum"
     }
