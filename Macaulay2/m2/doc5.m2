--		Copyright 1993-1999 by Daniel R. Grayson

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

document { "engine communication protocol",
     "Here is a description of the protocol for communication between the 
     front end and the engine.  At the moment, this protocol is used only
     for transmissions from the engine to the front end.",
     PARA,
     MENU {
	  TO "transmitting a positive integer",
	  TO "transmitting an integer",
	  TO "transmitting an integer mod n",
     	  TO "transmitting a sequence",
	  TO "transmitting a monomial",
	  TO "transmitting a polynomial",
	  TO "transmitting a vector",
	  TO "transmitting a matrix",
	  TO "convert"
	  }
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

document { "transmitting a vector",
     "The method for transmitting a vector depends on the ring involved.",
     PARA,
     "If the ring is a monoid ring (e.g., a polynomial ring), then
     the vector is transmitted as a sequence of triples ", TT "(i,m,a)", ", 
     where ", TT "i", " is the number of the row, ", TT "m", " is the monomial,
     and ", TT "a", " is the coefficient.",
     PARA,
     "If the ring is not a monoid ring, then the vector is transmitted
     as a sequences of pairs ", TT "(i,r)", " where ", TT "i", " is the 
     number of the row, and ", TT "r", " is the entry.",
     PARA,
     "The columns of a matrix are transmitted as vectors.",
     SEEALSO {"transmitting a monomial", "transmitting a matrix"}
     }

document { "transmitting a matrix",
     "Most objects in the engine are stored as matrices.  Even single
     polynomials are usually stored as 1 by 1 matrices.",
     PARA,
     "A matrix is transmitted by sending the columns as a sequence of
     vectors.",
     EXAMPLE "R = ZZ/101[x,y,z];",
     EXAMPLE "f = matrix ( R, {{11,0,33},{0,22,34}} )",
     EXAMPLE "ascii sendgg(ggPush f, ggtonet)",
     SEEALSO "transmitting a vector"
     }

document { "transmitting an integer mod n",
     "An integer mod n is sent as an integer.",
     PARA,
     EXAMPLE "ZZ/101[x];",
     EXAMPLE "s = 44 + x - x",
     EXAMPLE "ascii sendgg( ggPush s, ggleadcoeff, ggtonet)"
     }

document { "transmitting a polynomial",
     "A polynomial is transmitted as a sequence of pairs (m,c), where
     m is a monomial and c is a coefficient.",
     PARA,
     EXAMPLE "ZZ/101[x,y,z];",
     EXAMPLE "ascii callgg(ggtonet, 22*x^66+11*y^77)"
     }

document { "transmitting a monomial",
     "A monomial is transmitted as a sequence of pairs (i,e) of integers,
     where i refers to the i-th variable in the ring, and e is the exponent.",
     PARA,
     EXAMPLE "ZZ/3[t,u,x,y,z];",
     EXAMPLE "ascii sendgg(ggPush (t^22 * y^33 * z^55), ggleadmonom, ggtonet)"
     }

document { "transmitting a sequence",
     "Several items of the same type are transmitted as follows.  The
     number of items is transmitted first, as a positive integer of
     28 bits or less.  See ", TO "transmitting a positive integer", ".
     Then the items are transmitted.",
     PARA,
     EXAMPLE "ascii gg {33,44,55}"
     }

document { "transmitting a positive integer",
     "The integer 0 is transmitted as a single zero byte.
     A positive integer of 28 bits or less is sent 7 bits at a time, with
     the high-order nonzero seven bits sent first.  The highest bit of
     each byte except the last one is set to 1.",
     PRE "
     00000000    or
     0xxxxxxx    or
     1xxxxxxx 0xxxxxxx    or
     1xxxxxxx 1xxxxxxx 0xxxxxxx    or
     1xxxxxxx 1xxxxxxx 1xxxxxxx 0xxxxxxx",
     PARA,
     "A positive integer of more than 28 bits is sent as follows.  First
     come four bytes, with seven bits of the number in each one, with 
     the high bit of each byte set to 1.  Then comes the number of succeeding
     bytes, transmitted as described above for a positive integer of 28
     bits or less.  Finally come the succeeding bytes, each containing 8
     bits of the intger.  Format:",
     PRE "
     1xxxxxxx 1xxxxxxx 1xxxxxxx 1xxxxxxx  (first 28 bits)
     1xxxxxxx 0xxxxxxx                    (number of succeeding bytes)
     xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx  (succeeding bytes)
     xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx  (succeeding bytes)
     ...
     xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx  (succeeding bytes)
     xxxxxxxx xxxxxxxx                    (succeeding bytes)",
     "It may happen that the first byte in the sequence above has the
     form 10000000."
     }

document { "transmitting an integer",
     "The integer 0 is transmitted as a single zero byte.
     Otherwise, the sign of the integer is put into bit 6 of the first byte,
     and the bits of the absolute value of the integer are packed as follows:
     6 bits into the first byte, 7 bits into 1, 2, or 3 more bytes, and 
     8 bits into each of the succeeding bytes.  If 8 bit bytes are needed,
     then the number of them is sent as a positive integer after the
     first four bytes are sent.  See also ", 
     TO "transmitting a positive integer", ".
     In the following illustration, S denotes the sign bit, and x's denote
     the bits of the integer.",
     PRE "
     00000000     or

     0Sxxxxxx     or

     1Sxxxxxx 0xxxxxxx	or

     1Sxxxxxx 1xxxxxxx 0xxxxxxx	 or

     1Sxxxxxx 1xxxxxxx 1xxxxxxx 0xxxxxxx   or

     1Sxxxxxx 1xxxxxxx 1xxxxxxx 1xxxxxxx   (first 27 bits)
     1xxxxxxx 0xxxxxxx    	      	   (number of succeeding bytes)
     xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx   (succeeding bytes)
     xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx   (succeeding bytes)
     ...
     xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx   (succeeding bytes)
     xxxxxxxx xxxxxxxx	      	           (succeeding bytes)",
     EXAMPLE "binary = s -> concatenate between (\" \",
          apply(s,x -> apply(8, i-> toString ((x >> 7-i) % 2))));",
     EXAMPLE "<< binary ascii gg 63 << endl;",
     EXAMPLE "<< binary ascii gg 64 << endl;",
     EXAMPLE "<< binary ascii gg 127 << endl;",
     EXAMPLE "<< binary ascii gg 128 << endl;",
     EXAMPLE "<< binary ascii gg 2^10 << endl;",
     EXAMPLE "<< binary ascii gg 2^20 << endl;",
     EXAMPLE "<< binary ascii gg (-2^20) << endl;",
     EXAMPLE "<< binary ascii gg 2^30 << endl;",
     EXAMPLE "<< binary ascii gg 2^40 << endl;",
     EXAMPLE "<< binary ascii gg 2^50 << endl;"
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

document { original,
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

document { (quote _, Tally, Thing),     
     TT "t_x", " -- returns the number of times ", TT "x", " is counted
     by ", TT "x", ".",
     SEEALSO "Tally"
     }

document { Tally,
     TT "Tally", " -- a class designed to hold tally results, i.e., multisets.",
     PARA,
     "Operations:",
     MENU {
	  (TO (quote **,Tally,Tally), " -- Cartesian product"),
	  (TO (quote +,Tally,Tally), "  -- sum"),
	  (TO (quote -,Tally,Tally), "  -- difference"),
	  (TO (quote ?,Tally,Tally), "  -- comparison"),
	  (TO (quote _,Tally,Thing), "  -- access"),
	  (TO "tally", "                -- tally the elements of a list"),
	  (TO "toList", "               -- a list of the elements"),
	  (TO (sum,Tally), "           -- add the elements"),
	  (TO (product,Tally), "       -- multiply the elements")
	  }
     }

document { (quote **, Tally, Tally),
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

document { (quote ?, Tally, Tally),
     TT "x ? y", " -- compares two tallies, returning ", TT "quote <", ", ",
     TT "quote >", ", ", TT "quote ==", ", or ", TO "incomparable", ".",
     SEEALSO "Tally"
     }

document { (quote +, Tally, Tally),
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

document { (quote -, Tally, Tally),
     TT "x - y", " -- produces the difference of two tallies.",
     PARA,
     EXAMPLE "tally {a,a,b,c} - tally {c,d,d}",
     SEEALSO "Tally"
     }

document { tally,
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
     TT "Set", " -- the class of all sets.",
     PARA,
     "Function for creating sets:",
     MENU {
	  TO "set"
	  },
     "Operations on sets:",
     MENU {
	  (TO (quote +,Set,Set), " -- union"),
	  (TO (quote ++,Set, Set), " -- disjoint union"),
	  (TO (quote -,Set, Set), "  -- difference"),
	  (TO (quote *,Set,Set), " -- intersection"),
	  (TO (quote **, Set, Set), " -- Cartesian product"),
	  (TO "#", " -- the number of elements"),
	  (TO "member", " -- whether something is a member"),
	  (TO (product,Set), " -- multiply the elements"),
	  (TO (isSubset,Set,Set), " -- whether a set is a subset of another"),
	  (TO (subsets,Set), " -- a list of the subsets"),
	  (TO (subsets,Set,ZZ), " -- a list of the subsets with given cardinality"),
	  (TO (sum,Set), " -- sum the elements"),
	  (TO "toList", " -- a list of the elements"),
	  }
     }

document { (quote -, Set, Set),
     TT "x - y", " -- the difference of two sets.",
     SEEALSO {"Set", "-"}
     }

document { (isSubset,Set,Set),
     TT "isSubset(X,Y)", " -- tells whether X is a subset of Y.",
     PARA,
     SEEALSO "isSubset"
     }
document { isSubset,
     TT "isSubset(x,y)", " -- whether ", TT "x", " is a subset of ", TT "y", ".",
     PARA,
     MENU {
	  TO (isSubset,Set,Set)
	  }
     }

document { (quote ++, Set, Set),
     TT "x ++ y", " -- the disjoint union of two sets.",
     PARA,
     EXAMPLE "set {a,b,c} ++ set {b,c,d}",
     SEEALSO {"Set", "++"}
     }

document { (quote *, Set, Set),
     TT "x * y", " -- the intersection of two sets.",
     PARA,
     EXAMPLE "set {1,2,3} * set {2,3,4}",
     SEEALSO {"Set", "*"}
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

document { Handle,
     TT "Handle", " -- the class of all Handles.",
     PARA,
     "This concept is mainly for internal use.",
     PARA,
     "A handle is essentially a small integer by means of which the ", TO "engine", "
     refers to the algebraic entities in it.  A Handle (capitalize), on the other
     hand, is a hash table, and in it, under the key ", TO "value", " is stored
     the handle.",
     PARA,
     "One advantage of a Handle is that it can be registered with the
     Boehm garbage collector for last minute action at the time the Handle
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

document { "engine", 
     "The engine is the part of the program that is dedicated to
     performing the computation of Groebner bases with Buchberger's
     algorithm.  It is coded directly in C++ for speed, and it communicates
     with the front-end interpreter through a bidirectional stream of bytes,
     so that in future implementations the engine may reside in a separate
     process on a distant machine.",
     MENU {
	  TO "engine communication protocol",
     	  TO "low level gb engine commands",
	  TO "high level gb engine commands"
	  },
     PARA,
     "The Macaulay 2 engine provides fast polynomial and matrix operations,
     and Groebner bases, syzygies, Hilbert functions, resolutions and
     other operations that we feel need to be implemented directly for
     efficiency reasons.",
     }

document { "high level gb engine commands",
     "Sending commands to the engine:",
     MENU {
	  TO "callgg",
	  TO "gg",
	  TO "ggPush",
	  TO "handle",
	  TO "sendToEngine",
	  TO "sendgg"
	  },
     "This class provides an interface to rings implemented by the engine.",
     MENU {
	  TO "EngineRing"
	  },
     "These routines take an element off the stack.",
     MENU {
	  TO "eePop",
	  TO "eePopBool",
	  TO "eePopInt",
	  TO "eePopIntarray",
	  TO "getMatrix"
	  },
     "These functions transfer ring elements to other rings.",
     MENU {
	  TO "eeLift",
	  TO "eePromote"
	  },
     "These functions are used mainly for debugging the engine.",
     MENU {
	  TO "look",
	  TO "engineMemory",
	  TO "engineStack",
	  TO "heap",
	  TO "see"
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

document { accumulate,
     TT "accumulate(f,x0,{x1,...,xn})", " -- computes the list 
     ", TT "{f(x0,x1),f(f(x0,x1),x2),...}", ".",
     BR,NOINDENT,
     TT "accumulate({xn,...,x1},x0,f)", " -- computes the list 
     ", TT "{...,f(x2,f(x1,x0)),f(x1,x0)}", ".",
     BR,NOINDENT,
     TT "accumulate(f,{x0,x1,...,xn})", " -- computes the list 
     ", TT "{f(x0,x1),f(f(x0,x1),x2),...}", ".",
     BR,NOINDENT,
     TT "accumulate({xn,...,x1,x0},f)", " -- computes the list 
     ", TT "{...,f(x2,f(x1,x0)),f(x1,x0)}", ".",
     PARA,
     EXAMPLE {
	  "accumulate(plus,1,{10,100,1000})",
	  "accumulate(toList,{a,b,c,d})",
	  },
     SEEALSO {"fold"}
     }

TEST ///
     assert( accumulate(toList,a,{b,c,d}) == {{a, b}, {{a, b}, c}, {{{a, b}, c}, d}} )
     assert( accumulate({a,b,c},d,toList) == {{a, {b, {c, d}}}, {b, {c, d}}, {c, d}} )
     assert( accumulate(toList,{a,b,c,d}) == {{a, b}, {{a, b}, c}, {{{a, b}, c}, d}} )
     assert( accumulate({a,b,c,d},toList) == {{a, {b, {c, d}}}, {b, {c, d}}, {c, d}} )
///     

document { fold,
     TT "fold(f,x0,{x1,...,xn})", " -- computes ", TT "f(...f(f(x0,x1),x2)...)}", ".",
     BR,NOINDENT,
     TT "fold({xn,...,x1},x0,f)", " -- computes ", TT "f(...f(x2,f(x1,x0))...)}", ".",
     BR,NOINDENT,
     TT "fold(f,{x0,x1,...,xn})", " -- computes ", TT "f(...f(f(x0,x1),x2)...)}", ".",
     BR,NOINDENT,
     TT "fold({xn,...,x1,x0},f)", " -- computes ", TT "f(...f(x2,f(x1,x0))...)}", ".",
     EXAMPLE {
	  "fold(toList, {a,b,c,d,e})"
	  },
     SEEALSO {"accumulate"}
     }

TEST ///
     assert( fold(toList, a, {b,c,d}) === {{{a, b}, c}, d} )
     assert( fold({a,b,c}, d, toList) === {a, {b, {c, d}}} )
     assert( fold(toList, {a,b,c,d}) === {{{a, b}, c}, d} )
     assert( fold({a,b,c,d}, toList) === {a, {b, {c, d}}} )
///

document { demark,
     TT "demark(s,x)", " -- given a list of strings ", TT "x", " and
     a string ", TT "s", " provides the string obtained by concatenating
     the elements of ", TT "x", " with a copy of ", TT "x", " inserted
     between each successive pair.",
     PARA,
     EXAMPLE "demark(\"+\",{\"a\",\"b\",\"c\"})"
     }

document { InfiniteNumber,
     TT "InfiniteNumber", " -- the class of all infinite numbers.",
     PARA,
     SEEALSO { "infinity", "-infinity" }
     }

document { infinity,
     TT "infinity", " -- a representation of infinity.",
     PARA,
     SEEALSO { "-infinity", "InfiniteNumber" }
     }

document { IndeterminateNumber,
     TT "IndeterminateNumber", " -- the class of indeterminate numbers (of
     which there is only one).",
     PARA,
     SEEALSO "indeterminate"
     }

document { indeterminate,
     TT "indeterminate", " -- a representation of an indeterminat number, such as might
     result from multiplying 0 by infinity.",
     PARA,
     SEEALSO "IndeterminateNumber"
     }

document { "-infinity",
     TT "-infinity", " -- a representation of negative infinity.",
     SEEALSO { "infinity", "InfiniteNumber" }
     }

document { max,
     TT "max x", " -- yields the maximum of the elements in the list or sequence x."
     }

document { min,
     TT "min x", " -- yields the minimum of the elements in the list or sequence x."
     }

TEST ///
assert(max{4,5,6} === 6)
assert(min{4,5,6} === 4)
assert(max(4,5,6) === 6)
assert(min(4,5,6) === 4)
///

document { sort,
     TT "sort v", " -- produces a sorted version of the list v.",
     PARA,
     "The sort function uses ", TO "<=", " to compare elements of the
     list, which in turn calls upon ", TO "?", ".",
     EXAMPLE {
	  "sort {c,e,a,f,b,f}",
	  "sort {4,2,6,3,8,2}"
	  },
     SEEALSO { "rsort", "<=", "?" }
     }

document { rsort,
     TT "rsort v", " -- produces a reverse sorted version of the list v.",
     PARA,
     "The rsort function uses ", TO "<=", " to compare elements of the
     list, which in turn calls upon ", TO "?", ".",
     EXAMPLE {
	  "rsort {g,d,w,s,c,a,r}",
	  "rsort {4,2,3,1}",
	  },
     SEEALSO { "sort", "<=", "?" }
     }


document { pack,
     TT "pack(v,n)", " -- packs the elements of the list or sequence
     ", TT "v", " into a table ", TT "n", " at a time.",
     PARA,
     "It produces, from a list ", TT "v", ", a list of lists formed 
     by packing the elements of ", TT "v", " into lists ", TT "n", " 
     at a time.  The last of the lists produced may have fewer 
     than ", TT "n", " elements.",
     EXAMPLE "pack({a,b,c,d,e,f,g,h,i,j,k},3)",
     }

document { join,
     TT "join(u,v,...)", " -- joins the elements of the lists or
     sequences u, v, ... into a single list.",
     PARA,
     "The class of the result is the same as the class of the first argument.
     If there is just one argument, and it's mutable, a copy is returned.",
     EXAMPLE "join({1,2,3},{a,b,c},{7,8,9})",
     PARA,
     "The operator ", TO (quote |, List, List), " can be used as a synonym."
     }

document { take,
     TT "take(v,n)    ", " -- yields a list containing the first n elements of the list v.",
     BR,NOINDENT,
     TT "take(v,-n)", "    -- yields a list containing the last n elements of the list v.",
     BR,NOINDENT,
     TT "take(v,{m,n})", " -- yields a list containing the elements of the list v 
     in positions m through n.",
     PARA,
     EXAMPLE {
	  "take({a,b,c,d,e,f},3)",
	  "take({a,b,c,d,e,f},-3)",
	  "take({a,b,c,d,e,f},{2,4})",
	  },
     SEEALSO "drop"
     }

document { first,
     TT "first v", " -- yields the first element of the list v.",
     PARA,
     "See also ", TO "last", "."
     }

document { last,
     TT "last v", " -- yields the last element of the list v.",
     PARA,
     "See also ", TO "first", "."
     }

document { positions,
     TT "positions(v,f)", " -- yields a list of integers giving the positions of the
     elements of the list v which yield the value true when
     the function f is applied."
     }

TEST "
assert( 3 === position({a,b,c,d,e,f},i->i===d ) )
"

document { position,
     TT "position(v,f)", " -- returns the index of the first element of v satisfying 
     the condition f, or null if there is none."
     }

document { delete,
     TT "delete(x,v)", " -- removes any occurrences of the expression ", TT "x", "
     from the list ", TT "v", ".",
     PARA,
     "Equality is determined with ", TO "==", " which may do extensive
     calculations in certain cases.",
     EXAMPLE {
	  "delete(c,{a,b,c,d,e,a,b,c,d,e})",
	  },
     SEEALSO "member"
     }

document { "functions",
     "There are two types of functions, those functions built in
     to the system, and those created by the user, but in practice
     the user has no way of distinguishing the two types.  The user
     creates new functions with the ", TO "->", " operator.",
     PARA,
     "Operations on functions:",
     MENU {
 	  (TO "@@", " -- composition"),
	  (TO "ultimate", " -- ultimate value for an iteration")
 	  },
     "Particular functions:",
     MENU {
	  TO "identity"
	  }
     }

TEST "
stream = (action,state) -> () -> stream(action, action state)
fib = stream( (i,j) -> (j,i+j), (0,1))
scan(1 .. 22, i -> fib = fib())
"

document { ultimate,
     TT "ultimate(f,x)", " -- yields the value ultimately obtained by
     applying the function ", TT "f", " to ", TT "x", ".",
     PARA,
     "Iteration ceases when an error occurs during application of the
     function, or the result is the same.  Errors are not reported.",
     PARA,
     "It's a bad idea to use this function, because unexpected errors will
     produce unexpected results silently."
     }

document { tmpname,
     TT "tmpname x", " -- create a temporary file name based on the string x
     unique to this process.",
     PARA,
     "The code for this function is Unix dependent at the moment.",
     PARA,
     "The routine doesn't actually check to see whether file exists."
     }

document { subsets,
     TT "subsets", " -- a function for computing a list of subsets
     of a set or list.",
     PARA,
     MENU {
	  TO (subsets,ZZ,ZZ),
	  TO (subsets,Set,ZZ),
	  TO (subsets,List),
	  TO (subsets,Set)
	  }
     }

document { (subsets,ZZ,ZZ),
     TT "subsets(n,j)", " -- for an integer ", TT "n", ", yields a list of those
     subsets of ", TT "{0, ..., n-1}", " which have ", TT "j", " elements.",
     PARA,
     EXAMPLE "subsets(3,2)",
     SEEALSO "subsets"
     }
document { (subsets,Set,ZZ),
     TT "subsets(s,j)", " -- yields a list of those subsets of the list or 
     set ", TT "s", " which have ", TT "j", " elements.",
     PARA,
     EXAMPLE "subsets(set {a,b,c},2)",
     SEEALSO "subsets"
     }
document { (subsets,List),
     TT "subsets s", " -- yields a list of the subsets of the list ", TT "s", ".",
     PARA,
     "The subsets are returned as lists whose elements are in the same order.",
     EXAMPLE "subsets {1,2,3}",
     SEEALSO "subsets"
     }
document { (subsets,Set),
     TT "subsets s", " -- yields a list of the subsets of the set ", TT "s", ".",
     PARA,
     EXAMPLE "subsets set {a,b,c}",
     SEEALSO "subsets"
     }

TEST "
assert( subsets(4,2) === {{0,1},{0,2},{1,2},{0,3},{1,3},{2,3}} )
assert( subsets({a,b,c,d},2) === {{a,b},{a,c},{b,c},{a,d},{b,d},{c,d}} )
assert( 
     set subsets(set {a,b,c,d},2) === 
     set apply({{a,b},{a,c},{b,c},{a,d},{b,d},{c,d}},set) )
assert( partitions 4 === {{4},{3,1},{2,2},{2,1,1},{1,1,1,1}} )
assert( partitions(5,3) === {{3,2},{3,1,1},{2,2,1},{2,1,1,1},{1,1,1,1,1}} )
"


document { partitions,
     TT "partitions n", " -- returns a list of the partitions of the integer n.",
     BR, NOINDENT,
     TT "partitions(n,k)", " -- returns a list of the partitions of the integer n
     into terms each of which does not exceed k.",
     PARA,
     EXAMPLE {
	  "partitions 4",
      	  "partitions(4,2)",
	  },
     }

document { examples,
     -- this should come after the doc for partitions, because of the example
     TT "examples f", " -- returns a list of strings containing examples
     of code using the function ", TT "f", " provided in the documentation
     of ", TT "f", ".",
     PARA,
     EXAMPLE {
	  ///examples partitions///,
      	  ///printExamples partitions///,
	  },
     SEEALSO {"document", "printExamples"}
     }

TEST ///
     assert( class examples MutableList === List )
     assert( # examples MutableList > 0 )
///

document { (quote +, List, List),
     TT "v + w", " -- the sum of two vectors represented as lists."
     }

document { (quote _, List, List),
     TT "w_{i,j,...}", " -- selects entries from a list or sequence ", TT "w", ".",
     PARA,
     EXAMPLE {
	  "w = {a,b,c,d,e,f,g,h};",
      	  "w_{1,3,4}",
	  },
     "We can use this operation to compute composition of permutations
     represented as lists.",
     EXAMPLE "{4,2,3,1,0} _ {2,1,3,4,0}"
     }

document { number,
     TT "number(x,f)", " -- the number of elements e of the list x for which f(e) is true.",
     PARA,
     "See also ", TO "positions", " and ", TO "select", "."
     }

document { all,
     TT "all(v,f)", " -- whether each element x of a list or hash table
     v has f(x) true.",
     PARA,
     "Returns the value true if all elements v#i of the list v yield 
     the value true when the function f is applied, otherwise returns 
     false.  For hash tables, the function is applied to all its key/value
     pairs (k,v), just as with ", TO "any", ".",
     PARA,
     SEEALSO { "scan", "apply", "select", "any", "member" }
     }

document { same,
     TT "same v", " -- whether every element of the list v is the same.
     The comparison is done with ", TO "==", "."
     }

document { member,
     TT "member(e,x)", " -- whether ", TT "e", " is an element of the list, set, or 
     sequence ", TT "x", ".",
     PARA,
     EXAMPLE {
	  "x = {a,b,c,d,e};",
      	  "member(c,x)",
      	  "member(f,x)",
      	  {"positions", "Set"}
	  },
     }

document { sum,
     TT "sum", " -- provides the sum of the members of a list, set, 
     or chain complex, optionally with a function applied to each one.",
     PARA,
     MENU {
	  TO {(sum, List), " -- sum the elements of a list or sequence"},
	  TO {(sum, List, List, Function), " -- sum results of applying a function"},
	  TO {(sum, List, Function), " -- sum results of applying a function"},
	  TO {(sum, ZZ, Function), " -- sum consecutive values of a function"},
	  TO {(sum, Tally), " -- sum elements of a tally"},
	  TO {(sum, Set), " -- sum elements of a tally"},
	  TO {(sum, ChainComplex), " -- sum modules in a chain complex"},
	  TO {(sum, ChainComplexMap), " -- sum components in a map of chain complexes"}
	  },
     SEEALSO "product"
     }
document { (sum, List),
     TT "sum v", " -- yields the sum of the elements in the list v.",
     PARA,
     EXAMPLE "sum {1,2,3,4,5}",
     SEEALSO "sum"
     }
document { (sum, List, List, Function),
     TT "sum(v,w,f)", " -- yields the sum of the results obtained by
     applying ", TT "f", " to each of the pairs ", TT "(i,j)", " of elements from 
     the lists or sequences ", TT "v", " and ", TT "w", ", which should be of 
     the same length.",
     PARA,
     EXAMPLE {
	  "R = ZZ[x,y,z];",
      	  "sum({2,3,4},{x,y,z},(i,j)->j^i)",
	  },
     SEEALSO "sum"
     }
document { (sum, List, Function),
     TT "sum(v,f)", " -- yields the sum of the expressions obtained by
     applying ", TT "f", " to each of the elements of the list or sequence ", TT "v", ".",
     PARA,
     EXAMPLE "sum(1 .. 10, i -> i^2)",
     SEEALSO "sum"
     }
document { (sum, ZZ, Function),
     TT "sum(n,f)", " -- compute the sum ", TT "f(0) + f(1) + ... + f(n-1)", ".",
     PARA,
     EXAMPLE "sum(10, i -> i^2)",
     SEEALSO "sum"
     }
document { (sum, Tally),
     TT "sum v", " -- yields the sum of the elements in the tally ", TT "v", ".",
     PARA,
     EXAMPLE {
	  "a = tally{1,1,1,1,1,10,10,10,100,100}",
      	  "sum a",
	  },
     SEEALSO "sum"
     }
document { (sum, Set),
     TT "sum v", " -- yields the sum of the elements in the set ", TT "v", ".",
     PARA,
     EXAMPLE {
	  "a = set{1,100,10000}",
      	  "sum a",
	  },
     SEEALSO "sum"
     }

document { product,
     TT "product", " -- provides the product of the members of a list or set,
     optionally with a function applied to each one.",
     PARA,
     MENU {
	  TO {(product, List), " -- product the elements of a list or sequence"},
	  TO {(product, List, List, Function), " -- product results of applying a function"},
	  TO {(product, List, Function), " -- product results of applying a function"},
	  TO {(product, ZZ, Function), " -- product consecutive values of a function"},
	  TO {(product, Tally), " -- product elements of a tally"},
	  TO {(product, Set), " -- product elements of a tally"}
	  }
     }
document { (product, List),
     TT "product v", " -- yields the product of the elements in the list v.",
     PARA,
     EXAMPLE "product {1,2,3,4,5}",
     SEEALSO "product"
     }
document { (product, List, List, Function),
     TT "product(v,w,f)", " -- yields the product of the results obtained by
     applying ", TT "f", " to each of the pairs ", TT "(i,j)", " of elements from 
     the lists or sequences ", TT "v", " and ", TT "w", ", which should be of 
     the same length.",
     PARA,
     EXAMPLE {
	  "M = monoid [x,y,z];",
      	  "product({2,3,4},{x,y,z},(i,j)->j^i)",
	  },
     SEEALSO "product"
     }
document { (product, List, Function),
     TT "product(v,f)", " -- yields the product of the expressions obtained by
     applying ", TT "f", " to each of the elements of the list or sequence ", TT "v", ".",
     PARA,
     EXAMPLE "product(1 .. 5, i -> i^2)",
     SEEALSO "product"
     }
document { (product, ZZ, Function),
     TT "product(n,f)", " -- compute the product ", TT "f(0) * f(1) * ... * f(n-1)", ".",
     PARA,
     EXAMPLE "product(5, i -> 2*i+1)",
     SEEALSO "product"
     }
document { (product, Tally),
     TT "product v", " -- yields the product of the elements in the tally ", TT "v", ".",
     PARA,
     EXAMPLE {
	  "a = tally{2,2,2,2,2,3,3,3,5,5}",
      	  "product a",
	  },
     SEEALSO "product"
     }
document { (product, Set),
     TT "product v", " -- yields the product of the elements in the set ", TT "v", ".",
     PARA,
     EXAMPLE {
	  "a = set select(1..50, isPrime)",
      	  "product a",
	  },
     SEEALSO "product"
     }

document { toString,
     TT "toString x", " -- converts ", TT "x", " to a string.",
     PARA,
     "See also ", TO "toExternalString", " which will try to convert ", TT "x", "
     to a string which can be read back into the program later."     
     }

document { toExternalString,
     TT "toExternalString x", " -- converts ", TT "x", " to a string, in such a way
     that it can be read back into the program later.",
     PARA,
     "See also ", TO "toString", " which simply converts ", TT "x", "
     to a string which can be displayed meaningfully.",     
     PARA,
     "Not everything can be converted to a string in such a way that it
     can be read back into the program later, because circular data structures
     are common."
     }

document { HeaderType,
     TT "HeaderType", " -- the class of all types ", TT "X", " of lists which can be
     constructed by expressions of the form ", TT "X {a,b,c,...}", ".  They
     also act on sequences.",
     PARA,
     EXAMPLE {
	  "X = new HeaderType of BasicList",
	  "X {a,b,c}"
	  },
     SEEALSO {"WrapperType", "SelfInitializingType"}
     }

document { WrapperType,
     TT "WrapperType", " -- the class of all types ", TT "X", " of lists which can be
     constructed by expressions of the form ", TT "X {a,b,c,...}", ", or, for lists
     of length one, by an expression of the form ", TT "X a", ".  They also act
     on sequences.",
     PARA,
     EXAMPLE {
	  "X = new WrapperType of BasicList",
	  "X {a,b,c}",
	  "X a"
	  },
     SEEALSO {"HeaderType", "SelfInitializingType"}
     }

document { AssociativeExpression,
     TT "AssociativeExpression", " -- a type of ", TO "Expression", ".",
     PARA,
     "Types of associative expression:",
     MENU {
	  TO "Equation",
	  TO "Product",
	  TO "Sum"
	  },
     SEEALSO "Expression"
     }

document { Holder,
     TT "Holder", " -- a type of ", TO "Expression", ".",
     PARA,
     "This type of expresssion is a container for a single, arbitrary, thing which
     is basic enough that the correct method for printing does not depend
     on its neighbors in the containing expression.  A negative number would
     not be basic enough for this purpose, since as a member of a sum, it would
     require special treatment."
     }

document { ZeroExpression,
     TT "ZeroExpression", " -- a type of ", TO "Expression", " of which
     there is just one instance, an expression representing the number 0."
     }
document { OneExpression,
     TT "OneExpression", " -- a type of ", TO "Expression", " of which
     there is just one instance, an expression representing the number 1."
     }

document { DoubleArrow,
     TT "DoubleArrow", " -- a type of ", TO "Expression", " which represents
     something of the form ", TT "a => b", ".",
     PARA,
     "This is experimental, and intended for internal use only."
     }

document { Expression,
     TT "Expression", " -- the class of all expressions.",
     PARA,
     "These expressions are symbolic representations of algebraic
     expressions, mainly useful in printing.  The method for 
     producing them is ", TO "expression", ".  The usual algebraic
     operations are available for them, but most simplifications do not
     occur.",
     PARA,
     "The parts of expressions are not always expressions.  For example,
     ", TO "factor", " returns such an expression.",
     PARA,
     EXAMPLE "(expression 2)^5 * (expression 3)^3 / ((expression 5) * (expression 11)^2)^6",
     PARA,
     "Types of expressions:",
     MENU {
	  TO "Adjacent",
	  TO "AssociativeExpression",
	  TO "BinaryOperation",
	  TO "Divide",
	  TO "DoubleArrow",
     	  TO "FunctionApplication",
	  TO "Holder",
	  TO "MatrixExpression",
	  TO "Minus",
	  TO "NonAssociativeProduct",
	  TO "OneExpression",
	  TO "Power",
	  TO "Product",
	  TO "RowExpression",
	  TO "SparseMonomialVectorExpression",
	  TO "SparseVectorExpression",
	  TO "Subscript",
	  TO "Superscript",
	  TO "Sum",
	  TO "Table",
	  TO "ZeroExpression",
	  },
     "Functions which create expressions:",
     MENU {
	  TO "hold",
	  },
     "Functions which act on expressions:",
     MENU {
	  TO "value",
	  TO "precedence"
	  }
     }
document { expression,
     TT "expression x", " -- make an ", TO "Expression", " from x."
     }

document { Divide,
     TT "Divide", " -- a type of ", TO "Expression", " representing a quotient."
     }
document { Table,
     TT "Table", " -- a type of ", TO "Expression", " representing
     a table, i.e., a list of lists of the same length.",
     PARA,
     EXAMPLE {
	  ///Table {{a,b,c},{a,bb,ccc}}///,
	  ///value oo///,
	  },
     SEEALSO {"MatrixExpression"}
     }
document { MatrixExpression,
     TT "MatrixExpression", " -- a type of ", TO "Expression", " representing
     a matrix.",
     PARA,
     EXAMPLE ///MatrixExpression {{a,b,c},{a,bb,ccc}}///,
     SEEALSO {"Table"}
     }
document { RowExpression,
     TT "RowExpression", " -- a type of ", TO "Expression", " representing
     a horizontal sequence of expressions."
     }
document { Minus,
     TT "Minus", " -- a type of ", TO "Expression", " representing negation.",
     PARA,
     "This is a unary operator."
     }
document { NonAssociativeProduct,
     TT "NonAssociativeProduct", " -- a type of ", TO "Expression", " representing
     a nonassociative product."
     }
document { Power,
     TT "Power", " -- a type of ", TO "Expression", " representing a power.",
     PARA,
     "Normally power expressions with an exponent equal to 1 will not be
     produced.  But it is desirable for ", TO "factor", " to return 
     a product of powers, and some of them will have 1 as exponent.  The
     routines for printing of expressions will take this into account,
     suppress exponents equal to 1, and arrange for parenthesization
     correctly."
     }
document { Product,
     TT "Product", " -- a type of ", TO "Expression", " representing a product."
     }
document { SparseVectorExpression,
     TT "SparseVectorExpression", " -- a type of ", TO "Expression", "
     representing a sparse vector."
     }
document { SparseMonomialVectorExpression,
     TT "SparseMonomialVectorExpression", " -- a type of ", TO "Expression", "
     representing a sparse monomial vector.",
     PARA,
     "The difference between this and ", TO "SparseVectorExpression", " is that
     the basis vectors are treated like variables for printing purposes."
     }
document { BinaryOperation,
     TT "BinaryOperation", " -- a type of ", TO "Expression", " representing
     the result of a binary operation."
     }
document { Subscript,
     TT "Subscript", " -- a type of ", TO "Expression", " representing a
     subscripted expression."
     }
document { Adjacent,
     TT "Adjacent", " -- a type of ", TO "Expression", " representing a pair
     of adjacent expressions, separated only by white space."
     }
document { FunctionApplication,
     TT "FunctionApplication", " -- a type of ", TO "Expression", " representing an
     application of a function."
     }
document { Superscript,
     TT "Superscript", " -- a type of ", TO "Expression", " representing a
     superscripted expression."
     }
document { Equation,
     TT "Equation", " -- a type of ", TO "Expression", " representing an
     equation."
     }
document { Sum,
     TT "Sum", " -- a type of ", TO "Expression", " representing a sum."
     }

document { tex,
     TT "tex x", " -- convert ", TT "x", " to TeX format.",
     PARA,
     EXAMPLE {
	  "R = ZZ[a..f]",
      	  "tex matrix {{a^2+2,b,c},{d,e,f^3-a}}",
	  },
     SEEALSO {"TeX", "texMath"}
     }

document { texMath,
     TT "texMath x", " -- convert ", TT "x", " to TeX format
     for use in TeX math mode.",
     PARA,
     "The main difference between this and ", TO "tex", " is that the
     surrouding dollar signs aren't there.",
     PARA,
     EXAMPLE {
	  "R = ZZ[x]",
      	  "texMath (x-1)^6",
	  },
     SEEALSO {"TeX", "tex"}
     }

document { TeX,
     TT "TeX x", " -- convert ", TT "x", " to TeX format, and display it on the screen.",
     PARA,
     "The code for this function is Unix dependent at the moment.",
     PARA,
     SEEALSO "tex"
     }

document { print,
     TT "print x", " -- prints ", TT "x", " on the standard output followed by a new line",
     PARA,
     "The return value is ", TO "null", "."
     }
document { (quote <<, File, Thing),
     TT "f << x", " -- prints the expression x on the output file f.",
     PARA,
     "Returns f as its value.  Parsing associates leftward, so that 
     several expressions may be displayed with something like ", TT "f<<x<<y<<z", ".
     If ", TT "f", " is a string, then a new file with name ", TT "f", " is created,
     the expression ", TT "x", " is printed into ", TT "f", ", and the file ", TT "f", " is closed.",
     PARA,
     EXAMPLE {
	  "x = 5",
      	  ///<< "the value of x is " << x << endl///,
	  },
     SEEALSO {"<<"}
     }     

document { hold,
     TT "hold x", " -- embeds it argument x in a list of class ", TO "Holder", ".",
     PARA,
     "It might be useful for displaying an integer in factored form,
     for example, because the usual algebraic operations are available
     for ", TO "Expression", "s, but no simplification occurs.",
     PARA,
     EXAMPLE "(hold 2)^5 * (hold 3)^3 * (hold 5) * (hold 11)^2",
     PARA,
     "Here is example of a little function that expresses rational numbers
     as Egyptian fractions using ", TT "hold", ".",
     EXAMPLE {
	  "egyptian = method();",
	  ///egyptian QQ := x -> (
    if x == 0 then 0
    else (
         n := ceiling(1/x);
         hold(1/n) + egyptian(x - 1/n) 
         ));///,
     	  "egyptian(30/31)"
     	  }
     }

document { RightArrow,
     TT "RightArrow", " -- an entity used in hypertext to represent an
     rightward pointing arrow."
     }

document { DownArrow,
     TT "DownArrow", " -- an entity used in hypertext to represent an
     downward pointing arrow."
     }

document { peek,
     TT "peek s", " -- displays contents of ", TT "s", " to depth 1, bypassing
     installed methods.",
     PARA,
     EXAMPLE {
	  "t = set {1,2,3}",
      	  "peek t",
      	  "new MutableHashTable from {a=>3, b=>44}",
      	  "peek oo"
	  },
     SEEALSO "peek2"
     }

document { peek2,
     TT "peek2(s,n)", " -- displays contents of ", TT "s", " to depth ", TT "n", ", 
     bypassing installed methods.",
     PARA,
     "It applies the default output method to the object ", TT "s", ",
     bypassing the installed method for objects of its class.",
     EXAMPLE {
	  "s = factor 112",
      	  "peek s",
      	  "peek2(s,2)"
	  },
     SEEALSO "peek"
     }

document { pad,
     TT "pad(s,n)", " -- pads the string s to length n with spaces on the right.",
     BR,
     NOINDENT, 
     TT "pad(n,s)", " -- pads the string s to length n with spaces on the left."
     }

document { columnate,
     TT "columnate(s,w)", " -- arranges the strings in the list s in columns, returning
     a ", TO "Net", " suitable for output to a terminal with a linewidth of w.",
     PARA,
     EXAMPLE {
	  "columnate(characters ascii (65 .. 90) , 12)",
	  }
     }

document { ScriptedFunction,
     TT "ScriptedFunction", " -- the class of all scripted functions,
     by which we mean those functions 'f' of one argument 'x' which
     accept their argument as a subscript 'f_x' or as a superscript 'f^x'.",
     PARA,
     "To create a new subscripted function use a statement of the following
     form.",
     PRE "     f = new ScriptedFunction from { subscript => (x) -> ... }",
     "To create a new superscripted function use a statement of the following
     form.",
     PRE "     f = new ScriptedFunction from { superscript => (x) -> ... }",
     "The subscript and superscript options can be combined to create a
     scripted function which accepts either a subscript or a superscript.",
     PARA,
     "A good example of a subscripted function is ", TO "identity", ".",
     PARA,
     SEEALSO "ScriptedFunctor"
     }

document { ScriptedFunctor,
     TT "ScriptedFunctor", " -- the class of all functors which accept a 
     subscript or a superscript, the primary example of which is ", TO "HH", ".",
     SEEALSO {"subscript", "superscript", "argument"}
     }

document { argument,
     TT "argument", " -- a key used in scripted functors under which is
     stored the function that accepts the arguments.",
     SEEALSO "ScriptedFunctor"
     }

document { subscript,
     TT "subscript", " -- a key used in scripted functors under which is
     stored the function of one variable that accepts the subscript and
     returns a scripted functor that accepts the arguments.",
     SEEALSO "ScriptedFunctor"
     }

document { superscript,
     TT "superscript", " -- a key used in scripted functors under which is
     stored the function of one variable that accepts the superscript and
     returns a scripted functor that accepts the arguments.",
     SEEALSO "ScriptedFunctor"
     }

document { HH,
     TT "HH", " -- general homology and cohomology functor.",
     PARA,
     "Specific examples:",
     MENU {
	  (TO (homology, ZZ, ChainComplex), "      -- homology of a chain complex"),
	  (TO (homology, ChainComplex), "          -- total homology of a chain complex"),
	  (TO (homology, ZZ, ChainComplexMap), "   -- homology as a functor"),
	  (TO (homology, Matrix, Matrix), "        -- homology of a pair of maps"),
	  (TO (cohomology, ZZ, ChainComplex), "    -- cohomology of a chain complex"),
	  (TO (cohomology, ZZ, ChainComplexMap), " -- cohomology as a functor"),
	  (TO (cohomology, ZZ, Module), "          -- local cohomology"),
	  (TO (cohomology, ZZ, CoherentSheaf), "   -- sheaf cohomology"),
	  }
     }

TEST ("
     R=ZZ/101[a..d]
     C=resolution cokernel vars R
     D = C ++ C[1] ++ C[2]
     betti D
     assert( degree HH_1 D === 0 )
     assert( degree HH_0 D === 1 )
     assert( degree HH_-1 D === 1 )
     assert( degree HH_-2 D === 1 )
     ")

document { cohomology,
     TT "cohomology", " -- a method name available for computing expressions
     of the forms HH^i(X) and HH^i(M,N).",
     PARA,
     "If it is intended that i be of class ZZ, M be of class A, and N be of 
     class B, then the method can be installed with ",
     PRE "   cohomology(ZZ, A, B) := opts -> (i,M,N) -> ...",
     SEEALSO {"homology", "ScriptedFunctor"}
     }

document { homology,
     TT "homology(f,g)", " -- computes the homology module (kernel f)/(image g).",
     BR, NOINDENT,
     TT "homology", " -- a method name available for computing expressions
     of the forms HH_i(X) and HH_i(M,N).",
     PARA,
     "If it is intended that i be of class ZZ, M be of class A, and N be of
     class B, then the method can be installed with ",
     PRE "   homology(ZZ, A, B) := opts -> (i,M,N) -> ...",
     SEEALSO {"cohomology", "ScriptedFunctor"}
     }
