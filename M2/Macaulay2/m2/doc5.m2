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

document { (symbol _, Tally, Thing),     
     TT "t_x", " -- returns the number of times ", TT "x", " is counted
     by ", TT "x", ".",
     SEEALSO "Tally"
     }

document { Tally,
     TT "Tally", " -- a class designed to hold tally results, i.e., multisets.",
     PARA,
     "Operations:",
     MENU {
	  (TO (symbol **,Tally,Tally), " -- Cartesian product"),
	  (TO (symbol +,Tally,Tally), "  -- sum"),
	  (TO (symbol -,Tally,Tally), "  -- difference"),
	  (TO (symbol ?,Tally,Tally), "  -- comparison"),
	  (TO (symbol _,Tally,Thing), "  -- access"),
	  (TO "tally", "                -- tally the elements of a list"),
	  (TO "toList", "               -- a list of the elements"),
	  (TO (sum,Tally), "           -- add the elements"),
	  (TO (product,Tally), "       -- multiply the elements")
	  }
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
	  (TO (symbol +,Set,Set), " -- union"),
	  (TO (symbol ++,Set, Set), " -- disjoint union"),
	  (TO (symbol -,Set, Set), "  -- difference"),
	  (TO (symbol *,Set,Set), " -- intersection"),
	  (TO (symbol **, Set, Set), " -- Cartesian product"),
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

document { (symbol -, Set, Set),
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

document { (symbol ++, Set, Set),
     TT "x ++ y", " -- the disjoint union of two sets.",
     PARA,
     EXAMPLE "set {a,b,c} ++ set {b,c,d}",
     SEEALSO {"Set", "++"}
     }

document { (symbol *, Set, Set),
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
     HEADLINE "the class of all handles for engine objects",
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
