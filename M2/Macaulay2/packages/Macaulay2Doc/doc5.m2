--		Copyright 1993-2002 by Daniel R. Grayson

document {
     Key => override,
     Headline => "override default values for optional arguments",
     TT "override(defaults,args)", " overrides default values for
     optional arguments present in the argument sequence ", TT "args", ".",
     PARA{
	  "One possibility is for the argument ", TT "defaults", " to be an immutable hash table 
	  (of type ", TO "OptionTable", "), and ", TT "args", " should be
     	  a sequence of arguments, some of which are optional arguments of
     	  the form ", TT "x => v", ".  Each such optional argument
     	  is removed from ", TT "args", ", and the value in ", TT "defaults", "
     	  corresponding to the key ", TT "x", " is replaced by ", TT "v", ".
     	  The value returned is the modified pair ", TT "(defaults, args)", ".
	  An error is signalled if the key ", TT "x", " does not occur in ", TT "defaults", "."
	  },
     PARA {
	  "A second possibility is for the argument ", TT "defaults", " to be ", TO "true", ",
	  in which case the keys x are not checked for validity, and no default values
	  are provided.  The main use of this is to separate the optional arguments from
	  the other arguments, which can then be used for dispatching to the correct method."
	  },
     PARA{
	  "This function is intended for internal use only, and is used in the processing
     	  of optional arguments for method functions that accept them."
	  },
     EXAMPLE {
	  "defs = new HashTable from { a => 1, b => 2 };",
	  "override(defs, (4,b=>6,5))"
	  }
     }

document {
     Key => userSymbols,
     Headline => "a list of the user's symbols",
	Usage => "userSymbols ()",
	Outputs => {List => {" a list of symbols"}},
     TT "userSymbols ()", " provides a list of symbols defined by
     the user.",
     BR{},
     TT "userSymbols X", " limits the list to those symbols whose
     values are instances of the ", TO "class", " ", TT "X", ".",
     PARA{},
     "Protected symbols are excluded from the list.",
     SeeAlso => "listUserSymbols"
     }

document {
     Key => listUserSymbols,
     Headline => "display the user's symbols",
     SYNOPSIS {
     	  Usage => "listUserSymbols",
     	  Outputs => {
	       {"a display of the symbols defined and given values by the user, along with their types and values, in abbreviated form"}
	       },
	  PARA {
	       "A symbol is considered to have been give a value, if it's current value is not equal to itself."
	       },
	  EXAMPLE lines ///
	  t=3;
	  R=QQ[x];
	  listUserSymbols
	  ///
	  },
     SYNOPSIS {
     	  Usage => "listUserSymbols X",
	  Inputs => {
	       "X" => Type
	       },
     	  Outputs => {
	       {"a display of the symbols of type ", TT "X", " defined and given values by the user,
		    along with their types and values, in abbreviated form"}
	       },
	  EXAMPLE lines ///
	  listUserSymbols ZZ
	  ///
	  },
     SeeAlso => {"userSymbols"}
     }

document {
     Key => clearOutput,
     Headline => "forget output values",
	Usage => "clearOutput",
     TT "clearOutput", " is a command that attempts to release memory by 
     clearing the values retained by the output line symbols.",
     SeeAlso => { "clearAll" }
     }

document {
     Key => clearAll,
     Headline => "forget everything",
	Usage => "clearAll",
     TT "clearAll", " is a command that attempts to release memory by clearing 
     the values retained by the output line symbols and all the user symbols.",
     SeeAlso => {"userSymbols", "clearOutput"}
     }

document {
     Key => exec,
     Headline => "execute another program",
	Usage => "exec argv",
     TT "exec argv", "  uses the 'exec' operating system call to
     start up another program, replacing the current Macaulay2 process.
     Here ", TT "argv", " is a string, or a sequence or list of strings
     to be passed as arguments to the new process.  The first string
     is the name of the executable file."
     }

document {
     Key => restart,
     Headline => "restart Macaulay2",
     Usage => "restart",
     Consequences => { {"the program will be restarted from the beginning"} },
     PARA{
     	  "Functions previously registered with ", TO "addEndFunction", " will
     	  be called before the current instance of the program terminates.  Then the program will be invoked
	  afresh, as described in ", TO "Invoking the program", "."
	  }
     }

document {
     Key => {on,[on,CallLimit],[on,Name],[on,GenerateAssertions],GenerateAssertions,CallLimit},
     Headline => "trace a function each time it's run",
     Usage => "on f",
     Inputs => { 
	  "f" => Function,
	  CallLimit => ZZ => {"the maximum number of times to permit the function ", TT "f", " to be called"},
	  Name => String => {"the name to use for the function, in case ", TT "f", " is an anonymous function (not assigned to a global variable)"},
     	  GenerateAssertions => Boolean => {
	       "whether to print assertion statements that can be used as input to Macaulay2 to
	       check the behavior of the function remains the same.  Arguments and values are prepared
	       with ", TO "toExternalString", ", failure of which is sliently ignored."
	       }
	  },
     Outputs => { Function => {"a new function that returns the same values that ", TT "f", " would have returned, but has a few side effects
	       useful for debugging: upon entry, it prints its arguments, and upon exit it prints its return values.  The display includes the name of ", TT "f", ",
	       a sequence number in parentheses that tells how many times the function has been called, and a number in brackets that gives the nesting (recursion) depth.
	       The sequence number allows the entry and exit reports to be connected." 
	       }},
     PARA{
     	  "Ideally, this function would replace ", TT "f", ", i.e., we would write ", TT "f = on f", ".  Unfortunately, all the pre-installed system functions
	  are write-protected; fortunately, their methods are not, and can be replaced."
	  },
     EXAMPLE lines ///
     ker Matrix := on(lookup(ker,Matrix),GenerateAssertions=>true,Name=>"ker");
     f = x -> kernel (x|x);
     R = QQ[a..c];
     f vars R
     ///,
     SeeAlso => {"lookup"}
     }

document {
     Key => assert,
     Headline => "assert something is true",
	   Usage => "assert x",
     TT "assert x", " prints an error message if x isn't true.",
     EXAMPLE lines ///
     assert( (2+2) === 4 )
     ///,
     SeeAlso => {"generateAssertions"}
     }

document {
     Key => notImplemented,
     Headline => "print an 'not implemented' error message",
	Usage => "notImplemented()",
     TT "notImplemented()", " prints an error message that 
     says \"not implemented yet\"."
     }

document {
     Key => "errorDepth",
     Headline => "set the error printing depth",
     TT "errorDepth = i", " -- sets the error depth to ", TT "i", ", which should be
     a small integer, returning the old value.",
     PARA{
	  "During the backtrace after an error message, a position in interpreted
	  code is displayed and the debugger is entered only if the load depth was at least as large at the
	  time the code was parsed as the error depth is now.
	  The default value is 3, which shows only positions in the user's code and positions
	  inside loaded packages whose debugging mode is true.  Set it to 2 to also debug statements
	  inside loaded packages, except for the package ", TO "Core", ".  Set it to 1 to also
	  debug statements in the core, and set it to 0 to debug statements in the bootstrap code."
	  },
     SeeAlso => { "loadDepth" }
     }

document {
     Key => "loadDepth",
     Headline => "the load depth",
     TT "loadDepth = i", " -- sets the load depth to ", TT "i", ", which should be
     a small integer, returning the old value.",
     PARA{
	  "During the backtrace after an error message, a position in interpreted
	  code is displayed only if the load depth at the
	  time the code was parsed is at least as large as the error depth is now.  The load depth 
	  is set to 0 initially, is set to 1 when the files of the ", TO "Core::Core", "
	  package are being loaded, is set to 2 while loading a package with the ", TO "debuggingMode", " option
	  set to ", TO "false", ", and is set to 3 while loading a package with the ", TO "debuggingMode", " option
	  set to ", TO "true", " and for user input."
	  },
     PARA {
	  "The value of ", TO "loadDepth", " active when code is parsed is referred to later when
	  error messages are being handled: see ", TO "errorDepth", ", and it is also displayed, in parentheses,
	  when the error message is printed."
	  },
     Caveat => { "The user should not attempt to adjust the value of ", TO "loadDepth", "." },
     }

document {
     Key => benchmark,
     Headline => "accurate timing of execution",
     Inputs => {
	     "s" => String => "a string containing Macaulay2 code"
	     },
     Outputs => {
	     RR => {"the number of seconds it takes to evaluate the code in ", TT "s"}
	     },
     Usage => "benchmark s",
     "Produces an accurate timing for the code contained in the string ", TT "s", ".  The value returned is the number of seconds.",
     EXAMPLE {
		///benchmark "sqrt 2p100000"///
		},
     "The snippet of code provided will be run enough times to register
     meaningfully on the clock, and the garbage collector will be called
     beforehand.",
     }

document {
     Key => {memoize,(memoize, Function),(memoize, Function, List)},
     Headline => "record results of function evaluation for future use",
     TT "memoize f", " -- produces, from a function ", TT "f", ", a new function that
     behaves the same as ", TT "f", ", but remembers previous answers to be provided
     the next time the same arguments are presented.",
     PARA{},
     EXAMPLE lines ///
     fib = n -> if n <= 1 then 1 else fib(n-1) + fib(n-2)
     time fib 28
     fib = memoize fib
     time fib 28
     time fib 28
     ///,
     PARA{},
     "The function ", TT "memoize", " operates by constructing 
     a ", TO "MutableHashTable", " in which the argument sequences are used
     as keys for accessing the return value of the function.",
     PARA{},
     "An optional second argument to memoize provides a list of initial values,
     each of the form ", TT "x => v", ", where ", TT "v", " is the value to
     be provided for the argument ", TT "x", ".",
     PARA{},
     "Warning: when the value returned by ", TT "f", " is ", TO "null", ", it will always be 
     recomputed, even if the same arguments are presented.",
     PARA{},
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

document {
     Key => Tally,
     Headline => "the class of all tally results",
     TT "Tally", " -- a class designed to hold tally results, i.e., multisets.",
     SeeAlso => {VirtualTally}
     }

document {
     Key => {VirtualTally,(symbol -,VirtualTally,VirtualTally)},
     "The only difference between this class and ", TO "Tally", " is that this class allows negative numbers.",
     EXAMPLE lines ///
     	  x = tally {a,b,b,c,c,c}
     	  y = tally {a,a,a,b,b,c}
     	  x' = new VirtualTally from x
     	  y' = new VirtualTally from y
	  x-y
	  x'-y'
     ///,
     SeeAlso => { BettiTally }
     }

document {
     Key => (symbol **, VirtualTally, VirtualTally),
     Headline => "Cartesian product of tallies",
     TT "x ** y", " -- produces the Cartesian product of two tallies.",
     PARA{},
     "One of the arguments may be a ", TO "Set", ".",
     PARA{},
     EXAMPLE {
	  "x = tally {a,a,b}",
      	  "y = tally {1,2,2,2}",
     	  "x ** y",
	  },
     SeeAlso => {"Tally", "tally"}
     }

document {
     Key => (symbol +, VirtualTally, VirtualTally),
     Headline => "union of tallies",
     TT "x + y", " -- produces the union of two tallies.",
     PARA{},
     "One of the arguments may be a ", TO "Set", ".",
     PARA{},
     EXAMPLE {
	      "x = tally {a,b,b,c,c,c,d,d,d}",
      	  "y = tally {a,a,a,b,b,c,d}",
     	  "x' = new VirtualTally from x",
	 	  "y' = new VirtualTally from y",
	 	  "z' = y' - x'",
	 	  "z' + x'",
	  	  "z' + y'",
	  },
     }
     
document {
     Key => (symbol -, VirtualTally),
     Headline => "negation of a VirtualTally",
     TT "-x", " -- the negation of ", TT "x",
     PARA{},
     EXAMPLE {
      	  "x = tally {a,b,b,c,c,d,d,d}",
	 	  "x' = new VirtualTally from x",
	  	  "- x'",
     },
}     



document {
     Key => (symbol +, Tally, Tally),
     Headline => "union of tallies",
     TT "x + y", " -- produces the union of two tallies.",
     PARA{},
     "One of the arguments may be a ", TO "Set", ".",
     PARA{},
     EXAMPLE {
	  "x = tally {a,a,a,b,b,c}",
      	  "y = tally {b,c,c,d,d,d}",
      	  "x + y",
	  },
     SeeAlso => {"Tally", "tally"}
     }

document {
     Key => (symbol -, Tally, Tally),
     Headline => "difference of tallies",
     Usage => "x - y",
     Inputs => { "x", "y" },
     Outputs => { { "the difference of the two tallies" } },
     "The count associated to an item ", TT "i", " in the result is the difference of the counts in
     ", TT "x", " and in ", TT "y", " if it's positive, otherwise, zero.",
     EXAMPLE "tally {a,a,b,c} - tally {c,d,d}",
     SeeAlso => "Tally"
     }

document {
     Key => tally,
     Headline => "tally the elements of a list or sequence",
     TT "tally x", " tallies the frequencies of items in a list or sequence x.",
     SeeAlso => "Tally"
     }

TEST ///
assert( toString tally {1,1,1,2,1,3,2} === "new Tally from {1 => 4, 2 => 2, 3 => 1}" )
assert( tally {1,1,1,2,1,3,2} === new Tally from {(1,4),(2,2),(3,1)} )
///

document {
	Key => (tally, VisibleList),
	Headline => "tally the elements of a list, sequence, or array",
	Usage => "y = tally x",
	Inputs => {
		"x" => {}
		},
	Outputs => {
		"y" => "a listing of tallied results"
		},
	"It produces an hash table (multiset) ", TT "y", " which tallies the
     frequencies of occurrences of items in the list ", TT "x", ", i.e.,
     ", TT "y_i", " is the number of times ", TT "i", " appears in ", TT "x", ", or is ", TT "0", " if
     ", TT "i", " doesn't appear in the list.",
     EXAMPLE {
	  "y = tally {1,2,3,a,b,1,2,a,1,2,{a,b},{a,b},a}",
       	  "y_2",
	  "y_5",
	  "y_{a,b}",
	  }
  }

document {
     Key => Set, 
     Headline => "the class of all sets",
     "Elements of sets may be any immutable object, such as integers, ring elements
     and lists of such.  Ideals may also be elements of sets.",
     EXAMPLE {
	  "A = set {1,2};",
	  "R = QQ[a..d];",
	  "B = set{a^2-b*c,b*d}"
	  },
     "Set operations, such as ", 
     TO2((member,Thing,Set),"membership"), ", ",     
     TO2((symbol+,Set,Set),"union"), ", ",
     TO2((symbol*,Set,Set),"intersection"), ", ",
     TO2((symbol-,Set,Set),"difference"), ", ",
     TO2((symbol**,Set,Set),"Cartesian product"), ", ",
     TO2((symbol^**,VirtualTally,ZZ),"Cartesian power"), ", and ",
     TO2((isSubset,Set,Set),"subset"),
     " are available. For example,",
     EXAMPLE {
	  "toList B",
	  "member(1,A)",
	  "member(-b*c+a^2,B)",
     	  "A ** A",
	  "A^**2",
	  "set{1,3,2} - set{1}",
	  "set{4,5} + set{5,6}",
	  "set{4,5} * set{5,6}",
	  "set{1,3,2} === set{1,2,3}"
	  },
     PARA{},
     TO2(Ideal,"Ideals"), " in Macaulay2 come equipped with a specific sequence of generators, so the following two ideals are not considered strictly equal,
     and thus the set containing them will appear to have two elements.",
     EXAMPLE {
     	  "I = ideal(a,b); J = ideal(b,a);",
	  "I == J",
	  "I === J",
	  "C = set(ideal(a,b),ideal(b,a))"
	  },
     "However, if you ", TO trim, " the ideals, then the generating sets will be
     the same, and so the set containing them will have one element.",
     EXAMPLE {
	  "C1 = set(trim ideal(a,b),trim ideal(b,a))"
	  },
     PARA{},
     "A set is implemented as a ", TO HashTable, ", whose keys are the elements of the
     set, and whose values are all 1.  In particular, this means that two objects 
     are considered the same exactly when they are strictly equal, according to ", TO symbol===, "."
     }

undocumented {
     (NewFromMethod, Set, List)
     }

document {
     Key => {set, (set,VisibleList)},
     Headline => "make a set",
     Usage => "set v\nset(v1,v2,...)",
     Inputs => {"v" => List},
     Outputs => {Set => " the set whose elements are the members of the list v"},
     EXAMPLE {
	  "v = {1,2,3,2,1}",
	  "S = set v",
	  "T = set(a,b,c,a,b,d)"
	  },
     SeeAlso => { Set }
     }

document { 
     Key => (symbol #?, Set, Thing),
     Headline => "test set membership",
     Usage => "x#?e",
     Inputs => {
	  "x",
	  "e"
	  },
     Outputs => {
	  Boolean => {"whether e is in the set x"}
	  },
     "This is identical to ", TT "member(e,x)", ".",
     EXAMPLE {
	  "x = set{1,2,3}",
	  "x#?2",
	  "member(2,x)"
	  },
     SeeAlso => {Set}
     }

document { 
     Key => {(symbol -, Set, Set),
	  (symbol -, Set, List),
	  (symbol -, List, Set)},
     Headline => "set difference",
     Usage => "x - y",
     Inputs => {
	  "x" => {" or ", ofClass List},
	  "y" => {" or ", ofClass List}
	  },
     Outputs => {
	  Set => {"or ", ofClass List, ", consisting of those elements of x not in y"}
	  },
     "At least one of ", TT "x", ", ", TT "y", " must be a set, and the other 
     may be a list.  If ", TT "x", " is a list, then
     so is the result.",
     EXAMPLE {
	  "set{a,b,c} - set{a}",
	  "set{a,b,c} - {a}",
	  "{a,b,c} - set{a}"
	  },
     SeeAlso => {Set}
     }

document { 
     Key => (symbol +, Set, Set),
     Headline => "set union",
     Usage => "x + y",
     Inputs => {
	  "x",
	  "y",
	  },
     Outputs => {
	  Set => {"the union of ", TT "x", " and ", TT "y"},
	  },
     EXAMPLE {
	  "set{a,b,c} + set{a,d,f}",
	  },
     PARA {
	  "The function ", TT "sum", " can be used to form the union of a list of sets, but this can be slow for long lists."
	  },
     EXAMPLE lines ///
     	  x = apply(3, i -> set apply(3, j -> 10*i+j))
	  sum x
     ///,
     SeeAlso => {Set}
     }

document {
     Key => (symbol *, Set, Set),
     Headline => "intersection of sets",
     Usage => "x * y",
     Inputs => {
	  "x",
	  "y"
	  },
     Outputs => {
	  {"the intersection of ", TT "x", " and ", TT "y"}
	  },
     EXAMPLE "set {1,2,3} * set {2,3,4}",
     SeeAlso => {Set}
     }

document {
     Key => (symbol ^**, VirtualTally, ZZ),
     Headline => "Cartesian power of sets and tallies",
     Usage => "B = A^**n",
     Inputs => { "A", "n" },
     Outputs => {"B" => { "the tally of ", TT "n", "-tuples of elements from ", TT "A" }},
     "If ", TT "A", " is ", ofClass Set, ", then so is ", TT "B", ".",
     EXAMPLE lines ///
     	  A = set {1,2}
	  A^**3
     	  A = tally {1,1,2}
	  A^**3
	  ///,
     SeeAlso => {Set, (symbol**,Set,Set)}
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


document {
     Key => (symbol ^**, Module, ZZ),
     Headline => "tensor power",
     Usage => "M^**i",
     Inputs => { "M", "i" },
     Outputs => {Module => { "the ", TT "i", "-th tensor power of ", TT "M"}},
     "The second symmetric power of the canonical module of the
     rational quartic:",
     EXAMPLE lines ///
         R = QQ[a..d];
         I = monomialCurveIdeal(R,{1,3,4})
	 M = Ext^1(I,R^{-4})
	 M^**2
	 ///
     }

document {
     Key => (symbol ^**, CoherentSheaf, ZZ),
     Headline => "tensor power",
     Usage => "M^**i",
     Inputs => {"M" , "i" },
     Outputs => {CoherentSheaf => { "the ", TT "i", "-th tensor power of ", TT "M"}},
     "The second symmetric power of the canonical sheaf of the
     rational quartic:",
     EXAMPLE lines ///
         R = QQ[a..d];
         I = monomialCurveIdeal(R,{1,3,4})
	 X = variety I
	 KX = sheaf(Ext^1(I,R^{-4}) ** ring X)
	 K2 = KX^**2
	 prune K2
	 ///,
     "Notice that the resulting sheaf is not always presented in the most
     economical manner.  Use ", TO prune, " to improve the presentation.",
     SeeAlso => {monomialCurveIdeal, Ext, variety, sheaf, prune}
     }    

document {
     Key => setRandomSeed,
     Headline => "set starting point for random number generator"
     }

document {
     Key => 1 : setRandomSeed,
     Usage => "setRandomSeed()",
     Consequences => {
	  {"Initializes the random number generator to a fixed state, identical to the
	       initial state (upon program start) in version 1.2 and earlier of Macaulay2.  (After version 1.2,
	       the random number seed is initially set (when Macaulay2 starts) to a number that depends on the current date, 
	       the time (in seconds), and the process id, except for when running examples and tests
	       in packages (as signalled by use of the command line option ", TT "--no-randomize", "), where it is always initialized to 0.)"}
	  },
     EXAMPLE lines ///
     setRandomSeed()
     random 2^100
     setRandomSeed()
     random 2^100
     ///,
     SeeAlso => { (setRandomSeed,ZZ), (setRandomSeed,String) }
     }

document {
     Key => (setRandomSeed, ZZ),
     Usage => "setRandomSeed i",
     Inputs => {"i"},
     Consequences => {
     	  {"Sets the random number seed to the low-order 32 bits of the integer ", TT "i", ".
     	  The sequence of future pseudo-random results is determined by the seed."}
	  },
     EXAMPLE {
	  "setRandomSeed 123456",
	  "for i to 10 list random 100",
	  "setRandomSeed 123456",
	  "for i to 10 list random 100"
	  },
     SeeAlso => { 1:setRandomSeed, (setRandomSeed,String) }
     }

document {
     Key => (setRandomSeed, String),
     Usage => ///setRandomSeed s///,
     Inputs => {"s"},
     Consequences => {
	  {"Sets the random number seed to an integer computed from ", TT "s", ".  Every character 
	  of the string contributes to the seed, but only 32 bits of data are used.
	  The sequence of future pseudo-random results is determined by the seed."}
	  },
     EXAMPLE {
	  ///setRandomSeed "thrkwjsxz"///,
	  ///for i to 10 list random 100///,
	  ///setRandomSeed "thrkwjsxz"///,
	  ///for i to 10 list random 100///
	  },
     SeeAlso => { 1:setRandomSeed, (setRandomSeed,ZZ) }
     }

document {
     Key => {truncateOutput,(truncateOutput, ZZ),(truncateOutput, InfiniteNumber)},
     Usage => "truncateOutput w",
     Inputs => {"w" => ZZ },
     Consequences => {{
	  "The maximum output line width is set to ", TT "w", ", which should be an integer or ", TO "infinity", ".  
	  This function works by assigning a value to ", TT "Thing#{Standard,BeforePrint}", ", which 
     	  may conflict with other ", TO "BeforePrint", " methods installed by the user, or those installed by the system that do line wrapping."
	  }}
     }


document {
     Key => "printWidth",
     Usage => "printWidth = n",
     Inputs => {
	  "n" => ZZ => "the width to use for wrapping printed output"
	  },
     Consequences => {
	  "The function ", TO "wrap", " will use ", TT "n", " as the window width when wrapping
	  certain types of output."
	  }
     }
	  
document {
     Headline => "make a new link to a file",
     Key => {(linkFile, String, String),linkFile},
     Usage => "linkFile(o,n)",
     Inputs => {
	  "o" => String => "the path to an existing file",
	  "n" => String => "a new path to the file"
	  },
     Consequences => {
	  {"a new link ", TT "n", " is made to the existing file reachable using the path ", TT "o"}
	  },
     SeeAlso => { "moveFile", "copyFile" }
     }

document {
     Key => symbol dictionaryPath,
     "The value of ", TO "dictionaryPath", " is the list of global dictionaries whose symbols are visible.",
     EXAMPLE { "dictionaryPath" },
     SeeAlso => { Dictionary }
     }

document {
     Key => Dictionary,
     Headline => "the class of all dictionaries",
     "A dictionary is a special sort of hash table whose keys are the strings, and whose values are the
     corresponding symbols.",
     EXAMPLE {
	  "Core.Dictionary # \"sin\"",
	  "Core.Dictionary #? \"sin\""
	  }
     }

document {
     Key => {dictionary,(dictionary, Keyword),(dictionary, Symbol),(dictionary, Thing)},
     Headline => "determine the dictionary to which a symbol belongs",
     Usage => "dictionary x",
     Inputs => {
	  "x" => Thing
	  },
     Outputs => {
	  { "the dictionary to which the symbol ", TT "x", " belongs"}
	  },
     "If ", TT "x", " is the value of a symbol recorded in the internal table used to recover
     global symbol from the values assigned to them, then that symbol is used."
     }

document {
     Key => removeFile,
     Headline => "remove a file",
     Usage => "removeFile f",
     Inputs => { "f" => String },
     Consequences => {{ "the file reachable by the path ", TT "f", " is removed" }},
     PARA {
	  "Under a unix system such as GNU/Linux, what really happens is that the link to the file 
	  specified by ", TT "f", " is removed.  The file itself disappears after all the links to 
	  it are removed.  See ", TO "linkFile", "."
	  }
     }

document {
     Key => wrap,
     Usage => "wrap(wid,sep,s)",
     Inputs => {
	  "wid" => ZZ,
	  "sep" => String,
	  "s" => String
	  },
     Outputs => {
	  { "a string obtained by wrapping the string ", TT "s", ", in case it is wider than the number ", TT "wid", ", so that it occupies multiple lines,
	       separated by lines filled with the single character in the string ", TT "sep", ", if provided"}
	  },
     "The inputs ", TT "wid", " and ", TT "sep", " are optional, and can be given in either order.  The default for ", TT "wid", " is ", TT "printWidth", ",
     and the default for ", TT "sep", " is null.",
     EXAMPLE {
	  ///wrap(10,"abcdefghijklmnopqrstuvwxyz")///,
	  ///wrap(10,"-","abcdefghijklmnopqrstuvwxyz")///
	  }
     }

document { Key => {(eagonNorthcott,Matrix),eagonNorthcott},
     Headline => "Eagon-Northcott complex of a matrix of linear forms",
     Usage => "eagonNorthcott f",
     Inputs => { "f" => "a matrix of linear forms" },
     Outputs => { "C" => {"the Eagon-Northcott complex of ", TT "f"} },
     "The Eagon-Northcott complex is an explicit chain complex that gives a minimal projective
     resolution of the cokernel of the matrix maximal minors of a generic matrix of linear forms.",
     EXAMPLE lines ///
     	  R = QQ[a..z]
	  f = genericMatrix(R,3,5)
	  M = coker gens minors_3 f
	  C = res M
	  D = eagonNorthcott f
	  H = prune HH D
	  assert( H_0 == M and H_1 == 0 and H_2 == 0 and H_3 == 0 )
     ///,
     "This function was written by Greg Smith."
     }

document { Key => {(selectVariables,List,PolynomialRing),selectVariables},
     Headline => "make a subring of a polynomial ring generated by selected variables",
     Usage => "(S,f) = selectVariables(v,R)",
     Inputs => {
	  "v" => {"a sorted list of numbers specifying which variables to select"},
	  "R"
	  },
     Outputs => {
	  "S" => PolynomialRing => {"a polynomial ring generated as a subring of R by the variables whose indices
	       occur in the list v, together with the induced monomial ordering"
	       },
	  "f" => RingMap => {"the inclusion map from S to R"}
	  },
     EXAMPLE lines ///
     (S,f) = selectVariables({2,4}, QQ[a..h,Weights=>1..8]);
     describe S
     options S
     f
     ///
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
