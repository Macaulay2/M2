--		Copyright 1993-1998 by Daniel R. Grayson

document {
     Key => "shield",
     Headline => "shield evaluation from interrupts",
     TT "shield x", " -- executes the expression ", TT "x", ", temporarily
     ignoring interrupts."
     }

document {
     Key => symbol lineNumber,
     Headline => "current line number",
     TT "lineNumber", " -- the current line number.",
     BR{},
     TT "lineNumber = n", " -- sets the line number to ", TT "n", ".",
     }

document {
     Key => FilePosition,
     Headline => "the class of all file positions",
     TT "FilePosition", " -- a type of list designed to represent a position
     in a file.",
     PARA{},
     "It's implemented as a list whose three elements are the file name,
     the line number, and the column number."
     }

document {
     Key => flagLookup,
     Headline => "flag a symbol",
     TT "flagLookup x", " -- arranges for each subsequent reference to a
     symbol x to be flagged with a warning message."
     }

document {
     Key => instance,
     Headline => "whether something has a certain type",
     TT "instance(x,X)", " -- tells whether ", TT "x", " is an instance
     of the type ", TT "X", ".",
     PARA{},
     "We say that x is an instance of X if X is the class of x, or a parent
     of the class of x, or a grandparent, and so on.",
     PARA{},
     SeeAlso => { "classes and types", "class", "parent" }
     }

document {
     Key => alarm,
     Headline => "set an alarm",
     Usage => "alarm n",
     Inputs => {
	  "n" => ZZ,
	  },
     Consequences => {
	  { "the alarm will be sounded after ", TT "n", " seconds; it can be intercepted with ", TO "try" }
	  },
     "If ", TT "n", " is zero, then no alarm is scheduled, and any previously scheduled alarm is cancelled.
     Any pending alarm will be cancelled when any other error occurs, or when the top level loop offers an input prompt to the user.",
     PARA{},
     "The value returned is the number of seconds remaining until any previously scheduled alarm was due to be delivered, or
     zero if there was no previously scheduled alarm.",
     PARA{},
     "This command may interfere with ", TO "time", " or ", TO "sleep", " on some systems."
     }

document {
     Key => symbol "++",
     Headline => "a binary operator, usually used for direct sum"
     }

document {
     Key => symbol "(*)",
     Headline => "a unary postfix operator, used for indicating a graded object"
     }

document {
     Key => symbol "<==>",
     Headline => "a binary operator"
     }

document {
     Key => symbol ",",
     Headline => "the comma, used for separating entries in a list or sequence"
     }

document {
     Key => symbol "==>",
     Headline => "a binary operator"
     }

document {
     Key => symbol "|-",
     Headline => "a binary operator"
     }

document {
     Key => symbol "===>",
     Headline => "a binary operator"
     }

document {
     Key => symbol "@@",
     Headline => "a binary operator"
     }

document {
     Key => (symbol @@, Function, Function),
     Headline => "composition of functions",
     Usage => "f @@ g",
     Inputs => { "f", "g" },
     Outputs => {{ "the composite function of ", TT "f", " and ", TT "g", "." }},
     EXAMPLE {
	  "f = i -> i+1",
	  "g = i -> i^2",
	  "apply(0 .. 10, f @@ g)",
	  "apply(0 .. 10, g @@ f)"
	  }
     }

document {
     Key => symbol "@",
     Headline => "a binary operator",
     "This operator is right associative."
     }

document {
     Key => symbol "\\",
     Headline => "a binary operator"
     }

document {
     Key => Number,
     Headline => "the class of all numbers"
     }

document {
     Key => {(symbol "/",List,Thing)},
     Headline => "vector division",
     Usage => "v/c",
     Inputs => {"v" => "to be treated as a vector", "c" => "a number or scalar ring element"},
     Outputs => {{ "the quotient vector; every element of ", TT "v", " is divided by ", TT "c" }},
     EXAMPLE "{1,2,3,4} / 3"     
     }

document {
     Key => { (symbol "/",VisibleList,Function),
	  (symbol "/",List,Function),
	  (symbol "\\",Function,VisibleList),
	  (symbol "\\",Function,Tally),
	  (symbol "\\",SelfInitializingType,VisibleList),
	  (symbol "\\",Command,VisibleList),
	  (symbol "\\",RingMap,List),
	  (symbol "\\",Command,Tally),
	  (symbol "/",List,SelfInitializingType),
	  (symbol "/",VisibleList,SelfInitializingType),
	  (symbol "/",List,Command),
	  (symbol "/",Tally,Command),
	  (symbol "/",Tally,Function),
	  (symbol "/",List,RingMap),
	  (symbol "/",VisibleList,Command)
	  },
     Headline => "apply a function to elements of a list",
     Usage => "x/f\nf\\x",
     Inputs => { "x" => Nothing => {ofClass{VisibleList,List,Sequence,Array,Tally,Set}}, "f" => Nothing => {ofClass{Function,Command,SelfInitializingType,RingMap}} },
     Outputs => {{ "the list, tally, or set obtained by applying ", TT "f", " to each element of ", TT "x", "; it has the same type as ", TT "x", " has" }},
     PARA {
	  "The function ", TT "apply", " does the same thing."
	  },
     PARA {
     	  "The operator ", TO "/", " is left associative, which means that ", TT "w / f / g", " is interpreted as ", TT "(w / f) / g", ".
     	  The operator ", TO "\\", " is right associative, so ", TT ///g \ f \ w///, " is interpreted as ", TT ///g \ (f \ w)///, ".
	  Both operators have parsing precedence lower than that of ", TO "@@", ", which means that the previous two expressions are equivalent to ", TT "w / g @@ f", "
	  and ", TT "g @@ f \\ w", ", respectively. See ", TO "precedence of operators", "."
	  },
     EXAMPLE lines ///
     	  f = x -> x+1
	  g = x -> 2*x
     	  g \ (1 .. 10)
     	  (1 .. 10) / g
     	  f \ g \ (1 .. 10)
     	  f @@ g \ (1 .. 10)
	  set (1 .. 10)
	  g \ oo
	  R = QQ[x];
	  f = map(R,R,{x^2})
	  f \ {x,x^2,x^3,x^4}
     ///,
     SourceCode => {(symbol "/",VisibleList,Function)},
     }

document {
     Key => {(symbol "//",Thing,Function),(symbol "\\\\",Function,Thing),
	  (symbol "//",Thing,Command),(symbol "\\\\",Command,Thing),
	  (symbol "//",Thing,SelfInitializingType),(symbol "\\\\",SelfInitializingType,Thing)
	  },
     Headline => "apply a function",
     Usage => "x // f\nf \\\\ x",
     Inputs => { "x", "f" => Nothing => {ofClass{Function,Command,SelfInitializingType}}},
     Outputs => {{ "the result of applying ", TT "f", " to ", TT "x", ", i.e., ", TT "f x" }},
     SeeAlso => {(symbol "/",VisibleList,Function)},
     PARA {
	  "The parsing precedence of the operators ", TT "//", " and ", TT "\\\\", " is rather low, which makes
	  them useful for avoiding parentheses.  See ", TO "precedence of operators", "."
	  },
     EXAMPLE lines ///
     	  toList \\ sin \ ( 1 .. 5 )
	  (x -> (x,x)) \ (a,b,c,d)
	  splice \\ (x -> (x,x)) \ (a,b,c,d)
     ///
     }

document {
     Key => hash,
     Headline => "hash code of an object",
     TT "hash x", " -- returns the hash code of ", TT "x", ".",
     PARA{},
     "The hash code of ", TT "x", " is an integer produced in a deterministic way
     from ", TT "x", ", and perhaps from the hash codes of the contents of ", TT "x", ".
     See ", TO "hashing", " for a discussion of the requirements that
     the hash codes used here are designed to satisfy."
     }

document {
     Key => remove,
     Headline => "remove an entry from a hash table",
     TT "remove(x,k)", " -- removes the entry stored in the hash table ", TT "x", "
     under the key ", TT "k", ".",
     PARA{},
     EXAMPLE {
	  "x = new MutableHashTable from {a => 1, b => 2}",
	  "remove(x,a)",
	  "x"
	  }
     }

document {
     Key => Boolean,
     Headline => "the class of Boolean values",
     "Predicate functions return these as values, and the logical connectives 
     expect to receive them as arguments.",
     PARA{},
     "Special operators dealing with truth values.",
     UL {
	  TO "not",
	  TO "and",
	  TO "or",
	  TO "if"
	  }
     }

document {
     Key => Symbol,
     Headline => "the class of all symbols",
     "Symbols are entered as an alphabetic character followed by a
     sequence of alphanumeric characters; case is significant.
     The single symbol character ' is regarded as alphabetic, so that
     symbols such as ", TT "x'", " may be used.",
     PARA{},
     "Symbols are used as names for values to be preserved, as indeterminates
     in polynomial rings, and as keys in hash tables.  They may have
     global scope, meaning they are visible from every line of code,
     or local scope, with visibility restricted to a single file or
     function body.",
     EXAMPLE {
	  "x",
	  "ab12"
	  },
     SeeAlso => {":="}
     }

document {
     Key => Keyword,
     Headline => "the class of all keywords",
     "Keywords are symbols that are treated specially by the system while parsing user input.  Some of them,
     such as ", TO "and", ", consist of alphanumeric characters and look just like
     ordinary symbols.  Others, such as ", TO "==>", ", consist of special characters
     and are called operators."
     }

document {
     Key => File,
     Headline => "the class of all files",
     "Files may be input files, output files, pipes, or sockets.
     A list of currently open files may be obtained with ", TO "openFiles", "."
     }

document {
     Key => connectionCount,
     Headline => "the number of connections",
     TT "connectionCount f", " -- returns the number of connections accepted by 
     the listener ", TT "f", " so far."
     }

document {
     Key => echoOn,
     Headline => "turn on echoing",
     TT "echoOn f", " -- turns on echoing for the file ", TT "f", "."
     }

document {
     Key => echoOff,
     Headline => "turn off echoing",
     TT "echoOff f", " -- turns off echoing for the file ", TT "f", "."
     }

document {
     Key => printString,
     Headline => "lowlevel function to print a string, net, or symbol",
     TT "printString(o,s)", " -- send the string ", TT "s", " to the output file ", TT "o", ".",
     PARA{},
     "This function is intended for internal use only.",
     PARA{},
     "The argument ", TT "s", " may also be a sequence or list, in which case
     its elements are printed.  If an integer is encountered, then
     it specifies a number of spaces to be printed.  If a symbol
     or indeterminate is encountered, its name is printed.  If ", TO "null", "
     is encountered, nothing is printed.",
     PARA{},
     EXAMPLE ///printString(stdio, (a,10,"b",20,c))///
     }

document {
     Key => mutable,
     Headline => "whether something may be modified",
     TT "mutable x", " -- returns true or false, depending on whether x is mutable.",
     PARA{},
     "If ", TT "x", " is a hash table, list, or database, then it's mutable if its contents
     can be destructively altered.",
     PARA{},
     "If ", TT "x", " is a symbol, then it's mutable if a value can be assigned to
     it. (See ", TO "protect", ".)",
     PARA{},
     "If ", TT "x", " is anything else, then it isn't mutable.",
     PARA{},
     "The contents of a mutable hash table do not participate in strong comparison
     with ", TO "===", " or in ", TO "hashing", ".",
     SeeAlso => {"MutableList", "MutableHashTable"}
     }

document {
     Key => setEcho,
     Headline => "turn on echoing",
     TT "setEcho stdio", " -- turn on echoing of characters typed to the standard
     input."
     }

document {
     Key => clearEcho,
     Headline => "turn off echoing",
     TT "clearEcho stdio", " -- turn off echoing of characters typed to the standard
     input."
     }

document {
     Key => CacheTable,
     Headline => "hash tables for caching",
     "A type of mutable hash table designed for caching computed values that
     could always be recomputed.  Cache tables are designed so their contents
     will not participate in any comparisons by the strict comparison
     operator ", TT "===", ".  To that end, any two cache tables with the same
     class and parent are considered equal to each other and have hash code equal to 0."
     }

document {
     Key => "using functions with optional inputs",
     "Some functions accept optional inputs in addition to their required inputs.  In the documentation,
     such an optional input is indicated by writing ", TT "NAME => ...", ", where ", TT "NAME", " is the
     name of the optional input, and the dots indicate the place where the user will provide the
     value of the optional input.",
     PARA{},
     "Optional inputs can be provided between parentheses along with the
     other inputs (or arguments) of the function.  For example, if the function is normally used with two
     required inputs, then instead of typing ", TT "f(x,y)", ", you may type 
     ", TT "f(x,y,FOO => t, BAR => u)", ", where ", TT "t", " is the value to be provided to ", TT "f", " as
     the value of the optional input named ", TT "FOO", " and ", TT "u", " is the value to be
     provided to ", TT "f", " as the value of the optional input named ", TT "BAR", ".",
     PARA{},
     "The optional inputs can be inserted
     in any order, and may even occur before the required inputs.  If more than one optional input with the same
     option name are given, then the value accompanying the right-most one is the value provided to the function.",
     PARA{},
     "Use ", TO2{ (options,Function), "options" }, " to discover the optional arguments accepted by a function.",
     SUBSECTION "Examples",
     EXAMPLE {
     	  "R = ZZ/101[x,y,z,w];",
     	  "gb ideal(x*y-z^2,y^2-w^2)",
	  "gens oo"
	  },
     "One of the optional arguments for ", TO "gb", "
     is named ", TO "DegreeLimit", "; we may use ", TO2{ (options,Function), "options" }, " to discover that,
     as follows.",
     EXAMPLE {
	  "options gb"
	  },
     "The optional input named ", TO "DegreeLimit", " can be used to specify that the computation should stop after a certain degree has been reached.",
     EXAMPLE {
	  "gb(ideal(x*y-z^2,y^2-w^2), DegreeLimit => 2)",
	  "gens oo",
	  },
     SUBSECTION "Programming hint",
     "The value returned by ", TO2{ (options,Function), "options" }, " is a type of hash table that can be used to obtain default values.",
     EXAMPLE {
	  "(options gb).Syzygies"
	  }
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
