--		Copyright 1994 by Daniel R. Grayson

<< "--loading documentation files..." << endl

document { length,
     TT "length C", " -- returns the length of a graded module or a chain
     complex.",
     PARA,
     MENU {
	  TO (length,ChainComplex)
	  }
     }

document { sendgg,
     TT "sendgg s", " -- uses ", TO "sendToEngine", " to send the  string ", TT "s", " 
     of data and commands to the engine.  The first byte of the result is examined 
     for an error indication, and then an error is raised or the remainder of the 
     string is returned.",
     PARA,
     SEEALSO "engine communication protocol"
     }

document { parent,
     TT "parent X", " -- yields the parent P of X.",
     PARA,
     "Methods for the ", TO {"instance", "s"}, " of X which are not found
     in X itself are sought in P, and its parent, and so on.",
     PARA,
     "The mathematical notion of a set z and a subset y can modeled
     in this way, with z being the parent of y.",
     PARA,
     "Things which don't have instances have the empty class, called
     ", TO "Nothing", " as their parent.",
     PARA,
     "The of Thing is Thing itself (reflecting the fact that there can
     be no larger class).",
     SEEALSO "classes"
     }

document { Array,
     TT "Array", " -- the class of all arrays.",
     PARA,
     "An array is like a list, except that brackets are used instead of
     braces when entering or displaying an array, and arrays can't be used
     as vectors.  Their main use is notational: for example, they appear
     in the construction of polynomial rings.",
     EXAMPLE {
	  "ZZ[a,b,c]"
	  }
     }

document { Sequence,
     TT "Sequence", " -- the class of all sequences.",
     PARA,
     SEEALSO "sequences"
     }

document { singleton,
     TT "singleton x", " -- returns a sequence of length one whose single 
     element is ", TT "x", ".",
     PARA,
     EXAMPLE {
	  "singleton 3",
	  "#oo",
	  },
     SEEALSO "Sequence"
     }

document { List,
     TT "List", " -- the class of all lists.",
     PARA,
     "Creating new lists or sequences:",
     MENU {
	  TO "..",
	  TO (quote :, ZZ, Thing),	  -- was ":"
	  TO "toList",
	  TO "newClass",
	  TO "sequence",
	  TO "singleton",
	  },
     "Selecting elements of lists:",
     MENU {
	  TO (quote _, List, ZZ),
	  TO "#",
	  TO "first",
	  TO "last"
	  },
     "Manipulating lists:",
     MENU {
	  TO "accumulate",
 	  TO "append",
	  TO "between",
	  TO "copy",
	  TO "deepSplice",
	  TO "delete",
	  TO "drop",
     	  TO "join",
	  TO "mingle",
 	  (TO "pack", "       -- pack a list into a table"),
	  TO "prepend",
	  TO "reverse",
	  TO "rsort",
	  TO "sort",
	  TO "splice",
	  TO "take",
	  TO "unique",
	  TO "toSequence"
	  },
     "Examining lists:",
     MENU {
	  TO "all",
	  TO "any",
	  {TO "#", " -- length of a list"},
	  TO "max",
	  TO "maxPosition",
	  TO "member",
	  TO "min",
	  TO "minPosition",
	  TO "position",
	  TO "same"
	  },
     "Combining lists:",
     MENU {
	  TO (quote +,List,List),
	  TO "demark",
	  TO "fold",
	  TO "mergePairs"
	  },
     "Mapping functions:",
     MENU {
	  (TO "apply", "      -- apply function to entries in list or hash table"),
 	  (TO "applyTable", " -- apply a function to entries in a table"),
	  TO "number",
	  TO "product",
	  TO "scan",
	  TO "select",
 	  (TO "subtable", "   -- extract a subtable"),
	  TO "sum",
 	  (TO "table", "      -- make a table")
	  }
     }

document { Type,
     TT "Type", " -- the class of all types.",
     PARA, 
     "A type is a hash table intended to contain methods for 
     its instances.",
     PARA,
     SEEALSO {"parent",  "class", "using methods"}
     }

document { Print,
     TT "Print", " -- a method applied at top level to print the result, 
     ", TT "r", " of an evaluation.",
     PARA,
     "The code for the default Print method will apply the ", TO "AfterEval", "
     method to ", TT "r", " if there one, and replace ", TT "r", " by the result.  
     Then it applies ", TO "BeforePrint", " method, if there is one,
     to ", TT "r", ", and prints its result instead.  The actual printing
     will be done with ", TO "<<", ".  It will then apply the appropriate
     ", TO "AfterPrint", " method to ", TT "r", ", which is normally used to 
     provide auxiliary information to the user about the result.",
     SEEALSO "NoPrint"
     }

document { NoPrint,
     TT "NoPrint", " -- a method applied at top level to a result suppression of
     whose printing has been indicated by a semicolon.",
     PARA,
     "The code for the default NoPrint method will apply the ", TO "AfterEval", "
     method to ", TT "r", " if there one, and replace ", TT "r", " by the result.  
     It will then apply the appropriate ", TO "AfterNoPrint", " method to
     ", TT "r", ", which is normally used to provide auxiliary information to 
     the user about the result.",
     SEEALSO "Print"
     }

document { BeforePrint,
     TT "BeforePrint", " -- a method applied at top level to the result of an evaluation,
     whose result supplants the original for printing.",
     SEEALSO "Print"
     }

document { AfterEval,
     TT "AfterEval", " -- a method applied at top level to the result of an evaluatino,
     whose result replaces the original for storing in the output variable and for
     printing.",
     SEEALSO "Print"
     }

document { AfterPrint,
     TT "AfterPrint", " -- a method applied at top level to the result of an evalution
     after printing.",
     SEEALSO "Print"
     }

document { AfterNoPrint,
     TT "AfterNoPrint", " -- a method applied at top level to the result of an 
     evalution when printing of the result has been suppressed by a semicolon.",
     SEEALSO "Print"
     }

document { setrecursionlimit,
     TT "setrecursionlimit n", " -- sets the recursion limit to n.",
     PARA,
     " It returns the old value.  The recursion limit governs the nesting level
     permissible for calls to functions."
     }

document { "commandLine",
     TT "commandLine", " -- a constant whose value is the list of arguments 
     passed to the interpreter, including argument 0, the name of the program.",
     }

document { "environment",
     TT "environment", " -- a constant whose value is the list containing the
     environment strings for the process."
     }

document { Function,
     TT "Function", " -- the class of all functions.",
     PARA,
     SEEALSO "functions"
     }

document { "->",
     TT "x -> e", " -- denotes a function.  When the function is called, the initial 
     	      value of the variable x is the argument if there is just one, or
	      else is the sequence of arguments.",
     BR,NOINDENT,
     TT "(x) -> e", " -- denotes a function of one argument.  When the function is 
     applied to an expression w three things may happen:",
     UL {
     	  "if w is not a sequence, then the initial value of x is w;",
     	  "if w is a sequence of length one, then the initial value
     	  of x is the unique element of w; or",
     	  "if w is a sequence of length other than one, then it
     	  is an error."
	  },
     BR,NOINDENT,
     TT "(x,y) -> e", " -- denotes a function of two arguments.",
     PARA,
     "Similarly for more arguments.",
     PARA,
     "These operations create what is usually called a ", ITALIC "closure", ",
     which signifies that the function remembers the values of local
     variables in effect at the time of its creation, can change
     those values, and share the changes with other functions created
     at the same time.",
     PARA,
     EXAMPLE {
	  "f = x -> 2*x+1",
	  "f 100"
	  },
     PARA,
     "The class of all functions is ", TO "Function", "."
     }

document { "path",
     TT "path", " -- a list of strings containing names of directories in which\n", 
     TO "load", " and ", TO "input", " should seek files."
     }

document { HashTable,
     TT "HashTable", " -- the class of all hash tables.",
     PARA,
     "A hash table consists of: a class type, a parent type, and a
     set of key-value pairs.  The keys and values can be anything.
     The access functions below accept a key and return the
     corresponding value.  For details of the mechanism
     underlying this, see ", TO "hashing", ".",
     PARA,
     "One important feature of hash tables that when the keys
     are consecutive integers starting at 0, the keys are scanned
     in the natural order.",
     PARA,
     "There is a subclass of HashTable called ", TO "MutableHashTable", "
     which consists of those hash tables whose entries can be changed.",
     PARA,
     "Access functions:",
     MENU {
 	  TO "#",
 	  TO ".",
 	  TO "pairs",
 	  TO "keys",
 	  TO "values"
 	  },
     "Query functions:",
     MENU {
 	  TO "#?",
 	  TO ".?",
	  TO "mutable"
 	  },
     "Structural functions:",
     MENU {
 	  TO "copy",
	  TO "remove"
 	  },
     "Other functions:",
     MENU {
 	  TO "applyKeys",
 	  TO "applyPairs",
 	  TO "combine",
	  TO "hashTable",
 	  TO "merge",
     	  TO "new",
	  TO (NewFromMethod, HashTable, List),
	  TO "scanKeys",
 	  TO "scanPairs",
	  TO "scanValues",
	  TO "select"
 	  },
     "Examining hash tables:",
     MENU {
	  TO "browse",
	  TO "peek",
	  TO "peek2",
	  },
     "Types of hash tables:",
     MENU {
	  TO "MutableHashTable",
	  TO Set,
	  TO Tally
	  }
     }

document { maxPosition,
     TT "maxPosition x", " -- yields the position of the largest element in the list.",
     PARA,
     "If it occurs more than once, then the first occurrence
     is used.  If x has length zero an error results."
     }

document { minPosition,
     TT "minPosition x", " -- yields the position of the smallest element in the list.",
     PARA,
     "If it occurs more than once, then the first occurrence
     is used.  If x has length zero an error results."
     }

document { keys,
     TT "keys t", " -- yields a list of the keys occurring in the hash table t.",
     PARA,
     EXAMPLE {
	  "x = new HashTable from {a => 1, b => 2}",
	  "keys x",
	  }
     }

document { values,
     TT "values t", " -- yields a list of the values occurring in the hash table t.",
     PARA,
     EXAMPLE {
	  "x = new HashTable from {a => 1, b => 2}",
	  "values x",
	  }
     }

document { splice,
     TT "splice v", " -- yields a new list v where any members of v which are sequences
     are replaced by their elements.",
     PARA,
     "Works also for sequences, and leaves other expressions unchanged.
     Copying the list v is always done when v is mutable.
     Certain functions always splice their arguments or their argument
     lists for the sake of convenience.",
     EXAMPLE {
	  "splice ((a,b),c,(d,(e,f)))",
      	  "splice [(a,b),c,(d,(e,f))]",
	  },
     SEEALSO "deepSplice"
     }

document { deepSplice,
     TT "deepSplice v", " -- yields a new list v where any members of v 
     which are sequences are replaced by their elements, and so on.",
     PARA,
     "Works also for sequences, and leaves other expressions unchanged.
     Copying the list v is always done when v is mutable.",
     EXAMPLE "deepSplice { (a,b,(c,d,(e,f))), g, h }",
     SEEALSO "splice"
     }

document { ",",
     TT "x,y,...,z", " -- the comma is used to separate elements of a list or
     sequence.",
     PARA,
     EXAMPLE {
	  "a,b,c",
	  "{a,b,c}"
	  }
     }

document { apply,
     TT "apply(v,f)", " -- applies the function ", TT "f", " to each element of the 
     list ", TT "v", ", returning the list of results. If ", TT "v", " is 
     a sequence, then a sequence is returned.",
     PARA,
     EXAMPLE {
	  "apply(1 .. 5, i->i^2)",
      	  "apply({1,3,5,7}, i->i^2)",
	  },
     PARA,
     NOINDENT,
     TT "apply(v,w,f)", " -- produces, from lists or 
     sequences ", TT "v", " and ", TT "w", ",
     a list ", TT "z", " in which the i-th element ", TT "w_i", " is obtained
     by evaluating ", TT "f(v_i,w_i)", ".  If ", TT "v", " and ", TT "w", " are
     lists of the same class, then the result is also of
     that class.  If ", TT "v", " and ", TT "w", " are sequences, then so
     is the result.",
     PARA,
     EXAMPLE {
	  "apply(1 .. 5, a .. e, identity)",
      	  "apply({1,3,5,7}, i->i^2)",
	  },
     PARA,
     NOINDENT,
     TT "apply(n,f)", " -- equivalent to apply(list (0 .. n-1),f), for an integer n.",
     PARA,
     SEEALSO{ "scan", "select",  "any",  "all", "member"},
     PARA,
     NOINDENT,
     TT "apply(x,f)", " -- produces a new hash table ", TT "y", " from
     an hash table ", TT "x", " by applying the function
     ", TT "f", " to each of the values of ", TT "x", ".  This means that
     if ", TT "x#k === v", " then ", TT "y#k === f(v)", ".",
     SEEALSO {(quote /,List, Function), (quote \, Function, List)}
     }

document { scan,
     TT "scan(v,f)", " -- applies the function ", TT "f", " to each element of the 
     list ", TT "v", ".  The function values are discarded.",
     PARA,
     EXAMPLE "scan({a,4,\"George\",2^100}, print)",
     NOINDENT,
     TT "scan(n,f)", " -- equivalent to scan(0 .. n-1, f), for an integer n.",
     EXAMPLE "scan(3,print)",
     SEEALSO { "select", "any", "all", "member"}
     }

document { scanPairs,
     TT "scanPairs(x,f)", " -- applies the function ", TT "f", " to each
     pair ", TT "(k,v)", " where ", TT "k", " is a key in the hash 
     table ", TT "x", " and ", TT "v", " is the corresponding 
     value ", TT "x#k", ".",
     PARA,
     "This function requires an immutable hash table.  To scan the pairs in
     a mutable hash table, use ", TT "scan(pairs x, f)", ".",
     PARA,
     SEEALSO "scan"
     }

document { select,
     TT "select(v,f)", " -- select elements of the list or hash table
     ", TT "v", " which yield ", TT "true", " when the function 
     ", TT "f", " is applied.",
     BR,NOINDENT,
     TT "select(n,v,f)", " -- select at most ", TT "n", " elements of the
     list or hash table ", TT "v", " which yield ", TT "true", " when
     the function ", TT "f", " is applied.",
     PARA,
     "For a list, the order of the elements in the result will be the
     same as in the original list ", TT "v", ".",
     PARA,
     "For a hash table, the function is applied to each value.  This may
     change, for perhaps it should be applied to the key/value pair.  The
     hash table should be immutable: to scan the values in a mutable hash
     table, use ", TT "scan(values x, f)", ".",
     PARA,
     EXAMPLE {
	  "select({1,2,3,4,5}, odd)",
      	  "select(2,{1,2,3,4,5}, odd)",
	  },
     PARA,
     SEEALSO{ "scan", "apply", "any", "all", "member", "mutable"}
     }

--document { quote find,
--     TT "find(x,f)", " -- applies the function ", TT "f", " to each element
--     of ", TT "x", ", returning the result not equal to ", TT "null", ".
--     If no result is non-null, then it returns null."
--     }

document { any,
     TT "any(v,f)", " -- yields the value true or false depending on 
     whether any element ", TT "v#i", " of ", TT "v", " yields the value true 
     when the predicate ", TT "f", " is applied.",
     PARA,
     "Works when v is a list, sequence, or hash table, but when v is an
     hash table, f is applied to each pair (k,x) consisting of a key k
     and a value x from v.",
     PARA,
     SEEALSO{ "scan", "apply", "select", "all", "member"}
     }

document { describe,
     TT "describe x", " -- returns a string containing the real
     name of ", TT "x", ", bypassing the feature which causes certian
     types of things to acquire the names of global variables to which
     they are assigned.",
     PARA,
     EXAMPLE {
	  "R = ZZ[a,b,c_1,c_2];",
      	  "R",
      	  "describe R",
	  },
     PARA,
     "Currently, this function works by temporarily removing the value
     stored under ", TT "name", " from the hash table ", TT "x", ",
     which therefore must be mutable.",
     PARA,
     SEEALSO "toString"
     }

document { input,
     TT "input \"f\"", " -- reads and executes the commands found in the 
     file named f, echoing the input, printing the values, and incrementing
     the line number.",
     PARA,
     "The file is sought along the ", TO "path", ", unless the name of the
     file begins with '/' or './' or '../' .",
     PARA,
     SEEALSO{ "path", "needs", "load"}
     }

document { load,
     TT "load \"f\"", " -- reads and executes Macaulay 2 expressions found
     in the file named ", TT "f", ".",
     PARA,
     "The file is sought along the ", TO "path", ", unless the name of the
     file begins with the character(s) in ", TO "pathSeparator", ".  The
     file is read without echoing the input, printing the values, or
     incrementing the line number.",
     PARA,
     SEEALSO{ "path", "needs", "input"}
     }

document { needs,
     TT "needs \"f\"", " -- loads the file named ", TT "f", " if it hasn't 
     been loaded yet.",
     PARA,
     SEEALSO "load"
     }

document { plus,
     TT "plus(x,y,...)", " -- yields the sum of its arguments.",
     PARA,
     "If the arguments are strings, they are concatenated.  If there
     are no arguments, the answer is the integer 0."
     }

document { times,
     TT "times(x,y,...)", " -- yields the product of its arguments.",
     PARA,
     "If there are no arguments, the value is the integer 1."
     }

document { power,
     TT "power(x,n)", " -- yields the n-th power of ", TT "x", ".",
     PARA,
     SEEALSO "^"
     }

document { difference, 
     TT "difference(x,y)", " -- returns ", TT "x-y", "." 
     }

document { minus,
     TT "minus(x)   ", " -- yields ", TT "-x", ".",
     PARA,
     "minus(x,y)  -- yields x-y, but see also ", TO "difference", "."
     }

document { append,
     TT "append(v,x)", " -- yields the list obtained by appending ", TT "x", " to the 
     list ", TT "v", ".  Similarly if ", TT "v", " is a sequence.",
     PARA,
     EXAMPLE "append( {a,b,c}, x )",
     PARA,
     SEEALSO{ "prepend", "join"}
     }

document { prepend,
     TT "prepend(x,v)", " -- yields the list obtained by prepending x to the 
     list ", TT "v", ".  Similarly if ", TT "v", " is a sequence.",
     PARA,
     EXAMPLE "prepend( x, {a,b,c} )",
     PARA,
     SEEALSO{ "append", "join"}
     }

document { "--",
     TT "--", " introduces a comment in the text of a program.  The comment runs from
     the double hyphen to the end of the line."
     }

document { ascii,
     TT "ascii s", " -- convert a string to a list of ascii codes.", BR,
     NOINDENT,
     TT "ascii v", " -- convert a list of ascii codes to a string.",
     PARA,
     EXAMPLE {///ascii "abcdef"///, ///ascii oo///},
     SEEALSO{ "String" }
     }

document { transnet,
     TT "transnet v", " -- takes a list ", TT "v", " of integers, and assembles the bytes of the
     integers, four at a time, in network order (high order byte
     first), into a string.",
     BR,
     BR,
     NOINDENT,
     TT "transnet s", " -- takes a string ", TT "s", " whose length is a multiple 
     of 4, and assembles its bytes four at a time into integers, returning the list 
     of assembled integers.",
     PARA,
     EXAMPLE { "transnet {1,2,3}", "transnet oo"},
     SEEALSO{ "String" }
     }

document { " ",
     TT "f x", " -- yields the result of applying the function f to x.",
     PARA,
     "The user may install ", TO {"binary method", "s"}, " for this operator 
     with code such as ",
     PRE "         X Y := (x,y) -> ...",
     "where ", TT "X", " is the class of ", TT "x", " and ", TT "Y", " is the
     class of ", TT "y", "."
     }

document { "*",
     TT "x * y", " -- yields the product of x and y.",
     BR,NOINDENT,
     TT "* x", " -- unary operator available to the user.",
     PARA,
     "The user may install ", TO {"binary method", "s"}, " for this operator with code
     such as ",
     PRE "         X * Y := (x,y) -> ...",
     "where ", TT "X", " is the class of ", TT "x", " and ", TT "Y", " is the
     class of ", TT "y", ".",
     PARA,
     "A unary method for this operator may be installed with code such as ", 
     PRE "          * X := x -> ... ",
     "Here are some of the methods installed.",
     MENU {
	  TO (quote *, Set, Set)
	  },
     SEEALSO{ "times", "product" }
     }

document { "&",
     TT "x & y", " -- a binary operator.",
     PARA,
     "The user may install ", TO {"binary method", "s"}, " for this operator 
     with code such as ",
     PRE "         X & Y := (x,y) -> ...",
     "where ", TT "X", " is the class of ", TT "x", " and ", TT "Y", " is the
     class of ", TT "y", ".",
     PARA,
     "See also:",
     MENU {
	  TO (quote &,ZZ,ZZ)
	  }
     }

document { (quote &, ZZ, ZZ),
     TT "m & n", " -- produce an integer obtained from the bits of the 
     integers ", TT "m", " and ", TT "n", " by logical 'and'."
     }

document { "&&",
     TT "x && y", " -- a binary operator.",
     PARA,
     "The user may install ", TO {"binary method", "s"}, " for this operator 
     with code such as ",
     PRE "         X && Y := (x,y) -> ...",
     "where ", TT "X", " is the class of ", TT "x", " and ", TT "Y", " is the
     class of ", TT "y", "."
     }

-- document { quote ::,
--      TT "x :: y", " -- a binary operator.",
--      PARA,
--      "The user may install ", TO {"binary method", "s"}, " for this operator 
--      with code such as ",
--      PRE "         X :: Y := (x,y) -> ...",
--      "where ", TT "X", " is the class of ", TT "x", " and ", TT "Y", " is the
--      class of ", TT "y", "."
--      }

document { "^^",
     TT "x ^^ y", " -- a binary operator.",
     PARA,
     "The user may install ", TO {"binary method", "s"}, " for this operator 
     with code such as ",
     PRE "         X ^^ Y := (x,y) -> ...",
     "where ", TT "X", " is the class of ", TT "x", " and ", TT "Y", " is the
     class of ", TT "y", "."
     }

document { "+",
     TT "x + y", " -- a binary operator used for addition in many situations
     and union of sets.",
     PARA,
     "The user may install ", TO {"binary method", "s"}, " for this operator 
     with code such as ",
     PRE "         X + Y := (x,y) -> ...",
     "where ", TT "X", " is the class of ", TT "x", " and ", TT "Y", " is the
     class of ", TT "y", ".",
     PARA,
     MENU {
	  TO (quote +, Set, Set)
	  },
     SEEALSO{ "plus", "sum" }
     }

document { (quote +, Set, Set),
     TT "s + t", " -- union of two sets",
     PARA,
     SEEALSO "+"
     }

document { "-",
     TT "x - y", " -- a binary operator used for subtraction in many situations
     and set difference.",
     BR,NOINDENT,
     TT "- y", "   -- a unary operator used for negation.",
     PARA,
     "The user may install ", TO {"binary method", "s"}, " for this operator 
     with code such as ",
     PRE "         X - Y := (x,y) -> ...",
     "where ", TT "X", " is the class of ", TT "x", " and ", TT "Y", " is the
     class of ", TT "y", ".",
     PARA,
     "The user may install a method for this unary operator with code
     such as ",
     PRE "          - Y := y -> ...",
     "where ", TT "Y", " is the class of ", TT "y", ".",
     SEEALSO{ "difference", "minus" }
     }

document { "/",
     TT "x / y", " -- a binary operator.",
     PARA,
     "The user may install ", TO {"binary method", "s"}, " for this operator 
     with code such as ",
     PRE "         X / Y := (x,y) -> ...",
     "where ", TT "X", " is the class of ", TT "x", " and ", TT "Y", " is the
     class of ", TT "y", ".",
     MENU {
	  TO (quote /, Ring, Ideal),
	  TO (quote /, Module, Module),
	  TO (quote /, Module, Ideal),
	  TO (quote /, Ideal, Ideal),
	  TO (quote /, List, Function)
	  }
     }

document { "%",
     TT "x % y", " -- a binary operator used for remainder and reduction.",
     PARA,
     "The user may install ", TO {"binary method", "s"}, " for this operator 
     with code such as ",
     PRE "         X % Y := (x,y) -> ...",
     "where ", TT "X", " is the class of ", TT "x", " and ", TT "Y", " is the
     class of ", TT "y", "."
     }

document { "//",
     TT "x // y", " -- a binary operator used for quotients (with a possible
     remainder).",
     PARA,
     "The user may install ", TO {"binary method", "s"}, " for this operator 
     with code such as ",
     PRE "         X // Y := (x,y) -> ...",
     "where ", TT "X", " is the class of ", TT "x", " and ", TT "Y", " is the
     class of ", TT "y", "."
     }

document { "\\\\",
     TT "x \\ y", " -- a binary operator used for quotients (with a possible
     remainder).",
     PARA,
     "The user may install ", TO {"binary method", "s"}, " for this operator 
     with code such as ",
     PRE "         X \\ Y := (x,y) -> ...",
     "where ", TT "X", " is the class of ", TT "x", " and ", TT "Y", " is the
     class of ", TT "y", "."
     }

document { "^",
     TT "x ^ y", " -- a binary operator used for powers and raising nets.",
     PARA,
     "The user may install ", TO {"binary method", "s"}, " for this operator 
     with code such as ",
     PRE "         X ^ Y := (x,y) -> ...",
     "where ", TT "X", " is the class of ", TT "x", " and ", TT "Y", " is the
     class of ", TT "y", ".",
     PARA,
     "Here are some methods for computing powers:",
     MENU {
	  TO "SimplePowerMethod",
	  TO "BinaryPowerMethod"
	  },
     PARA,
     "If n is 0, then the unit element ", TT "(class x)#1", " is returned.
     If n is negative, then the method named ", TO "InverseMethod", "
     will be called."
     }

document { "/^",
     TT "x/^  y", " -- a binary operator, used for divided powers.",
     PARA,
     "The user may install ", TO {"binary method", "s"}, " for this operator 
     with code such as ",
     PRE "         X /^ Y := (x,y) -> ...",
     "where ", TT "X", " is the class of ", TT "x", " and ", TT "Y", " is the
     class of ", TT "y", ".",
     MENU {
	  TO (quote /^, Thing, ZZ)
	  }
     }

document { (quote /^, Thing, ZZ),
     TT "x /^ n", " -- computes the n-th divided power of x.",
     PARA,
     "This is implemented naively as ", TT "x^n/n!", ".",
     PARA,
     EXAMPLE {
	  "ZZ/101[x];",
      	  "x/^3"
	  },
     }

document { substring,
     TT "substring(s,i,n)", " -- yields the substring of the string s starting at 
     position i with length n.",
     PARA,
     "substring(s,i)   -- yields the substring of s starting at position i and
     continuing to the end of s.",
     PARA,
     "Positions are numbered starting at 0.",
     PARA,
     "Requests for character positions out of bounds are 
     silently ignored.",
     PARA,
     SEEALSO{ "String" }
     }

document { reverse,
     TT "reverse v", " -- yields a list containing the elements of the 
     list ", TT "v", " in reverse order.",
     PARA,
     EXAMPLE "reverse {a,b,c,d}"
     }

document { read,
     TT "read f", "  -- yields a string obtained by reading bytes from the input file
     ", TT "f", ".",BR,
     NOINDENT, 
     TT "read ()", " -- reads from ", TT "stdio", ", getting input from the user.",BR,
     NOINDENT, 
     TT "read s", "  -- reads from ", TT "stdio", ", getting input from the user, prompting
     with the string ", TT "s", ".",BR,
     PARA,
     "Input files are buffered, so the current contents of the buffer are returned
     if the buffer is not empty, otherwise reading from the file is attempted first.",
     SEEALSO {"get", "File"}
     }

document { get,
     TT "get \"f\"", " -- yields a string containing the contents of the file whose name
     is f.",
     PARA,
     NOINDENT,
     TT "get \"!f\"", " -- yields a string containing the output from the shell
     command \"f\".",
     PARA,
     NOINDENT,
     TT "get \"$hostname:service\"", " -- yields a string containing the
     input from the socket obtained by connecting to the specified host at
     the port appropriate for the specified service.  Warning: if the process
     providing the service expects interaction, it will not get it, and this
     command will hang.  This feature is not available on Sun computers,
     because Sun doesn't provide static versions of crucial libraries dealing
     with network communications.",
     PARA,
     NOINDENT,
     TT "get f", " -- yields a string containing the rest of the input from the 
     file f.",
     PARA,
     EXAMPLE {
	  ///"junk" << "hi there" << endl << close///,
      	  ///get "junk"///,
     	  if version#"operating system" =!= "SunOS"
     	  and version#"operating system" =!= "Windows-95-98-NT"
      	  then ///get "!date"///
	  },
     SEEALSO{ "File", "String", "read" }
     }

document { lines,
     TT "lines s", " -- yields an array of strings obtained from the
     string ", TT "s", " by breaking it at newline or return characters.",
     BR,NOINDENT,
     TT "lines(s,nl)", " -- yields an array of strings obtained from the 
     string ", TT "s", " by breaking it at the newline characters
     specified by the string ", TT "nl", ".",
     PARA,
     "The form ", TT "lines s", " is designed to break lines correctly
     when the file follows the Unix, MS-DOS, or Macintosh convention and
     contains no extraneous isolated newline or return characters.  In
     other words, it will break a line at \"\\r\\n\", \"\\n\", or \"\\r\".",
     PARA,
     "The string ", TT "nl", " should be a string of length 1 or 2.",
     SEEALSO "newline"
     }

document { "!",
     "n ! -- computes n factorial, 1*2*3*...*n."
     }

document { "not",
     TT "not x", " -- yields the negation of x, which must be true or false.",
     SEEALSO{ "and", "or" }
     }

document { "|",
     TT "x | y", " -- a binary operator.",
     PARA,
     "The user may install ", TO {"binary method", "s"}, " for this operator 
     with code such as ",
     PRE "         X | Y := (x,y) -> ...",
     "where ", TT "X", " is the class of ", TT "x", " and ", TT "Y", " is the
     class of ", TT "y", ".",
     PARA,
     MENU {
	  TO {(quote |, List, List), " -- join two lists"},
	  TO {(quote |, String, String), " -- concatenate two strings or nets horizontally"},
	  TO {(quote |, ZZ, ZZ), " -- logical OR of two integers"},
	  TO {(quote |, Matrix, Matrix), " -- join two matrices horizontally"}
	  },
     SEEALSO "||"
     }
document { (quote |, List, List),
     TT "v|w", " -- join two lists.", 
     PARA,
     EXAMPLE "{1,2,3}|{4,5,6}",
     SEEALSO "|"
     }
document { (quote |, String, String),
     TT "s|t", " -- concatenates strings or nets horizontally.", 
     PARA,
     "The result is a string if the arguments are all strings, otherwise it
     is a net.  The baselines of the nets are aligned.",
     EXAMPLE {
	  ///"abc" | "def"///,
      	  ///x = "abc" || "ABC"///,
      	  ///x|"x"|x///,
	  },
     PARA,
     "If one of the two arguments is an integer, it is converted to a string first.",
     EXAMPLE ///"t = " | 333///,      
     SEEALSO {"|", "horizontalJoin", "Net"}
     }
document { (quote |, ZZ, ZZ),
     TT "m|n", " -- produce an integer obtained from the bits of the 
     integers ", TT "m", " and ", TT "n", " by logical 'or'.",
     PARA,
     EXAMPLE "5 | 12",
     SEEALSO "|"
     }
document { (quote |, Matrix, Matrix),
     TT "f|g", " -- concatenate matrices horizontally.",
     PARA,
     "It is assumed that ", TT "f", " and ", TT "g", " both have the same target.",
     EXAMPLE {
	  "R = ZZ/101[x,y,z]",
      	  "f = matrix {{x,0,0},{0,y,0},{0,0,z}}",
      	  "f|f|f",
	  },
     "If one of the arguments is ring element or an integer, then it
     will be multiplied by a suitable identity matrix.",
     PARA,
     EXAMPLE "2|f|3",
     SEEALSO {"|", (quote ||, Matrix, Matrix)}
     }

document { "||",
     TT "x || y", " -- a binary operator.",
     PARA,
     "The user may install ", TO {"binary method", "s"}, " for this operator 
     with code such as ",
     PRE "         X || Y := (x,y) -> ...",
     "where ", TT "X", " is the class of ", TT "x", " and ", TT "Y", " is the
     class of ", TT "y", ".",
     PARA,
     MENU {
	  TO (quote ||, Net, Net),
	  TO (quote ||, Matrix, Matrix)
	  }
     }

document { (quote ||, Net, Net),
     TT "m||n", " -- joins nets or strings by concatenating
     them vertically.  The baseline of the result is the baseline of the
     first one.",
     PARA,
     "In this example, we build a large net with arrows to indicate
     the location of the baseline.",
     EXAMPLE {
	  ///x = "x" | "3"^1///,
      	  ///"<--- " | ( x || "" || x ) | " --->"///,
	  },
     SEEALSO {"||", "|", "Net", "stack"}
     }
document { (quote ||, Matrix, Matrix),
     TT "f||g", " -- yields the matrix obtained from matrices ", TT "f", " and ", TT "g", " by
     concatenating the columns.",
     PARA,
     EXAMPLE {
	  "R = ZZ[a..h];",
      	  "p = matrix {{a,b},{c,d}}",
      	  "q = matrix {{e,f},{g,h}}",
      	  "p || q",
	  },
     "If one of the arguments is ring element or an integer, then it
     will be multiplied by a suitable identity matrix.",
     EXAMPLE "p || 33",
     PARA,
     SEEALSO{"||", (quote ||, Matrix, Matrix)}
     }

document { "===",
     TT "x === y", " -- returns true or false depending on whether the 
     expressions x and y are strictly equal.",
     PARA,
     "Strictly equal expressions have the same type, so ", TT "0===0.", " and
     ", TT "0===0/1", " are false; the three types involved here are ", TO
     "ZZ", ", ", TO "RR", ", and ", TO "QQ", ".",
     PARA,
     "If x and y are ", TO "mutable", " then they are strictly equal only
     if they are identical (i.e., at the same address in memory).  For
     details about why strict equality cannot depend on the contents of
     mutable hash tables, see ", TO "hashing", ".  On the other hand, if x
     and y are non-mutable, then they are strictly equal if and only if
     all their contents are strictly equal.",
     EXAMPLE "{1,2,3} === {1,2,3}",
     EXAMPLE "{1,2,3} === {2,1,3}",
     "In the current implementation, matrices are mutable objects, so ", TT "===", "
     will yield false more often than you might expect.  We hope to change this
     in the future.",
     EXAMPLE "matrix {{2}} === matrix {{2}}",
     EXAMPLE "matrix {{2}} == matrix {{2}}",
     EXAMPLE "matrix {{2}} == matrix {{3}}",
     PARA,
     SEEALSO{ "==",  "=!=" }
     }

document { "=!=",
     TT "x =!= y", " -- returns true or false depending on whether the expressions
     x and y are strictly unequal.",
     PARA,
     "See ", TO "===", " for details."
     }

document { "==",
     TT "x == y", " -- a binary operator for testing mathematical equality.",
     PARA,
     "The user may install ", TO {"binary method", "s"}, " for this operator 
     with code such as ",
     PRE "         X == Y := (x,y) -> ...",
     "where ", TT "X", " is the class of ", TT "x", " and ", TT "Y", " is the
     class of ", TT "y", ".  This code will not be consulted if ", TT "x===y", "
     is true.",
     PARA,
     "A test for mathematical equality will typically involve doing a computation
     to see whether two representations of the same mathematical object are being
     compared.  For example, an ideal in a ring is represented by giving its
     generators, and checking whether two sets of generators produce the same
     ideal involves a computation with Groebner bases.",
     PARA,
     "It may happen that for certain types of objects, there is no method installed
     for testing mathematical equality, in which case an error will be given.  If
     you wanted to test strict equality, use the operator ", TO "===", " or 
     ", TO "=!=", ".",
     PARA,
     "Warning: this comparison operator returning true is not necesarily related to
     the comparison operator ", TO "?", " returning ", TT "quote ==", ".",
     PARA,
     SEEALSO{ "!=" }
     }

document { "!=",
     TT "x != y", " -- the negation of ", TT "x == y", ".",
     PARA,
     SEEALSO{ "==" }
     }

document { "**", 
     TT "x ** y", " -- a binary operator used for tensor product and
     cartesian product.",
     PARA,
     "The user may install ", TO {"binary method", "s"}, " for this operator 
     with code such as ",
     PRE "         X ** Y := (x,y) -> ...",
     "where ", TT "X", " is the class of ", TT "x", " and ", TT "Y", " is the
     class of ", TT "y", "."
     }

document { (quote **, Set, Set),
     TT "X ** Y", " -- form the Cartesian product of two sets.",
     PARA,
     "Its elements are the sequences (x,y), where x is an element
     of X, and y is an element of Y.",
     PARA,
     EXAMPLE "set {1,2} ** set {a,b,c}",
     PARA,
     SEEALSO { "**", "Set" }
     }

document { set,
     TT "set v", " -- yields the set whose elements are the members of the list v.",
     PARA,
     SEEALSO { "Set" }
     }

document { random,
     TT "random n", " -- for n an integer, yields a random integer in the range 0 .. n-1.",
     BR,
     NOINDENT, 
     TT "random x", " -- for real x, yields a random real number in the range 0 .. x.",
     BR,
     NOINDENT, 
     TT "random R", " -- yields a random element of the ring R.",
     BR,
     NOINDENT, 
     TT "random(n,R)", " -- yields a random homogeneous element of degree n 
     in the ring R, where n is an integer or a list of integers.",
     BR,
     NOINDENT, 
     TT "random(F,G)", " -- yields a random graded, degree 0, map from the free
     module G to the free module F.",
     PARA,
     "Warning: doesn't correctly handle the case when n an integer is larger
     than 2^31-1.",
     EXAMPLE {
	  "tally apply(100, i -> random 10)",
	  },
     EXAMPLE {
	  "R = ZZ/101[t];",
      	  "sum(7,i->random 101 * t^i)",
	  },
     EXAMPLE {
	  "R = ZZ/101[x,y];",
      	  "random(R^{1,2,3},R^{1,2,3})"
	  },
     }

document { true,
     PARA,
     "true -- a value indicating truth.",
     SEEALSO{"false", "Boolean"}
     }

document { false,
     PARA,
     "false -- a value indicating falsity.",
     SEEALSO{"true", "Boolean"}
     }
