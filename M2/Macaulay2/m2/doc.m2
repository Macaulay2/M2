--		Copyright 1993-1999 by Daniel R. Grayson

stderr << "--loading documentation files..." << endl

document { length,
     Headline => "length"
     }

document { (length, GradedModule),
     Headline => "length of a graded module",
     "The length of a graded module is the difference between the largest and
     smallest indices of occupied spots.  Chain complexes are graded modules,
     so this function applies to them, too.",
     EXAMPLE {
	  "R = QQ[x..z];",
	  "C = res coker vars R",
	  "length C"
	  }
     }

document { sendgg,
     Headline => "send commands to engine",
     TT "sendgg s", " -- uses ", TO "sendToEngine", " to send the string ", TT "s", " 
     of data and commands to the engine.  The first byte of the result is examined 
     for an error indication, and then an error is raised or the remainder of the 
     string is returned.",
     PARA,
     SEEALSO "engine communication protocol"
     }

document { (parent,Thing),
     Headline => "parent type of an object",
     Synopsis => {
	  "P = parent X",
	  "X" => "anything",
	  "P" => { "the parent class of ", TT "X" }
	  },
     PARA,
     "Methods for the instances of ", TT "X", " which are not found
     in ", TT "X", " itself are sought in ", TT "P", ", and its parent, and so on.
     Thus the mathematical notion of a set ", TT "P", " and a subset ", TT "X", " 
     can modeled in this way.",
     PARA,
     "Things which don't have instances have the empty class, called
     ", TO "Nothing", " as their parent.  The parent of ", TO "Thing", "
     is ", TO "Thing", " itself (because every thing is a thing).",
     PARA,
     "The entire structure of types and their parents can be displayed
     with ", TO "showStructure", ".",
     SEEALSO "classes and types"
     }

document { Array,
     Headline => "the class of all arrays -- [...]",
     "An array is like a list, except that brackets are used instead of
     braces when entering or displaying an array, and arrays can't be used
     as vectors.  Their main use is notational: for example, they appear
     in the construction of polynomial rings.",
     EXAMPLE {
	  "v = [a,b,c]",
	  "v#2",
	  "ZZ[a,b,c]"
	  },
     SEEALSO "lists"
     }

document { Sequence,
     Headline => "the class of all sequences -- (...)",
     "A sequence is an ordered collection of things enclosed by parentheses
     and separated by commas.  Use ", TO "#", " to get the length of a
     sequence of to get one of the elements.",
     EXAMPLE {
	  "v = (a,b,c)",
	  "#v",
	  "v#2"
	  },
     SEEALSO "sequences"
     }

document { singleton,
     Headline => "make a sequence of length one",
     Synopsis => {
	  "y = singleton x",
	  "x" => null,
	  "y" => {"a sequence of length one whose single element is ", TT "x", "."},
	  },
     EXAMPLE {
	  "singleton 3",
	  "#oo",
	  },
     SEEALSO {"unSingleton"}
     }

document { List,
     Headline => "the class of all lists -- {...}",
     SEEALSO "lists",
     PARA,
     "Common operations on lists:",
     MENU {
	  TO "append",
	  TO "join",
	  TO "prepend",
	  },
     "Common ways to apply functions to elements of lists:",
     MENU {
	  TO (apply,BasicList,Function),
	  TO (scan,BasicList,Function),
	  },
     "Common ways to test elements of lists:",
     MENU {
	  TO (all,BasicList,Function),
	  TO (any,BasicList,Function),
	  },
     "Common ways to find things in lists:",
     MENU {
	  TO (position,VisibleList,Function),
	  TO (positions,VisibleList,Function),
	  TO (select,BasicList,Function),
	  TO (select,ZZ,BasicList,Function),
	  },
     "Common ways to extract elements from lists:",
     MENU {
	  TO "drop",
	  TO "take",
	  TO (symbol #, List, ZZ),
	  TO (symbol #?, List, ZZ),
	  TO (symbol _, List, ZZ),
	  TO (symbol _, VisibleList, List)
	  },
     }

document { VisibleList,
     Headline => "the class of all visible lists",
     "There are three types of lists that can be entered directly from
     the keyboard, as follows.",
     EXAMPLE {
	  "{a,b,c}",
	  "[a,b,c]",
	  "(a,b,c)",
	  },
     "We introduce the class of visible lists as a convenience for
     referring to lists of these types."
     }

document { Type,
     Headline => "the class of all types",
     "Everything in the system is classified, and the class that a thing
     belongs to is a type.  A type is implemented as a hash table containing
     method functions for its instances.",
     PARA,
     "The list of objects of class Type known to the system is displayed below."
     }

document { Print,
     Headline => "top level method for printing results",
     Synopsis => {
	  "X.Print = f",
	  "f" => { "a function to be used for printing a top-level evaluation
	       result ", TT "r", " of type ", TT "X", "."},
	  null
	  },
     "The code for the default ", TT "Print", " method will apply the ", TO "AfterEval", "
     method to ", TT "r", " if there one, and replace ", TT "r", " by the result.  
     Then it applies ", TO "BeforePrint", " method, if there is one,
     to ", TT "r", ", and prints its result instead.  The actual printing
     will be done with ", TO "<<", ".  It will then apply the appropriate
     ", TO "AfterPrint", " method to ", TT "r", ", which is normally used to 
     provide auxiliary information to the user about the type of the result."
     }

document { NoPrint,
     Headline => "top level method for non-printing results",
     Synopsis => {
	  "X.NoPrint = f",
	  "f" => { "a function to be applied if a top-level evaluation
	       result ", TT "r", " of type ", TT "X", " is not to
	       be printed, as indicated by a semicolon."},
     	  null
	  },
     "The code for the default ", TT "NoPrint", " method will apply
     the ", TO "AfterEval", " method to ", TT "r", " if there is
     one, and replace ", TT "r", " by the result.  It will then
     apply the appropriate ", TO "AfterNoPrint", " method to
     ", TT "r", ", which is normally used to provide auxiliary 
     information to the user about the type of the result."
     }

document { BeforePrint,
     Headline => "top level method applied before printing results",
     Synopsis => {
	  "X.BeforePrint = f",
	  "f" => { "a function to be applied before printing a 
	       top-level evaluation result ", TT "r", " of 
	       type ", TT "X", "."},
     	  null
	  },
     "The value returned supplants the original for printing."
     }

document { AfterEval,
     Headline => "top level method applied after evaluation",
     Synopsis => {
	  "X.AfterEval = f",
	  "f" => { "a function to be applied after evaluating a 
	       top-level evaluation result ", TT "r", " of 
	       type ", TT "X", "."},
     	  null
	  },
     "The value returned result replaces the original for
     storing in the output variables and for printing."
     }

document { AfterPrint,
     Headline => "top level method applied after printing",
     Synopsis => {
	  "X.AfterPrint = f",
	  "f" => { "a function to be applied after printing a 
	       top-level evaluation result ", TT "r", " of 
	       type ", TT "X", "."},
     	  null
	  },
     "This is used currently to print the type of the result of
     a computation.",
     EXAMPLE {
	  "3/4"
	  },
     "We could suppress that output for a single type as
     follows.",
     EXAMPLE {
	  "QQ.AfterPrint = r -> r;",
	  "3/4"
	  }
     }

document { AfterNoPrint,
     Headline => "top level method applied after not printing",
     Synopsis => {
	  "X.AfterNoPrint = f",
	  "f" => { "a function to be applied after not printing a 
	       top-level evaluation result ", TT "r", " of 
	       type ", TT "X", "."},
     	  null
	  },
     "The function ", TT "f", " will be applied at top level to the 
     result of an evalution when printing of the result has
     been suppressed by a semicolon."
     }

document { (setrecursionlimit,ZZ),
     Headline => "set the limit on recursion",
     Synopsis => {
	  "m = setrecursionlimit n",
	  "n" => "the desired limit on recursion depth",
	  "m" => "the previous limit"
	  },
     "Each time a function is called, the recursion depth is incremented by
     1, and each time a function returns, the recursion depth is decremented.
     This limit on recursion depth is a way to detect infinite loops."
     }

document { toString symbol commandLine,
     Headline => "the command line arguments",
     "A constant whose value is the list of arguments passed to the interpreter,
     including argument 0, the name of the program."
     }

document { toString symbol environment,
     Headline => "the environment variables",
     "A constant whose value is the list containing the
     environment strings for the process."
     }

document { Function,
     Headline => "the class of all functions",
     "Common ways to make a function:",
     SHIELD MENU {
	  TO "->"
	  },
     "Returning from functions:",
     MENU {
	  TO "return"
	  },
     SEEALSO "functions"
     }

document { "->",
     Headline => "make a function",
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
     Headline => "list of directories to look in",
     "A list of strings containing names of directories in
     which ", TO "load", " and ", TO "input", " should seek files.  These strings
     are simply concatenated with the filename being sought, so should include
     any necessary terminal slashes.",
     PARA,
     EXAMPLE {
	  "path",
	  ///path = append(path, getenv "HOME" | pathSeparator | "resolutions" | pathSeparator)///
	  }
     }

document { HashTable,
     Headline => "the class of all hash tables",
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
     SHIELD MENU {
 	  TO "#",
 	  TO "."
 	  },
     "Query functions:",
     SHIELD MENU {
 	  TO "#?",
 	  TO ".?"
 	  }
     }

document { maxPosition,
     Headline => "position of largest element",
     }

document { (maxPosition,BasicList),
     Synopsis => {
	  "n = maxPosition x",
	  "x" => null,
	  "n" => { "the position of the largest element in the list ", TT "x", "." },
	  },
     "If the largest element occurs more than once, then the first occurrence
     is used.  If ", TT "x", " has length 0 an error results.",
     SEEALSO {
	  (minPosition,BasicList)
	  }
     }

document { minPosition,
     Headline => "position of smallest element"
     }

document { (minPosition,BasicList),
     Synopsis => {
	  "n = minPosition x",
	  "x" => null,
	  "n" => { "the position of the smallest element in the list ", TT "x", "." },
	  },
     "If the smallest element occurs more than once, then the first occurrence
     is used.  If ", TT "x", " has length 0 an error results.",
     SEEALSO {
	  (maxPosition,BasicList)
	  }
     }

document { keys,
     Headline => "keys used in a hash table"
     }

document { (keys,HashTable),
     Synopsis => {
	  "x = keys t",
	  "t" => null,
	  "x" => {"a list of the keys occurring in the hash table ", TT "t", "."}
	  },
     EXAMPLE {
	  "x = new HashTable from {a => 1, b => 2}",
	  "keys x",
	  }
     }

document { values,
     Headline => "values in a hash table"
     }

document { (values,HashTable),
     Synopsis => {
	  "x = values t",
	  "t" => null,
	  "x" => {"a list of the values occurring in the hash table ", TT "t", "."}
	  },
     EXAMPLE {
	  "x = new HashTable from {a => 1, b => 2}",
	  "values x",
	  }
     }

document { splice,
     Headline => "remove subsequences",
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
     Headline => "remove subsequences",
     TT "deepSplice v", " -- yields a new list v where any members of v 
     which are sequences are replaced by their elements, and so on.",
     PARA,
     "Works also for sequences, and leaves other expressions unchanged.
     Copying the list v is always done when v is mutable.",
     EXAMPLE "deepSplice { (a,b,(c,d,(e,f))), g, h }",
     SEEALSO "splice"
     }

document { ",",
     Headline => "separator for elements of a visible list",
     TT "x,y,...,z", " -- the comma is used to separate elements of a list or
     sequence.",
     PARA,
     EXAMPLE {
	  "a,b,c",
	  "{a,b,c}"
	  }
     }

document { (apply,BasicList,BasicList,Function),
     Headline => "apply function to elements in pairs",
     TT "apply(v,w,f)", " -- produces, from lists ", TT "v", " and ", TT "w", ",
     a list ", TT "z", " in which the i-th element ", TT "w_i", " is obtained
     by evaluating ", TT "f(v_i,w_i)", ".  If ", TT "v", " and ", TT "w", " are
     lists of the same class, then the result is also of that class.",
     EXAMPLE "apply({1,2,3}, {100,200,300}, (i,j) -> i+j)"
     }

document { (apply,BasicList,Function),
     Headline => "apply function to each element",
     Synopsis => {
	  "r = apply(v,f)",
	  "v" => null,
	  "f" => null,
	  "r" => {"the list obtained by applying ", TT "f", " to each element of ", TT "v", "."}
	  },
     "The result ", TT "r", " will have the same class as ", TT "v", ".",
     EXAMPLE "apply([1,3,5,7], i->i^2)",
     SEEALSO {(symbol /,VisibleList, Function), (symbol \, Function, VisibleList)}
     }

document { (apply,HashTable,Function),
     Headline => "apply function to each value",
     TT "apply(x,f)", " -- produces a new hash table ", TT "y", " from
     an hash table ", TT "x", " by applying the function
     ", TT "f", " to each of the values of ", TT "x", ".  This means that
     if ", TT "x#k === v", " then ", TT "y#k === f(v)", "."
     }
document { (apply,ZZ,Function),
     Headline => "apply function to 0 .. n-1",
     TT "apply(n,f)", " -- applies the function ", TT "f", " to each integer
     in the range ", TT "0 .. n-1", " returning the sequence of results.
     This is equivalent to ", TT "apply( toList(0 .. n-1),f)", ".",
     EXAMPLE "apply(10, i -> i^2)"
     }
document { apply,
     Headline => "apply a function to each element",
     SEEALSO { "mapping over lists"}
     }

document { scan,
     Headline => "apply a function to each element",
     SEEALSO { "mapping over lists"}
     }

document { (scan,BasicList,Function),
     Headline => "apply a function to each element of a list",
     TT "scan(v,f)", " -- applies the function ", TT "f", " to each element of the 
     list ", TT "v", ".  The function values are discarded.",
     EXAMPLE "scan({a,4,\"George\",2^100}, print)"
     }

document { (scan,ZZ,Function),
     Headline => "apply a function to 0 .. n-1",
     TT "scan(n,f)", " -- applies the function ", TT "f", " to each integer
     in the range ", TT "0 .. n-1", " discarding the results.",
     PARA,
     "This is equivalent to ", TT "scan(0 .. n-1, f)", ".",
     EXAMPLE {
	  "scan(3,print)",
	  "v = {a,b,c}",
	  "scan(#v, i -> print(i,v#i))"
	  }
     }

document { scanPairs,
     Headline => "apply a function to pairs in a hash table" }

document { (scanPairs,HashTable,Function),
     Headline => "apply a function to pairs in a hash table",
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
     Headline => "select elements from a list or hash table",
     SEEALSO{ "scan", "apply", "any", "all", "member", "mutable"}
     }
document { (select,BasicList,Function),
     Headline => "select elements from a list",
     TT "select(v,f)", " -- select elements of the list
     ", TT "v", " that yield ", TT "true", " when the function 
     ", TT "f", " is applied.",
     PARA,
     "The order of the elements in the result will be the same as
     in the original list ", TT "v", ", and the class will be the same,
     too.",
     EXAMPLE {
	  "select({1,2,3,4,5}, odd)",
	  "select([1,2,3,4,5], odd)",
	  }
     }
document { (select,HashTable,Function),
     Headline => "select pairs from a hash table",
     TT "select(v,f)", " -- select pairs of the hash table ", TT "v", "
     that yield ", TT "true", " when the function ", TT "f", " is applied to
     the value.",
     PARA,
     "The hash table should be immutable: to scan the values in a mutable hash
     table, use ", TT "scan(values x, f)", "."
     }
document { (select,ZZ,BasicList,Function),
     Headline => "select a limited number of elements from a list",
     TT "select(n,v,f)", " -- select at most ", TT "n", " elements of the list
     ", TT "v", " that yield ", TT "true", " when the function 
     ", TT "f", " is applied.",
     PARA,
     "The order of the elements in the result will be the same as
     in the original list ", TT "v", ".",
     EXAMPLE "select(2,[1,2,3,4,5], odd)"
     }
document { (select,ZZ,HashTable,Function),
     Headline => "select a limited number of pairs from a hash table",
     TT "select(n,v,f)", " -- select at most ", TT "n", " pairs of the hash 
     table ", TT "v", " that yield ", TT "true", " when the function ", TT "f", " 
     is applied to the value.",
     PARA,
     "The hash table should be immutable: to scan the values in a mutable hash
     table, use ", TT "scan(values x, f)", "."
     }

--document { find,
--     TT "find(x,f)", " -- applies the function ", TT "f", " to each element
--     of ", TT "x", ", returning the result not equal to ", TO "null", ".
--     If no result is non-null, then it returns null."
--     }

document { any,
     Headline => "whether an element satisfies a condition",
     TT "any(v,f)", " -- yields the value true or false depending on
     whether any element ", TT "v#i", " of ", TT "v", " yields the value
     ", TT "true", " when the predicate ", TT "f", " is applied.",
     PARA,
     "Works when v is a list, sequence, or hash table, but when v is an
     hash table, f is applied to each pair (k,x) consisting of a key k
     and a value x from v.",
     PARA,
     SEEALSO{ "scan", "apply", "select", "all", "member"}
     }

document { describe,
     Headline => "real description",
     TT "describe x", " -- returns an ", TO "Expression", " containing the 
     real description of ", TT "x", ", bypassing the feature which causes
     certain types of things to acquire the names of global variables to which
     they are assigned.",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[a,b,c_1,c_2];",
      	  "R",
      	  "describe R",
	  "toString describe R",
	  },
     PARA,
     "Currently, this function works by temporarily removing the value
     stored under ", TT "name", " from the hash table ", TT "x", ",
     which therefore must be mutable.",
     PARA,
     SEEALSO "toString"
     }

document { input,
     Headline => "read Macaulay 2 commands and echo",
     TT "input \"f\"", " -- reads and executes the commands found in the 
     file named ", TT "f", ", echoing the input, printing the values, and incrementing
     the line number.",
     PARA,
     "The file is sought in the directory containing the file currently being
     loaded, if any, and then along the ", TO "path", ", unless the name of
     the file begins with the character(s) in ", TO "pathSeparator", ".",
     PARA,
     "If one of the expressions in the file evaluates to the symbol ", TO "end", "
     the reading of the file is stopped at that point.",
     PARA,
     "If an error occurs while evaluating the expressions in the file,
     reading is stopped.",
     PARA,
     SEEALSO{ "path", "needs", "load"}
     }

document { end,
     Headline => "stop loading a file",
     TT "end", " -- a symbol which causes loading of a file to be stopped.",
     PARA,
     SEEALSO{ "needs", "load", "input" }
     }

document { load,
     Headline => "read Macaulay 2 commands",
     TT "load \"f\"", " -- reads and executes Macaulay 2 expressions found
     in the file named ", TT "f", ".",
     PARA,
     "The file is sought in the directory containing the file currently being
     loaded, if any, and then along the ", TO "path", ", unless the name of
     the file begins with the character(s) in ", TO "pathSeparator", ".
     The file is read without echoing the input, printing the values,
     or incrementing the line number.",
     PARA,
     SEEALSO{ "path", "needs", "input"}
     }

document { needs,
     Headline => "read Macaulay 2 commands if necessary",
     TT "needs \"f\"", " -- loads the file named ", TT "f", " if it hasn't 
     been loaded yet.",
     PARA,
     SEEALSO "load"
     }

document { plus,
     Headline => "addition",
     TT "plus(x,y,...)", " -- yields the sum of its arguments.",
     PARA,
     "If the arguments are strings, they are concatenated.  If there
     are no arguments, the answer is the integer 0."
     }

document { times,
     Headline => "multiplication",
     TT "times(x,y,...)", " -- yields the product of its arguments.",
     PARA,
     "If there are no arguments, the value is the integer 1."
     }

document { power,
     Headline => "power",
     TT "power(x,n)", " -- yields the ", TT "n", "-th power of ", TT "x", ".",
     PARA,
     SEEALSO "^"
     }

document { difference, 
     Headline => "difference",
     TT "difference(x,y)", " -- returns ", TT "x-y", "." 
     }

document { minus,
     Headline => "additive inverse",
     TT "minus(x)   ", " -- yields ", TT "-x", ".",
     PARA,
     "minus(x,y)  -- yields x-y, but see also ", TO "difference", "."
     }

document { append,
     Headline => "add to the end of a list",
     TT "append(v,x)", " -- yields the list obtained by appending ", TT "x", " to the 
     list ", TT "v", ".  Similarly if ", TT "v", " is a sequence.",
     PARA,
     EXAMPLE "append( {a,b,c}, x )",
     PARA,
     SEEALSO{ "prepend", "join"}
     }

document { prepend,
     Headline => "add to the beginning of a list",
     TT "prepend(x,v)", " -- yields the list obtained by prepending x to the 
     list ", TT "v", ".  Similarly if ", TT "v", " is a sequence.",
     PARA,
     EXAMPLE "prepend( x, {a,b,c} )",
     PARA,
     SEEALSO{ "append", "join"}
     }

document { "--",
     Headline => "comment",
     "Use a double hyphen (", TT "--", ") to introduce a comment in the text
     of a program.  The comment runs from to the end of the line.",
     PARA,
     "Emacs does a good job displaying the comments in a different color
     for visibility.",
     EXAMPLE {
	  "x = 1 -- this is a comment",
	  }
     }

document { ascii, Headline => "ASCII character conversion" }

document { (ascii, List),
     Synopsis => {
	  "s = ascii v",
	  "v" => "a list of small integers",
	  "s" => {"the string whose characters have the ASCII codes listed in ", TT "v"}
	  },
     EXAMPLE {///ascii {65,66,67}///, ///ascii oo///},
     SEEALSO { (ascii, String) }
     }

document { (ascii, String),
     Synopsis => {
	  "v = ascii s",
	  "s" => "a string",
	  "v" => {"the list of (small integer) ASCII codes
	       of the characters of ", TT "s"}
	  },
     EXAMPLE {///ascii "abcdef"///, ///ascii oo///, ///first ascii "A"///},
     SEEALSO { (ascii, List) }
     }

document { transnet,
     Headline => "assemble bytes into 4-byte integers",
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

document { symbol " ", 
     Headline => "blank operator for adjacent expressions",
     SEEALSO(symbol " ", Function, Thing)		    -- not really a method
     }

document { (symbol " ", Function, Thing),
     Headline => "function application",
     TT "f x", " -- yields the result of applying the function ", TT "f", " to ", TT "x", ".",
     }

document { symbol "*",
     Headline => "a binary operator, usually used for multiplication",
     TT "x * y", " -- usually yields the product of x and y.",
     PARA,
     SEEALSO{ "times", "product" }
     }

document { symbol "&",
     Headline => "a binary operator",
     }

document { (symbol &, ZZ, ZZ),
     Headline => "logical and",
     TT "m & n", " -- produce an integer obtained from the bits of the 
     integers ", TT "m", " and ", TT "n", " by logical 'and'."
     }

document { symbol "&&",
     Headline => "a binary operator",
     }

document { symbol "^^",
     Headline => "a binary operator",
     }

document { symbol "+",
     Headline => "a binary operator",
     TT "x + y", " -- a binary operator used for addition in many situations
     and for union of sets.",
     PARA,
     SEEALSO{ "plus", "sum" }
     }

document { (symbol +, Set, Set),
     Headline => "union",
     TT "s + t", " -- union of two sets" }

document { symbol "-",
     Headline => "a unary or binary operator, usually used for negation or subtraction",
     TT "x - y", " -- a binary operator used for subtraction in many situations
     and set difference.",
     BR,NOINDENT,
     TT "- y", "   -- a unary operator usually used for negation.",
     PARA,
     SEEALSO{ "difference", "minus" }
     }

document { symbol "/",
     Headline => "a binary operator, usually used for division",
     TT "x / y", " -- a binary operator usually used for division, yielding a
     fraction, or for quotients (ring by ideal, etc.).",
     PARA,
     SEEALSO { "//" }
     }

document { symbol "%",
     Headline => "a binary operator, usually used for remainder",
     TT "x % y", " -- a binary operator used for remainder and reduction." }

document { symbol "//",
     Headline => "a binary operator, usually used for quotient",
     TT "x // y", " -- a binary operator used for quotients in the same
     ring (with a possible remainder).",
     PARA,
     SEEALSO { "/" }
     }

document { symbol "\\\\",
     Headline => "a binary operator",
     }

document { symbol "^",
     Headline => "a binary operator, usually used for exponents",
     TT "x ^ y", " -- a binary operator used for powers and raising nets.",
     PARA,
     "When computing powers, one of the following methods is used.",
     MENU {
	  TO "SimplePowerMethod",
	  TO "BinaryPowerMethod"
	  },
     NOINDENT, "In addition, if n is 0, then the unit element 
     ", TT "(class x)#1", " is returned.  If n is negative, then the method
     named ", TO "InverseMethod", " will be called."
     }

document { symbol "/^",
     Headline => "a binary operator",
     }

document { (symbol /^, Thing, ZZ),
     Headline => "divided power",
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
     Headline => "extract part of a string",
     TT "substring(s,i,n)", " -- yields the substring of the string s starting at 
     position i with length n.",
     PARA,
     "substring(s,i)   -- yields the substring of s starting at position i and
     continuing to the end of s.",
     PARA,
     "Positions are numbered starting at 0.",
     PARA,
     "Requests for character positions out of bounds are 
     silently ignored."
     }

document { reverse,
     Headline => "reverse a list",
     TT "reverse v", " -- yields a list containing the elements of the 
     list ", TT "v", " in reverse order.",
     PARA,
     EXAMPLE "reverse {a,b,c,d}"
     }

document { (read,Sequence),
     Synopsis => {
	  "s = read()",
     	  "()" => null,
	  "s" => { "a string obtained by reading from the standard input 
	       file ", TO "stdio", "." }
	  }
     }

document { (read,String),
     Synopsis => {
	  "s = read p",
     	  "p" => "a string containing a prompt to be displayed for the user",
	  "s" => { "a string obtained by reading from the standard
	       input file ", TO "stdio", "." }
	  }
     }

document { (read,File),
     Synopsis => {
	  "s = read f",
     	  "f" => "an input file",
	  "s" => { "a string obtained by reading from ", TT "f", "." }
	  },
     if version#"operating system" =!= "Windows-95-98-NT"
     then EXAMPLE {
	  ///f = openInOut "!cat"///,
	  ///isReady f///,
	  ///f << "hi there" << flush;///,
	  ///isReady f///,
	  ///read f///,
	  ///isReady f///,
	  },
     SEEALSO {"isReady"}
     }

document { (read,File,ZZ),
     Synopsis => {
	  "s = read(f,n)",
     	  "f" => "a file",
	  "n" => "an integer specifying the maximum number of bytes to read",
	  "s" => { "a string obtained by reading from ", TT "f", "." }
	  }
     }

document { read,
     Headline => "read from a file",
     PARA,
     "Input files are buffered, so the current contents of the buffer are returned
     if the buffer is not empty, otherwise reading from the file is attempted first.",
     SEEALSO {"openIn", "get"}
     }

document { get,
     Headline => "get an entire file",
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
     file ", TT "f", ", closing the file.",
     PARA,
     EXAMPLE {
	  ///"junk" << "hi there" << close///,
      	  ///get "junk"///,
     	  if version#"operating system" =!= "SunOS"
     	  and version#"operating system" =!= "Windows-95-98-NT"
      	  then ///get "!date"///
	  },
     SEEALSO{ "File", "read" }
     }

document { separate,
     Headline => "split a string into pieces",
     TT "separate(d,s)", " -- split the string ", TT "s", " into pieces 
     delimited by the string ", TT "d", ".",
     PARA,
     "The value is a list of the pieces, the number of which is one
     more than the number of occurences of d in s, so that the pieces
     may be reassembled with ", TO "between", ".",
     EXAMPLE {
	  ///separate( ".", "a.b.c.d" )///,
	  ///peek separate( ".", "a.b.c.d" )///,
	  ///concatenate between("=",ooo)///
	  }
     }

document { lines,
     Headline => "split a string into lines",
     TT "lines s", " -- yields an array of strings obtained from the
     string ", TT "s", " by breaking it at newline or return characters.",
     BR,NOINDENT,
     TT "lines(nl,s)", " -- yields an array of strings obtained from the 
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

document { symbol "!",
     Headline => "factorial",
     TT "n !", " -- computes n factorial, 1*2*3*...*n."
     }

document { "not",
     Headline => "negation",
     TT "not x", " -- yields the negation of x, which must be true or false.",
     SEEALSO{ "and", "or" }
     }

document { symbol "|", 
     Headline => "a binary operator",
     SEEALSO "||" }

document { (symbol |, List, List),
     Headline => "join lists",
     TT "v|w", " -- join two lists.", 
     PARA,
     EXAMPLE "{1,2,3}|{4,5,6}"
     }

document { (symbol |, Net, Net),
     Headline => "join strings or nets",
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
     EXAMPLE ///"t = " | 333///
     }

document { (symbol |, ZZ, ZZ),
     Headline => "logical or",
     TT "m|n", " -- produce an integer obtained from the bits of the 
     integers ", TT "m", " and ", TT "n", " by logical 'or'.",
     PARA,
     EXAMPLE "5 | 12"
     }

document { (symbol |, Matrix, Matrix),
     Headline => "join matrices horizontally",
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
     SEEALSO {(symbol ||, Matrix, Matrix)}
     }

document { symbol ||,
     Headline => "a binary operator"
     }

document { (symbol ||, Net, Net),
     Headline => "join nets or strings vertically",
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
     SEEALSO {"stack"}
     }

document { (symbol ||, Matrix, Matrix),
     Headline => "join matrices vertically",
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
     SEEALSO{(symbol |, Matrix, Matrix)}
     }

document { symbol "===",
     Headline => "strict equality",
     TT "x === y", " -- returns true or false depending on whether the 
     expressions x and y are strictly equal.",
     PARA,
     "Strictly equal expressions have the same type, so ", TT "0===0.", " and
     ", TT "0===0/1", " are false; the three types involved here are ", 
     TO "ZZ", ", ", TO "RR", ", and ", TO "QQ", ".",
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

document { symbol "=!=",
     Headline => "strict inequality",
     TT "x =!= y", " -- returns true or false depending on whether the expressions
     x and y are strictly unequal.",
     PARA,
     "See ", TO "===", " for details."
     }

document { symbol "==",
     Headline => "equality",
     TT "x == y", " -- a binary operator for testing mathematical equality.",
     PARA,
     "A test for mathematical equality will typically involve doing a computation
     to see whether two representations of the same mathematical object are being
     compared.  For example, an ideal in a ring is represented by giving its
     generators, and checking whether two sets of generators produce the same
     ideal involves a computation with Groebner bases.",
     PARA,
     "It may happen that for certain types of objects, there is no method installed
     for testing mathematical equality, in which strict equality will be tested with
     the operator ", TO "===", ".  If a test for mathematical equality is installed
     later, your results may change.",
     -- "It may happen that for certain types of objects, there is no method installed
     -- for testing mathematical equality, in which case an error will be given.  If
     -- you wanted to test strict equality, use the operator ", TO "===", " or 
     -- ", TO "=!=", ".",
     PARA,
     "Warning: whether this comparison operator returns true is not necessarily 
     related to whether the comparison operator ", TO "?", " returns ", TT "symbol ==", ".",
     PARA,
     SEEALSO{ "!=" }
     }

document { symbol "!=",
     Headline => "inequality",
     TT "x != y", " -- the negation of ", TT "x == y", ".",
     PARA,
     SEEALSO{ "==" }
     }

document { symbol "**",
     Headline => "a binary operator, usually used for tensor product",
     }

document { symbol "^**",
     Headline => "a binary operator, usually used for tensor power",
     }

document { (symbol **, Set, Set),
     Headline => "Cartesian product",
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
     Headline => "make a set",
     TT "set v", " -- yields the set whose elements are the members of the list v.",
     PARA,
     SEEALSO { "Set" }
     }

document { random,
     Headline => "get a random element",
     "This function can be used to get random elements of various sorts.",
     SEEALSO {"setRandomSeed"}
     }
document { (random, ZZ), 
     Headline => "random integer",
     TT "random n", " -- yields a random integer in the range 0 .. n-1.",
     PARA,
     "Warning: doesn't correctly handle the case when n an integer is larger
     than 2^31-1.",
     EXAMPLE "tally apply(100, i -> random 10)",
     SEEALSO {"setRandomSeed"}
     }

document { (random, RR), 
     Headline => "random real number",
     TT "random x", " -- yields a random real number in the range 0 .. x.",
     SEEALSO {"setRandomSeed"}
     }

document { (random, Ring),
     Headline => "random element of a ring",
     Synopsis => {
	  "r = random R",
	  "R" => null,
	  "r" => { "a random element of the ring ", TT "R" }
	  },
     "Note: not implemented yet for ", TO "RR", ", ", TO "CC", ", and polynomial rings.",
     SEEALSO {"setRandomSeed"}
     }

document { (random, ZZ, Ring),
     Headline => "a random ring element of a given degree",
     Synopsis => {
	  "r = random(n,R)",
	  "n" => null,
	  "R" => null,
	  "r" => {"a random homogeneous element of degree ", TT "n"}
	  },
     EXAMPLE {
	  "R = GF(9,Variable=>a)[x,y];",
	  "random(3,R)"
	  },
     SEEALSO {"setRandomSeed"}
     }

document { (random, List, Ring),
     Headline => "a random ring element of a given degree",
     Synopsis => {
	  "r = random(n,R)",
	  "n" => "a list of integers",
	  "R" => null,
	  "r" => {"a random homogeneous element of degree ", TT "n"}
	  },
     "The length of ", TT "n", " should be the same as ", TT "degreeLength R", ".",
     SEEALSO {"setRandomSeed"}
     }

document { (random, Module, Module),
     Headline => "make a random module map",
     TT "random(F,G)", " -- yields a random graded, degree 0, map from the free
     module G to the free module F.",
     EXAMPLE {
	  "R = ZZ/101[x,y];",
      	  "random(R^{1,2,3},R^{1,2,3})"
	  },
     SEEALSO {"setRandomSeed"}
     }

document { true,
     Headline => "Boolean value true",
     TT "true", " -- a value indicating truth."
     }

document { false,
     Headline => "Boolean value false",
     TT "false", " -- a value indicating falsity."
     }
