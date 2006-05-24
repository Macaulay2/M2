--		Copyright 1993-1999 by Daniel R. Grayson

document {
     Key => "length",
     Headline => "length"
     }

document {
     Key => (length, Dictionary),
     Headline => "length of a dictionary",
     Usage => "n = length d",
     Inputs => { "d" => "" },
     Outputs => { "n" => { "the number of entries in ", TT "d" } }
     }

document {
     Key => (length, VisibleList),
     Headline => "length of a visible list",
     Usage => "n = length x",
     Inputs => { "x" => "" },
     Outputs => { "n" => { "the number of entries in ", TT "x" } }
     }

document {
     Key => (length, String),
     Headline => "length of a string",
     Usage => "n = length s",
     Inputs => { "s" => "" },
     Outputs => { "n" => { "the length of the string ", TT "s" } }
     }

document {
     Key => (length, GradedModule),
     Headline => "length of a graded module",
     "The length of a graded module is the difference between the largest and
     smallest indices of occupied spots.  Chain complexes are graded modules,
     so this function applies to them, too.",
     SUBSECTION "Examples",
     EXAMPLE {
	  "R = QQ[x..z];",
	  "C = res coker vars R",
	  "length C"
	  }
     }

document {
     Key => parent,
     Headline => "parent type of an object",
     Usage => "X = parent x",
     Inputs => {
	  "x" => ""
	  },
     Outputs => {
	  "X" => { "the parent class of ", TT "x" }
	  },
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
     SeeAlso => "classes and types"
     }

document {
     Key => Type,
     Headline => "the class of all types",
     "Everything in the system is classified, and the class that a thing
     belongs to is a type.  A type is implemented as a hash table containing
     method functions for its instances.",
     PARA,
     "The list of types known to the system is displayed below."
     }

document {
     Key => Print,
     Headline => "top level method for printing results",
     Usage => "X.Print = f",
     Inputs => {
	  "X" => Type => "",
	  "f" => Function => { " that can print something of type ", TT "X"}
	  },
     Consequences => {
	  { "at top level, whenever it is time to print an output value of type ", TT "X", ", the function ", TT "f", " will be called" }
	  },
     "The function ", TT "f", " is responsible for printing the output prompt and for applying the ", TO "BeforePrint", " and ", TO "AfterPrint", " methods, if desired.",
     SUBSECTION "Examples",
     EXAMPLE "code Thing.Print"
     }

document {
     Key => NoPrint,
     Headline => "top level method for non-printing results",
     Usage => "X.NoPrint = f",
     Inputs => {
	  "X" => Type => "",
	  "f" => Function => { " that can accept something of type ", TT "X"}
	  },
     Consequences => {
	  {
	       "at top level, whenever it is time, as indicated by a semicolon at the end of an input line,
	       to suppress printing of an output value of type ", TT "X", ", the function ", TT "f", " will be called" }
	  },
     SUBSECTION "Examples",
     EXAMPLE "code Thing.NoPrint"
     }

document {
     Key => BeforePrint,
     Headline => "top level method applied before printing results",
     Usage => "X.BeforePrint = f",
     Inputs => {
	  "f" => { "a function to be applied before printing a top-level evaluation result ", TT "r", " of type ", TT "X", "." },
	  },
     Consequences => {
	  {"The value returned by ", TT "f", " is printed instead."}
	  }
     }

document {
     Key => AfterEval,
     Headline => "top level method applied after evaluation",
     Usage => "X.AfterEval = f",
     Inputs => {
	  "f" => { "a function to be applied after evaluating a top-level evaluation result ", TT "r", " of type ", TT "X", "."},
	  },
     Consequences => {
	  "The value returned result replaces the original for storing in the output variables and for printing"
	  },
     }

document {
     Key => AfterPrint,
     Headline => "top level method applied after printing",
     Usage => "X.AfterPrint = f",
     Inputs => {
	  "f" => { "a function to be applied after printing a top-level evaluation result ", TT "r", " of type ", TT "X", "."}
	  },
     Outputs => {
	  {"The value returned by ", TT "f", " is discarded."}
	  },
     "This method is used to print the type of the result of a computation.",
     EXAMPLE {
	  "3/4"
	  },
     "We could suppress that output for a single type as follows.",
     EXAMPLE {
	  "QQ.AfterPrint = r -> r;",
	  "3/4"
	  }
     }

document {
     Key => AfterNoPrint,
     Headline => "top level method applied after not printing",
     Usage => "X.AfterNoPrint = f",
     Inputs => {
	  "f" => { "a function to be applied after not printing a top-level evaluation result ", TT "r", " of type ", TT "X", "." }
	  },
     Consequences => {
	  {
	       "The function ", TT "f", " will be applied at top level to the 
	       result of an evaluation when printing of the result has
	       been suppressed by a semicolon."
	       }
	  }
     }

document {
     Key => "recursionLimit",
     Headline => "set the limit on recursion",
     Usage => "recursionLimit = n",
     Inputs => { "n" => ZZ => "" },
     Consequences => { {"The recursion depth limit for the interpreter is set to ", TT "n", "."} },
     "Each time a function is called, the recursion depth is incremented by
     1, and each time a function returns, the recursion depth is decremented.
     This limit on recursion depth is a way to detect infinite loops."
     }

document {
     Key => "environment",
     Headline => "the environment variables",
     "A constant whose value is the list containing the
     environment strings for the process."
     }

document {
     Key => Function,
     Headline => "the class of all functions",
     "Common ways to make a function:",
     UL {
	  TO "->"
	  },
     "Returning from functions:",
     UL {
	  TO "return"
	  },
     SeeAlso => "functions"
     }

document {
     Key => "->",
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
     EXAMPLE {
	  "f = x -> 2*x+1",
	  "f 100"
	  },
     "The class of all functions is ", TO "Function", "."
     }

document {
     Key => "path",
     Headline => "list of directories to look in",
     "A list of strings containing names of directories in
     which ", TO "load", " and ", TO "input", " should seek files.  These strings
     are simply concatenated with the filename being sought, so should include
     any necessary terminal slashes.  One further directory is implicitly searched 
     first: the directory containing the current input file; when input is coming 
     from the standard input, that directory is the current directory of the process.",
     EXAMPLE {
	  "path",
	  ///path = append(path, homeDirectory | "resolutions/")///
	  }
     }

document {
     Key => HashTable,
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
     UL {
 	  TO "#",
 	  TO "."
 	  },
     "Query functions:",
     UL {
 	  TO "#?",
 	  TO ".?"
 	  }
     }

document {
     Key => {maxPosition,(maxPosition,BasicList)},
     Usage => "maxPosition x",
     Headline => "position of largest element",
     Inputs => {
	  "x" => BasicList => ""
	  },
     Outputs => {
	  { "the position of the largest element in the list ", TT "x" }
	  },
     "If the largest element occurs more than once, then the first occurrence
     is used.  If ", TT "x", " has length 0 an error results.",
     EXAMPLE {
	  "maxPosition {1,6,4,2}"
	  },
     "Notice that the position of the maximal element is 1, as indexing in
     Macaulay 2 always starts at 0.",
     PARA,
     "For elements in a polynomial ring, the order used is the ", 
     TO2("monomial orderings","monomial order"),
     " associated with the ring.",
     EXAMPLE {
	  "R = QQ[r,s,x,y,z];",
	  "z^2 + x*y + s*z",
	  "maxPosition {z^2, s*z, x*y}",
	  "maxPosition(z^2, s*z, x*y)"	  
	  },
     
     SeeAlso => { minPosition, max, min, sort, position }
     }

document {
     Key => {minPosition,(minPosition,BasicList)},
     Usage => "minPosition x",
     Headline => "position of smallest element",
     Inputs => {
	  "x" => BasicList => ""
	  },
     Outputs => {
	  { "the position of the smallest element in the list ", TT "x" }
	  },
     "If the smallest element occurs more than once, then the first occurrence
     is used.  If ", TT "x", " has length 0 an error results.",
     EXAMPLE {
	  "minPosition {1,6,4,2}"
	  },
     "Notice that the position of the minimal element is 0, as indexing in
     Macaulay 2 always starts at 0.",
     PARA,
     "For elements in a polynomial ring, the order used is the ", 
     TO2("monomial orderings","monomial order"),
     " associated with the ring.",
     EXAMPLE {
	  "R = QQ[r,s,x,y,z];",
	  "z^2 + x*y + s*z",
	  "minPosition {z^2, s*z, x*y}",
	  "minPosition(z^2, s*z, x*y)"
	  },
     SeeAlso => { maxPosition, max, min, sort, position }
     }

document {
     Key => keys, Headline => "keys used in a hash table" }
document {
     Key => (keys,HashTable),
     Usage => "keys t",
     Inputs => {
	  "t" => ""
	  },
     Outputs => {
	  {"a list of the keys occurring in the hash table ", TT "t"}
	  },
     EXAMPLE {
	  "x = new HashTable from {a => 1, b => 2}",
	  "keys x",
	  }
     }

document {
     Key => values, Headline => "values in a hash table" }
document {
     Key => (values,HashTable),
     Usage => "values t",
     Inputs => {
	  "t" => ""
	  },
     Outputs => {
	  {"a list of the values occurring in the hash table ", TT "t", "."}
	  },
     EXAMPLE {
	  "x = new HashTable from {a => 1, b => 2}",
	  "values x",
	  }
     }

document {
     Key => scan,
     Headline => "apply a function to each element",
     SeeAlso => { "mapping over lists"}
     }

document {
     Key => (scan,BasicList,Function),
     Headline => "apply a function to each element of a list",
     TT "scan(v,f)", " -- applies the function ", TT "f", " to each element of the 
     list ", TT "v", ".  The function values are discarded.",
     EXAMPLE "scan({a,4,\"George\",2^100}, print)"
     }

document {
     Key => (scan,ZZ,Function),
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

document {
     Key => scanPairs,
     Headline => "apply a function to pairs in a hash table" }

document {
     Key => (scanPairs,HashTable,Function),
     Headline => "apply a function to pairs in a hash table",
     TT "scanPairs(x,f)", " -- applies the function ", TT "f", " to each
     pair ", TT "(k,v)", " where ", TT "k", " is a key in the hash 
     table ", TT "x", " and ", TT "v", " is the corresponding 
     value ", TT "x#k", ".",
     PARA,
     "This function requires an immutable hash table.  To scan the pairs in
     a mutable hash table, use ", TT "scan(pairs x, f)", ".",
     PARA,
     SeeAlso => "scan"
     }

--document { find,
--     TT "find(x,f)", " -- applies the function ", TT "f", " to each element
--     of ", TT "x", ", returning the result not equal to ", TO "null", ".
--     If no result is non-null, then it returns null."
--     }


document {
     Key => describe,
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
     SeeAlso => "toString"
     }

document {
     Key => input,
     Headline => "read Macaulay 2 commands and echo",
     TT "input \"f\"", " -- reads and executes the commands found in the 
     file named ", TT "f", ", echoing the input, printing the values, and incrementing
     the line number.",
     PARA,
     "The file is sought in the directory containing the file currently being
     loaded, if any, and then along the ", TO "path", ", unless the name of
     the file begins with a slash (/).",
     PARA,
     "If one of the top level expressions in the file evaluates to the symbol ", TO "end", "
     the reading of the file is stopped at that point.",
     PARA,
     "If an error occurs while evaluating the expressions in the file,
     reading is stopped.",
     PARA,
     SeeAlso =>{ "path", "needs", "load"}
     }

document {
     Key => end,
     Headline => "stop loading a file",
     TT "end", " -- a symbol which causes loading of a file to be stopped.",
     SeeAlso =>{ "needs", "load", "input" }
     }

document {
     Key => load,
     Headline => "read Macaulay 2 commands",
     TT "load \"f\"", " -- reads and executes Macaulay 2 expressions found
     in the file named ", TT "f", ".",
     PARA,
     "The file is sought in the directory containing the file currently being
     loaded, if any, and then along the ", TO "path", ", unless the name of
     the file begins with a slash (/). The file is read without echoing the
     input, printing the values, or incrementing the line number.",
     PARA,
     "If one of the top level expressions in the file evaluates to the symbol ", TO "end", "
     the reading of the file is stopped at that point.",
     SeeAlso =>{ "path", "needs", "input"}
     }

document {
     Key => needs,
     Headline => "read Macaulay 2 commands if necessary",
     TT "needs \"f\"", " -- loads the file named ", TT "f", " if it hasn't 
     been loaded yet.",
     PARA,
     SeeAlso => "load"
     }

document {
     Key => plus,
     Headline => "addition",
     TT "plus(x,y,...)", " -- yields the sum of its arguments.",
     PARA,
     "If the arguments are strings, they are concatenated.  If there
     are no arguments, the answer is the integer 0."
     }

document {
     Key => times,
     Headline => "multiplication",
	Usage => "times(x,y, ...)",
     TT "times(x,y, ...)", " yields the product of its arguments. 
	If there are no arguments, the value is the integer 1."
     }

document {
     Key => power,
     Headline => "power",
	Usage => "(x,n)",
     TT "power(x,n)", " yields the ", TT "n", "-th power of ", TT "x", ".",
     PARA,
     SeeAlso => "^"
     }

document {
     Key => difference, 
     Headline => "difference",
	Usage => "difference(x,y)",
     TT "difference(x,y)", " returns ", TT "x-y", "." 
     }

document {
     Key => minus,
     Headline => "additive inverse",
	Usage => "minus(x)",
     TT "minus(x)", " yields ", TT "-x", ".",
     PARA,
     TT "minus(x,y)", " yields ", TT"x-y", " but see also ", TO "difference", "."
     }

document {
     Key => "--",
     Headline => "comment",
     Consequences => {"Macaulay 2 ignores commented text"},
     "Use a double hyphen ", TT "--", " to introduce a comment in the text
     of a program.  The comment runs from to the end of the line.",
     PARA,
     "Emacs does a good job displaying the comments in a different color
     for visibility.",
     EXAMPLE {
	  "x = 1 -- this is a comment",
	  }
     }

document {
     Key => ascii, Headline => "ASCII character conversion" }

document {
     Key => (ascii, List),
     Usage => "ascii v",
     Inputs => {
	  "v" => "a list of small integers"
	  },
     Outputs => {
	  {"the string whose characters have the ASCII codes listed in ", TT "v"}
	  },
     EXAMPLE {///ascii {65,66,67}///, ///ascii oo///},
     SeeAlso => { (ascii, String) }
     }

document {
     Key => (ascii, String),
     Usage => "ascii s",
     Inputs => {
	  "s" => "",
	  },
     Outputs => {
	  {"the list of (small integer) ASCII codes of the characters of ", TT "s"}
	  },
     EXAMPLE {///ascii "abcdef"///, ///ascii oo///, ///first ascii "A"///},
     SeeAlso => { (ascii, List) }
     }

document {
     Key => symbol " ", 
     Headline => "blank operator for adjacent expressions",
     SeeAlso =>(symbol " ", Function, Thing)		    -- not really a method
     }

document {
     Key => (symbol " ", Function, Thing),
     Headline => "function application",
     TT "f x", " -- yields the result of applying the function ", TT "f", " to ", TT "x", ".",
     }

undocumented {
    (symbol*,  Expression, Product),
    (symbol*,  Product, Expression),
    (symbol*,  RingElement, ZZ),
    (symbol*,  Holder, Expression),
    (symbol*,  QQ, RingElement),
    (symbol*,  RingElement, QQ),
    (symbol*,  RingElement, RR),
    (symbol*,  Minus, Expression),
    (symbol*,  Product, Holder),
    (symbol*,  Holder, Holder),
    (symbol*,  RingElement, Vector),
    (symbol*,  ZZ, CC),
    (symbol*,  ZZ, Matrix),
    (symbol*,  Matrix, ZZ),
    (symbol*,  Matrix, QQ),
    (symbol*,  RR, CC),
    (symbol*,  Thing, Thing),
    (symbol*,  Thing, Expression),
    (symbol*,  Expression, Thing),
    (symbol*,  ZeroExpression, Expression),
    (symbol*,  Minus, Minus),
    (symbol*,  ZZ, InfiniteNumber),
    (symbol*,  InfiniteNumber, ZZ),
    (symbol*,  Expression, Expression),
    (symbol*,  Product, ZeroExpression),
    (symbol*,  ZZ, Ideal),
    (symbol*,  Product, OneExpression),
    (symbol*,  ZeroExpression, Holder),
    (symbol*,  Holder, ZeroExpression),
    (symbol*,  Holder, OneExpression),
    (symbol*,  ZZ, RingElement),
    (symbol*,  Expression, Holder),
    (symbol*,  RR, RingElement),
    (symbol*,  ZZ, GradedModuleMap),
    (symbol*,  InfiniteNumber, InfiniteNumber),
    (symbol*,  Expression, Minus),
    (symbol*,  Product, Product),
    (symbol*,  Holder, Product),
    (symbol*,  Thing),
    (symbol*,  ZZ, Module),
    (symbol*,  CC, ZZ),
    (symbol*,  CC, QQ),
    (symbol*,  QQ, CC),
    (symbol*,  CC, RR),
    (symbol*,  QQ, Matrix),
    (symbol*,  CC, CC),
    (symbol*,  ZZ, MonomialIdeal),
    (symbol*,  String),
    (symbol*,  ZZ, ChainComplexMap),
    (symbol*,  Expression, ZeroExpression),
    (symbol*,  Expression, OneExpression),
    (symbol*,  OneExpression, Expression)
    }

     
document {
     Key => {symbol*,
    (symbol*,  Ring, Ideal),
    (symbol*,  MutableMatrix, MutableMatrix),
    (symbol*,  Ideal, Module),
    (symbol*,  Ring, RingElement),
    (symbol*,  GradedModuleMap, GradedModuleMap),
    (symbol*,  RingElement, Matrix),
    (symbol*,  Ideal, CoherentSheaf),
    (symbol*,  RingMap, RingMap),
    (symbol*,  RingElement, MutableMatrix),
    (symbol*,  Ring, MonomialIdeal),
    (symbol*,  MonomialIdeal, Module),
    (symbol*,  AffineVariety, AffineVariety),
    (symbol*,  MonomialIdeal, MonomialIdeal),
    (symbol*,  RingElement, Ideal),
    (symbol*,  Matrix, Vector),
    (symbol*,  MonomialIdeal, Ring),
    (symbol*,  Ring, Vector),
    (symbol*,  RingElement, ChainComplexMap),
    (symbol*,  Ideal, Vector),
    (symbol*,  RingElement, MonomialIdeal),
    (symbol*,  Matrix, RingElement),
    (symbol*,  RingElement, Module),
    (symbol*,  RingElement, GradedModuleMap),
    (symbol*,  Ideal, Ring),
    (symbol*,  ChainComplexMap, ChainComplexMap),
    (symbol*,  RingElement, RingElement),
    (symbol*,  Thing, List),
    (symbol*,  ZZ, ProjectiveHilbertPolynomial)
	  },
     Headline => "a binary operator, usually used for multiplication",
     Usage => "x * y",
     "The return type depends on the types of x and y.  If they have the
     same type, then usually the return type is the common type of x and y.
     Returns true or false, depending on whether 
     the objects x and y are (mathematically) equal.  The objects x and y are
     typically numbers, elements of rings, matrices, modules, ideals,
     chain complexes, and so on.",
     PARA,
     "Multiplication involving ring elements (including integers, rational numbers,
     real and complex numbers), ideals, vectors, matrices, modules is 
     generally the usual multiplication, or composition of functions.",
     EXAMPLE {
	  },
     PARA,
     "The intersection of sets is given by multiplication.  See ", TO (symbol*,Set,Set), ".",
     EXAMPLE {
	  "set{hi,you,there} * set{hi,us,here,you}"
	  },
     PARA,
     "Multiplication involving a list attempts to multiply each element of the list.",
     EXAMPLE {
	  "R = QQ[a..d];",
	  "a * {b,c,d}"
	  },
     PARA,
     "Multiplication of matrices (", TO (symbol*,Matrix,Matrix),") or ring maps is the same as composition.",
     EXAMPLE {
	  "f = map(R,R,{b,c,a,d})",
	  "g = map(R,R,{(a+b)^2,b^2,c^2,d^2})",
	  "f*g",
	  "(f*g)(a) == f(g(a))"
	  },
     PARA,
     "Submodules of modules may be produced using multiplication and addition.",
     EXAMPLE {
	  "M = R^2; I = ideal(a+b,c);",
	  "N = I*M + a*R^2",
	  "isHomogeneous N"
	  },
     SeeAlso =>{ "times", "product" }
     }

document {
     Key => symbol "&",
     Headline => "a binary operator",
     }

document {
     Key => (symbol &, ZZ, ZZ),
     Headline => "logical and",
     TT "m & n", " -- produce an integer obtained from the bits of the 
     integers ", TT "m", " and ", TT "n", " by logical 'and'."
     }

document {
     Key => symbol "^^",
     Headline => "a binary operator",
     }

document {
     Key => symbol "+",
     Headline => "a binary operator",
     TT "x + y", " -- a binary operator used for addition in many situations
     and for union of sets.",
     PARA,
     SeeAlso =>{ "plus", "sum" }
     }

document {
     Key => symbol "-",
     Headline => "a unary or binary operator, usually used for negation or subtraction",
     TT "x - y", " -- a binary operator used for subtraction in many situations
     and set difference.",
     BR,NOINDENT,
     TT "- y", "   -- a unary operator usually used for negation.",
     PARA,
     SeeAlso =>{ "difference", "minus" }
     }

document {
     Key => symbol "/",
     Headline => "a binary operator, usually used for division",
     TT "x / y", " -- a binary operator usually used for division, yielding a
     fraction, or for quotients (ring by ideal, etc.).",
     PARA,
     SeeAlso => { "//" }
     }

document {
     Key => symbol "%",
     Headline => "a binary operator, usually used for remainder",
     TT "x % y", " -- a binary operator used for remainder and reduction." }

document {
     Key => symbol "//",
     Headline => "a binary operator, usually used for quotient",
     TT "x // y", " -- a binary operator used for quotients in the same
     ring (with a possible remainder).",
     PARA,
     SeeAlso => { "/" }
     }

document {
     Key => symbol "\\\\",
     Headline => "a binary operator",
     }

document {
     Key => symbol "^",
     Headline => "a binary operator, usually used for exponents",
     TT "x ^ y", " -- a binary operator used for powers and raising nets.",
     NOINDENT, "In addition, if n is 0, then the unit element 
     ", TT "(class x)#1", " is returned.  If n is negative, then the method
     named ", TO "InverseMethod", " will be called."
     }

-- document {
--      Key => symbol "/^",
--      Headline => "a binary operator",
--      }

-- document {
--      Key => (symbol /^, Thing, ZZ),
--      Headline => "divided power",
--      TT "x /^ n", " -- computes the n-th divided power of x.",
--      PARA,
--      "This is implemented naively as ", TT "x^n/n!", ".",
--      EXAMPLE {
-- 	  "ZZ/101[x];",
--       	  "x/^3"
-- 	  },
--      }

document {
     Key => substring,
     Headline => "extract part of a string",
     TT "substring(i,n,s)", " -- yields the substring of the string ", TT "s", " starting at 
     position ", TT "i", " with length ", TT "n", ".",
     PARA,
     "substring(i,s)   -- yields the substring of ", TT "s", " starting at position ", TT "i", " and
     continuing to the end of ", TT "s", ".",
     PARA,
     "Positions are numbered starting at 0.",
     PARA,
     "If the starting position ", TT "i", " is negative, it means to start from the end of the string.",
     PARA,
     "Requests for character positions out of bounds are silently ignored.",
     PARA,
     "In an older version of the program the string argument was placed first;
     the old way will still work."
     }

document {
     Key => reverse,
     Headline => "reverse a list",
     TT "reverse v", " -- yields a list containing the elements of the 
     list ", TT "v", " in reverse order.",
     EXAMPLE "reverse {a,b,c,d}"
     }

document {
     Key => read, Headline => "read from a file", }
document {
     Key => (read,Sequence),
     Usage => "read()",
     Inputs => {
	  "()" => null
	  },
     Outputs => {
	  { "a string obtained by reading a line from the standard input file, ", TO "stdio", "." }
	  },
     }
document {
     Key => (read,String),
     Usage => "read p",
     Inputs => {
	  "p" => "a string containing a prompt to be displayed for the user"
	  },
     Outputs => {
	  { "a string obtained by reading from the standard input file ", TO "stdio" }
	  },
     }
document {
     Key => (read,File),
     Usage => "read f",
     Inputs => {
	  "f" => "an input file"
	  },
     Outputs => {
	  { "a string obtained by reading from ", TT "f", "." }
	  },
     EXAMPLE {
	  ///f = openInOut "!cat"///,
	  ///isReady f///,
	  ///f << "hi there" << flush;///,
	  ///isReady f///,
	  ///read f///,
	  ///isReady f///,
	  },
     SeeAlso => {"openIn", "get", "isReady"}
     }
document {
     Key => (read,File,ZZ),
     Usage => "read(f,n)",
     Inputs => {
	  "f" => "a file",
	  "n" => "an integer specifying the maximum number of bytes to read"
	  },
     Outputs => {
	  { "a string obtained by reading from ", TT "f" }
	  },
     "Input files are buffered, so the current contents of the buffer are returned
     if the buffer is not empty, otherwise reading from the file is attempted first.",
     SeeAlso => {"openIn", "get", "isReady"}
     }

document {
     Key => get,
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
     EXAMPLE {
	  ///"junk" << "hi there" << close///,
      	  ///get "junk"///,
	  ///removeFile "junk"///,
     	  if version#"operating system" =!= "Windows-95-98-NT" then ///get "!date"///
	  },
     SeeAlso =>{ "File", "read" }
     }

document {
     Key => separate,
     Headline => "split a string into pieces",
     TT "separate(d,s)", " -- split the string ", TT "s", " into pieces 
     delimited by the string ", TT "d", ".",
     PARA,
     "The value is a list of the pieces, the number of which is one
     more than the number of occurrences of d in s, so that the pieces
     may be reassembled with ", TO "between", ".",
     EXAMPLE {
	  ///separate( ".", "a.b.c.d" )///,
	  ///peek separate( ".", "a.b.c.d" )///,
	  ///demark("=",ooo)///
	  }
     }

document {
     Key => lines,
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
     SeeAlso => "newline"
     }

document {
     Key => symbol "!",
     Headline => "factorial",
     TT "n !", " -- computes n factorial, 1*2*3*...*n."
     }

document {
     Key => "not",
     Headline => "negation",
     TT "not x", " -- yields the negation of x, which must be true or false.",
     SeeAlso =>{ "and", "or" }
     }

document {
     Key => symbol "|", 
     Headline => "a binary operator",
     SeeAlso => "||" }

document {
     Key => (symbol |, List, List),
     Headline => "join lists",
     TT "v|w", " -- join two lists.", 
     PARA,
     EXAMPLE "{1,2,3}|{4,5,6}"
     }

document {
     Key => (symbol |, Net, Net),
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
     "If one of the two arguments is an integer, it is converted to a string first.",
     EXAMPLE ///"t = " | 333///
     }

document {
     Key => (symbol |, ZZ, ZZ),
     Headline => "logical or",
     TT "m|n", " -- produce an integer obtained from the bits of the 
     integers ", TT "m", " and ", TT "n", " by logical 'or'.",
     PARA,
     EXAMPLE "5 | 12"
     }

document {
     Key => (symbol |, Matrix, Matrix),
     Headline => "join matrices horizontally",
	Usage => "f = g | h",
	Inputs => {
		"g" => {"a ", TT "m", " by ", TT "n", " matrix."},
		"h" => {"a ", TT "m", " by ", TT "r", " matrix."}
		},
	Outputs => {
		"f" => {"a ", TT "m", " by ", TT "n+r", " matrix."}
		},
     EXAMPLE {
		"R = ZZ/101[x,y,z]",
      	"f = matrix {{x,0,0},{0,y,0},{0,0,z}}",
      	"f|f|f"
	  },
     "If one of the arguments is ring element or an integer, then it
     will be multiplied by a suitable identity matrix.",
	EXAMPLE "33 | f",
     Caveat => {"It is assumed that ", TT "g", " and ", TT "h", " both
		have the same target. Moreover, it is assumed that both ", TT "g", " and ", 
		TT "h", " have the same ", TO "Ring", "."},
     SeeAlso => {(symbol ||, Matrix, Matrix), (ring, Matrix)}
     }


document {
     Key => (symbol |, ZZ, Matrix),
     Headline => "join an integer multiple of an identity matrix to a matrix horizontally",
     Usage => "f = n | g",
	Inputs => {
		"n" => null,
		"g" => {"a ", TT "r", " by ", TT "s", " matrix."}
		},
	Outputs => {
		"f" => {"a ", TT "r", " by ", TT "r+s", " matrix."}
		},
    EXAMPLE {
	    "R = ZZ/101[x,y,z]",
         "f = matrix {{x,0,0},{0,y,0},{0,0,z}}",
         "2|f|3"
	  },
  	"This also works for a arbitrary element of the ring of the matrix."
     }



document {
     Key => (symbol |, Matrix, ZZ),
     Headline => "join an integer multiple of an identity matrix to a matrix horizontally",
     Usage => "f = g | n",
	Inputs => {
    		"g" => {"a ", TT "r", " by ", TT "s", " matrix."},
    		"n" => null
		},
	Outputs => {
		"f" => {"a ", TT "r", " by ", TT "r+s", " matrix."}
		},
     EXAMPLE {
	  "R = ZZ/101[x,y,z]",
       "f = matrix {{x,0,0},{0,y,0},{0,0,z}}",
      "2|f|3"
	  },
   	"This also works for a arbitrary element of the ring of the matrix."
     }

document {
     Key => symbol ||,
     Headline => "a binary operator"
     }

document {
     Key => (symbol ||, Net, Net),
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
     SeeAlso => {"stack"}
     }

document {
     Key => (symbol ||, Matrix, Matrix),
     Headline => "join matrices vertically",
	Usage => "f = g || h",
	Inputs => {
		"g" => {"a ", TT "m", " by ", TT "n", " matrix."},
		"h" => {"a ", TT "r", " by ", TT "n", " matrix."}
		},
	Outputs => {
		"f" => {"a ", TT "m+r", " by ", TT "n", " matrix."}
		},
     TT "g||h", " yields the matrix obtained from matrices ", TT "g", " and ", TT "h", " by
     concatenating the columns.",
     EXAMPLE {
	  "R = ZZ[i..p];",
      	  "g = matrix {{i,j},{k,l}}",
      	  "h = matrix {{m,n},{o,p}}",
      	  "f= g || h",
	  },
     "If one of the arguments is ring element or an integer, then it
     will be multiplied by a suitable identity matrix.",
	EXAMPLE "f || 33",
	Caveat => {"It is assumed that the matrices ", TT "g", " and ", TT "h", " have the same ", TO "Ring", "."},
     SeeAlso =>{(symbol |, Matrix, Matrix), (ring, Matrix)}
     }


document {
     Key => (symbol ||, ZZ, Matrix),
     Headline => "join an integer multiple of an identity matrix to a matrix vertically",
     Usage => "f = n || g",
	Inputs => {
		"n" => null,
		"g" => {"a ", TT "r", " by ", TT "s", " matrix."}
		},
	Outputs => {
		"f" => {"a ", TT "r+s", " by ", TT "s", " matrix."}
		},
    EXAMPLE {
	    "R = ZZ/101[x,y,z]",
         "f = matrix {{x,0,0},{0,y,0},{0,0,z}}",
         "2||f||3"
	  },
	"This also works for a arbitrary element of the ring of the matrix."
     }


document {
     Key => (symbol ||, Matrix, ZZ),
     Headline => "join an integer multiple of an identity matrix to a matrix vertically",
     Usage => "f =  g || n",
	Inputs => {
		"g" => {"a ", TT "r", " by ", TT "s", " matrix."},
		"n" => null
		},
	Outputs => {
		"f" => {"a ", TT "r+s", " by ", TT "s", " matrix."}
		},
    EXAMPLE {
	    "R = ZZ/101[x,y,z]",
         "f = matrix {{x,0,0},{0,y,0},{0,0,z}}",
         "2||f||3"
	  },
  	"This also works for a arbitrary element of the ring of the matrix."
     }


document {
     Key => symbol "===",
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
     EXAMPLE { "{1,2,3} === {1,2,3}", "{1,2,3} === {2,1,3}" },
     "For some types, such as ring elements and matrices, 
     strict equality is the same as mathematical equality.
     This tends to be the case for objects for which computation is not required
     to test equality.",
     EXAMPLE {
	  "R = QQ[a..d];",
	  "a^2+b === b+a^2",
	  "ideal(a^2+b,c*d) === ideal(b+a^2,c*d+b+a^2)",
     	  "matrix{{a,b,c}} === matrix{{a,b,c}}",
       	  "matrix{{a,b,c}} === transpose matrix{{a},{b},{c}}"
	  },
     SeeAlso =>{ symbol==,  symbol=!=, "operators" }
     }

document {
     Key => symbol "=!=",
     Headline => "strict inequality",
     TT "x =!= y", " -- returns true or false depending on whether the expressions
     x and y are strictly unequal.",
     PARA,
     "See ", TO "===", " for details."
     }

undocumented {
         (symbol==, RingElement, ZZ),
         (symbol==, RingElement, Matrix),
         (symbol==, Ideal, MonomialIdeal),
         (symbol==, GradedModuleMap, ZZ),
         (symbol==, InfiniteNumber, InfiniteNumber),
         (symbol==, Holder, Holder),
         (symbol==, Equation, Expression),
         (symbol==, ZZ, Ring),
         (symbol==, ZZ, QQ),
         (symbol==, ZZ, RR),
         (symbol==, RR, ZZ),
         (symbol==, ZZ, Matrix),
         (symbol==, Matrix, ZZ),
         (symbol==, RR, QQ),
         (symbol==, ZZ, CC),
         (symbol==, ZZ, ChainComplex),
         (symbol==, ChainComplex, ZZ),
         (symbol==, RR, CC),
         (symbol==, Thing, Thing),
         (symbol==, MonomialIdeal, Ring),
         (symbol==, Ring, MonomialIdeal),
         (symbol==, MonomialIdeal, ZZ),
         (symbol==, Expression, Thing),
         (symbol==, Thing, Expression),
         (symbol==, Equation, Holder),
         (symbol==, Holder, Equation),
         (symbol==, ChainComplexMap, ZZ),
         (symbol==, ZZ, MutableMatrix),
         (symbol==, Expression, Expression),
         (symbol==, ZZ, InfiniteNumber),
         (symbol==, Nothing, Nothing),
         (symbol==, Equation, Equation),
         (symbol==, GradedModuleMap, RingElement),
         (symbol==, RingElement, GradedModuleMap),
         (symbol==, String, Net),
         (symbol==, Ideal, Ring),
         (symbol==, ZZ, Ideal),
         (symbol==, Ideal, ZZ),
         (symbol==, ZZ, RingElement),
         (symbol==, Matrix, RingElement),
         (symbol==, MonomialIdeal, Ideal),
         (symbol==, ZZ, GradedModuleMap),
         (symbol==, GradedModule, GradedModule),
         (symbol==, Expression, Equation),
         (symbol==, Ring, Ring),
         (symbol==, Module, ZZ),
         (symbol==, ZZ, Module),
         (symbol==, ChainComplexMap, RingElement),
         (symbol==, RingElement, ChainComplexMap),
         (symbol==, Ring, ZZ),
         (symbol==, QQ, ZZ),
         (symbol==, QQ, RR),
         (symbol==, CC, ZZ),
         (symbol==, CC, QQ),
         (symbol==, QQ, CC),
         (symbol==, CC, RR),
         (symbol==, CC, CC),
         (symbol==, ZZ, MonomialIdeal),
         (symbol==, ZZ, ChainComplexMap),
         (symbol==, MutableMatrix, ZZ),
         (symbol==, MonoidElement, MonoidElement),
         (symbol==, MonomialIdeal, MonomialIdeal),
         (symbol==, InfiniteNumber, ZZ),
         (symbol==, ChainComplexMap, ChainComplexMap),
         (symbol==, Net, Net),
         (symbol==, Net, String),
         (symbol==, Module, Ideal),
         (symbol==, Ideal, Module),
         (symbol==, Ring, Ideal)
	  }

document {
     Key => {symbol ==,
	  (symbol==, Matrix, Matrix),
         (symbol==, ProjectiveHilbertPolynomial, ProjectiveHilbertPolynomial),
         (symbol==, ChainComplex, ChainComplex),
         (symbol==, RingElement, RingElement),
         (symbol==, GradedModuleMap, GradedModuleMap),
         (symbol==, Set, Set),
         (symbol==, Ideal, Ideal),
         (symbol==, MutableMatrix, MutableMatrix),
         (symbol==, Module, Module)
	  },
     Headline => "equality",
     Usage => "x == y",
     "Returns true or false, depending on whether 
     the objects x and y are (mathematically) equal.  The objects x and y are
     typically numbers, elements of rings, matrices, modules, ideals, 
     chain complexes, and so on.",
     PARA,
     "A test for mathematical equality will typically involve doing a computation
     to see whether two representations of the same mathematical object are being
     compared.  For example, an ideal in a ring is represented by giving its
     generators, and checking whether two sets of generators produce the same
     ideal involves a computation with Groebner bases.  The ideals must be defined
     in the same ring.",
     HEADER3 "Ideals",
     EXAMPLE {
	  "R = QQ[a,b,c];",
	  "ideal(a^2-b,a^3) == ideal(b^2, a*b, a^2-b)"
	  },
     "Often mathematical objects can be tested to see if they are 0 or 1.",
     EXAMPLE {
	  "L = ideal(a^2-a-1,a^3+a+3)",
	  "L == 1",
	  "L == 0"
	  },
     HEADER3 "Matrices",
     "Two ", TO "matrices", " are equal if their entries are equal, the source and target are
     the same (including degrees), and the degree of the matrices are the same.  In this example,
     m and n have different source free modules.",
     EXAMPLE {
	  "m = matrix{{a,b},{c,a}}",
     	  "n = map(R^2,R^2,m)",
	  "m == n",
	  "source m == source n"
	  },
     "If you only want to know if they have the same entries, test the difference against zero.",
     EXAMPLE {
	  "m-n == 0"
	  },
     HEADER3 "Rings",     
     "Rings are handled in a different manner in Macaulay2.  Each time you create a 
     polynomial ring in Macaulay2, you are handed a new ring, which is not
     equal to any other ring.  For example, the rings A and B below are not
     considered the same by Macaulay2.",
     EXAMPLE {
     	  "A = QQ[x,y,z]; B = QQ[x,y,z];",
     	  "A == B"
     	  },
     HEADER3 "Modules",
     "Two ", TO "modules", " are equal if they are isomorphic as subquotients of the
     same ambient free module.",
     EXAMPLE {
      	  "image matrix {{2,a},{1,5}} == R^2",
      	  "image matrix {{2,a},{0,5}} == R^2"
	  },
     PARA,
     "It may happen that for certain types of objects, there is no method installed
     for testing mathematical equality, in which case strict equality will be tested with
     the operator ", TO "===", ".  If a test for mathematical equality is installed
     later, your results may change.",
     PARA,
     Caveat => {"Warning: whether this comparison operator returns true is not necessarily 
     related to whether the comparison operator ", TO symbol?, " returns ", TT "symbol==", "."},
     SeeAlso =>{ symbol!=, symbol===, symbol=!=, "operators" }
     }

document {
     Key => symbol "!=",
     Headline => "inequality",
     TT "x != y", " -- the negation of ", TT "x == y", ".",
     PARA,
     SeeAlso =>{ "==" }
     }

undocumented {
    (symbol**, OneExpression, Holder),
    (symbol**, Holder, Expression),
    (symbol**, QQ, RingElement),
    (symbol**, RingElement, QQ),
    (symbol**, QuotientRing, PolynomialRing),
    (symbol**, Expression, NonAssociativeProduct),
    (symbol**, Holder, Holder),
    (symbol**, QuotientRing, QuotientRing),
    (symbol**, ZZ, Matrix),
    (symbol**, Matrix, ZZ),
    (symbol**, Matrix, QQ),
    (symbol**, Thing, Expression),
    (symbol**, Expression, Thing),
    (symbol**, NonAssociativeProduct, NonAssociativeProduct),
    (symbol**, Expression, Expression),
    (symbol**, Holder, OneExpression),
    (symbol**, ZZ, RingElement),
    (symbol**, Expression, Holder),
    (symbol**, PolynomialRing, PolynomialRing),
    (symbol**, PolynomialRing, QuotientRing),
    (symbol**, NonAssociativeProduct, Expression),
    (symbol**, QQ, Matrix),
    (symbol**, NonAssociativeProduct, Holder),
    (symbol**, Holder, NonAssociativeProduct),
    (symbol**, Expression, OneExpression),
    (symbol**, OneExpression, Expression)
     }

document {
     Key => {symbol**,
    --(symbol**, ChainComplex, GradedModule),
    --(symbol**, GradedModule, ChainComplex),
    (symbol**, ProjectiveVariety, Ring),
    (symbol**, RingElement, ZZ),
    (symbol**, RingElement, Matrix),
    (symbol**, GradedModule, GradedModule),
    --(symbol**, RingMap, Module),
    --(symbol**, Module, Module),
    --(symbol**, Matrix, Ring),
    (symbol**, Thing, Thing),
    (symbol**, ChainComplex, Module),
    --(symbol**, Matrix, Matrix),
    (symbol**, ChainComplexMap, Module),
    (symbol**, Module, ChainComplexMap),
    --(symbol**, ChainComplex, ChainComplexMap),
    --(symbol**, CoherentSheaf, CoherentSheaf),
    (symbol**, GradedModule, Module),
    (symbol**, RingElement, RingElement),
    --(symbol**, Monoid, Monoid),
    --(symbol**, ChainComplexMap, ChainComplexMap),
    (symbol**, AffineVariety, Ring),
    --(symbol**, Tally, Tally),
    --(symbol**, Set, Set),
    (symbol**, Matrix, RingElement),
    --(symbol**, Ring, Ring),
    --(symbol**, Module, Ring),
    (symbol**, ChainComplex, Ring),
    --(symbol**, Matrix, Module),
    (symbol**, Module, Matrix),
    (symbol**, Module, ChainComplex),
    --(symbol**, ChainComplex, ChainComplex),
    --(symbol**, ChainComplexMap, ChainComplex),
    (symbol**, Module, GradedModule)
	  },
     Headline => "a binary operator, usually used for tensor product",
     "This here is tensor product land."
     }

document {
     Key => symbol "^**",
     Headline => "a binary operator, usually used for tensor power",
     }

document {
     Key => (symbol **, Set, Set),
     Headline => "Cartesian product",
     Usage =>  "x ** y", 
     Inputs => {
	  "x" => "",
	  "y" => ""
	  },
     Outputs => {
	  Set => "whose elements are the sequences (a,b), where a is an element
     	  of x, and b is an element of y."
	  },
     EXAMPLE "set {1,2} ** set {a,b,c}",
     "Suppose we wish to form the set of
     all triples with entries either in the set A below.",
     EXAMPLE {
	  "A = set{1,2}",
	  "A ** A ** A"
	  },
     "To make this last a set of triples, ", TO splice, " each element together.  
     Or, use ", TO (symbol^**,Set,ZZ), ".",
     EXAMPLE {
	  "(A ** A ** A)/splice",
	  "A^**3"
	  },
     SeeAlso => { Set }
     }

document {
     Key => random,
     Headline => "get a random element",
     "This function can be used to get random elements of various sorts.",
     SeeAlso => {"setRandomSeed"}
     }
document {
     Key => (random, ZZ), 
     Headline => "random integer",
	Usage => "random n",
	Inputs => {"n"=> {}},
	Outputs => {ZZ => {"a random integer in the range ", TT "0 .. n-1"}},
     Caveat => "doesn't correctly handle the case when n an integer is larger
     than 2^31-1.",
	"Here is a basic example:",
	EXAMPLE "random 57",
	"Here is an example using the ", TO "tally", " command.",
     EXAMPLE "tally apply(100, i -> random 10)",
     SeeAlso => {"setRandomSeed"}
     }

document {
     Key => (random, RR), 
     Headline => "random real number",
	Usage => "random x",
	Inputs => {"x" => {}},
	Outputs => {RR=> {"a random real number in the range ", TT "0 .. x"}},
     "Here is a basic example:",
	EXAMPLE "random pi",
     SeeAlso => {"setRandomSeed"}
     }

document {
     Key => (random, Ring),
     Headline => "random element of a ring",
     Usage => "random R",
     Inputs => {
	  "R" => null
	  },
     Outputs => {
	  { "a random element of the ring ", TT "R" }
	  },
	TT "random ZZ", " outputs a random integer from the closed interval bounded by  ", TT "-10", " and ", TT "10", ".",
	EXAMPLE "random ZZ",
	TT "random QQ", " outputs a rational number whose numerator is a random integer from the closed interval 
	bounded by ", TT "-10", " and ", TT "10", " and whose denominator is a random integer from the closed interval bounded
	by ", TT "0", " and ", TT "10", ".",
	EXAMPLE "random QQ",
     "Note: not implemented yet for ", TO "RR", ", ", TO "CC", ", and polynomial rings.",
     SeeAlso => {"setRandomSeed"}
     }

document {
     Key => (random, QuotientRing),
     Headline => "random element of a quotient ring",
	"See: ", TO (random, Ring), "."
	}

document {
     Key => (random, GaloisField),
     Headline => "random element of a Galois field",
	"See: ", TO (random, Ring), "."
	}


document {
     Key => (random, ZZ, Ring),
     Headline => "a random ring element of a given degree",
     Usage => "r = random(n,R)",
     Inputs => {
	  "n" => null,
	  "R" => null
	  },
     Outputs => {
	  {"a random homogeneous element in the ring ", TT "R", " of degree ", TT "n"}
	  },
     EXAMPLE {
	  "R = GF(9,Variable=>a)[x,y];",
	  "random(3,R)"
	  },
     SeeAlso => {"setRandomSeed"}
     }

document {
     Key => (random, List, Ring),
     Headline => "a random ring element of a given degree",
     Usage => "r = random(n,R)",
     Inputs => {
	  "n" => "a list of integers",
	  "R" => null
	  },
     Outputs => {
	  {"a random homogeneous element in the ring ", TT "R", " of multi-degree ", TT "n"}
	  },
     "The length of ", TT "n", " should be the same as ", TT "degreeLength R", ".",
     SeeAlso => {"setRandomSeed"}
     }

document {
     Key => (random, Module, Module),
     Headline => "make a random module map",
	Usage => "f = random(F,G)",
	Inputs => {
		"F" => {"a free module"},
		"G" => {"a free module"}
		},
	Outputs => {"f" => {"a random, graded, degree ", TT "0", " map, from ", TT "G", " to ", TT "F"}},
     EXAMPLE {
	  "R = ZZ/101[x,y];",
      	  "random(R^{1,2,3},R^{1,2,3})"
	  },
     SeeAlso => {"setRandomSeed"}
     }

document {
     Key => true,
     TT "true", " is a value indicating truth."
     }

document {
     Key => false,
     TT "false", " is a value indicating falsity."
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
