--		Copyright 1993-1999 by Daniel R. Grayson

undocumented toList (symbol a .. symbol Z)

document {
     Key => "length",
     Headline => "length"
     }

document {
     Key => (length, Dictionary),
     Headline => "length of a dictionary",
     Usage => "n = length d",
     Inputs => { "d" },
     Outputs => { "n" => { "the number of entries in ", TT "d" } }
     }

document {
     Key => (length, VisibleList),
     Headline => "length of a visible list",
     Usage => "n = length x",
     Inputs => { "x" },
     Outputs => { "n" => { "the number of entries in ", TT "x" } }
     }

document {
     Key => (length, GradedModule),
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

document {
     Key => parent,
     Headline => "parent type of an object",
     Usage => "X = parent x",
     Inputs => {
	  "x"
	  },
     Outputs => {
	  "X" => { "the parent class of ", TT "x" }
	  },
     "Methods for the instances of ", TT "X", " which are not found
     in ", TT "X", " itself are sought in ", TT "P", ", and its parent, and so on.
     Thus the mathematical notion of a set ", TT "P", " and a subset ", TT "X", " 
     can modeled in this way.",
     PARA{},
     "Things which don't have instances have the empty class, called
     ", TO "Nothing", " as their parent.  The parent of ", TO "Thing", "
     is ", TO "Thing", " itself (because every thing is a thing).",
     PARA{},
     "The entire structure of types and their parents can be displayed
     with ", TO "showStructure", "."
     }

document {
     Key => Type,
     Headline => "the class of all types",
     "Everything in the system is classified, and the class that a thing
     belongs to is a type.  A type is implemented as a hash table containing
     method functions for its instances.",
     PARA{},
     "The list of types known to the system is displayed below."
     }

document {
     Key => Standard,
     Headline => "the standard top level printing method",
     "This symbol is used (tentatively) as the first element of a pair to specify various top level interpreter methods.",
     SeeAlso => { TeXmacs, Print, NoPrint, BeforePrint, AfterPrint,AfterNoPrint}
     }

document {
     Key => TeXmacs,
     Headline => "the TeXmacs top level printing method",
     "This symbol is used (tentatively) as the first element of a pair to specify various top level interpreter methods, in connection with
     the use of TeXmacs as front end.",
     SeeAlso => { Standard, Print, NoPrint, BeforePrint, AfterPrint,AfterNoPrint}
     }

document {
     Key => Print,
     Headline => "top level method for printing results",
     Usage => "X#{Standard,Print} = f",
     Inputs => {
	  "X" => Type,
	  "f" => Function => { " that can print something of type ", TT "X"}
	  },
     Consequences => {
	  { "at top level, whenever it is time to print an output value of type ", TT "X", ", the function ", TT "f", " will be called" }
	  },
     "The function ", TT "f", " is responsible for printing the output prompt and for applying the ", TO "BeforePrint", " and ", TO "AfterPrint", " methods, if desired.",
     EXAMPLE "code Thing#{Standard,Print}"
     }

document {
     Key => NoPrint,
     Headline => "top level method for non-printing results",
     Usage => "X#{Standard,NoPrint} = f",
     Inputs => {
	  "X" => Type,
	  "f" => Function => { " that can accept something of type ", TT "X"}
	  },
     Consequences => {
	  {
	       "At top level, whenever it is time, as indicated by a semicolon at the end of an input line,
	       to suppress printing of an output value of type ", TT "X", ", the function ", TT "f", " will be called." }
	  }
     }


document {
     Key => BeforePrint,
     Headline => "top level method applied before printing results",
     Usage => "X#{Standard,BeforePrint} = f",
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
     Usage => "X#AfterEval = f",
     Inputs => {
	  "f" => { "a function to be applied after evaluating a top-level evaluation result ", TT "r", " of type ", TT "X", "."},
	  },
     Consequences => {
	  "The value returned result replaces the original for storing in the output variables and for printing"
	  }
     }

document {
     Key => AfterPrint,
     Headline => "top level method applied after printing",
     Usage => "X#{Standard,AfterPrint} = f",
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
	  "QQ#{Standard,AfterPrint} = r -> r;",
	  "3/4"
	  }
     }

document {
     Key => AfterNoPrint,
     Headline => "top level method applied after not printing",
     Usage => "X#{Standard,AfterNoPrint} = f",
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
     Inputs => { "n" => ZZ },
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
     SeeAlso => { 
	  "using functions",
	  "using functions with optional inputs",
	  "making functions",
	  "local variables in a function",
	  "making functions with a variable number of arguments",
	  "making functions with multiple return values",
	  "making new functions with optional arguments"
	  }
     }

document {
     Key => "->",
     Headline => "make a function",
     TT "x -> e", " -- denotes a function.  When the function is called, the initial 
     	      value of the variable x is the argument if there is just one, or
	      else is the sequence of arguments.",
     BR{},
     TT "(x) -> e", " -- denotes a function of one argument.  When the function is 
     applied to an expression w three things may happen:",
     UL {
     	  "if w is not a sequence, then the initial value of x is w;",
     	  "if w is a sequence of length one, then the initial value
     	  of x is the unique element of w; or",
     	  "if w is a sequence of length other than one, then it
     	  is an error."
	  },
     BR{},
     TT "(x,y) -> e", " -- denotes a function of two arguments.",
     PARA{},
     "Similarly for more arguments.",
     PARA{},
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
     "A list of strings containing names of directories in which ", TO "load",
     ", ", TO "input", ", ", TO "loadPackage", ", ", TO "needsPackage", ", and
     ", TO "installPackage", " should seek files.  These strings are simply
     concatenated with the filename being sought, so should include any
     necessary terminal slashes.  One further directory is implicitly searched
     first: the directory containing the current input file; when input is
     coming from the standard input, that directory is the current directory of
     the process.",
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
     PARA{},
     "One important feature of hash tables that when the keys
     are consecutive integers starting at 0, the keys are scanned
     in the natural order.",
     PARA{},
     "There is a subclass of HashTable called ", TO "MutableHashTable", "
     which consists of those hash tables whose entries can be changed.",
     PARA{},
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
	  "x" => BasicList
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
     PARA{},
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
	  "x" => BasicList
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
     PARA{},
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
     Key => keys,
     Headline => "keys used in a hash table" }
document {
     Key => (keys,HashTable),
     Usage => "keys t",
     Inputs => {
	  "t"
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
     Key => values,
     Headline => "values in a hash table" }
document {
     Key => (values,HashTable),
     Usage => "values t",
     Inputs => {
	  "t"
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
     PARA{},
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
     PARA{},
     "This function requires an immutable hash table.  To scan the pairs in
     a mutable hash table, use ", TT "scan(pairs x, f)", ".",
     PARA{},
     SeeAlso => "scan"
     }

--document { find,
--     TT "find(x,f)", " -- applies the function ", TT "f", " to each element
--     of ", TT "x", ", returning the result not equal to ", TO "null", ".
--     If no result is non-null, then it returns null."
--     }

document {
     Key => {describe,
	  (describe, PolynomialRing),
	  (describe, QuotientRing),
	  (describe, FractionField),
	  (describe, GaloisField),
	  (describe, GeneralOrderedMonoid),
	  (describe, Variety),
	  (describe, Thing)
	  },
     Headline => "real description",
     TT "describe x", " -- returns ", ofClass Net, " containing the 
     real description of ", TT "x", ", bypassing the feature which causes
     certain types of things to acquire, for brevity, the names of global variables to which
     they are assigned.  For polynomial rings, it also displays the options used at creation.",
     PARA{},
     EXAMPLE lines ///
	  R = ZZ/101[a,b,c_1,c_2];
      	  R
      	  describe R
	  toString describe R
	  toExternalString R
	  QQ[x,d,WeylAlgebra=>{x=>d}]
	  describe oo
	  ///,
     SeeAlso => {"toString", "toExternalString"}
     }

document {
     Key => input,
     Headline => "read Macaulay 2 commands and echo",
     TT "input \"f\"", " -- reads and executes the commands found in the 
     file named ", TT "f", ", echoing the input, printing the values, and incrementing
     the line number.",
     PARA{},
     "The file is sought in the directory containing the file currently being loaded, if any, and then along
     the ", TO "path", ", unless the name of the file begins with ", TT "/", ", ", TT "$", ", or ", TT "!", ".
     If the file begins with ", TT "./", " or ", TT "../", ", then it looks instead in the directory of the
     file currently being loaded (see ", TO "currentFileDirectory", ").  If no file is being loaded it will
     look in the current directory (see ", TO "currentDirectory", ").",
     PARA{},
     "If one of the top level expressions in the file evaluates to the symbol ", TO "end", "
     the reading of the file is stopped at that point.",
     PARA{},
     "If an error occurs while evaluating the expressions in the file,
     reading is stopped.",
     PARA{},
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
     PARA{},
     "The file is sought in the directory containing the file currently being loaded, if any, and then along
     the ", TO "path", ", unless the name of the file begins with ", TT "/", ", ", TT "$", ", or ", TT "!", ".
     If the file begins with ", TT "./", " or ", TT "../", ", then it looks instead in the directory of the
     file currently being loaded (see ", TO "currentFileDirectory", ").  If no file is being loaded it will
     look in the current directory (see ", TO "currentDirectory", ").",
     PARA{},
     "The file is read without echoing the input, printing the values, or incrementing the line number.",
     PARA{},
     "If one of the top level expressions in the file evaluates to the symbol ", TO "end", "
     the reading of the file is stopped at that point.",
     SeeAlso =>{ "path", "needs", "input"}
     }

document {
     Key => needs,
     Headline => "read Macaulay 2 commands if necessary",
     TT "needs \"f\"", " -- loads the file named ", TT "f", " if it hasn't 
     been loaded yet.",
     PARA{},
     SeeAlso => "load"
     }

document {
     Key => plus,
     Headline => "addition",
     TT "plus(x,y,...)", " -- yields the sum of its arguments.",
     PARA{},
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
     PARA{},
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
     PARA{},
     TT "minus(x,y)", " yields ", TT"x-y", " but see also ", TO "difference", "."
     }

document {
     Key => "--",
     Headline => "comment",
     Consequences => {"Macaulay 2 ignores commented text"},
     "Use a double hyphen ", TT "--", " to introduce a comment in the text
     of a program.  The comment runs from to the end of the line.",
     PARA{},
     "Emacs does a good job displaying the comments in a different color
     for visibility.",
     EXAMPLE {
	  "x = 1 -- this is a comment",
	  }
     }

document {
     Key => ascii,
     Headline => "ASCII character conversion" }

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
	  "s",
	  },
     Outputs => {
	  {"the list of (small integer) ASCII codes of the characters of ", TT "s"}
	  },
     EXAMPLE {///ascii "abcdef"///, ///ascii oo///, ///first ascii "A"///},
     SeeAlso => { (ascii, List) }
     }


undocumented {
     (symbol SPACE, RingMap, Number)
     }

(symbol SPACE, WrapperType, List),
(symbol SPACE, WrapperType, Sequence),
(symbol SPACE, Manipulator, Database),
(symbol SPACE, Function, Thing),
(symbol SPACE, MarkUpType, Hypertext),
(symbol SPACE, Command, Thing),
(symbol SPACE, Manipulator, Nothing),
(symbol SPACE, WrapperType, Thing),
(symbol SPACE, Expression, Expression),
(symbol SPACE, Expression, Holder),
(symbol SPACE, Holder, Expression),
(symbol SPACE, Expression, Thing),
(symbol SPACE, Thing, Expression),
(symbol SPACE, Holder, Holder),
(symbol SPACE, HeaderType, List),
(symbol SPACE, HeaderType, Sequence),
(symbol SPACE, MarkUpType, Net),
(symbol SPACE, MarkUpType, String),
(symbol SPACE, Thing, Thing),

(symbol SPACE, Ring, Array),
(symbol SPACE, Ring, OrderedMonoid),

(symbol SPACE, RingMap, RingElement),
(symbol SPACE, RingMap, ChainComplex),
(symbol SPACE, RingMap, Matrix),
(symbol SPACE, RingMap, Ideal),
(symbol SPACE, RingMap, Vector),
(symbol SPACE, RingMap, Module),
(symbol SPACE, ProjectiveHilbertPolynomial, ZZ),
(symbol SPACE, RingElement, Array),

(symbol SPACE, ChainComplex, Array),
(symbol SPACE, Module, Array),
(symbol SPACE, GradedModule, Array),

(symbol SPACE, SheafOfRings, ZZ),
(symbol SPACE, SheafOfRings, LowerBound),
(symbol SPACE, CoherentSheaf, ZZ),
(symbol SPACE, CoherentSheaf, LowerBound),

(symbol SPACE, ScriptedFunctor, Thing),

(symbol SPACE, Manipulator, File),
(symbol SPACE, Manipulator, NetFile),

(symbol SPACE, SelfInitializingType, Thing),

document {
     Key => symbol SPACE, 
     Headline => "blank operator; often used for function application, making polynomial rings",
     SeeAlso =>(symbol SPACE, Function, Thing)		    -- not really a method
     }

document {
     Key => (symbol SPACE, Function, Thing),
     Headline => "function application",
     TT "f x", " -- yields the result of applying the function ", TT "f", " to ", TT "x", ".",
     }

undocumented {
     (symbol *, Number, RingElement),
     (symbol *, RingElement, Number),
    (symbol*,  Expression, Product),
    (symbol*,  Product, Expression),
    (symbol*,  Holder, Expression),
    (symbol*,  Minus, Expression),
    (symbol*,  Product, Holder),
    (symbol*,  Holder, Holder),
    (symbol*,  ZeroExpression, Expression),
    (symbol*,  Minus, Minus),
    (symbol*,  ZZ, InfiniteNumber),
    (symbol*,  InfiniteNumber, ZZ),
    (symbol*,  Product, ZeroExpression),
    (symbol*,  ZZ, Ideal),
    (symbol*,  Product, OneExpression),
    (symbol*,  ZeroExpression, Holder),
    (symbol*,  Holder, ZeroExpression),
    (symbol*,  Holder, OneExpression),
    (symbol*,  Expression, Holder),
    (symbol*,  ZZ, GradedModuleMap),
    (symbol*,  InfiniteNumber, InfiniteNumber),
    (symbol*,  Expression, Minus),
    (symbol*,  Product, Product),
    (symbol*,  Holder, Product),
    (symbol*,  ZZ, Module),
    (symbol*,  ZZ, MonomialIdeal),
    (symbol*,  String),
    (symbol*,  ZZ, ChainComplexMap),
    (symbol*,  Expression, ZeroExpression),
    (symbol*,  Expression, OneExpression),
    (symbol*,  OneExpression, Expression),
    (symbol*,  Number, Vector)
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
    (symbol*,  ZZ, ProjectiveHilbertPolynomial),
    (symbol*,  RingElement, Vector)
	  },
     Headline => "a binary operator, usually used for multiplication",
     Usage => "x * y",
     "The return type depends on the types of x and y.  If they have the
     same type, then usually the return type is the common type of x and y.",
     PARA{},
     "Multiplication involving ring elements (including integers, rational numbers,
     real and complex numbers), ideals, vectors, matrices, modules is 
     generally the usual multiplication, or composition of functions.",
     PARA{},
     "The intersection of sets is given by multiplication.  See ", TO (symbol*,Set,Set), ".",
     EXAMPLE {
	  "set{hi,you,there} * set{hi,us,here,you}"
	  },
     PARA{},
     "Multiplication involving a list attempts to multiply each element of the list.",
     EXAMPLE {
	  "R = QQ[a..d];",
	  "a * {b,c,d}"
	  },
     PARA{},
     "Multiplication of matrices (", TO (symbol*,Matrix,Matrix),") or ring maps is the same as composition.",
     EXAMPLE {
	  "f = map(R,R,{b,c,a,d})",
	  "g = map(R,R,{(a+b)^2,b^2,c^2,d^2})",
	  "f*g",
	  "(f*g)(a) == f(g(a))"
	  },
     PARA{},
     "Submodules of modules may be produced using multiplication and addition.",
     EXAMPLE {
	  "M = R^2; I = ideal(a+b,c);",
	  "N = I*M + a*R^2",
	  "isHomogeneous N"
	  },
     SeeAlso =>{ "times", "product"}
     }

document {
     Key => symbol &,
     Headline => "a binary operator",
     }

document {
     Key => (symbol &, ZZ, ZZ),
     Headline => "logical and",
     Usage => "m & n",
     Inputs => {"m", "n"},
     Outputs => {
	  ZZ => {"obtained from the bits of the 
     	       integers ", TT "m", " and ", TT "n", " by logical 'and'."}
	  },
     EXAMPLE "(2^15 + 2^13 + 2^42) & (2^15 + 2^23 + 2^42) == 2^15 + 2^42",
     SeeAlso => {(symbol |,ZZ,ZZ),xor}
     }

document {
     Key => symbol ^^,
     Headline => "a binary operator"
     }

undocumented {
	  (symbol +, Number, RingElement),
	  (symbol +, RingElement, Number),
	  (symbol +, Thing)
	  }
document {
     Key => {symbol +,
	  (symbol +, ChainComplexMap, ChainComplexMap),
	  (symbol +, ZZ, InfiniteNumber),
	  (symbol +, ProjectiveHilbertPolynomial, ProjectiveHilbertPolynomial),
	  (symbol +, MonomialIdeal, MonomialIdeal),
	  (symbol +, RingElement, GradedModuleMap),
	  (symbol +, Matrix, Number),
	  (symbol +, ZZ, ChainComplexMap),
	  (symbol +, ChainComplexMap, ZZ),
	  (symbol +, RingElement, RingElement),
	  (symbol +, InfiniteNumber, InfiniteNumber),
	  (symbol +, Ideal, RingElement),
	  (symbol +, RingElement, ChainComplexMap),
	  (symbol +, MutableMatrix, MutableMatrix),
	  (symbol +, Matrix, Matrix),
	  (symbol +, GradedModuleMap, GradedModuleMap),
	  (symbol +, InfiniteNumber, ZZ),
	  (symbol +, GradedModuleMap, RingElement),
	  (symbol +, Number, Matrix),
	  (symbol +, Vector, Vector),
	  (symbol +, Matrix, RingElement),
	  (symbol +, RingElement, Matrix),
	  (symbol +, ChainComplexMap, RingElement)
     	  },
     Headline => "a unary or binary operator, usually used for addition",
     Usage => "+y \n x+y",
     "In most cases, this operator refers to standard addition.",
     PARA{},
     "In many cases, the integer 1 can be used as the identity, and scalars function as multiples
     of the identity.  
     For example, the 1 below refers to the identity matrix
     and the 2 to twice the identity matrix.",
     EXAMPLE lines ///
     	  M = matrix{{1,2,3},{2,3,4},{3,4,6}}
	  M+1, M+2
     ///,
     SeeAlso =>{ "plus", "sum"}
     }

undocumented {
     (symbol -, Number, RingElement),
     (symbol -, RingElement, Number),
     }

document {
     Key => {symbol -,
	  (symbol -, ChainComplexMap, ChainComplexMap),
	  (symbol -, Minus),
	  (symbol -, ProjectiveHilbertPolynomial, ProjectiveHilbertPolynomial),
	  (symbol -, ZZ, InfiniteNumber),
	  (symbol -, RingElement, GradedModuleMap),
	  (symbol -, GradedModuleMap),
	  (symbol -, Matrix, Number),
	  (symbol -, ProjectiveHilbertPolynomial),
	  (symbol -, RingElement),
	  (symbol -, RingElement, RingElement),
	  (symbol -, InfiniteNumber),
	  (symbol -, InfiniteNumber, InfiniteNumber),
	  (symbol -, ZeroExpression),
	  (symbol -, RingElement, ChainComplexMap),
	  (symbol -, ChainComplexMap),
	  (symbol -, MutableMatrix, MutableMatrix),
	  (symbol -, Matrix, Matrix),
	  (symbol -, GradedModuleMap, GradedModuleMap),
	  (symbol -, InfiniteNumber, ZZ),
	  (symbol -, GradedModuleMap, RingElement),
	  (symbol -, Number, Matrix),
	  (symbol -, MutableMatrix),
	  (symbol -, Matrix, RingElement),
	  (symbol -, Matrix),
	  (symbol -, RingElement, Matrix),
	  (symbol -, ChainComplexMap, RingElement),
	  (symbol -, Holder),
	  (symbol -, Vector, Vector)
	  },
     Headline => "a unary or binary operator, usually used for negation or subtraction",
     Usage => "-y \n x-y",
     "In most cases, this operator refers to standard negation or subtraction.",
     PARA{},
     "In many cases, the integer 1 can be used as the identity, and scalars function as multiples
     of the identity.  
     For example, the 1 below refers to the identity matrix
     and the 2 to twice the identity matrix.",
     EXAMPLE lines ///
     	  M = matrix{{1,2,3},{2,3,4},{3,4,6}}
	  M-1, M-2
     ///,
     SeeAlso =>{ "difference", "minus"}
     }

undocumented {
     (symbol /, InfiniteNumber, InfiniteNumber),
     (symbol /, RingElement, Number),
     (symbol /, Number, RingElement),
     (symbol /, ZZ, InfiniteNumber),
     (symbol /, InfiniteNumber, ZZ),
     (symbol /, EngineRing, Ideal),
     (symbol /, Expression, Holder),
     (symbol /, Holder, OneExpression),
     (symbol /, Expression, OneExpression),
     (symbol /, Holder, Expression),
     (symbol /, Holder, Holder),
     (symbol /, List, SelfInitializingType)
     }

document {
     Key => symbol /,
     Headline => "a binary operator, usually used for division",
     Usage => "x / y",
     "This operator is currently used in several ways in Macaulay2.",
     UL {
	  "division in a ring, yielding a fraction",
     	  "division in unevaluated expressions",
	  "quotient rings, modules and sheaves",
	  "applying a function or ring map to every element of a list or set"
	  },
     SeeAlso => { "//"}
     }

undocumented {
	  (symbol %, CC, CC),
	  (symbol %, CC, QQ),
	  (symbol %, CC, RR),
	  (symbol %, CC, ZZ),
	  (symbol %, Number, RingElement),
	  (symbol %, QQ, QQ),
	  (symbol %, QQ, ZZ),
	  (symbol %, RingElement, Number),
	  (symbol %, RR, QQ),
	  (symbol %, RR, RR),
	  (symbol %, RR, ZZ),
	  (symbol %, ZZ, GroebnerBasis),
	  (symbol %, ZZ, Ideal),
	  (symbol %, ZZ, MonomialIdeal),
     	  }


document {
     Key => {symbol %,
	  (symbol %, ZZ, ZZ)
	  },	  
     Headline => "a binary operator, usually used for remainder and reduction",
     Usage => "x % y",
     "The usual meaning for this operator is remainder, or normal form with respect
     to a Groebner basis.",
     PARA{},
     "For integers, the remainder is non-negative.",
     EXAMPLE lines ///
       1232132141242345 % 3457834
       (-4)%5
       ///,
     PARA{},
     "In polynomial rings, the division algorithm is used.",
     EXAMPLE lines ///
       A = ZZ[a,b]
       (3*a^3-a*b-4) % (5*a-b)
       pseudoRemainder(3*a^3-a*b-4, 5*a-b)
       B = QQ[a,b]
       (3*a^3-a*b-4) % (5*a-b)
     ///,
     "In more complicated situations, Groebner bases are usually needed.  See ",
     TO "methods for normal forms and remainder", ".",
     SeeAlso => { remainder, remainder', pseudoRemainder, "//"}
     }

undocumented {
     (symbol //, CC, CC),
     (symbol //, CC, QQ),
     (symbol //, CC, RR),
     (symbol //, CC, ZZ),
     (symbol //, InfiniteNumber, ZZ),
     (symbol //, Matrix, ZZ),
     (symbol //, QQ, QQ),
     (symbol //, Number, RingElement),
     (symbol //, QQ, ZZ),
     (symbol //, RingElement, Number),
     (symbol //, RR, QQ),
     (symbol //, RR, RR),
     (symbol //, RR, ZZ),
     (symbol //, ZZ, InfiniteNumber),
     (symbol //, ZZ, Matrix),
     (symbol //, ZZ, MonomialIdeal),
     }

     
document {
     Key => symbol //,
     Headline => "a binary operator, usually used for quotient",
     Usage => "x // y",
     "For ring elements in the integers, polynomial rings, and other rings,
     there are two types of division:  exact division, whose result is often in a larger
     field, such as the rationals or a function field, and division with remainder,
     whose result is in the same ring.  In Macaulay2, '/' denotes the first kind of division, while
     '//' denotes the latter kind.
     The following example shows
     the difference between ", TO "//", " and ", TO "/", ".",
     EXAMPLE lines ///
     	  4/2
	  4//2
     ///,
     EXAMPLE lines ///
     	  R = QQ[x];
	  (x^2-3)//(x-1)
	  (x^2-3)%(x-1)
	  (x^2-3)/(x-1)
     ///,
     SeeAlso => { "/", "%" }
     }

document {
     Key => symbol \,
     Headline => "a binary operator",
     }

document {
     Key => symbol \\,
     Headline => "a binary operator"
     }

undocumented {
     (symbol ^, Thing, QQ),
     (symbol ^, ZeroExpression, Holder),
     (symbol ^, Holder, ZeroExpression),
     (symbol ^, Holder, OneExpression),
     (symbol ^, Holder, Holder),
     (symbol ^, Expression, ZeroExpression),
     (symbol ^, ZeroExpression, Expression),
     (symbol ^, Expression, OneExpression),
     (symbol ^, Expression, Holder),
     (symbol ^, Holder, Expression),
     (symbol ^, ZeroExpression, ZeroExpression),
     }

(symbol ^, Matrix, ZZ),
(symbol ^, ChainComplexMap, ZZ),
(symbol ^, GradedModuleMap, ZZ),

(symbol ^, ChainComplex, ZZ),

(symbol ^, ChainComplexMap, Array),
(symbol ^, Module, List),


(symbol ^, Module, Array),
(symbol ^, Matrix, Array),
(symbol ^, ChainComplex, Array),
(symbol ^, GradedModuleMap, Array),
(symbol ^, Matrix, List),


(symbol ^, ScriptedFunctor, Thing),

(symbol ^, String, Sequence), -- make a net

(symbol ^, Net, ZZ), -- raise a net
(symbol ^, String, ZZ), -- make a net


document {
     Key => symbol ^,
     Headline => "a binary operator, usually used for powers",
     Usage => "x ^ y",
     PARA{},
     "This operator is used for exponentiation, making free modules and sheaves, 
     for shifting complexes left or right, for projection maps involving direct sums, 
     and for making nets.",
     }

document {
     Key => {substring,(substring, String, ZZ),(substring, ZZ, String),(substring, ZZ, ZZ, String),(substring, String, ZZ, ZZ)},
     Headline => "extract part of a string",
     TT "substring(i,n,s)", " -- yields the substring of the string ", TT "s", " starting at 
     position ", TT "i", " with length ", TT "n", ".",
     PARA{},
     "substring(i,s)   -- yields the substring of ", TT "s", " starting at position ", TT "i", " and
     continuing to the end of ", TT "s", ".",
     PARA{},
     "Positions are numbered starting at 0.",
     PARA{},
     "If the starting position ", TT "i", " is negative, it means to start from the end of the string.",
     PARA{},
     "Requests for character positions out of bounds are silently ignored.",
     PARA{},
     "In an older version of the program the string argument was placed first;
     the old way will still work."
     }

document {
     Key => {(reverse, BasicList),reverse},
     Headline => "reverse a list",
     TT "reverse v", " -- yields a list containing the elements of the 
     list ", TT "v", " in reverse order.",
     EXAMPLE "reverse {a,b,c,d}"
     }

document {
     Key => read,
     Headline => "read from a file", }
document {
     Key => (read,Sequence),
     Usage => "read()",
     Inputs => {
	  "()"
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
     Key => {get,(get, File),(get, String)},
     Headline => "get the contents of a file",
     TT "get \"f\"", " -- yields a string containing the contents of the file whose name
     is f.",
     PARA{},
     TT "get \"!f\"", " -- yields a string containing the output from the shell
     command \"f\".",
     PARA{},
     TT "get \"$hostname:service\"", " -- yields a string containing the
     input from the socket obtained by connecting to the specified host at
     the port appropriate for the specified service.  Warning: if the process
     providing the service expects interaction, it will not get it, and this
     command will hang.  This feature is not available on Sun computers,
     because Sun doesn't provide static versions of crucial libraries dealing
     with network communications.",
     PARA{},
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
     Key => {symbol !,
	  (symbol !, ZZ)},
     Headline => "factorial",
     Usage => "n!",
     Inputs => {"n"=>ZZ},
     Outputs => { ZZ => "n factorial, 1*2*3*...*n."},
     EXAMPLE "30!"
     }

document {
     Key => "not",
     Headline => "negation",
     TT "not x", " -- yields the negation of x, which must be true or false.",
     SeeAlso =>{ "and", "or" }
     }

document {
     Key => symbol |, 
     Headline => "a binary operator, often used for horizontal concatenation",
     SeeAlso => {"||"}
     }

document {
     Key => {(symbol |, List, List),
	  (symbol |, Array, Array),
	  (symbol |, Sequence, Sequence)},
     Headline => "join lists, sequences or arrays",
     Usage => "v|w",
     Inputs => {"v" => Nothing =>  {ofClass List, ", ",
	       ofClass Array, ", or ",
	       ofClass Sequence},
	  "w" => Nothing => {"of the same type as ", TT "v"}},
     Outputs => {
	  Nothing => "The join of the lists, sequences or arrays."},
     EXAMPLE "{1,2,3}|{4,5,6}",
     EXAMPLE "(a,b,c)|(1,2)",
     SeeAlso => {join}
     }

document {
     Key => {(symbol |, Net, Net),
	  (symbol |, String, String),
	  (symbol |, String, ZZ),
	  (symbol |, ZZ, String)},
     Headline => "join strings or nets",
     TT "s|t", " -- concatenates strings or nets horizontally.", 
     PARA{},
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
     Usage => "m | n",
     Inputs => {"m", "n"},
     Outputs => {
	  ZZ => {"obtained from the bits of the 
     	       integers ", TT "m", " and ", TT "n", " by logical 'or'."}
	  },
     EXAMPLE "2^42 | 2^15 == 2^42 + 2^15",
     SeeAlso => {(symbol &,ZZ,ZZ),xor}
     }

document {
     Key => {(symbol |, Matrix, Matrix),
	  (symbol |, RingElement, Matrix),
	  (symbol |, Matrix, RingElement),
	  (symbol |, RingElement, RingElement),
	  (symbol |, ZZ, Matrix),
	  (symbol |, Matrix, ZZ)
	  },
     Headline => "join matrices horizontally",
	Usage => "f = g | h",
	Inputs => {
		"g" => {"a ", TT "m", " by ", TT "n", " matrix"},
		"h" => {"a ", TT "m", " by ", TT "r", " matrix"}
		},
	Outputs => {
		"f" => {"the ", TT "m", " by ", TT "n+r", " matrix,
		     obtained from matrices ", TT "g", " and ", TT "h", " by
     		     concatenating the rows"}
	        },
     EXAMPLE lines ///
	  R = ZZ[i..p];
      	  g = matrix {{i,j},{k,l}}
      	  h = matrix {{m,n},{o,p}}
      	  f= g | h
	  ///,
     "If one of the arguments is a ring element or an integer, then it
     will be multiplied by a suitable identity matrix.",
	EXAMPLE "f | (m-n)",
	Caveat => {"It is assumed that the matrices ", TT "g", " and ", TT "h", " have the same ", TO Ring, "."},
     SeeAlso =>{(symbol ||, Matrix, Matrix), (ring, Matrix)}
     }

document {
     Key => symbol ||,
     Headline => "a binary operator, often used for vertical concatenation"
     }

document {
     Key => (symbol ||, Net, Net),
     Headline => "join nets or strings vertically",
     TT "m||n", " -- joins nets or strings by concatenating
     them vertically.  The baseline of the result is the baseline of the
     first one.",
     PARA{},
     "In this example, we build a large net with arrows to indicate
     the location of the baseline.",
     EXAMPLE {
	  ///x = "x" | "3"^1///,
      	  ///"<--- " | ( x || "" || x ) | " --->"///,
	  },
     SeeAlso => {"stack"}
     }

document {
     Key => {(symbol ||, Matrix, Matrix),
	  (symbol ||, RingElement, Matrix),
	  (symbol ||, Matrix, RingElement),
	  (symbol ||, RingElement, RingElement),
	  (symbol ||, Matrix, ZZ),
	  (symbol ||, ZZ, Matrix)
	  },
     Headline => "join matrices vertically",
	Usage => "f = g || h",
	Inputs => {
		"g" => {"a ", TT "m", " by ", TT "n", " matrix."},
		"h" => {"a ", TT "r", " by ", TT "n", " matrix."}
		},
	Outputs => {
		"f" => {"the ", TT "m+r", " by ", TT "n", " matrix,
		     obtained from matrices ", TT "g", " and ", TT "h", " by
     		     concatenating the columns."}
		},
     EXAMPLE lines ///
	  R = ZZ[i..p];
      	  g = matrix {{i,j},{k,l}}
      	  h = matrix {{m,n},{o,p}}
      	  f= g || h
	  ///,
     "If one of the arguments is a ring element or an integer, then it
     will be multiplied by a suitable identity matrix.",
	EXAMPLE "f || 33",
	Caveat => {"It is assumed that the matrices ", TT "g", " and ", TT "h", " have the same ", TO Ring, "."},
     SeeAlso =>{(symbol |, Matrix, Matrix), (ring, Matrix)}
     }

document {
     Key => {symbol ===,(symbol===,Thing,Thing)},
     Headline => "strict equality",
     TT "x === y", " -- returns true or false depending on whether the 
     expressions x and y are strictly equal.",
     PARA{},
     "Strictly equal expressions have the same type, so ", TT "0===0.", " and
     ", TT "0===0/1", " are false; the three types involved here are ", 
     TO "ZZ", ", ", TO "RR", ", and ", TO "QQ", ".",
     PARA{},
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
     SeeAlso =>{ symbol==,  symbol=!=, "operators","equality versus strict equality" }
     }

document {
     Key => {symbol =!=, (symbol=!=,Thing,Thing)},
     Headline => "strict inequality",
     TT "x =!= y", " -- returns true or false depending on whether the expressions
     x and y are strictly unequal.",
     PARA{},
     "See ", TO "===", " for details."
     }

undocumented {
         (symbol==, RingElement, Number),
         (symbol==, Number, RingElement),
         (symbol==, RingElement, Matrix),
         (symbol==, Ideal, MonomialIdeal),
         (symbol==, GradedModuleMap, ZZ),
         (symbol==, InfiniteNumber, InfiniteNumber),
         (symbol==, Holder, Holder),
         (symbol==, Equation, Expression),
         (symbol==, ZZ, Ring),
         (symbol==, ZZ, QQ),
         (symbol==, ZZ, Matrix),
         (symbol==, Matrix, ZZ),
         (symbol==, ZZ, ChainComplex),
         (symbol==, ChainComplex, ZZ),
         (symbol==, MonomialIdeal, Ring),
         (symbol==, Ring, MonomialIdeal),
         (symbol==, MonomialIdeal, ZZ),
         (symbol==, Equation, Holder),
         (symbol==, Holder, Equation),
         (symbol==, ChainComplexMap, ZZ),
         (symbol==, ZZ, MutableMatrix),
         (symbol==, ZZ, InfiniteNumber),
         (symbol==, Nothing, Nothing),
         (symbol==, Equation, Equation),
         (symbol==, GradedModuleMap, RingElement),
         (symbol==, RingElement, GradedModuleMap),
         (symbol==, String, Net),
         (symbol==, Ideal, Ring),
         (symbol==, ZZ, Ideal),
         (symbol==, Ideal, ZZ),
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
         (symbol==, Ring, Ideal),
	 (symbol==, RingMap, ZZ),
	 (symbol==, ZZ, RingMap)
	  }

document {
     Key => {symbol ==,
	  (symbol==, Matrix, Matrix),
         (symbol==, ProjectiveHilbertPolynomial, ProjectiveHilbertPolynomial),
         (symbol==, ChainComplex, ChainComplex),
         (symbol==, RingElement, RingElement),
         (symbol==, GradedModuleMap, GradedModuleMap),
         (symbol==, Ideal, Ideal),
         (symbol==, MutableMatrix, MutableMatrix),
         (symbol==, Module, Module),
	 (symbol==, Vector, Vector),
	 (symbol==, RingMap, RingMap),
	 (symbol==, BettiTally, BettiTally),
	 (symbol==, VisibleList, VisibleList)
	  },
     Headline => "equality",
     Usage => "x == y",
     "Returns true or false, depending on whether 
     the objects x and y are (mathematically) equal.  The objects x and y are
     typically numbers, elements of rings, matrices, modules, ideals, 
     chain complexes, and so on.",
     PARA{},
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
     PARA{},
     "It may happen that for certain types of objects, there is no method installed
     for testing mathematical equality, in which case strict equality will be tested with
     the operator ", TO "===", ".  If a test for mathematical equality is installed
     later, your results may change.",
     PARA{},
     Caveat => {"Warning: whether this comparison operator returns true is not necessarily 
     related to whether the comparison operator ", TO symbol?, " returns ", TT "symbol==", "."},
     SeeAlso =>{ symbol!=, symbol===, symbol=!=, "operators", "equality versus strict equality" }
     }

document {
     Key => "equality versus strict equality",
     "The operator ", TT "==", " implements ", EM "equality", ", and the operator ", TT "===", " implements ", EM "strict equality", "."
     
     -- I am writing here...

     }

document {
     Key => symbol !=,
     Headline => "inequality",
     TT "x != y", " -- the negation of ", TT "x == y", ".",
     PARA{},
     SeeAlso =>{ "==", "operators" }
     }

undocumented {
    (symbol**, OneExpression, Holder),
    (symbol**, Holder, Expression),
    (symbol**, QuotientRing, PolynomialRing),
    (symbol**, Expression, NonAssociativeProduct),
    (symbol**, Holder, Holder),
    (symbol**, QuotientRing, QuotientRing),
    (symbol**, Number, Matrix),
    (symbol**, Matrix, Number),
    (symbol**, NonAssociativeProduct, NonAssociativeProduct),
    (symbol**, Holder, OneExpression),
    (symbol**, Expression, Holder),
    (symbol**, PolynomialRing, PolynomialRing),
    (symbol**, PolynomialRing, QuotientRing),
    (symbol**, NonAssociativeProduct, Expression),
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
    (symbol**, RingElement, Number),
    (symbol**, RingElement, Matrix),
    (symbol**, GradedModule, GradedModule),
    --(symbol**, RingMap, Module),
    --(symbol**, Module, Module),
    --(symbol**, Matrix, Ring),
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
    --(symbol**, Module, Matrix),
    (symbol**, Module, ChainComplex),
    --(symbol**, ChainComplex, ChainComplex),
    --(symbol**, ChainComplexMap, ChainComplex),
    (symbol**, Module, GradedModule)
	  },
     Headline => "a binary operator, usually used for tensor product or Cartesian product",
     SeeAlso => {symbol ^**}
     }

document {
     Key => symbol ^**,
     Headline => "a binary operator, usually used for tensor or Cartesian power",
     }

document {
     Key => (symbol **, Set, Set),
     Headline => "Cartesian product",
     Usage =>  "x ** y", 
     Inputs => {
	  "x",
	  "y"
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
     Or, use ", TO (symbol^**,Tally,ZZ), ".",
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
     Key => (random, ZZ,ZZ), 
     Headline => "random integer in a range",
	Usage => "random(min,max)",
	Inputs => {"min","max"},
	Outputs => {ZZ => {"a random integer in the range ", TT "min .. max"}},
     Caveat => "doesn't correctly handle the case when the length of the range is larger
     than 2^31-1.",
	EXAMPLE lines ///
	    tally apply (10, i -> random(5,2^34+5))
	    ///,
	"Here is an example using the ", TO "tally", " command.",
     EXAMPLE "tally apply(100, i -> random(10,15))",
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
	  "R"
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
     Inputs => { "n", "R" },
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
     Inputs => { "n" => "a list of integers", "R" },
     Outputs => {
	  {"a random homogeneous element in the ring ", TT "R", " of multi-degree ", TT "n"}
	  },
     "The length of ", TT "n", " should be the same as ", TT "degreeLength R", ".",
     SeeAlso => {"setRandomSeed"}
     }

document {
     Key => {(random, Module, Module),[random, MaximalRank]},
     Headline => "make a random module map",
     Usage => "f = random(F,G)",
     Inputs => {
	  "F" => {"a free module"},
	  "G" => {"a free module"},
	  MaximalRank => {"whether to ensure that the resulting map has maximal rank"}
	  },
     Outputs => {"f" => {"a random, graded, degree ", TT "0", " map, from ", TT "G", " to ", TT "F"}},
     EXAMPLE lines ///
	  R = ZZ/101[x,y];
      	  random(R^{1,2,3},R^{1,2,3})
	  random(ZZ^3,ZZ^6,MaximalRank=>true)
	  ///,
     Caveat => {
	  "Over a polynomial ring, specifying ", TT "MaximalRank=>true", " will yield a non-homogeneous matrix."
	  },
     SeeAlso => {"setRandomSeed"}
     }

document {
     Key => (random,List),
     Headline => "shuffle a list randomly",
     Usage => "random x",
     Inputs => { "x" },
     Outputs => { List => {"a new list containing the elements of ", TT "x", " in a shuffled random order"} },
     EXAMPLE lines ///
     	  random toList (0 .. 12)
     ///
     }

document {
     Key => {scanLines, (scanLines,Function,String)},
     Headline => "apply a function to each line of a file",
     Usage => "scanLines(f,fn)",
     Inputs => { "f", "fn" => "the name of a file" },
     Outputs => {
	  {
	       "returns ", TO "null", " unless the function uses ", TT "break x", " with a non-null value for ", TT "x", ",
	       in which case scanning stops and ", TT "x", " is returned immediately"
	       }
	  }
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
