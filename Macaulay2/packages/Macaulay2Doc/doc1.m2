scan((
     -- some optional arguments
	  FollowLinks,Hilbert,Options,InstallPrefix,PackagePrefix,Exclude,Encapsulate,CompleteIntersection,MaximalRank,MaxReductionCount,Reverse,
	  Algorithm,DebuggingMode,Dense,DivideConquer,First,Format,GBDegrees,Hermitian,CoefficientRing,Undo,SourceCode,Description,Variables,
	  Boxes,BaseRow,HorizontalSpace,VerticalSpace,Alignment,Minimize,FileName,Unmixed,Decompose, AbsoluteLinks, RunExamples,
	  CheckDocumentation, IgnoreExampleErrors, MakeDocumentation, MakeInfo, MakeLinks, RemakeAllDocumentation, RerunExamples, UserMode, Generic,
	  KeepZeroes,Heading, ClosestFit, Density, Height, Norm, UpperTriangular,EncapsulateDirectory, Local, Binomial, Monomial, DegreeMap, DegreeLift,
	  Join ),
     s -> if s =!= null then document {
	  Key => s,
	  Headline => "name for an optional argument",
	  "A symbol used as the name of an optional argument, for some function(s)."
	  }
     )

scan((
     	  Center, Right, Left, Quotient, Intersection
	  ),
     s -> document {
	  Key => s,
	  Headline => "value for an optional argument",
	  "A symbol used as the value of an optional argument, for some function(s)."
	  }
     )

document {
     Key => "initial help",				    -- display by the help command by default
     "Welcome to Macaulay 2",
     PARA{},
     "Try entering '2+2' at your next input prompt, which begins with ", TT "i", ".
     The two output prompts begin with ", TT "o", ".  The first one, with the
     equal sign, '=', gives the value computed from your input, and the second one, with
     the colon, ':', tells what type of thing the value is.",
     PARA{},
     "Type one of these commands to get started reading the documentation:",
     UL {
     	  SPAN (///copyright///, ///                         -- the copyright///),
     	  SPAN (///help "Macaulay2"///, ///                  -- top node of the documentation.///),
     	  SPAN (///help "reading the documentation"///, ///  -- ///),
     	  SPAN (///help "getting started"///, ///            -- ///),
     	  SPAN (///help "a first Macaulay 2 session"///, /// -- ///),
     	  SPAN (///help x///, ///                            -- display the documentation for ///, TT ///x///),
	  SPAN (///printWidth = 80///, ///                   -- set print width to 80 characters///),
     	  SPAN (///viewHelp///, ///                          -- view documentation in a browser///),
     	  SPAN (///viewHelp x///, ///                        -- view documentation on ///, TT ///x///, /// in browser///),
	  },
     "To read the documentation in info form, in case you happen to be running Macaulay 2 in a 
     terminal window, replace ", TO "help", " by ", TO "infoHelp", " in any of the commands 
     above."
     }
document {
     Key => {help,(help,Array),(help,DocumentTag),(help,List),(help,Sequence),(help,String),(help,Symbol),(help,Thing)},
     Headline => "help command",
     "Various ways to get help:",
     UL {
     	  (TT "help \"Macaulay 2\"", " -- displays the base of the online documentation tree."),
     	  (TT "help X", " -- displays the online documentation for ", TT "X"),
	  (TT "help methods X", " -- displays help messages about the methods usable with things of type ", TT "X"),
	  (TT "help methods res", " -- displays help messages about the methods usable with the function ", TT "res"),
	  (TT "help methods symbol **", " -- displays help messages about the methods usable with the operator ", TT "**"),
	  (TT "help methods (res, X)", " -- displays help messages about the methods usable with the function ", TT "res", " and a thing of class ", TT "X"),
	  (TT "help methods (symbol **, X)", " -- displays help messages about the methods usable with the operator ", TT "**", " and a thing of class ", TT "X"),
	  (TT "help methods (X, Y)", " -- displays help messages about the methods usable with a thing of class ", TT "X", " and a thing of class ", TT "Y")
	  },
     "The ", TT "help", " command is used to display online documentation.  Use ", TO viewHelp, " to display the corresponding
     documentation in your web browser.",
     EXAMPLE {
	  "help",
	  "help ideal",
	  "help (ideal,List)"
	  },
     "Some other potential help topics:",
     UL {
	  -- Mike wanted this: TT "help \"monomial orders\"",
	  TT "help \"Groebner bases\"",
	  TT "help \"multigraded polynomial rings\""
	  },
     SeeAlso => {viewHelp, infoHelp,  apropos, code, examples, "reading the documentation"}
     }
document {
     Key => viewHelp,
     Headline => "view online doc with a web browser",
     Usage => "viewHelp\nviewHelp s",
     Inputs => {
	  "s" => "a descriptor for a documentation node (see below for examples)"
	  },
     Consequences => {
	  {"The given documentation page is displayed in your default web browser.  If
	  the browser is not running, it is started.  If no argument is given to ", TT "viewHelp", 
	  "then the top page of your local html documentation is displayed."}},
     "Some example uses:",
     UL {
	  (TT "viewHelp", " -- top of local copy of the documentation, including installed packages"),	  
	  (TT "viewHelp \"Macaulay 2\"", " -- top of Macaulay2 doc"),
	  (TT "viewHelp ideal", " -- online doc for the 'ideal' function"),
	  (TT "viewHelp \"matrices\"", " -- overview of matrices in Macaulay 2"),
	  (TT "viewHelp (ideal,List)", " -- online doc for ideal(List) method"),
	  (TT "viewHelp (diff,Matrix,Matrix)", " -- online doc for the diff function taking two matrices as arguments"),
	  (TT "viewHelp [gb,DegreeLimit]", " -- view doc for the optional argument DegreeLimit to gb function"),
	  (TT "viewHelp (symbol**,Matrix,Matrix)", " -- view doc for Matrix**Matrix")
	  },
     Caveat => {"The ", TO help, " command allows other possible arguments, such as ", 
	  TT "help methods ideal", ", but for ", TT "viewHelp", " the argument ", TT "s",
	  " must refer to ony one web page."},
     SeeAlso => {apropos, examples, help, infoHelp, "reading the documentation"}
     }
document {
     Key => infoHelp,
     Headline => "view online doc with info",
     Usage => "infoHelp s",
     Inputs => {
	  "s" => "a descriptor for a documentation node (see below for examples)"
	  },
     Consequences => {
	  "The given documentation page is displayed using info, if you are running
	  Macaulay2 in a terminal window."},
     "Some example uses:",
     UL {
	  (TT "infoHelp \"Macaulay 2\"", " -- top of Macaulay2 doc"),
	  (TT "infoHelp ideal", " -- online doc for the 'ideal' function"),
	  (TT "infoHelp \"matrices\"", " -- overview of matrices in Macaulay 2"),
	  (TT "infoHelp (ideal,List)", " -- online doc for ideal(List) method"),
	  (TT "infoHelp (diff,Matrix,Matrix)", " -- online doc for the diff function taking two matrices as arguments"),
	  (TT "infoHelp [gb,DegreeLimit]", " -- view doc for the optional argument DegreeLimit to gb function"),
	  (TT "infoHelp (symbol**,Matrix,Matrix)", " -- view doc for Matrix**Matrix")
	  },
     "While in the ", TT "info", " program, there are many ways to navigate and search.
     Besides the arrow keys to move around on the page, here is a list of the most useful key strokes:",
     UL {
	  (TT "?", " -- display information about all of the possible keystrokes"),
	  (TT "q", " -- quit info, return to Macaulay2"),
	  (TT "n", " -- go to the next documentation node"),
	  (TT "p", " -- go to the revious node"),
	  (TT "m", " -- follow the menu link"),
	  (TT "r", " -- follow a cross-reference"),
	  (TT "l", " -- go to the last node visited"),
	  },
     Caveat => {"The ", TO help, " command allows other possible arguments, such as ", 
	  TT "help methods ideal", ", but ", TT "infoHelp", " requires that the argument ", TT "s",
	  " refers to only one documentation page."},
     SeeAlso => {apropos, examples, help, viewHelp, "reading the documentation"}
     }
document {
     Key => lookup,
     Headline => "look up methods",
     TT "lookup(M,A)", " -- provides the binary method named ", TT "M", " for class ", TT "A", ".
     The first place to look is ", TT "A#M", ".  The search proceeds with
     the parent of ", TT "A", ", and so on.",
     PARA{},
     TT "lookup(M,A,B)", " -- provides the binary method named ", TT "M", " for ", TT "(A,B)", ".
     The first place to look is ", TT "Y#(M,A,B)", " where ", TT "Y", " is the younger
     of ", TT "A", " and ", TT "B", ".  The search proceeds next with the parent of ", TT "B", ", 
     and so on. ",
     PARA{},
     TT "lookup(M,A,B,C)", " -- provides the ternary method named ", TT "M", " for
     ", TT "(A,B,C)", ".  The first place to look is ", TT "Y#(M,A,B,C)", " where ", TT "Y", " 
     is the youngest of ", TT "A", ", ", TT "B", ", and ", TT "C", ".  The search proceeds with 
     the parent of ", TT "C", ", and so on.",
     PARA{},
     TT "lookup(M,A,B,C,D)", " -- provides the quaternary method named ", TT "M", " for
     ", TT "(A,B,C,D)", ".  The first place to look is ", TT "Y#(M,A,B,C,D)", " where ", TT "Y", " 
     is the youngest of ", TT "A", ", ", TT "B", ", ", TT "C", ", and ", TT "D", ".  The search proceeds with 
     the parent of ", TT "D", ", and so on.",
     PARA{},
     TT "lookup x", " -- where ", TT "x", " is a symbol or function, returns ", TT "x", ".",
     PARA{},
     "If no method is found, then ", TO "null", " is returned.",
     PARA{},
     SeeAlso => {"#", "installMethod", "youngest"}
     }
document {
     Key => installMethod,
     Headline => "install methods",
     PARA{},
     "Most users will use a different way of installing methods.",
     PARA{},
     TT "installMethod(M,A,f)", "     -- installs a function ", TT "f", " as a unary method for
     the class ", TT "A", " under the name ", TT "M", ".  This is the same as ", "M A := f", " 
     if ", TT "M", " is a function.  As currently implemented, this is also the same 
     as ", TT "A#M = f", ".",
     PARA{},
     TT "installMethod(M,A,B,f)", "   -- installs a function ", TT "f", " as a binary method for
     classes ", TT "A", " and ", TT "B", " under the name ", TT "M", ".  This is the same as 
     ", TT "M(A,B) := f", " if ", TT "M", " is a
     function, or the same as ", TT "A M B := f", " if ", TT "M", " is a binary operator. As currently
     implemented, this is also the same as ", TT "Y#(M,A,B) = f", ", where ", TT "Y", " is 
     the younger of ", TT "A", " and ", TT "B", ".",
     PARA{},
     TT "installMethod(M,A,B,C,f)", " -- installs a function ", TT "f", " as a ternary method 
     for classes ", TT "A", ", ", TT "B", ", and ", TT "C", " under the name ", TT "M", ".  
     This is the same as ", TT "M(A,B,C) := f", " if ", TT "f", "
     is a function.  As currently implemented, this is also the same as
     ", TT "Y#(M,A,B,C) = f", ", where ", TT "Y", " is the youngest of ", TT "A", ", ", TT "B", ", 
     and ", TT "C", ".",
     PARA{},
     SeeAlso =>{"#", "lookup",  "new"}
     }
document {
     Key => "of",
     Headline => "a keyword",
     TT "of", " -- a keyword used with ", TO "new", "."
     }
document {
     Key => NewMethod,
     TT "NewMethod", " -- a symbol used as a method name in conjunction with
     the ", TO "new", " operator.",
     PARA{},
     "Intended for internal use only."
     }
document {
     Key => NewOfMethod,
     TT "NewOfMethod", " -- a symbol used as a method name in conjunction with
     the ", TO "new", " operator.",
     PARA{},
     "Intended for internal use only."
     }
document {
     Key => NewFromMethod,
     TT "NewFromMethod", " -- a symbol used as a method name in conjunction with
     the ", TO "new", " operator.",
     PARA{},
     "Intended for internal use only."
     }
undocumented {
     (NewFromMethod, Command, String),
     (NewFromMethod, Command, Function),
     (NewFromMethod, BR, List),
     (NewFromMethod, DocumentTag, List),
     (NewFromMethod, HR, List),
     (NewFromMethod, HREF, List),
     (NewFromMethod, MarkUpType, List),
     (NewFromMethod, Module, List),
     (NewFromMethod, TO, List),
     (NewFromMethod, TO2, List),
     (NewFromMethod, TOH, List),
     (NewFromMethod, MarkUpType, Sequence),
     (NewFromMethod, Module, Sequence),
     (NewFromMethod, TO2, Sequence),
     (NewFromMethod, TO, Hypertext),
     (NewFromMethod, TOH, Hypertext),
     (NewFromMethod, MarkUpType, Thing),
     (NewFromMethod, Matrix, MutableMatrix),
     (NewFromMethod, Matrix, Vector),
     (NewFromMethod, MutableMatrix, Matrix),
     (NewFromMethod, Vector, Matrix),
     (NewFromMethod, Eliminate, ZZ),
     (NewFromMethod, UL, VisibleList)
     }
document {
     Key => NewOfFromMethod,
     TT "NewOfFromMethod", " -- a symbol used as a method name in conjunction with
     the ", TO "new", " operator.",
     PARA{},
     "Intended for internal use only."
     }
document {
     Key => Thing,
     Headline => "the class of all things",
     "Everything in Macaulay 2 is a ", ITALIC "thing", ".  This 
     includes numbers, strings, and lists.  More complicated things such as 
     polynomials, groups, rings, and chain complexes are implemented
     as ", ITALIC "hash tables", ".  See ", TO "Type", " for information 
     about what types of things there are."
     }
document {
     Key => Nothing,
     Headline => "the empty class",
     "This class is useful for representing the class of an argument
     which is missing.  It is also used as the parent for those things which
     are not themselves types, i.e., which do not have instances." 
     }
document {
     Key => Option,
     Headline => "the class of all pairs x => y",
     PARA{},
     "Such pairs are used as optional arguments for functions.  There
     is also a way to make new hash tables with ", TO "new", " by
     providing a list of option pairs.",
     PARA{},
     EXAMPLE {
	  "a => 5",
      	  "peek (a => 5)",
	  "new HashTable from {a => 5, b => 7}",
	  },
     PARA{},
     "These pairs are implemented as lists, so that if ", TT "z", " is ", TT "x => y", ", then 
     ", TT "x", " is ", TT "z#0", " and ", TT "y", " is ", TT "z#1", ".",
     PARA{},
     SeeAlso => { "=>"}
     }
document {
     Key => (NewFromMethod, HashTable, List),
     Headline => "make a hash table from a list",
     TT "new HashTable from x", " -- produce a new hash table from a
     list ", TT "x", ".",
     PARA{},
     "Elements of ", TT "x", " which are options, ", TT "k => v", " cause
     the value ", TT "v", " to be stored in ", TT "x", " under the key ", TT "k", ".",
     SeeAlso => "hashTable"
     }
document {
     Key => OptionTable,
     Headline => "the class of hash tables for optional arguments",
     SeeAlso => ">>" }
document {
     Key => {(symbol >>, OptionTable, Function),
	  (symbol >>, List, Function),(symbol >>, Boolean, Function)},
     Headline => "attaching options to a function",
     Usage => "g = defs >> fun",
     Inputs => {
	  "defs" => { "(or ", ofClass List, " of option pairs), 
	       whose keys are the names of the optional arguments, and whose values are the
	       corresponding default values.  Alternatively, if ", TT "defs", " is ", TO "true", ",
	       then all optional arguments are accepted and no defaults are provided."},
	  "fun" => { "a function that expects optional arguments" }
	  },
     Outputs => {
	  "g" => { "a new function that pre-processes the optional arguments and then calls ", TT "fun" }
	  },
     PARA {
	  "The new function ", TT "g", " works as follows.
	  The value of ", TT "g args", ", say, is obtained by evaluation of 
	  ", TT "(fun opts)(args')", ", where ", TT "args'", " is obtained from
	  ", TT "args", " by removing the options of the form ", TT "X=>A", " 
	  (where ", TT "X", " is a name of an optional argument), and ", TT "opts", " 
	  is a hash table of the same form as ", TT "defs", " in which the default
	  values have been replaced by the user-supplied values, e.g., the
	  value stored under the key ", TT "X", " has been replaced by
	  ", TT "A", "."},
     PARA { "Remark: ", TT "defs", " can also be simply a list of options." },
     PARA {
     	  "In the following example we use a simple definition for ", TT "fun", "
     	  so we can see everything that ", TT "fun", " receives."},
     EXAMPLE lines ///
	  g = {a=>1, b=>2} >> opts -> args -> {args, opts}
	  g x
	  g(x,y,b=>66)
	  g(t,u,a=>44,b=>77)
	  h = true >> opts -> args -> {args, opts}
	  h(t,u,c=>55)
	  ///,
     SeeAlso => {"making new functions with optional arguments", "OptionTable", "Option", "=>"}
     }

document {
     Key => {(symbol ++, OptionTable, OptionTable),(symbol ++, OptionTable, List)},
     Usage => "x ++ y",
     Inputs => { "x", "y" },
     Outputs => {
	  {"a new ", TO "OptionTable", " obtained by merging x and y, preferring the default values provided by ", TT "y"}
	  },
     PARA {
	  "Alternatively, y can be a list of options."
	  },
     EXAMPLE lines ///
     	  options res ++ { Alpha => Omega }
     ///,
     SeeAlso => { Option }
     }

document {
     Key => "typicalValues",
     Headline => "types of values returned by functions",
     "A hash table used to store information about the type of values
     typically returned by functions and methods.",
     PARA{},
     "This information is used only to build documentation automatically.",
     EXAMPLE "typicalValues#isRing",
     SeeAlso => { "specifying typical values" }
     }

document {
     Key => {size,(size, RingElement)},
     Headline => "the size of an object",
     TT "size x", " -- returns the size of ", TT "x", " which usually gives
     a rough indication of memory space required to store the object ", TT "x", ".",
     PARA{},
     "For a polynomial, the size is the number of terms.",
     PARA{},
     "This function should be replaced by something more generally useful."
     }
document {
     Key => {baseName,(baseName, Symbol),(baseName, IndexedVariable),(baseName, RingElement),
	  (baseName, Subscript),(baseName, Holder),(baseName,IndexedVariableTable)},
     Headline => "the base name of a generator",
     TT "baseName x", " -- returns the variable or symbol upon which an indexed variable table
     or a generator of a monoid or polynomial ring is based.",
     EXAMPLE lines ///
     	  R = QQ[x_1 .. x_4,y]
	  y
	  baseName y
	  x_1
	  baseName x_1
	  x
	  baseName x
     ///
     }
document {
     Key => {isIsomorphism,(isIsomorphism, Matrix)},
     Headline => "whether a map is an isomorphism",
     TT "isIsomorphism f", " -- whether the map f of modules is an isomorphism."
     }
document {
     Key => {leadCoefficient,(leadCoefficient, RingElement)},
     Headline => "the leading coefficient",
     TT "leadCoefficient f", " -- return the leading coefficient of the polynomial
     or vector ", TT "f", ".",
     PARA{},
     SeeAlso => {"leadTerm", "leadMonomial", "leadComponent"}
     }
document {
     Key => {leadComponent,(leadComponent, Matrix),(leadComponent, Vector)},
     Headline => "the leading component of a vector or matrix",
     TT "leadComponent f", " -- return the leading component of the vector f,
     i.e., the integer i so that f_i is the first nonzero component of f.",
     PARA{},
     SeeAlso => {"leadTerm", "leadCoefficient", "leadMonomial"}
     }
document {
     Key => {leadMonomial,(leadMonomial, RingElement)},
     Headline => "the leading monomial",
     TT "leadMonomial f", " -- return the leading monomial of the polynomial
     or vector f, as a ring element.  (Warning: in version 0.9.2, a
     monoid element was returned.)",
     PARA{},
     SeeAlso => {"leadTerm", "leadCoefficient", "leadCoefficient"}
     }
document {
     Key => {flatten,(flatten, VisibleList)},
     Headline => "flatten a list by unnesting lists",
     TT "flatten m", " -- produces a new list from ", TT "m", " by
     effectively removing the braces surrounding the elements
     of any elements of m which happen to be lists.  Also works
     for matrices.",
     PARA{},
     EXAMPLE "flatten {{2,3,4},{{5}},6}"
     }
document {
     Key => (flatten,Matrix),
     Headline => "puts the columns of a matrix into a single row",
	Usage => "g = flatten f",
	Inputs => {
		"f" => {"a ", TT "m", " by ", TT "n", " matrix."}
		},
	Outputs => {
		"g" => { "a ", TT "1", " by ", TT "m*n", " matrix."}
		},
     TT "flatten f", " produces a new matrix from ", TT "f", " by placing the entries of the columns of ", TT "f", " all in a single row, one after the other.",
     EXAMPLE {
		"R = ZZ/101[x,y,z];",
		"f = matrix {{2, x},{y^2, 23},{z, z^3}}",
		"flatten f"
		},
	"Note that this is the matrix given by unnesting the list which was used to enter the matrix.",
     }
document {
     Key => {image,(image, Matrix),(image, ChainComplexMap),(image, GradedModuleMap),(image, RingElement)},
     Headline => "image of a map",
     TT "image h", " -- yields the image of the homomorphism ", TT "h", ".",
     PARA{},
     "The result will be a submodule of the target of h",
     PARA{},
     "If h is a ring element, it is interpreted as a one by one matrix."
     }

document {
     Key => {scanKeys,(scanKeys, Database, Function),(scanKeys, HashTable, Function)},
     Headline => "apply a function to each key in a hash table or database",
     TT "scanKeys(x,f)", " -- apply the function ", TT "f", " to each key used in the
     hash table or database ", TT "x", ".",
     PARA{},
     "This function requires an immutable hash table.  To scan the keys in
     a mutable hash table, use ", TT "scan(keys x, f)", "."
     }
document {
     Key => {scanValues,(scanValues, HashTable, Function)},
     Headline => "apply a function to each value in a hash table",
     TT "scanValues(x,f)", " -- apply the function ", TT "f", " to each value
     appearing in the hash table ", TT "x", ".",
     PARA{},
     "This function requires an immutable hash table.  To scan the values in
     a mutable hash table, use ", TT "scan(values x, f)", "."
     }
document {
     Key => GlobalAssignHook,
     Headline => "hook for assignment to global variables",
     Usage => "X.GlobalAssignHook = f",
     Inputs => {
	  "X" => Type,
	  "f" => Function => " of two variables: the symbol to which a value is about to be assigned, and the value being assigned",
	  },
     Consequences => {
	  {TT "f", " will be called just before an assignment to a global variable of an instance of class ", TT "X", " occurs"}
	  },
     PARA {
	  "This technique is used, for example, for instances of ", TO "Type", " and ", TO "Ring", "
	  to arrange for the name of the type or ring to be set to the name
	  of the global variable to which it is first assigned.  The functions
	  ", TO "globalAssignFunction", " and ", TO "globalReleaseFunction", " may installed
	  as methods for this purpose."},
     EXAMPLE {
	  ///RR.GlobalAssignHook = (sym,val) -> << "--about to assign " << val << " to " << sym << endl;///,
          "a = 4.5",
	  },
     SeeAlso => {"GlobalReleaseHook"}
     }
document {
     Key => GlobalReleaseHook,
     TT "GlobalReleaseHook", " -- a method name which is consulted when an
     assignment to a global variable is about to occur.",
     PARA{},
     "The method should be a function of two variables: the symbol to which
     a value is being assigned, and the old value about to be overwritten.  
     The method should be stored under the name ", TT "GlobalReleaseHook", " in the
     class of the old value.  It is executed before the assignment occurs,
     and before the execution of ", TO "GlobalAssignHook", ".",
     PARA{},
     EXAMPLE {
	  ///RR.GlobalReleaseHook = (sym,val) -> << concatenate (
     "assigning ", toString val, " to ", toString sym
     ) << endl///,
          "a=4.5",
      	  "a=5.4",
	  },
     SeeAlso => "GlobalAssignHook"
     }
document {
     Key => {complete,(complete, GradedModule),(complete, ChainComplexMap)},
     TT "complete C", " -- completely fills out the chain complex C by
     calling upon the engine to provide the maps and modules computed
     by ", TO "resolution", ".",
     PARA{},
     "This is mainly intended for developers of new routines for chain
     complexes which have to make use of their internal structure.
     Before running this routine, it is not possible to determine which
     spots in a chain complex are actually occupied by modules or maps."
     }
document {
     Key => (drop, BasicList, List),
     Usage => "drop(v,{m,n})",
     Inputs => { "v", Nothing => { TT "{m,n}", ", a pair of natural numbers" } },
     Outputs => { { "the list obtained by omitting those elements of the list ", TT "v", " in positions ", TT "m", " through ", TT "n" } },
     SeeAlso => {(take, BasicList, List)},
     EXAMPLE "drop({a,b,c,d,e},{2,4})"
     }
document {
     Key => (drop, BasicList, ZZ),
     Usage => "w = drop(v,n)",
     Inputs => {
	  "v",
	  "n"
	  },
     Outputs => {
	  {"the list obtained by omitting the first ", TT "n", " elements of the list ", TT "v", " if ", TT "n", " is positive, or
	       the last ", TT "-n", " elements if ", TT "n", " is negative."}
	  },
     EXAMPLE { "drop({a,b,c,d,e},2)", "drop({a,b,c,d,e},-2)" }
     }
document { Key => drop,
     Headline => "drop some elements from a list or sequence", SeeAlso => "take" }
document {
     Key => {(options, Function),(options, Command),(options, Sequence)},
     Headline => "get optional arguments and default values for a function which accepts them",
     Usage => "options f",
     Inputs => { "f" },
     Outputs => {
	  { "a hash table whose keys are the names of the optional arguments accepted by 
	       the function ", TT "f", " and whose values are the corresponding default values;
	       or ", TO "true", ", if the function accepts all option names and provides no default values" }
	  },
     EXAMPLE {
	  "options res",
	  "options codim"
	  }
     }
document {
     Key => {(options, Ring),(options, PolynomialRing),(options, QuotientRing)},
     Headline => "get values used for optional arguments",
     TT "options R", " -- returns the options used when the polynomial
     ring ", TT "R", " was created."
     }
document { Key => (options, MarkUpType),
     "Optional arguments of mark up types allow attributes to be added to html elements.",
     EXAMPLE lines ///
     	  DIV
     	  options DIV
	  d = DIV { "class" => "examples", "hi there" }
	  html d
	  net d
     ///}
document { Key => (options, Package),
     EXAMPLE lines ///
     	  options Core
     ///}
document {
     Key => {(options, Monoid),(options, GeneralOrderedMonoid)},
     Headline => "get values used for optional arguments",
     TT "options M", " -- returns the options used when the monoid ", TT "M", " 
     was created."
     }
document {
     Key => options,
     Headline => "get options" }

document {
     Key => symbol #,
     Headline => "length, or access to elements",
     "The precedence of ", TT "#", " when used as a binary operator is high,
     as high as ", TT ".", ", but the precedence when used as a unary operator
     is lower, as low as adjacency or function application.",
     SeeAlso =>{ "#?" }
     }
document {
     Key => (symbol #, BasicList),
     Headline => "length",
     TT "#x", " -- provides the length of a list.",
     }
document {
     Key => (symbol #, HashTable),
     Headline => "length",
     TT "#x", " -- provides the number of key-value pairs recorded
     in a hash table.",
     }
document {
     Key => (symbol #, Set),
     Headline => "cardinality",
     TT "#x", " -- provides the number of elements in the set ", TT "x", "."
     }
document {
     Key => (symbol #, String),
     Headline => "length",
     TT "#x", " -- provides the length of a string.",
     }
document {
     Key => (symbol #, HashTable, Thing),
     Headline => "get value from hash table",
     TT "x#i", " -- provides the value associated to the key ", TT "i", " in the hash table
     ", TT "x", ".",
     PARA{},
     "Assignment to ", TT "x#i", " can change the value if ", TT "x", " is mutable.",
     EXAMPLE {
	  "x = new MutableHashTable",
	  "x#i = p",
	  "x#i",
	  },
     SeeAlso => {(symbol #?, HashTable, Thing), "hashing"}
     }
document {
     Key => (symbol #, Database, String),
     Headline => "get value from database",
     TT "x#i", " -- provides the value associated to the key ", TT "i", " in the database
     ", TT "x", ".",
     SeeAlso => {(symbol #?, Database, String)}
     }
document {
     Key => (symbol #, String, ZZ),
     Headline => "get character from string",
     TT "x#i", " -- provides the ", TT "i", "-th character of the string ", TT "x", ",
     as a string of length 1, if there is one.",
     PARA{},
     "If ", TT "i", " is out of range, a string of length 0 is returned.
     If  ", TT "i", " is negative, then the ", TT "i", "-th character
     from the end is provided.",
     SeeAlso => {(symbol #?, String, ZZ)}
     }
document {
     Key => (symbol #, BasicList, ZZ),
     Headline => "get element from list",
     Usage => "x#i",
     Inputs => { "x", "i" },
     Outputs => { { "the ", TT "i", "-th element of the list ", TT "x" }},
     SeeAlso => {(symbol _, VisibleList, ZZ)},
     PARA{
     	  "The entries of the list are numbered starting with 0.  If  ", TT "i", " 
          is negative, then the ", TT "i", "-th entry counting from the end is provided.
          If ", TT "i", " is out of range, an error is signaled." },
     PARA{
	  "Assignment to ", TT "x#i", " can change the value if ", TT "x", " is
          mutable, i.e., an instance of the class ", TO "MutableList", "." },
     EXAMPLE lines ///
          x = a .. z
	  x#12
	  y = new MutableList from x
	  y#12 = foo
	  toSequence y
     ///
     }
document {
     Key => (symbol #?, HashTable, Thing),
     Headline => "check for value in hash table",
     TT "x#?i", " -- tells whether there is a value associated to the
     key ", TT "i", " stored in the hash table ", TT "x", ".",
     SeeAlso => {(symbol #, HashTable, Thing), "hashing"}
     }
document {
     Key => (symbol #?, Database, String),
     Headline => "check for value in database",
     TT "x#?i", " -- tells whether there is a value associated to the string
     ", TT "i", " in the database ", TT "x", ".",
     SeeAlso => {(symbol #, Database, String)}
     }
document {
     Key => (symbol #?, String, ZZ),
     Headline => "check for character in string",
     TT "x#?i", " -- tells whether there is an ", TT "i", "-th character in
     the string ", TT "x", ".",
     EXAMPLE {
	  ///"asdf" #? 2///,
	  ///"asdf" #? 22///
	  },
     SeeAlso => {(symbol #, String, ZZ)}
     }
document {
     Key => (symbol #?, BasicList, ZZ),
     Headline => "check for element in list",
     TT "x#?i", " --  tells whether there is an ", TT "i", "-th element in
     the list ", TT "x", ".",
     EXAMPLE {
	  ///{a,b,c} #? 2///,
	  ///{a,b,c} #? 22///
	  },
     SeeAlso => {(symbol #, BasicList, ZZ)}
     }
document {
     Key => symbol #?,
     Headline => "check for presence of elements",
     SeeAlso =>{ "#" }
     }

document {
     Key => ".",
     Headline => "access to elements whose key is a symbol",
     TT "x.k", " -- the same as ", TT "x#(global k)", ", i.e., treat ", TT "k", " as
     a global symbol and provide the value stored in the hash table ", TT "x", " 
     under the key ", TT "k", ".",
     PARA{},
     "May also be used in an assignment.",
     PARA{},
     EXAMPLE {
	  "x = new MutableHashTable;",
      	  "x.k = 444",
      	  "x.k",
      	  "peek x",
	  },
     SeeAlso => {"#", ".?", "global"}
     }
document {
     Key => ".?",
     Headline => "check for presence of elements whose key is a symbol",
     TT "x.?k", " -- the same as ", TT "x#?(global k)", ", tells whether a value is
     available with ", TT "x.k", ".",
     PARA{},
     SeeAlso =>{ ".", "#?" }
     }

undocumented {(autoload, Function, String)}
document {
     Key => {(autoload, Symbol, String),autoload},
     Headline => "arrange for a function to be loaded automatically",
     Usage => "autoload(f,x)",
     Inputs => { "f", "x" },
     Consequences => {{ "arranges for a function named ", TT "f", " to be automatically loaded from the file ", TT "x", " the first time it is used.
	  This is accomplished by creating a suitable function that will load the file and assigning the function to ", TT "f", "." }},
     EXAMPLE lines ///
     	  fn = temporaryFileName()
	  fn << "f = x -> x+1\n" << close
	  autoload(f,fn)
	  code f
	  f 4
	  removeFile fn
     ///
     }

TEST ///
     k = ZZ/101
     R = k[a,b,c,d]/(a^4+b^4+c^4+d^4)
     X = Proj R
     result = table(3,3,(p,q) -> timing ((p,q) => rank HH^q(cotangentSheaf(p,X))))
     assert( {{1, 0, 1}, {0, 20, 0}, {1, 0, 1}} === applyTable(result,last@@last) )
     print new MatrixExpression from result
     ///

-- Example 4.1: the bounds can be sharp.
TEST ///
     S = QQ[w,x,y,z];
     X = Proj S;
     I = monomialCurveIdeal(S,{1,3,4})
     N = S^1/I;
     assert(Ext^1(OO_X,N~(>= 0)) == prune truncate(0,Ext^1(truncate(2,S^1),N)))
     assert(Ext^1(OO_X,N~(>= 0)) != prune truncate(0,Ext^1(truncate(1,S^1),N)))
     ///

-- Example 4.2: locally free sheaves and global Ext.
TEST ///
     S = ZZ/32003[u,v,w,x,y,z];
     I = minors(2,genericSymmetricMatrix(S,u,3));
     X = variety I;
     R = ring X;
     Omega = cotangentSheaf X;
     OmegaDual = dual Omega;
     assert(Ext^1(OmegaDual, OO_X^1(>= 0)) == Ext^1(OO_X^1, Omega(>= 0)))
     ///

-- Example 4.3: Serre-Grothendieck duality.
TEST ///
     S = QQ[v,w,x,y,z];
     X = variety ideal(w*x+y*z,w*y+x*z);
     R = ring X;
     omega = OO_X^{-1};
     G = sheaf cokernel genericSymmetricMatrix(R,R_0,2);
     assert(Ext^2(G,omega) == dual HH^0(G))
     assert(Ext^1(G,omega) == dual HH^1(G))
     assert(Ext^0(G,omega) == dual HH^2(G))
     ///


document {
     Key => "new",
     Headline => "new objects and new types",
     PARA {
	  "In this reference section we discuss how to make new types of object and new objects of those types."
	  },
     SYNOPSIS (
	  Heading => "installing a new method for new-of-from",
	  Usage => "new AA of BB from C := (A,B,c) -> ...",
	  Inputs => {
	       "AA" => HashTable,
	       "BB" => HashTable,
	       "C" => HashTable,
	       { TT "(A,B,c) -> ...", ", a function of 3 arguments: ", TT "AA", " will be an ancestor of ", TT "A", ",
		    ", TT "BB", " will be an ancestor of ", TT "B", ", and ", TT "C", " will be an ancestor of the class of ", TT "c", ".
		    Alternatively, ", TT "A", " will be a type of ", TT "AA", ", ", TT "B", " will be a type of ", TT "BB", ", and ", TT "c", " will be an instance of ", TT "C", "." }
	       },
	  Consequences => {
	       { "the function will be installed as the method for ", TT "new AA of BB from C", ".  It will be stored under the key ", TT "(NewOfFromMethod,AA,BB,C)", "
		    in the youngest of the hash tables ", TT "AA", ", ", TT "BB", ", and ", TT "C", "." }
	       },
	  Outputs => {
	       { "the function is returned as the value of the expression" }
	       },
	  PARA { "In this example we install a creation method for new types of types from functions.  The function is used to construct the method for converting
	       instances of the new type to nets for display."
	       },
	  EXAMPLE lines ///
	       new Type of BasicList from Function := (A,B,f) -> hashTable { net => f };
	  ///,
	  PARA {
	       "The hash tables ", TT "AA", ", ", TT "BB", ", and ", TT "C", " will normally be instances of ", TO "Type", "."
	       }
	  ),
     SYNOPSIS (
	  Heading => "new-of-from",
	  Usage => "new A of B from c",
	  Inputs => {
	       "A" => HashTable,
	       "B" => HashTable,
	       "c" => { "an instance of ", TT "C" }
	       },
	  Consequences => {
	       { "the function previously installed as the method for ", TT "new A of B from C", " will be called with arguments ", TT "(A,B,c)", "." },
	       { "if no such method has been installed, then ancestors of ", TT "A", ", ", TT "B", ", and ", TT "C", ", will be consulted,
		    searching lexicographically for a method; see ", TO "inheritance", "." },
	       { "if no method is found by searching the ancestors, then the function ", TT "(A,B,c) -> c", " will be used" },
	       { "the value returned by the function, (or ", TT "c", ", if no method was found), will have
		    its class set to ", TT "A", " and its parent set to ", TT "B", "; see ", TO "newClass", "." 
		    },
	       },
	  Outputs => {
	       { "the new object of class ", TT "A", " and with parent ", TT "B", " described above" }
	       },
	  PARA "We use the creation method installed above to create a new type of list.",
	  EXAMPLE lines ///
	       f = s -> "--list of type X--"
	       X = new Type of List from f
     	       class X
	       parent X
	       peek X
	  ///,
	  PARA { "Now we use ", TO "new", " to create a new list of type ", TT "X", " from a list.  The system knows how to convert lists to lists of type ", TT "X", ", so no creation
	       method need be installed for ", TT "new X from List", "."
	       },
	  EXAMPLE lines ///
	       x = new X from {1,3,11,12}
	       class x
	       parent x
	       peek x
	  ///
	  ),
     SYNOPSIS (
	  Heading => "installing a new method for new-of",
	  Usage => "new AA of BB := (A,B) -> ...",
	  Inputs => {
	       "AA" => HashTable,
	       "BB" => HashTable,
	       { TT "(A,B) -> ...", ", a function of 2 arguments: ", TT "AA", " will be an ancestor of ", TT "A", ", 
		    and ", TT "BB", " will be an ancestor of ", TT "B", ".  
		    Alternatively, ", TT "A", " will be a type of ", TT "AA", ", and ", TT "B", " will be a type of ", TT "BB", "." }
	       },
	  Consequences => {
	       { "the function will be installed as the method for ", TT "new AA of BB", ".  It will be stored under the key ", TT "(NewOfMethod,AA,BB)", "
		    in the younger of the hash tables ", TT "AA", " and ", TT "BB", "." }
	       },
	  Outputs => {
	       { "the function is returned as the value of the expression" }
	       },
 	  PARA {
	       "This operation turns out to be needed infrequently, because there is no ", TT "from", "-clause to provide data for initializing the instance of ", TT "A", "."
 	       },
 	  EXAMPLE ///new Type of BasicList := (type,array) -> (
		    stderr << "--new " << type << " of " 
		           << array << " being made" << endl;
		    new MutableHashTable)///
	  ),
     SYNOPSIS (
	  Heading => "new-of",
	  Usage => "new A of B",
	  Inputs => {
	       "A" => HashTable,
	       "B" => HashTable,
	       },
	  Consequences => {
	       { "the function previously installed as the method for ", TT "new A of B", " will be called with arguments ", TT "(A,B)", "." },
	       { "if no such method has been installed, then ancestors of ", TT "A", " and ", TT "B", " will be consulted,
		    searching lexicographically for a method; see ", TO "inheritance", "." },
	       { "the value returned by the function will have its class set to ", TT "A", " and its parent set to ", TT "B", "; see ", TO "newClass", "." },
	       { "if no method is found by searching the ancestors, then a new empty instance of ", TT "A", " with parent ", TT "B", " will be created" }
	       },
	  Outputs => {
	       { "the new object of class ", TT "A", " and parent ", TT "B", " described above" }
	       },
 	  PARA {
     	       "We illustrate this operation by making a new type of basic list, and then by making a list of that type."
 	       },
 	  EXAMPLE lines ///
	       M = new Type of BasicList
	       m = new M from {3,4,5}
	       class m
	       m#1
 	  ///,
	  PARA {
	       "Now let's define a method for reversing the elements of a list of class ", TT "M", ", using the unary operator ", TT "-", "."
	       },
 	  EXAMPLE lines ///
	       - M := reverse
	       - m
	  ///
	  ),
     SYNOPSIS (
	  Heading => "installing a new method for new-from",
	  Usage => "new AA from C := (A,c) -> ...",
	  Inputs => {
	       "AA" => HashTable,
	       "C" =>Type,
	       { TT "(A,c) -> ...", ", a function of 2 arguments: ", TT "AA", " will be an ancestor of ", TT "A", ", 
		    and ", TT "C", " will be an ancestor of the class of ", TT "c", ".  
		    Alternatively, ", TT "A", " will be a type of ", TT "AA", " and ", TT "c", " will be an instance of ", TT "C", "." }
	       },
	  Consequences => {
	       { "the function will be installed as the method for ", TT "new AA from C", ".  It will be stored under the key ", TT "(NewFromMethod,AA,C)", "
		    in the younger of the hash tables ", TT "AA", " and ", TT "C", "." }
	       },
	  Outputs => {
	       { "the function is returned as the value of the expression" }
	       },
	  PARA {
	       "Let's use the class ", TT "M", " defined above, and introduce a method for creating lists of class ", TT "M", " from integers.  Then we use it
	       in the subsection below."
	       },
	  EXAMPLE lines ///
	       new M from ZZ := (M',i) -> 0 .. i
	  ///
	  ),
     SYNOPSIS (
	  Heading => "new-from",
	  Usage => "new A from c",
	  Inputs => {
	       "A" => HashTable,
	       "c" => { "an instance of ", TT "C" }
	       },
	  Consequences => {
	       { "the function previously installed as the method for ", TT "new A from C", " will be called with arguments ", TT "(A,c)", "." },
	       { "if no such method has been installed, then ancestors of ", TT "A", " and ", TT "C", ", will be consulted, searching
		    lexicographically for a method; see ", TO "inheritance", "." },
	       { "if no method is found by searching the ancestors, then the function ", TT "(A,c) -> c", " will be used" },
	       { "the value returned by the function, (or ", TT "c", ", if no method was found), will have
		    its class set to ", TT "A", " and its parent retained; see ", TO "newClass" }
	       },
	  Outputs => {
	       { "the new object of class ", TT "A", " initialized from ", TT "c", " described above" }
	       },
 	  PARA {
	       "We use the new-from method for ", TT "new M from ZZ", " installed above."
 	       },
 	  EXAMPLE lines ///
	       n = new M from 13
	       - n
 	  ///
	  ),
     SYNOPSIS (
	  Heading => "installing a new method for new",
	  Usage => "new AA := (A) -> ...",
	  Inputs => {
	       "AA" => HashTable,
	       { TT "(A) -> ...", ", a function of 1 argument: ", TT "AA", " will be an ancestor of ", TT "A", ".  
		    Alternatively, ", TT "A", " will be a type of ", TT "AA", "." }
	       },
	  Consequences => {
	       { "the function will be installed as the method for ", TT "new AA", ".  It will be stored under the key ", TT "NewMethod", "
		    in the hash table ", TT "AA", "." }
	       },
	  Outputs => {
	       { "the function is returned as the value of the expression" }
	       },
 	  PARA {
	       "We use the class ", TT "M", " introduced above, and install a method for ", TT "new M", ", and we use it in the next subsection."
 	       },
 	  EXAMPLE lines ///
	       new M := (M') -> {a,b,c}
 	       ///
	  ),
     SYNOPSIS (
	  Heading => "new",
	  Usage => "new A",
	  Inputs => { "A" => HashTable },
	  Consequences => {
	       { "the function previously installed as the method for ", TT "new A", " will be called with argument ", TT "A", "." },
	       { "if no such method has been installed, then ancestors of ", TT "A", " will be consulted, searching for a method; see ", TO "inheritance", "." },
	       { "the value returned by the function will have its class set to ", TT "A", "; see ", TO "newClass" },
	       { "if no method is found by searching the ancestors, then a new empty instance of ", TT "A", " will be created, if possible" },
	       },
	  Outputs => {
	       { "the new object of class ", TT "A", " described above" }
	       },
 	  PARA {
	       "We use the method for ", TT "new M", " installed above."
 	       },
 	  EXAMPLE lines ///
	       new M
 	  ///
	  ),
     Subnodes => {
	  TO "newClass"
	  }
     }

document {
     Key => newClass,
     Headline => "set the class and parent of an object",
     SYNOPSIS (
	  Heading => "setting the class and parent",
	  Usage => "newClass(A,B,x)",
	  Inputs => { "A" => HashTable, "B" => HashTable, "x" },
	  Outputs => {{"a copy (possibly) of ", TT "x", " with ", TT "A", " as class and ", TT "B", " as parent"}},
	  ),
     SYNOPSIS (
	  Heading => "setting the class",
	  Usage => "newClass(A,x)",
	  Inputs => { "A" => HashTable, "x" },
	  Outputs => {{"a copy (possibly) of ", TT "x", " with ", TT "A", " as the new class"}},
	  ),
     SUBSECTION "common remarks",
     PARA{
	  "If ", TT "x", " is a basic list or sequence, then ", TO "BasicList", " should be an ancestor of ", TT "A", " and ", TT "B", " should be ", TO "Nothing", ".
	  If ", TT "x", " is a hash table, then ", TO "HashTable", " should be an ancestor of ", TT "A", "."
	  },
     PARA {
	  "If the class (and parent) of x are already equal to A (and B, respectively), then copying of the elements of ", TT "x", " is not required, and is not done."
	  },
     PARA{
	  "If ", TT "x", " is mutable, and instances of class ", TT "A", " are also mutable, then copying of the elements of ", TT "x", " is not required, and is not done."
	  },
     PARA{
	  "If ", TT "x", " is not a hash table, basic list, or sequence, then its class will be set to ", TT "A", " internally, essentially by wrapping it
	  in a special kind of object designed solely to indicate the new class.  The new class ", TT "A", " must be a specialization of the class of ", TT "x", ".
	  The parent cannot be reset this way.
	  Not all of the internal code of Macaulay 2 is ready to recognize such wrapped objects, which are part of a new feature, except for the code that handles functions."
	  },
     EXAMPLE lines ///
     	  t = 1..4
          newClass(Array,t)
	  x = new HashTable from { a => 1, b => 2 }
	  z = newClass(ImmutableType,Vector,x)
	  parent z
     ///,
     PARA {
	  "The difference between ", TT "new A of B from x", " and ", TT "newClass(A,B,x)", " is that the methods installed for ", TO "new", " are not used."
	  },
     EXAMPLE {
	  ///new Thing of Thing from Thing := (A,B,c) -> (
       << "-- new " << A << " of " << B 
       << " from " << toString c << endl;
       c);///,
	  "new ImmutableType of Vector from x",
	  "newClass(ImmutableType,Vector,x)"
     	  },
     SeeAlso => { "new", "copy", "toList" }
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
