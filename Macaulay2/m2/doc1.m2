--		Copyright 1993-1999 by Daniel R. Grayson

TEST ///
assert( null =!= documentation sin)
assert( null =!= documentation "sin")
assert( null =!= documentation symbol sin)
///

document { document,
     Headline => "install documentation",
     TT "document {s, d}", " -- install documentation ", TT "d", " for 
     the topic ", TT "s", ".",
     PARA,
     "The documentation ", TT "d", " should be ", TO "hypertext", ".  The topic
     ", TT "s", " may be one of the special forms useable with ", TO "TO", ".  As
     a convenience, lists and sequences in ", TT "d", " are converted to ", TT "SEQ", "
     mark up items, and instances of ", TO "MarkUpType", ", such as ", TO "PARA", "
     are converted to empty instances of their type.",
     PARA,
     "Special documentation entries:",
     MENU {
	  SEQ { TT "Headline => \"...\"", " -- some text to be displayed along
	       with menu items that lead to this node.  The text should be
	       less than a line full." },
	  SEQ { TT "Usage => ...", " -- text to be displayed as a usage message
	       when the user types the name of this object at top level.  In its
	       absence, the synopsis, if any, is displayed, otherwise the
	       available methods are displayed." },
	  SEQ { TT ///Synopsis => { "z = f(x,y)", "x" => ..., "y" => ..., "z" => ... }///,
	       " -- synopsis of the use of a function with a return value."
	       },
	  SEQ { TT ///Synopsis => { "f(x,y)", "x" => ..., "y" => ..., null }///,
	       " -- synopsis of the use of a function without a return value."
	       },
	  SEQ { TT ///EXAMPLE { "...", "..." }///, " -- bits of example code to display"},
	  SEQ { TT ///SEEALSO { "a", "b" }///, " -- a menu of related nodes to visit." },
	  },
     PARA,
     SEEALSO {"help functions"}
     }

document { TEST,
     Headline => "commands for testing later",
     TT "TEST s", " -- writes the string ", TT "s", " to a new test file.  The
     commands in that file can be run later as a test.",
     PARA,
     "Intended for internal use only."
     }

document { between,
     Headline => "insert something between elements of a list",
     TT "between(m,v)", " -- inserts ", TT "m", " between each pair of elements 
     of the list or sequence ", TT "v", ", returning a list.",
     PARA,
     EXAMPLE {
	  "between(55,{a,b,c,d})"
	  }
     }

document { SEEALSO,
     Headline => "crossreferences in documentation",
     TT "SEEALSO {a, b, ...}", " -- inserts, into a documentation page, a sentence
     instructing the reader to see some other topics.",
     PARA,
     "The topics may have the special forms used with ", TO "TO", ".",
     SEEALSO "document"
     }

document { briefDocumentation,
     Headline => "get brief documentation",
     TT "briefDocumentation s", " -- provides the first paragraph of the online
     documentation for the topic ", TT "s", ", in internal ", TO "hypertext", "
     form, suitable for conversion to text with ", TO "text", " or to html 
     with ", TO "html", ".",
     SEEALSO "documentation"
     }

document { documentation,
     Headline => "get documentation, unformatted",
     TT "documentation s", " -- provides the online documention for the topic s, in
     internal ", TO "hypertext", " form, suitable for conversion to
     text with ", TO "text", " or to html with ", TO "html", ".",
     PARA,
     EXAMPLE "documentation partitions"
     }

document { help,
     Headline => "get help",
     -- no PARA in this documentation, so it all gets displayed.
     TT "help X", " -- displays the online documentation for ", TT "X", ".",
     BR, NOINDENT,
     TT "help \"Macaulay 2\"", " -- displays the base of the online documentation
     tree.",
     BR, NOINDENT,
     TT "help methods X", " -- displays help messages about the methods usable
     with things of type ", TT "X", ".",
     BR, NOINDENT,
     TT "help methods res", " -- displays help messages about the methods 
     usable with the function ", TT "res", ".",
     BR, NOINDENT,
     TT "help methods symbol **", " -- displays help messages about the methods 
     usable with the operator ", TT "**", ".",
     BR, NOINDENT,
     TT "help methods (res, X)", " -- displays help messages about the 
     methods usable with the function ", TT "res", " and a thing of
     class ", TT "X", ".",
     BR, NOINDENT,
     TT "help methods (symbol **, X)", " -- displays help messages about the 
     methods usable with the operator ", TT "**", " and a thing of
     class ", TT "X", ".",
     BR, NOINDENT,
     TT "help methods (X, Y)", " -- displays help messages about the 
     methods usable with a thing of class ", TT "X", " and a thing of class
     ", TT "Y", "."
     }

document { topicList,
     Headline => "list of help topics",
     TT "topicList()", " -- provides a complete list of topics on which help 
     is available.",
     PARA,
     "Intended to be used in programs.  Users will prefer 
     to use ", TO "topics", ".",
     PARA,
     SEEALSO "help"
     }

document { topics,
     Headline => "display available help topics",
     TT "topics", " -- displays a list of topics on which help is available.",
     PARA,
     "topics() -- Does the same in a function or file.",
     PARA,
     SEEALSO "help"
     }

document { apropos,
     Headline => "symbols matching a pattern",
     TT "apropos s", " -- displays a list of global symbols which match
     the pattern specified by the string ", TT "s", ".",
     PARA,
     "The pattern may contain '*'s as wild card characters.",
     EXAMPLE "apropos \"scan\""
     }

document { printExamples,
     Headline => "print examples of use from documentation",
     TT "printExamples f", " -- prints out the examples of code using
     the function ", TT "f", " provided in the documentation for
     ", TT "f", ".",
     PARA,
     EXAMPLE "printExamples partition",
     SEEALSO {"examples", "document"}
     }

document { symbol "Symbols",
     Headline => "find the symbol with a given value",
     TT "Symbols", " -- a hash table which can be used to obtain the global
     symbol with a particular value.",
     PARA,
     "This is useful internally for getting the name of a function, for 
     example."
     }

document { symbol "Documentation",
     Headline => "where the documentation is stored",
     TT "Documentation", " -- a hash table which is used to store
     pointers to documentation of functions, symbols, and methods.",
     PARA,
     "This hash table is used by the routines that display documentation,
     is intended for internal use only, and its format may change.",
     PARA,
     "The documentation is stored both in a hash table in memory, and in a 
     database file.",
     SEEALSO {"Symbols", ":="}
     }

document { formatDocumentTag,
     Headline => "format documentation tags",
     TT "formatDocumentTag x", " -- formats the tags used with ", TO "TO", " for
     display purposes in documents.",
     PARA,
     "This function is intended for internal use only."
     }

document { uniform,
     Headline => "test whether elements of a list are of the same class",
     TT "uniform x", " -- whether all elements of the list x have the same class."
     }

document { newClass,
     Headline => "copy an object, changing the class",
     TT "newClass(N,m)", " -- makes a copy of m with N as the new class", BR, NOINDENT,
     TT "newClass(N,M,m)", " -- makes a copy of m with N as class and M as parent",
     PARA,
     "If m is a list, then BasicList should be an ancestor of N.  If m is 
     a hash table, then ", TT "HashTable", " should be an ancestor of N.",
     PARA,
     "If m is mutable, and instances of class N are also mutable, then
     copying is not required, and is not done.",
     PARA,
     SEEALSO { "copy", "toList" }
     }

document { MutableList,
     Headline => "the class of all mutable lists",
     PARA,
     "Normally the entries in a mutable list are not printed, to prevent
     infinite loops in the printing routines.  To print them out, use 
     ", TO "peek", ".",
     PARA,
     EXAMPLE {
	  "s = new MutableList from {a,b,c};",
      	  "s#2 = 1234;",
	  "s",
      	  "peek s",
	  },
     SEEALSO {"BasicList"}
     }

document { lookup,
     Headline => "look up methods",
     NOINDENT,
     TT "lookup(M,A)", " -- provides the binary method named ", TT "M", " for class ", TT "A", ".
     The first place to look is ", TT "A#M", ".  The search proceeds with
     the parent of ", TT "A", ", and so on.",
     PARA,
     NOINDENT, TT "lookup(M,A,B)", " -- provides the binary method named ", TT "M", " for ", TT "(A,B)", ".
     The first place to look is ", TT "Y#(M,A,B)", " where ", TT "Y", " is the younger
     of ", TT "A", " and ", TT "B", ".  The search proceeds next with the parent of ", TT "B", ", 
     and so on.",
     PARA,
     NOINDENT, TT "lookup(M,A,B,C)", " -- provides the ternary method named ", TT "M", " for
     ", TT "(A,B,C)", ".  The first place to look is ", TT "Y#(M,A,B,C)", " where ", TT "Y", " 
     is the youngest of ", TT "A", ", ", TT "B", ", and ", TT "C", ".  The search proceeds with 
     the parent of ", TT "C", ", and so on.",
     NOINDENT, TT "lookup x", " -- where ", TT "x", " is a symbol or function, returns ", TT "x", ".",
     PARA,
     "If no method is found, then ", TO "null", " is returned.",
     PARA,
     SEEALSO {"#", "classes and types", "installMethod", "youngest"}
     }

document { installMethod,
     Headline => "install methods",
     PARA,
     "Most users will use a different way of installing methods.",
     PARA,
     NOINDENT,
     TT "installMethod(M,A,f)", "     -- installs a function ", TT "f", " as a unary method for
     the class ", TT "A", " under the name ", TT "M", ".  This is the same as ", "M A := f", " 
     if ", TT "M", " is a function.  As currently implemented, this is also the same 
     as ", TT "A#M = f", ".",
     PARA,
     NOINDENT,
     TT "installMethod(M,A,B,f)", "   -- installs a function ", TT "f", " as a binary method for
     classes ", TT "A", " and ", TT "B", " under the name ", TT "M", ".  This is the same as 
     ", TT "M(A,B) := f", " if ", TT "M", " is a
     function, or the same as ", TT "A M B := f", " if ", TT "M", " is a binary operator. As currently
     implemented, this is also the same as ", TT "Y#(M,A,B) = f", ", where ", TT "Y", " is 
     the younger of ", TT "A", " and ", TT "B", ".",
     PARA,
     NOINDENT,
     TT "installMethod(M,A,B,C,f)", " -- installs a function ", TT "f", " as a ternary method 
     for classes ", TT "A", ", ", TT "B", ", and ", TT "C", " under the name ", TT "M", ".  
     This is the same as ", TT "M(A,B,C) := f", " if ", TT "f", "
     is a function.  As currently implemented, this is also the same as
     ", TT "Y#(M,A,B,C) = f", ", where ", TT "Y", " is the youngest of ", TT "A", ", ", TT "B", ", 
     and ", TT "C", ".",
     PARA,
     SEEALSO{"#", "lookup",  "new", "classes and types"}
     }

document { "new",
     Headline => "new objects of various types",
     TT "new A of b from c", " -- make a hash table of class ", TT "A", " and 
     parent ", TT "b", " initialized from ", TT "c", ".", BR,
     NOINDENT,
     TT "new A of b", " -- make a hash table of class ", TT "A", " 
     and parent ", TT "b", ".", BR,
     NOINDENT,
     TT "new A from c", " -- make a new instance of class ", TT "A", " 
     initialized from ", TT "c", ".", BR,
     NOINDENT,
     TT "new A", " -- makes a new instance ", TT "n", " 
     of class ", TT "A", ".", BR,
     PARA,
     HR,
     NOINDENT,
     TT "new A of b from c", " -- make a hash table ", TT "n", " of
     Here ", TT "A", " and ", TT "b", " are hash tables, and ", TT "c", " is
     any expression.  Let ", TT "b", " be an instance of ", TT "B", ", ", TT "c", "
     be an instance of ", TT "C", ", and let ", TT "AA", " be an
     ancestor of ", TT "A", ".  Then use",
     PRE "          new AA of B from C := (A,b,c) -> ... ",
     "to install the corresponding optional creation routine -- the
     value it returns will be converted so its class is ", TT "A", " and its
     parent is ", TT "b", "; this will involve copying unless the returned value 
     is mutable and objects of class ", TT "A", " are mutable.",
     PARA,
     "If no installation routine has been installed, then ", TT "c", " should be
     a hash table or a list, and it will be converted directly.",
     PARA,
     "The class ", TT "A", " should be a type of type, which means that
     ", TT "Type", " is an ancestor of ", TT "A", " and of the class of ", TT "A", ".",
     HR,
     NOINDENT,
     TT "new A of b", " -- make a hash table of class ", TT "A", "
     and parent ", TT "b", ".",
     PARA,
     "Same as above, except ", TT "c", " is missing.  Use ",
     PRE "          new AA of B := (A,b) -> ... ",
     "to install the initialization routine.",
     PARA,
     "The class ", TT "A", " should be a type of type, which means that
     ", TT "Type", " is an ancestor of ", TT "A", " and of the class of ", TT "A", ".",
     HR,
     NOINDENT,
     TT "new A from c", " -- make a hash table or list ", TT "n", " of 
     class ", TT "A", " initialized from ", TT "c", ".",
     PARA,
     "The same as above except ", TT "b", " is missing.  Use ",
     PRE "          new AA from C := (A,c) -> ... ",
     "to install the corresponding initialization routine.",
     PARA,
     "Since no parent ", TT "b", " has been provided, the value returned by the
     initialization routine will not have its parent reset.  If there
     is no initialization routine the parent will be set to Nothing.",
     PARA,
     "The class ", TT "A", " should be a type, which means that
     ", TT "Type", " is an ancestor of the class of ", TT "A", ".",
     HR,
     NOINDENT,
     TT "new A", " -- make a new instance ", TT "n", " of 
     class ", TT "A", ".",
     PARA,
     "Same as above, except ", TT "b", " and ", TT "c", " are missing.
     Use ", TT "new AA := A -> ... ", " to install the initialization routine.",
     PARA,
     "Since no parent ", TT "b", " has been provided, the value returned by the
     initialization routine will not have its parent reset.  If there
     is no initialization routine the parent will be set to Nothing.",
     PARA,
     "The class ", TT "A", " should be a type, which means that
     ", TT "Type", " is an ancestor of the class of ", TT "A", ".",
     HR,
     "Note that if the ", TT "of", " option is not used, then the class ", TT "A", "
     need not consist of hash tables or lists.  We are using this feature by
     installing a method so that ", TT "new ZZ", " returns an integer popped
     from the top of the engine's stack.",
     PARA,
     "The symbols ", TO "NewMethod", ", ", TO "NewOfMethod", ", ", 
     TO "NewFromMethod", ", and ", TO "NewOfFromMethod", " are used internally
     for installation of the initialization routines.",
     SEEALSO {"classes and types"}
     }

document { "of",
     Headline => "a keyword",
     TT "of", " -- a keyword used with ", TO "new", "."
     }

document { NewMethod,
     TT "NewMethod", " -- a symbol used as a method name in conjuction with
     the ", TO "new", " operator.",
     PARA,
     "Intended for internal use only."
     }

document { NewOfMethod,
     TT "NewOfMethod", " -- a symbol used as a method name in conjuction with
     the ", TO "new", " operator.",
     PARA,
     "Intended for internal use only."
     }

document { NewFromMethod,
     TT "NewFromMethod", " -- a symbol used as a method name in conjuction with
     the ", TO "new", " operator.",
     PARA,
     "Intended for internal use only."
     }

document { NewOfFromMethod,
     TT "NewOfFromMethod", " -- a symbol used as a method name in conjuction with
     the ", TO "new", " operator.",
     PARA,
     "Intended for internal use only."
     }

document { Thing,
     Headline => "the class of all things",
     "Everything in Macaulay 2 is a ", ITALIC "thing", ".  This 
     includes numbers, strings, and lists.  More complicated things such as 
     polynomials, groups, rings, and chain complexes are implemented
     as ", ITALIC "hash tables", ".  See ", TO "Type", " for information 
     about what types of things there are."
     }

document { Nothing,
     Headline => "the empty class",
     "This class is useful for representing the class of an argument
     which is missing.  It is also used as the parent for those things which
     are not themselves types, i.e., which do not have instances." 
     }

document { Option,
     Headline => "the class of all pairs x => y",
     PARA,
     "Such pairs are used as optional arguments for functions.  There
     is also a way to make new hash tables with ", TO "new", " by
     providing a list of option pairs.",
     PARA,
     EXAMPLE {
	  "a => 5",
      	  "peek (a => 5)",
	  "new HashTable from {a => 5, b => 7}",
	  },
     PARA,
     "These pairs are implemented as lists, so that if ", TT "z", " is ", TT "x => y", ", then 
     ", TT "x", " is ", TT "z#0", " and ", TT "y", " is ", TT "z#1", ".",
     PARA,
     SEEALSO {"classes and types", "=>"}
     }

document { (NewFromMethod, HashTable, List),
     Headline => "make a hash table from a list",
     TT "new HashTable from x", " -- produce a new hash table from a
     list ", TT "x", ".",
     PARA,
     "Elements of ", TT "x", " which are options, ", TT "k => v", " cause
     the value ", TT "v", " to be stored in ", TT "x", " under the key ", TT "k", ".",
     SEEALSO "hashTable"
     }

document { OptionTable,
     Headline => "the class of hash tables for optional arguments",
     SEEALSO "==>" }

document { (symbol "==>", List, Function),
     Headline => "attaching options to a function",
     "See ", TO (symbol "==>", OptionTable, Function), " for details."
     }

document { (symbol "==>", OptionTable, Function),
     Headline => "attaching options to a function",
     Usage => { TT "defs ==> fun", " -- a new function made from the
	  function ", TT "fun", " that processes optional arguments
	  specified by ", TT "defs", "."},
     Synopsis => {
	  "g = defs ==> fun",
	  "defs" => { "a hash table whose keys are the names
     of the optional arguments, and whose values are the
     corresponding default values." },
     	  "fun" => { "a function that expects optional arguments." },
     	  "g" => { "a new function that pre-processes the optional
     arguments and then calls ", TT "fun", "." }
     	  },
     PARA,
     "The new function ", TT "g", " works as follows.
     The value of ", TT "g args", ", say, is obtained by evaluation of 
     ", TT "(fun opts)(args')", ", where ", TT "args'", " is obtained from
     ", TT "args", " by removing the options of the form ", TT "X=>A", " 
     (where ", TT "X", " is a name of an optional argument), and ", TT "opts", " 
     is a hash table of the same form as ", TT "defs", " in which the default
     values have been replaced by the user-supplied values, e.g., the
     value stored under the key ", TT "X", " has been replaced by
     ", TT "A", ".",
     PARA,
     "Remark: ", TT "defs", " can also be simply a list of options.",
     PARA,
     "In the following example we use a simple definition for ", TT "fun", "
     so we can see everything that ", TT "fun", " receives.",
     EXAMPLE {
	  "g = {a=>1, b=>2} ==> opts -> args -> {args, opts}",
	  "g x",
	  "g(x,y,b=>66)",
	  "g(t,u,a=>44,b=>77)",
	  },
     SEEALSO {"making new functions with optional arguments", "OptionTable", "Option", "=>"}
     }

document { method => SingleArgumentDispatch,
     Headline => "method functions with a variable number of arguments",
     Synopsis => {
	  "f = method(SingleArgumentDispatch => true)",
	  "f" => "a method function that treats several arguments as
	  a single argument, i.e., as a sequence."
	  },
     PARA,
     "Here is an example.",
     EXAMPLE {
	  "f = method(SingleArgumentDispatch=>true);",
	  "f ZZ := i -> -i;",
	  "f Sequence := S -> reverse S;",
	  "f 44",
	  "f(3,4,5,6)"
	  },
     PARA,
     "Normally, at most three arguments could be handled by such a method
     function, and the types would have to be considered separately."
     }

document { symbol "typicalValues",
     Headline => "types of values returned by functions",
     "A hash table used to store information about the type of values
     typically returned by functions and methods.",
     PARA,
     "This information is used only to build documentation automatically.",
     EXAMPLE "typicalValues#isRing",
     SEEALSO { "specifying typical values" }
     }

document { (method => TypicalValue),
     Headline => "specify return value type",
     TT "TypicalValue => X", " -- an option to ", TO "method", "
     which specifies that values returned by the method function will
     typically be of type ", TT "X", ".",
     PARA,
     "This information is used only to build documentation automatically, and
     is stored in the hash table ", TO "typicalValues", ".",
     SEEALSO { "specifying typical values" }
     }

document { method,
     Headline => "make a new method function",
     TT "f = method()", " -- creates a method function",
     PARA,
     "The code above creates a method function which takes up to three 
     arguments, looking up the appropriate method according to the classes of 
     the arguments, with inheritance.  To install a method for two arguments,
     (x,y), of classes X and Y, use code like this:",
     PRE "     f(X,Y) := (x,y) -> ...",
     "where '...' represents the body of the function you wish to install.
     The syntax for one or three arguments is analogous.  For a single
     argument ", TT "x", " of class ", TT "X", ", one could omit the parentheses, and write:",
     PRE "     f X := (x) -> ...",
     "the effect of which happens to be the same as that of",
     PRE "     X#f := (x) -> ...",
     PARA,
     SEEALSO {"methods" }
     }

document { method => Associative,
     Headline => "allows associative methods to be created",
     NOINDENT,
     TT "f = method(Associative=>true)", " -- creates an associative
     method which will call upon the appropriate binary methods for its arguments 
     two at a time.",
     PARA,
     "In the following example we install a method which isn't associative
     to illustrate the order of evaluation.",
     EXAMPLE {
	  ///f = method(Associative => true)///,
	  ///f(String,String) := (i,j) -> "(" | i | ")," | j;///,
      	  ///f("a","b","c","d")///,
	  }
     }

document { size,
     Headline => "the size of an object",
     TT "size x", " -- returns the size of ", TT "x", " which usually gives
     a rough indication of memory space required to store the object ", TT "x", ".",
     PARA,
     "For a polynomial, the size is the number of terms.",
     PARA,
     "This function should be replaced by something more generally useful."
     }

document { baseName,
     Headline => "the base name of a generator",
     TT "baseName x", " -- returns the variable or symbol upon which a generator of a
     monoid or polynomial ring is based."
     }

document { degree,
     Headline => "the degree",
     TT "degree X", " -- returns the degree of a polynomial, vector, 
     matrix, monomial, or module.",
     PARA,
     "The degree may be an integer, or a vector of integers.  The length
     of that vector is referred to as the 'number of degrees', and is
     provided by ", TO "degreeLength", ".",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[x,y,z]",
      	  "degree (x^2+y^2)^5",
      	  "F = R^{2,3,4}",
      	  "v = F_2",
      	  "degree v",
	  },
     "The degree of a module of finite length is the same as its length.",
     EXAMPLE "degree cokernel symmetricPower ( 2, vars R )",
     PARA,
     "Implemented with a method of the same name."
     }

document { degreeLength,
     Headline => "the number of degrees",
     TT "degreeLength x", " -- returns the number of degrees of x.",
     PARA,
     "Here x may be a ring, in which case it returns the number of degrees
     (the length of the degree vector) used in grading the ring.",
     SEEALSO "degree"
     }

document { coefficients,
     Headline => "the coefficients",
     TT "coefficients({i,j,...},p)", " -- yields the coefficients and
     monomials of the polynomial or matrix p with respect to variables 
     numbered i, j, ... .  This has to
     be completely redone, so I don't document it further, but it is used in
     the factoring code.",
     BR,NOINDENT,
     TT "coefficients(p)", " -- yields the coefficients and monomials of
     the polynomial or matrix p with respect to all of the variables."
     }

document { isIsomorphism,
     Headline => "whether a map is an isomorphism",
     TT "isIsomorphism f", " -- whether the map f of modules is an isomorphism."
     }

document { isConstant,
     Headline => "test whether something is constant"
     }

document { (isConstant, RingElement),
     Synopsis => {
	  "t = isConstant f",
	  "f" => null,
	  "t" => {"whether the polynomial ", TT "f", " is constant"}
	  }
     }

document { isHomogeneous,
     Headline => "test for homogeneity"
     }

document { (isHomogeneous,Matrix),
     Synopsis => {
	  "t = isHomogeneous f",
	  "f" => { "a map ", TT "F", " <-- ", TT "G", "" },
	  "t" => {"whether the matrix ", TT "f", " is homogeneous"}
	  },
     "Associated with every matrix is an arbitary integer called
     its degree: it has nothing to do with the degrees of the entries
     of the matrix; see ", TO (degree,Matrix), ".  The
     matrix ", TT "f", " is called homogeneous if every entry
     ", TT "f_(i,j)", " has degree equal to
     ", TT "degree G_i - degree F_j + degree f", ".  Another way to
     say it is that applying f to a homogeneous vector add
     ", TT "degree f", " to its degree.",
     EXAMPLE {
	  "R = QQ[x];",
	  "f = map(F = R^{0}, G = R^{-1}, {{x^3}}, Degree => 2)",
	  "degree f",
	  "degree G_0",
	  "degree F_0",
	  "f * G_0",
	  "degree (f * G_0)",
	  "isHomogeneous f"
	  }
     }

document { vars, 
     Headline => "variables"
     }

document { (vars,Ring),
     Headline => "row matrix of the variables",
     Usage => {
	  "v = vars R",
	  "R" => null,
	  "v" => { "the ", TT "1", " by ", TT "n", " matrix whose 
	       entries are the variables of the polynomial 
	       ring ", TT "R", "."}
	  },
     EXAMPLE {
      	  "R = QQ[a..e]",
      	  "vars R",
	  "ideal vars R",
	  "coker vars R",
	  "res coker vars R",
	  }
     }

document { (vars,Sequence),
     Usage => {
	  "w = vars(i .. j)",
	  "(i .. j)" => "a sequence of integers",
	  "w" => { "a sequence of symbols which can be used as indeterminates
	  in a polynomial ring, the ", TT "i", "-th one through the ", TT "j", "-th one." }
	  },
     "There is no limit on the size or sign of the integers ", TT "i", " 
     and ", TT "j", ".  The symbols returned are single letters, or the letter 
     ", TT "x", " or ", TT "X", " followed by some digits.",
     EXAMPLE {
	  "vars(3 .. 9, 33 .. 35, 1000 .. 1002, -100 .. -98)"
	  }
     }

document { leadCoefficient,
     Headline => "the leading coefficient",
     TT "leadCoefficient f", " -- return the leading coefficient of the polynomial
     or vector ", TT "f", ".",
     PARA,
     SEEALSO {"leadTerm", "leadMonomial", "leadComponent"}
     }

document { leadComponent,
     Headline => "the leading component of a vector",
     TT "leadComponent f", " -- return the leading component of the vector f,
     i.e., the integer i so that f_i is the first nonzero component of f.",
     PARA,
     SEEALSO {"leadTerm", "leadCoefficient", "leadMonomial"}
     }

document { leadMonomial,
     Headline => "the leading monomial",
     TT "leadMonomial f", " -- return the leading monomial of the polynomial
     or vector f.",
     PARA,
     SEEALSO {"leadTerm", "leadCoefficient", "leadCoefficient"}
     }

document { flatten,
     Headline => "flatten a list by unnesting lists",
     TT "flatten m", " -- produces a new list from ", TT "m", " by
     effectively removing the braces surrounding the elements
     of any elements of m which happen to be lists.  Also works
     for matrices.",
     PARA,
     EXAMPLE "flatten {{2,3,4},{{5}},6}"
     }

///
document docTemplate {
     Header => (flatten,Matrix),
     Briefly => "produce a row matrix with the same entries as a given matrix",
     Usage => "g = flatten f",
     Input => {("f",Matrix) => { "An m by n matrix" }},
     Output => {("g",Matrix) => { "The 1 by mn matrix with the same entries as f" }},
     Description => {"The matrix produced consists of the first column followed by
	  the second, and so on.  Both the target and the source of the matrix ", 
	  TT "f", " must be free modules."},
     Example => {
       "R = QQ[a..f]; m = matrix{{a,b,c},{d,e,f}}",
       "flatten m"
       }
     }
///
	  
document { symbol "coker",
     Headline => "cokernel of a map",
     "An abbreviation for ", TO "cokernel", "."
     }

document { cokernel,
     Headline => "cokernel of a map",
     TT "cokernel f", " -- produces the cokernel of the module homomorphism f",
     PARA,
     "The result will be a quotient module of the target of f.  If f is
     a ring element, it is interpreted as a one by one matrix.",
     PARA,
     "The generators of the cokernel are provided by the generators of the target
     of ", TT "f", ".  In other words, ", TT "cover target f", " and ", TT "cover cokernel f", " are equal.",
     PARA,
     "For an abbreviation, use ", TO "coker", ".",
     SEEALSO {"kernel", "cover"}
     }

TEST ///
    R = QQ[x,y,z]
    modules = {
	 image matrix {{x^2,x,y}},
	 coker matrix {{x^2,y^2,0},{0,y,z}},
	 R^{-1,-2,-3},
	 image matrix {{x,y}} ++ coker matrix {{y,z}}
	 }
    scan(modules, M -> assert( cover cokernel M_{1} ==  cover M ) )
///


document { image,
     Headline => "image of a map",
     TT "image h", " -- yields the image of the homomorphism ", TT "h", ".",
     PARA,
     "The result will be a submodule of the target of h",
     PARA,
     "If h is a ring element, it is interpreted as a one by one matrix."
     }

document { source,
     Headline => "source of a map",
     TT "source h", " -- the source of a morphism ", TT "h", ".",
     }

document { target,
     Headline => "target of a map",
     TT "target h", " -- the target of a morphism ", TT "h", " or Groebner basis.",
     }

document { ambient,
     Headline => "ambient free module of a subquotient, or ambient ring",
     TT "ambient M", " -- yields the ambient free module for the module ", TT "M", ".",
     BR,
     NOINDENT,
     TT "ambient R", " -- yields the ambient ring of the quotient ring ", TT "R", ".
     For a Galois field it yields the ring it was constructed from.",
     PARA,
     EXAMPLE "ambient(ZZ/101[a,b]/(a^3+b^3))",
     SEEALSO {"cover", "super"}
     }

document { (ambient,Module),
     Synopsis => {
	  "F = ambient M",
	  "M" => null,
	  "F" => {"the ambient free module of ", TT "M", "."}
	  },
     PARA,
     "The ambient free module is the target of both the generator matrix of ",
     TT "M", " and the relations matrix of ", TT "M", ".",
     PARA,
     "If ", TT "M", " is the module ", TT "M = (image(g) + image(h)) / image(h)",
     ", where ", TT "g : F <--- G", " and ", TT "h : F <--- H", " are matrices between
     free modules, then the ambient module of ", TT "M", " is ", TT "F", ".",
     EXAMPLE "R = QQ[x_1 .. x_10];",
     EXAMPLE "N = coker vars R ++ coker vars R",
     EXAMPLE "f = N_{0} - N_{1}",
     TT "f", " is  a matrix from a free module of rank 1 to ", TT "N", ".",
     EXAMPLE "M = image(f)",
     EXAMPLE "ambient M",
     EXAMPLE "ambient M == target generators M",
     EXAMPLE "ambient M == target relations M",
     SEEALSO {(cover,Module), (super,Module), (generators,Module), (relations,Module)}
     }     

document { Hom,
     Headline => "module of homomorphisms",
     TT "Hom(M,N)", " -- constructs the module of homomorphisms from M to N.",
     PARA,
     "Implemented with a method of the same name.",
     PARA,
     "Use ", TO "homomorphism", " to convert an element of the module of
     homomorphisms to a matrix."
     }

document { "gens",
     Headline => "matrix of generators",
     "See ", TO "generators", "."
     }

document { generators,
     Headline => "matrix of generators",
     TT "generators x", " -- produces the generators of x.",
     PARA,
     "For an abbreviation, use ", TO "gens", ".",
     PARA,
     "Produces the generators of a Groebner basis, a polynomial ring,
     a monoid ring, a free module, a free group, a submodule given by
     means of generators (or for which generators have been computed),
     or a free monoid.",
     PARA,
     "Usually the result is a list of generators, but the generators of
     a module or Groebner basis are provided as the columns in a matrix.  
     The matrix is stored in a module M under M.generators, unless the matrix
     is the identity matrix.",
     SEEALSO {"Monoid", "GroebnerBasis", "Module", "relations", "subquotient"}
     }

document { (generators,Module),
     Synopsis => {
	  "g = generators M",
	  "M" => null,
	  "g" => {"the matrix of generators of ", TT "M", "."}
	  },
     PARA,
     "Every module in Macaulay2 has, at least implicitly, a generator matrix and a 
     matrix of relations, both of which are matrices between free modules.  
     This function returns the generator matrix.",
     PARA,
     EXAMPLE "R = GF(8)",
     EXAMPLE "f = R_0 ++ R_0^2 ++ R_0^3 ++ R_0^4",
     EXAMPLE "generators(image f)",
     EXAMPLE "generators(cokernel f)",
     CAVEAT {
	  "This function returns a matrix with the given generators.  This 
	  set of generators may not be minimal, or sorted in any particular 
	  order. Use ", TO (trim,Module), " or ", TO (mingens,Module), " instead."
	  },
     SEEALSO {(relations,Module)}
     }

     
document { someTerms,
     Headline => "select some terms of a polynomial",
     TT "someTerms(f,i,n)", " -- select ", TT "n", " terms from the polynomial 
     ", TT "f", ", starting with the i-th one, and return the resulting polynomial."
     }

document { scanKeys,
     Headline => "apply a function to each key in a hash table or database",
     TT "scanKeys(x,f)", " -- apply the function ", TT "f", " to each key used in the
     hash table or database ", TT "x", ".",
     PARA,
     "This function requires an immutable hash table.  To scan the keys in
     a mutable hash table, use ", TT "scan(keys x, f)", "."
     }

document { scanValues,
     Headline => "apply a function to each value in a hash table",
     TT "scanValues(x,f)", " -- apply the function ", TT "f", " to each value
     appearing in the hash table ", TT "x", ".",
     PARA,
     "This function requires an immutable hash table.  To scan the values in
     a mutable hash table, use ", TT "scan(values x, f)", "."
     }

document { GlobalAssignHook,
     Headline => "hook for assignment to global variables",
     TT "GlobalAssignHook", " -- a method name which is consulted when an
     assignment to a global variable occurs.",
     PARA,
     "The method should be a function of two variables: the symbol to which
     a value is being assigned, and the value being assigned.  The method
     should be stored under then name ", TT "GlobalAssignHook", " in the
     class of the value.  It will be executed just before the assignment
     occurs.",
     PARA,
     "This method is used for instances of ", TO "Type", " and ", TO "Ring", "
     to arrange for the name of the type or ring to be set to the name
     of the global variable to which it is first assigned.  The functions
     ", TO "globalAssignFunction", " and ", TO "globalReleaseFunction", " may installed
     as methods for this purpose.",
     PARA,
     EXAMPLE {
	  ///RR.GlobalAssignHook = (sym,val) -> <<"assigning " <<val <<" to " <<sym <<endl;///,
          "a=4.5",
	  },
     SEEALSO "GlobalReleaseHook"
     }

document { GlobalReleaseHook,
     TT "GlobalReleaseHook", " -- a method name which is consulted when an
     assignment to a global variable is about to occur.",
     PARA,
     "The method should be a function of two variables: the symbol to which
     a value is being assigned, and the old value about to be overwritten.  
     The method should be stored under the name ", TT "GlobalReleaseHook", " in the
     class of the old value.  It is executed before the assignment occurs,
     and before the execution of ", TO "GlobalAssignHook", ".",
     PARA,
     EXAMPLE {
	  ///RR.GlobalReleaseHook = (sym,val) -> << concatenate (
     "assigning ", toString val, " to ", toString sym
     ) << endl///,
          "a=4.5",
      	  "a=5.4",
	  },
     SEEALSO "GlobalAssignHook"
     }

document { summary,
     TT "summary g", " -- describe the status of a Groebner basis computation
     or of a resolution computation.",
     PARA,
     EXAMPLE {
	  "ZZ/101[a..f]",
      	  "summary gb matrix {{a*b, b*c},{a^3*f, b^3*e}}",
	  },
     SEEALSO { "GroebnerBasis", "Resolution" }
     }

document { complete,
     TT "complete C", " -- completely fills out the chain complex C by
     calling upon the engine to provide the maps and modules computed
     by ", TO "resolution", ".",
     PARA,
     "This is mainly intended for developers of new routines for chain
     complexes which have to make use of their internal structure.
     Before running this routine, it is not possible to determine which
     spots in a chain complex are actually occupied by modules or maps."
     }

document { (drop, BasicList, List),
     Synopsis => {
	  "w = drop(v,{m,n})",
	  "v" => null,
	  "{m,n}" => "a pair of natural numbers",
	  "w" => {"a list by omitting those elements of the list
	       ", TT "v", " in positions ", TT "m", " through ", TT "n", "." }
	  },
     EXAMPLE "drop({a,b,c,d,e},{2,4})"
     }

document { (drop, BasicList, ZZ),
     Synopsis => {
	  "w = drop(v,n)",
	  "v" => null,
	  "n" => null,
	  "w" => {"a list obtained by omitting the first ", TT "n", " elements of 
	       the list ", TT "v", " if ", TT "n", " is positive, or
	       the last ", TT "-n", " elements if ", TT "n", " is negative."}
	  },
     EXAMPLE { "drop({a,b,c,d,e},2)", "drop({a,b,c,d,e},-2)", }
     }

document { drop,
     Headline => "drop some elements", 
     SEEALSO "take" }

document { (options, Function),
     Headline => "get optional arguments and defaults",
     TT "options f", " -- returns the table of option names and default values
     provided for the function ", TT "f", "."
     }
document { (options, Ring),
     Headline => "get values used for optional arguments",
     TT "options R", " -- returns the options used when the polynomial
     ring ", TT "R", " was created."
     }
document { (options, Monoid),
     Headline => "get values used for optional arguments",
     TT "options M", " -- returns the options used when the monoid ", TT "M", " 
     was created."
     }
document { options,
     Headline => "get options" }

document { (symbol <<, Nothing, Thing),
     Headline => "dummy file output",
     "null << x", " -- does nothing and returns ", TO "null", ".",
     PARA,
     "The intention here is that you can use ", TO "null", " as a dummy
     output file, but a bit of time is wasted converting ", TT "x", " to
     a net."
     }

document { mathML,
     Headline => "convert to MathML format",
     TT "mathML x", " -- converts ", TT "x", " to MathML form.",
     PARA,
     EXAMPLE {
	  "R = ZZ[x,y];",
	  "mathML matrix {{x,y},{x^2+2,0}}"
	  },
     SEEALSO "hypertext"
     }


document { symbol "#",
     Headline => "length, or access to elements",
     "The precedence of ", TT "#", " when used as a binary operator is high,
     as high as ", TT ".", ", but the precedence when used as a unary operator
     lower, as low as adjacency or function application.",
     SEEALSO{ "#?" }
     }
document { (symbol #, BasicList),
     Headline => "length",
     TT "#x", " -- provides the length of a list.",
     }
document { (symbol #, Sequence),
     Headline => "length",
     TT "#x", " -- provides the length of a sequence.",
     }
document { (symbol #, HashTable),
     Headline => "length",
     TT "#x", " -- provides the number of key-value pairs recorded
     in a hash table.",
     }
document { (symbol #, Set),
     Headline => "cardinality",
     TT "#x", " -- provides the number of elements in the set ", TT "x", "."
     }
document { (symbol #, String),
     Headline => "length",
     TT "#x", " -- provides the length of a string.",
     }
document { (symbol #, File),
     Headline => "length",
     TT "#x", " -- provides the length of a file.",
     }
document { (symbol #, HashTable, Thing),
     Headline => "get value from hash table",
     TT "x#i", " -- provides the value associated to the key ", TT "i", " in the hash table
     ", TT "x", ".",
     PARA,
     "Assignment to ", TT "x#i", " can change the value if ", TT "x", " is mutable.",
     EXAMPLE {
	  "x = new MutableHashTable",
	  "x#i = p",
	  "x#i",
	  },
     SEEALSO {(symbol #?, HashTable, Thing), "hashing"}
     }
document { (symbol #, Database, String),
     Headline => "get value from database",
     TT "x#i", " -- provides the value associated to the key ", TT "i", " in the database
     ", TT "x", ".",
     SEEALSO {(symbol #?, Database, String)}
     }
document { (symbol #, String, ZZ),
     Headline => "get character from string",
     TT "x#i", " -- provides the ", TT "i", "-th character of the string ", TT "x", ",
     as a string of length 1, if there is one.",
     PARA,
     "If ", TT "i", " is out of range, a string of length 0 is returned.
     If  ", TT "i", " is negative, then the ", TT "i", "-th character
     from the end is provided.",
     SEEALSO {(symbol #?, String, ZZ)}
     }
document { (symbol #, BasicList, ZZ),
     Headline => "get element from list",
     TT "x#i", " -- provides the ", TT "i", "-th element of the list ", TT "x", ".",
     PARA,
     "If ", TT "i", " is out of range, an error is signalled. If  ", TT "i", " 
     is negative, then the ", TT "i", "-th entry counting from the end is provided.",
     PARA,
     "Assignment to ", TT "x#i", " can change the value if ", TT "x", " is mutable.",
     SEEALSO {(symbol #?, BasicList, ZZ)}
     }
document { (symbol #, Sequence, ZZ),
     Headline => "get element from sequence",
     TT "x#i", " -- provides the ", TT "i", "-th element of the sequence ", TT "x", ".",
     PARA,
     "If ", TT "i", " is out of range, an error is signalled. If  ", TT "i", " 
     is negative, then the ", TT "i", "-th entry counting from the end is provided.",
     SEEALSO {(symbol #, Sequence, ZZ)}
     }
document { (symbol #?, HashTable, Thing),
     Headline => "check for value in hash table",
     TT "x#?i", " -- tells whether there is a value associated to the
     key ", TT "i", " stored in the hash table ", TT "x", ".",
     SEEALSO {(symbol #, HashTable, Thing), "hashing"}
     }
document { (symbol #?, Database, String),
     Headline => "check for value in database",
     TT "x#?i", " -- tells whether there is a value associated to the string
     ", TT "i", " in the database ", TT "x", ".",
     SEEALSO {(symbol #, Database, String)}
     }
document { (symbol #?, String, ZZ),
     Headline => "check for character in string",
     TT "x#?i", " -- tells whether there is an ", TT "i", "-th character in
     the string ", TT "x", ".",
     EXAMPLE {
	  ///"asdf" #? 2///,
	  ///"asdf" #? 22///
	  },
     SEEALSO {(symbol #, String, ZZ)}
     }
document { (symbol #?, BasicList, ZZ),
     Headline => "check for element in list",
     TT "x#?i", " --  tells whether there is an ", TT "i", "-th element in
     the list ", TT "x", ".",
     EXAMPLE {
	  ///{a,b,c} #? 2///,
	  ///{a,b,c} #? 22///
	  },
     SEEALSO {(symbol #, BasicList, ZZ)}
     }
document { (symbol #?, Sequence, ZZ),
     Headline => "check for element in sequence",
     TT "x#?i", " --tells whether there is an ", TT "i", "-th element in
     the sequence ", TT "x", ".",
     SEEALSO {(symbol #, Sequence, ZZ)}
     }

document { symbol "#?",
     Headline => "check for presence of elements",
     SEEALSO{ "#" }
     }

document { symbol "_",
     "A binary operator which is used for various mathematical operations
     that are customarily written with subscripts."
     }

document { (symbol _, List, ZZ),
     Headline => "get element from list",
     TT "x_i", " -- provides the ", TT "i", "-th element of the list ", TT "x", ".",
     PARA,
     "This is merely a convenient synonym for ", TT "x#i", ".",
     PARA,
     SEEALSO {(symbol #, BasicList, ZZ)}
     }

document { (symbol _, Sequence, ZZ),
     Headline => "get element from list",
     TT "x_i", " -- provides the ", TT "i", "-th element of the sequence ", TT "x", ".",
     PARA,
     "This is merely a convenient synonym for ", TT "x#i", ".",
     SEEALSO {(symbol #, BasicList, ZZ)}
     }

document { ".",
     Headline => "access to elements whose key is a symbol",
     TT "x.k", " -- the same as ", TT "x#(global k)", ", i.e., treat ", TT "k", " as
     a global symbol and provide the value stored in the hash table ", TT "x", " 
     under the key ", TT "k", ".",
     PARA,
     "May also be used in an assignment.",
     PARA,
     EXAMPLE {
	  "x = new MutableHashTable;",
      	  "x.k = 444",
      	  "x.k",
      	  "peek x",
	  },
     SEEALSO {"#", ".?", "global"}
     }

document { ".?",
     Headline => "check for presence of elements whose key is a symbol",
     TT "x.?k", " -- the same as ", TT "x#?(global k)", ", tells whether a value is
     available with ", TT "x.k", ".",
     PARA,
     SEEALSO{ ".", "#?" }
     }

document { autoload,
     Headline => "arrange for a function to be loaded automatically",
     TT "autoload(f,\"x\")", " -- arranges for a function ", TT "f", " to be 
     automatically loaded from the file named ", TT "x", " the first
     time it is used."
     }
