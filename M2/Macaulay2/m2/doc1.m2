--		Copyright 1993-1999 by Daniel R. Grayson

TEST ///
assert( null =!= documentation sin)
assert( null =!= documentation "sin")
assert( null =!= documentation symbol sin)
///

document { document,
     TT "document {s, d}", " -- install documentation ", TT "d", " for 
     the topic ", TT "s", ".",
     PARA,
     "The documentation ", TT "d", " should be ", TO "hypertext", ".  The topic
     ", TT "s", " may be one of the special forms useable with ", TO "TO", ".  As
     a convenience, lists and sequences in ", TT "d", " are converted to ", TT "SEQ", "
     mark up items, and instances of ", TO "MarkUpType", ", such as ", TO "PARA", "
     are converted to empty instances of their type.",
     PARA,
     SEEALSO {"help functions"}
     }

document { TEST,
     TT "TEST s", " -- writes the string ", TT "s", " to a new test file.  The
     commands in that file can be run later as a test.",
     PARA,
     "Intended for internal use only."
     }

document { between,
     TT "between(m,v)", " -- inserts ", TT "m", " between each pair of elements 
     of the list or sequence ", TT "v", ", returning a list.",
     PARA,
     EXAMPLE {
	  "between(55,{a,b,c,d})"
	  }
     }

document { SEEALSO,
     TT "SEEALSO {a, b, ...}", " -- inserts, into a documentation page, a sentence
     instructing the reader to see some other topics.",
     PARA,
     "The topics may have the special forms used with ", TO "TO", ".",
     SEEALSO "document"
     }

document { briefDocumentation,
     TT "briefDocumentation s", " -- provides the first paragraph of the online
     documentation for the topic ", TT "s", ", in internal ", TO "hypertext", "
     form, suitable for conversion to text with ", TO "text", " or to html 
     with ", TO "html", ".",
     SEEALSO "documentation"
     }

document { documentation,
     TT "documentation s", " -- provides the online documention for the topic s, in
     internal ", TO "hypertext", " form, suitable for conversion to
     text with ", TO "text", " or to html with ", TO "html", ".",
     PARA,
     EXAMPLE "documentation partitions"
     }

document { help,
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
     TT "topicList()", " -- provides a complete list of topics on which help 
     is available.",
     PARA,
     "Intended to be used in programs.  Users will prefer 
     to use ", TO "topics", ".",
     PARA,
     SEEALSO "help"
     }

document { topics,
     HEADLINE "available help topics",
     TT "topics", " -- displays a list of topics on which help is available.",
     PARA,
     "topics() -- Does the same in a function or file.",
     PARA,
     SEEALSO "help"
     }

document { apropos,
     HEADLINE "symbols matching a pattern",
     TT "apropos s", " -- displays a list of global symbols which match
     the pattern specified by the string ", TT "s", ".",
     PARA,
     "The pattern may contain '*'s as wild card characters.",
     EXAMPLE "apropos \"scan\""
     }

document { printExamples,
     TT "printExamples f", " -- prints out the examples of code using
     the function ", TT "f", " provided in the documentation for
     ", TT "f", ".",
     PARA,
     EXAMPLE "printExamples partition",
     SEEALSO {"examples", "document"}
     }

document { symbol "Symbols",
     TT "Symbols", " -- a hash table which can be used to obtain the global
     symbol with a particular value.",
     PARA,
     "This is useful internally for getting the name of a function, for 
     example."
     }

document { symbol "Documentation",
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
     TT "formatDocumentTag x", " -- formats the tags used with ", TO "TO", " for
     display purposes in documents.",
     PARA,
     "This function is intended for internal use only."
     }

document { uniform,
     TT "uniform x", " -- whether all elements of the list x have the same class."
     }
document { newClass,
     TT "newClass(N,m)", " -- makes a copy of m with N as the new class", BR,
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
     HEADLINE "the class of all mutable lists",
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
     HEADLINE "look up methods",
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
     "If no method is found, then ", TT "null", " is returned.",
     PARA,
     SEEALSO {"#", "classes", "installMethod", "youngest"}
     }

document { installMethod,
     HEADLINE "install methods",
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
     SEEALSO{"#", "lookup",  "new", "classes"}
     }

document { "new",
     HEADLINE "new objects of various types",
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
     SEEALSO {"classes"}
     }

document { "of", HEADLINE "a keyword",
     TT "of", " -- a keyword used with ", TO "new", "."
     }

document { "from", HEADLINE "a keyword",
     TT "from", " -- a keyword used with ", TO "new", "."
     }

document { NewMethod,
     TT "NewMethod", " -- a symbol used as a method name in conjuction with
     the ", TO "new", " operator."
     }

document { NewOfMethod,
     TT "NewOfMethod", " -- a symbol used as a method name in conjuction with
     the ", TO "new", " operator."
     }

document { NewFromMethod,
     TT "NewFromMethod", " -- a symbol used as a method name in conjuction with
     the ", TO "new", " operator."
     }

document { NewOfFromMethod,
     TT "NewOfFromMethod", " -- a symbol used as a method name in conjuction with
     the ", TO "new", " operator."
     }

document { Thing,
     HEADLINE "the class of all things",
     "Everything in Macaulay 2 is a ", ITALIC "thing", ".  This 
     includes numbers, strings, and lists.  More complicated things such as 
     polynomials, groups, rings, and chain complexes are implemented
     as ", ITALIC "hash tables", ".  See ", TO "Type", " for information 
     about what types of things there are."
     }

document { Nothing,
     HEADLINE "the empty class",
     "This class is useful for representing the class of an argument
     which is missing.  It is also used as the parent for those things which
     are not themselves types, i.e., which do not have instances." 
     }

document { Option,
     HEADLINE "the class of all pairs x => y",
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
     SEEALSO {"classes", "=>"}
     }

document { (NewFromMethod, HashTable, List),
     TT "new HashTable from x", " -- produce a new hash table from a
     list ", TT "x", ".",
     PARA,
     "Elements of ", TT "x", " which are options, ", TT "k => v", " cause
     the value ", TT "v", " to be stored in ", TT "x", " under the key ", TT "k", ".",
     SEEALSO "hashTable"
     }

document { OptionTable, HEADLINE "the class of hash tables for optional arguments", SEEALSO "==>" }

document { (symbol "==>", List, Function),
     "See ", TO (symbol "==>", OptionTable, Function), " for details."
     }

document { (symbol "==>", OptionTable, Function),
     TT "g = defs ==> fun", " -- produces a new function ", TT "g", " from 
     the function ", TT "fun", " that processes optional arguments.",
     PARA,
     "Here ", TT "defs", " is a hash table of class ", TO "OptionTable", " whose 
     keys are the names of the optional arguments, and whose values are the
     corresponding default values.
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

document { SingleArgumentDispatch,
     TT "SingleArgumentDispatch=>true", " -- an option to ", TO "method", "
     which specifies whether the method function should treat several
     arguments as a single argument, i.e., as a sequence.",
     PARA,
     "This allows the user to install a method for handling sequences, whereas
     normally, the types of up to the three arguments are considered.",
     EXAMPLE {
	  "f = method ( SingleArgumentDispatch => true )",
      	  "f Sequence := print",
	  "f (1,2,3)"
	  }
     }

document { symbol "typicalValues",
     HEADLINE "types of values returned by functions",
     "A hash table used to store information about the type of values
     typically returned by functions and methods.",
     PARA,
     "This information is used only to build documentation automatically.",
     EXAMPLE "typicalValues#isRing",
     SEEALSO { "specifying typical values" }
     }

document { (method => TypicalValue),
     HEADLINE "specify return value type",
     TT "TypicalValue => X", " -- an option to ", TO "method", "
     which specifies that values returned by the method function will
     typically be of type ", TT "X", ".",
     PARA,
     "This information is used only to build documentation automatically, and
     is stored in the hash table ", TO "typicalValues", ".",
     SEEALSO { "specifying typical values" }
     }

document { method,
     HEADLINE "make a new method function",
     TT "f = method()", " -- creates a method function",
     PARA,
     "The code above creates a method function which takes up to three 
     arguments, looking up the appropriate method according to the classes of 
     the arguments, with inheritance.  To install a method for two arguments,
     (x,y), of classes X and Y, use code like this:",
     PRE "     f(X,Y) := (x,y) -> ...",
     "where '...' represents the body of the function you wish to install.
     The syntax for one or three arguments is analogous.  For a single
     argument x of class X, one could also write:",
     PRE "     f X := (x) -> ...",
     "the effect of which happens to be the same as that of",
     PRE "     X#f := (x) -> ...",
     PARA,
     SEEALSO {"Options", "methods" }
     }

document { method => Associative,
     HEADLINE "allows associative methods to be created",
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
     TT "size x", " -- returns the size of ", TT "x", " which usually gives
     a rough indication of memory space required to store the object ", TT "x", ".",
     PARA,
     "For a polynomial, the size is the number of terms.",
     PARA,
     "This function should be replaced by something more generally useful."
     }

document { baseName,
     TT "baseName x", " -- returns the variable or symbol upon which a generator of a
     monoid or polynomial ring is based."
     }

document { degree,
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
     TT "degreeLength x", " -- returns the number of degrees of x.",
     PARA,
     "Here x may be a ring, in which case it returns the number of degrees
     (the length of the degree vector) used in grading the ring.",
     SEEALSO "degree"
     }

document { coefficients,
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
     HEADLINE "whether a map is an isomorphism",
     TT "isIsomorphism f", " -- whether the map f of modules is an isomorphism."
     }
document { isHomogeneous,
     HEADLINE "test for homogeneity",
     TT "isHomogeneous x", " -- whether the polynomial or ideal x is homogeneous."
     }
document { vars, 
     TT "vars R", " -- provides a 1 by n matrix whose entries are the
     variables of the polynomial ring R.",
     BR,
     NOINDENT,
     TT "vars(i .. j)", " -- provides a sequence of symbols which can be used
     as indeterminates in a polynomial ring, the i-th one through the j-th one.
     There is no limit on the size of the integers ", TT "i", " and ", TT "j", ".",
     PARA,
     EXAMPLE {
	  "vars(3 .. 9,1000,-100)",
      	  "R = ZZ/101[vars(3 .. 5)]",
      	  "vars R",
      	  "symmetricPower(2,vars R)"
	  }
     }

document { leadCoefficient,
     TT "leadCoefficient f", " -- return the leading coefficient of the polynomial
     or vector f.",
     PARA,
     SEEALSO {"leadTerm", "leadMonomial", "leadComponent"}
     }

document { leadComponent,
     TT "leadComponent f", " -- return the leading component of the vector f,
     i.e., the integer i so that f_i is the first nonzero component of f.",
     PARA,
     SEEALSO {"leadTerm", "leadCoefficient", "leadMonomial"}
     }

document { leadMonomial,
     TT "leadMonomial f", " -- return the leading monomial of the polynomial
     or vector f.",
     PARA,
     SEEALSO {"leadTerm", "leadCoefficient", "leadCoefficient"}
     }

document { flatten,
     TT "flatten m", " -- produces a new list from ", TT "m", " by
     effectively removing the braces surrounding the elements
     of any elements of m which happen to be lists.  Also works
     for matrices.",
     PARA,
     EXAMPLE "flatten {{2,3,4},{{5}},6}"
     }
document { symbol "coker",
     "An abbreviation for ", TO "cokernel", "."
     }
document { cokernel,
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
     TT "image h", " -- yields the image of the homomorphism h.",
     PARA,
     "The result will be a submodule of the target of h",
     PARA,
     "If h is a ring element, it is interpreted as a one by one matrix."
     }

document { source,
     TT "source h", " -- the source of a morphism h.",
     }

document { target,
     TT "target h", " -- the target of a morphism or Groebner basis.",
     }

document { ambient,
     TT "ambient M", " -- yields the ambient free module for the module M.",
     BR,
     NOINDENT,
     TT "ambient R", " -- yields the ambient ring of the quotient ring ", TT "R", ".
     For a Galois field it yields the ring it was constructed from.",
     PARA,
     EXAMPLE "ambient(ZZ/101[a,b]/b^3/a^3)",
     SEEALSO {"cover", "super"}
     }
     
document { Hom,
     TT "Hom(M,N)", " -- constructs the module of homomorphisms from M to N.",
     PARA,
     "Implemented with a method of the same name.",
     PARA,
     "Use ", TO "homomorphism", " to convert an element of the module of
     homomorphisms to a matrix."
     }
document { "gens",
     "See ", TO "generators", "."
     }
document { generators,
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

document { someTerms,
     TT "someTerms(f,i,n)", " -- selects n terms from the polynomial f, starting
     with the i-th one, and returns the resulting polynomial."
     }

document { scanKeys,
     TT "scanKeys(x,f)", " -- apply the function ", TT "f", " to each key used in the
     hash table or database ", TT "x", ".",
     PARA,
     "This function requires an immutable hash table.  To scan the keys in
     a mutable hash table, use ", TT "scan(keys x, f)", "."
     }

document { scanValues,
     TT "scanValues(x,f)", " -- apply the function ", TT "f", " to each value
     appearing in the hash table ", TT "x", ".",
     PARA,
     "This function requires an immutable hash table.  To scan the values in
     a mutable hash table, use ", TT "scan(values x, f)", "."
     }

document { GlobalAssignHook,
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
	  ///GlobalAssignHook RR := (sym,val) -> <<"assigning " <<val <<" to " <<sym <<endl;///,
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
	  ///GlobalReleaseHook RR := (sym,val) -> << concatenate (
     "assigning ", toString val, " to ", toString sym
     ) << endl///,
          "a=4.5",
      	  "a=5.4",
	  },
     SEEALSO "GlobalAssignHook"
     }

document { stats,
     TT "stats g", " -- describe the status of a Groebner basis computation
     or of a resolution computation.",
     PARA,
     EXAMPLE {
	  "ZZ/101[a..f]",
      	  "stats gb matrix {{a*b, b*c},{a^3*f, b^3*e}}",
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
     HEADLINE "drop some elements",
     TT "drop(v,{m,n})", " -- yields the list obtained from the list 
     ", TT "v", " by dropping the elements at positions ", TT "m", " through ", TT "n", ".",
     EXAMPLE {
	  "drop({a,b,c,d,e},{2,3})",
	  },
     SEEALSO{ "take"}
     }

document { (drop, BasicList, ZZ),
     HEADLINE "drop some elements",
     TT "drop(v,n) ", " -- yields the list obtained from the list ", TT "v", " by
     dropping the first ", TT "n", " elements.",
     PARA,
     "If ", TT "n", " is negative, then the last ", TT "-n", " elements are 
     dropped.",
     EXAMPLE {
	  "drop({a,b,c,d,e},2)",
	  "drop({a,b,c,d,e},-2)",
	  },
     SEEALSO{ "take"}
     }

document { drop, HEADLINE "drop some elements",
     SEEALSO{ "take"}
     }

document { (options, Function), HEADLINE "get optional arguments and defaults",
     TT "options f", " -- returns the table of option names and default values
     provided for the function ", TT "f", "."
     }
document { (options, Ring), HEADLINE "get values used for optional arguments",
     TT "options R", " -- returns the options used when the polynomial
     ring ", TT "R", " was created."
     }
document { (options, Monoid), HEADLINE "get values used for optional arguments",
     TT "options M", " -- returns the options used when the monoid ", TT "M", " 
     was created."
     }
document { options, HEADLINE "get options" }

document { (symbol <<, Nothing, Thing), HEADLINE "dummy file output",
     "null << x", " -- does nothing and returns ", TT "null", ".",
     PARA,
     "The intention here is that you can use ", TT "null", " as a dummy
     output file, but a bit of time is wasted converting ", TT "x", " to
     a net."
     }

document { mathML,
     TT "mathML x", " -- converts ", TT "x", " to MathML form.",
     PARA,
     EXAMPLE {
	  "R = ZZ[x,y];",
	  "mathML matrix {{x,y},{x^2+2,0}}"
	  },
     SEEALSO "hypertext"
     }


document { symbol "#",
     HEADLINE "length, or access to elements",
     "The precedence of ", TT "#", " when used as a binary operator is high,
     as high as ", TT ".", ", but the precedence when used as a unary operator
     lower, as low as adjacency or function application.",
     SEEALSO{ "#?" }
     }
document { (symbol #, BasicList), HEADLINE "length",
     TT "#x", " -- provides the length of a list.",
     }
document { (symbol #, Sequence), HEADLINE "length",
     TT "#x", " -- provides the length of a sequence.",
     }
document { (symbol #, HashTable), HEADLINE "length",
     TT "#x", " -- provides the number of key-value pairs recorded
     in a hash table.",
     }
document { (symbol #, Set), HEADLINE "cardinality",
     TT "#x", " -- provides the number of elements in the set ", TT "x", "."
     }
document { (symbol #, String), HEADLINE "length",
     TT "#x", " -- provides the length of a string.",
     }
document { (symbol #, File), HEADLINE "length",
     TT "#x", " -- provides the length of a file.",
     }
document { (symbol #, HashTable, Thing), HEADLINE "get value from hash table",
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
document { (symbol #, Database, String), HEADLINE "get value from database",
     TT "x#i", " -- provides the value associated to the key ", TT "i", " in the database
     ", TT "x", ".",
     SEEALSO {(symbol #?, Database, String)}
     }
document { (symbol #, String, ZZ), HEADLINE "get character from string",
     TT "x#i", " -- provides the ", TT "i", "-th character of the string ", TT "x", ",
     as a string of length 1, if there is one.",
     PARA,
     "If ", TT "i", " is out of range, a string of length 0 is returned.
     If  ", TT "i", " is negative, then the ", TT "i", "-th character
     from the end is provided.",
     SEEALSO {(symbol #?, String, ZZ)}
     }
document { (symbol #, BasicList, ZZ), HEADLINE "get element from list",
     TT "x#i", " -- provides the ", TT "i", "-th element of the list ", TT "x", ".",
     PARA,
     "If ", TT "i", " is out of range, an error is signalled. If  ", TT "i", " 
     is negative, then the ", TT "i", "-th entry counting from the end is provided.",
     PARA,
     "Assignment to ", TT "x#i", " can change the value if ", TT "x", " is mutable.",
     SEEALSO {(symbol #?, BasicList, ZZ)}
     }
document { (symbol #, Sequence, ZZ), HEADLINE "get element from sequence",
     TT "x#i", " -- provides the ", TT "i", "-th element of the sequence ", TT "x", ".",
     PARA,
     "If ", TT "i", " is out of range, an error is signalled. If  ", TT "i", " 
     is negative, then the ", TT "i", "-th entry counting from the end is provided.",
     SEEALSO {(symbol #, Sequence, ZZ)}
     }
document { (symbol #?, HashTable, Thing), HEADLINE "check for value in hash table",
     TT "x#?i", " -- tells whether there is a value associated to the
     key ", TT "i", " stored in the hash table ", TT "x", ".",
     SEEALSO {(symbol #, HashTable, Thing), "hashing"}
     }
document { (symbol #?, Database, String), HEADLINE "check for value in database",
     TT "x#?i", " -- tells whether there is a value associated to the string
     ", TT "i", " in the database ", TT "x", ".",
     SEEALSO {(symbol #, Database, String)}
     }
document { (symbol #?, String, ZZ), HEADLINE "check for character in string",
     TT "x#?i", " -- tells whether there is an ", TT "i", "-th character in
     the string ", TT "x", ".",
     EXAMPLE {
	  ///"asdf" #? 2///,
	  ///"asdf" #? 22///
	  },
     SEEALSO {(symbol #, String, ZZ)}
     }
document { (symbol #?, BasicList, ZZ), HEADLINE "check for element in list",
     TT "x#?i", " --  tells whether there is an ", TT "i", "-th element in
     the list ", TT "x", ".",
     EXAMPLE {
	  ///{a,b,c} #? 2///,
	  ///{a,b,c} #? 22///
	  },
     SEEALSO {(symbol #, BasicList, ZZ)}
     }
document { (symbol #?, Sequence, ZZ), HEADLINE "check for element in sequence",
     TT "x#?i", " --tells whether ther is an ", TT "i", "-th element in
     the sequence ", TT "x", ".",
     SEEALSO {(symbol #, Sequence, ZZ)}
     }

document { symbol "#?",
     HEADLINE "check for presence of elements",
     SEEALSO{ "#" }
     }

document { symbol "_",
     "A binary operator which is used for various mathematical operations
     that are customarily written with subscripts."
     }

document { (symbol _, List, ZZ), HEADLINE "get element from list",
     TT "x_i", " -- provides the ", TT "i", "-th element of the list ", TT "x", ".",
     PARA,
     "This is merely a convenient synonym for ", TT "x#i", ".",
     PARA,
     SEEALSO {(symbol #, BasicList, ZZ)}
     }

document { (symbol _, Sequence, ZZ), HEADLINE "get element from list",
     TT "x_i", " -- provides the ", TT "i", "-th element of the sequence ", TT "x", ".",
     PARA,
     "This is merely a convenient synonym for ", TT "x#i", ".",
     SEEALSO {(symbol #, BasicList, ZZ)}
     }

document { ".",
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
     TT "x.?k", " -- the same as ", TT "x#?(global k)", ", tells whether a value is
     available with ", TT "x.k", ".",
     PARA,
     SEEALSO{ ".", "#?" }
     }

document { autoload,
     TT "autoload(f,\"x\")", " -- arranges for a function ", TT "f", " to be 
     automatically loaded from the file named ", TT "x", " the first
     time it is used."
     }
