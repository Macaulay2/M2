--		Copyright 1993-1998 by Daniel R. Grayson
 
document { quote new,
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
     class ", TT "AA", " and parent ", TT "b", " initialized from ", TT "c", ".",
     PARA,
     "One may use this to model the mathematical notion
     that ", TT "x", " is an element of ", TT "A", " and a subset of ", TT "b", ".
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
     HR,
     NOINDENT,
     TT "new A of b", " -- make a hash table of class ", TT "A", "
     and parent ", TT "b", ".",
     PARA,
     "Same as above, except ", TT "c", " is missing.  Use ",
     PRE "          new AA of B := (A,b) -> ... ",
     "to install the initialization routine.",
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
     "Note that if the ", TT "of", " option is not used, then the class ", TT "A", "
     need not consist of hash tables or lists.  We are using this feature by
     installing a method so that ", TT "new ZZ", " returns an integer popped
     from the top of the engine's stack.",
     PARA,
     "The symbols ", TO "NewMethod", ", ", TO "NewOfMethod", ", ", 
     TO "NewFromMethod", ", and ", TO "NewOfFromMethod", " are used for 
     installation of the initialization routines.",
     SEEALSO {"classes", "of", "from"}
     }

document { quote "of",
     TT "of", " -- a keyword used with ", TO "new", "."
     }

document { quote "from",
     TT "from", " -- a keyword used with ", TO "new", "."
     }

document { quote NewMethod,
     TT "NewMethod", " -- a symbol used as a method name in conjuction with
     the ", TO "new", " operator."
     }

document { quote NewOfMethod,
     TT "NewOfMethod", " -- a symbol used as a method name in conjuction with
     the ", TO "new", " operator."
     }

document { quote NewFromMethod,
     TT "NewFromMethod", " -- a symbol used as a method name in conjuction with
     the ", TO "new", " operator."
     }

document { quote NewOfFromMethod,
     TT "NewOfFromMethod", " -- a symbol used as a method name in conjuction with
     the ", TO "new", " operator."
     }

document { quote Thing,
     TT "Thing", " -- the class of all things.",
     PARA,
     "Everything in Macaulay 2 is a ", ITALIC "thing", ".  This 
     includes numbers, strings, and lists.  More complicated things such as 
     polynomials, groups, rings, and chain complexes are implemented
     as ", ITALIC "hash tables", ".  The class of all things is ", TO "Thing", ".",
     PARA,
     "The basic types of things are:", 
     MENU {
          TO "BasicList",
          TO "Boolean",
          SHIELD TO "Database",
          TO "File",
          TO "Function",
          SHIELD TO "Handle",
          TO "HashTable",
          TO "Net",
          TO "Nothing",
          TO "QQ",
          TO "RR",
          TO "Sequence",
          TO "String",
          TO "Symbol",
          TO "Thing",
          TO "ZZ"
	  },
     "Operations on things:",
     MENU {
	  TO "comparison",
	  TO "assignment"
	  }
     }

document { quote Nothing,
     TT "Nothing", " -- the empty class.",
     PARA,
     "This class is useful for representing the class of an argument
     which is missing.  It is also used as the parent for those things which
     are not themselves types, i.e., which do not have instances." 
     }

document { quote Option,
     TT "Option", " -- the class of all pairs x => y.",
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
     the value ", TT "v", " to be stored in ", TT "x", " under the key ", TT "k", ".
     Other elements ", TT "s", " cause the value ", TT "true", " to be stored under 
     the key ", TT "s", "."
     }

document { quote OptionTable,
     TT "OptionTable", " -- the class of those hash tables which are used
     to store optional named parameters to functions.",
     SEEALSO "processArgs"
     }

document { quote processArgs,
     TT "processArgs(args,defs,fun)", " -- a helper function which processes
     optional arguments.  Intended for internal use only.",
     PARA,
     "Here ", TT "args", " is the sequence of arguments previously passed to 
     some function intended to accept optional arguments, ", TT "defs", " is a
     hash table whose keys are the names of the optional arguments, and 
     whose values are the corresponding default values.
     The return value is obtained by evaluation of ", TT "(fun opts)(args')", ",
     where ", TT "args'", " is obtained from ", TT "args", " by removing the
     options of the form ", TT "X=>A", " (where ", TT "X", " is a
     name of an optional argument), and ", TT "opts", " is a hash table
     of the same form as ", TT "defs", " in which the default
     values have been replaced by the user-supplied values, e.g., the
     value stored under the key ", TT "X", " has been replaced by
     ", TT "A", ".",
     PARA,
     "In the following example we use a simple definition for ", TT "fun", "
     so we can see everything that ", TT "fun", " receives from ", TT "processArgs", ".",
     EXAMPLE {
	  "defs = new OptionTable from {a=>1, b=>2}",
	  "fun = opts -> args -> {opts, args}",
	  "processArgs(x,defs,fun)",
	  "processArgs((x,y,b=>66),defs,fun)",
	  "processArgs((t,u,a=>44,b=>77),defs,fun)",
	  },
     SEEALSO {"making new functions with optional arguments", "OptionTable", "Option", "=>"}
     }

document { "using methods",
     "The method to be used for computing an expression such as ", TT "-x", " depends 
     on the type of ", TT "x", ".  For example, the method for negating a polynomial
     differs from the method for negating an integer modulo 111.  Each
     method is a function of one variable, and is stored in the class 
     of ", TT "x", " under a key which is referred to as the name of the method.
     For some built-in methods the method name is a symbol, but for
     methods created with ", TO "method", ", the method name is the same
     as the function used for calling it up.",
     PARA,
     "Let's assume that ", TT "X", " is the class of ", TT "x", ".  The way to install a method
     for the negation of an instance ", TT "x", " of ", TT "X", " is with a statement of the 
     following form.",
     PRE "- X := x ->( ... )",
     "Here ", TT "( ... )", " represents the body of the function, consisting of
     suitable code for the operation at hand.",
     PARA,
     "The method installed by the code above is automatically inherited by
     ", TO "subclass", "es of X.  Here is a brief description of the way 
     this works.  Suppose ", TT "X", " is the ", TO "parent", " of ", TT "P", ".  When an expression
     ", TT "-p", " is to be evaluated, where the class of ", TT "p", " is ", TT "P", ", then the method for
     ", TT "-P", " is applied, unless there isn't one, in which case the method for
     ", TT "-X", " is applied, and so on, all the way up the chain of parents to the
     topmost ancestor of everything, which is called ", TO "Thing", ".",
     PARA,
     "As an extreme example of inheritance, code like", 
     PRE "- Thing := x -> ...",
     "will install a method for negating anything, which will take
     effect as a last resort whenever more a specifically defined method
     isn't found.",
     PARA,
     "The user may introduce new methods as well as new method names.  So it
     is important to understand how methods are installed and consulted.",
     PARA,
     "Applying a method named ", TT "C", " to a thing ", TT "x", " whose class is ", TT "X", " means that",
     PRE "(lookup(C,X)) x",
     "is evaluated.  In other words, ", TT "C", " is used as a key
     to obtain a function from ", TT "X", " (or its parent, grandparent,
     and so on), and the function is applied to ", TT "x", ".  See ", TO "lookup", ".",
     PARA,
     "Installing a method named ", TT "C", " for the class ", TT "X", " is done with code such
     as ",
     PRE "C X := (x) -> ( ... )",
     "where ", TT "( ... )", " represents suitable code for the operation at hand.",
     PARA,
     "Here is the routine for making new methods.",
     MENU {
	  TO "method"
	  },
     SEEALSO{"binary method", "classes", "lookup"}
     }

document { "binary method",
     "The method for computing a sum ", TT "x+y", " depends on the types of ", TT "x", " and ", TT "y", ".
     For example, the method for adding an integer ", TT "x", " and a polynomial 
     ", TT "y", " differs from the method for adding two integers modulo 111.  Because
     both the type of ", TT "x", " and the type of ", TT "y", " must enter into the selection of
     the method, we refer to these methods as binary methods.  Each binary
     method is a function of two variables, and is stored either in the class
     of ", TT "x", " or in the class of ", TT "y", ".  See also ", TO "lookup", ".",
     PARA,
     "Let's assume that ", TT "X", " is the class (or type) of ", TT "x", ", 
     and that ", TT "Y", " is the class of ", TT "y", ".  The way to install a 
     method for the addition of an instance ", TT "x", " of class ", TT "X", " to 
     an instance ", TT "y", " of class ", TT "Y", " is with a statement of the form ",
     PRE "X + Y := (x,y) -> ( ... )",
     "where ", TT "( ... )", " represents the body of the function, consisting of suitable
     code for the operation at hand.",
     PARA,
     "The method installed by the code above is automatically inherited by 
     ", TO "subclass", "es of ", TT "X", " and ", TT "Y", ".  Here is a brief
     description of the way this works.  Suppose ", TT "X", " is the 
     ", TO "parent", " of ", TT "P", " and ", TT "Y", " is the parent of X.  When 
     a sum ", TT "p+q", " is evaluated where the class of ", TT "p", " is 
     ", TT "P", " and the class of ", TT "q", " is ", TT "Q", ", then the binary
     method for ", TT "P+Q", " is applied, unless there isn't one, in which
     case the binary method for ", TT "P+Y", " is applied, unless there isn't
     one, in which case the binary method for ", TT "X+Q", " is applied,
     unless there isn't one, in which case the binary method for ", TT "P+Q", "
     is applied.  In general this search for a binary method continues all
     the way up the chain of parents to the topmost ancestor of everything,
     which is called ", TO "Thing", ".",
     PARA,
     "As an extreme example of inheritance, the code ", 
     PRE "Thing + Thing := (x,y) -> ( ... )",
     "will install a binary method for adding any two things, which will take
     effect as a last resort whenever more a specifically defined method
     isn't found.",
     PARA,
     "The ", TO "new", " function also uses a ternary lookup table to
     find the initialization function for the new thing, and should
     be thought of as a ternary operator.  The initialization function
     for a new expression created by",
     PRE "new Z of x from y",
     "is obtained as",
     PRE "lookup(NewMethod,Z,X,Y)",
     "Here ", TT "X", " is ", TT "class x", ", and ", TT "Y", " is
     ", TT "class y", ".  The initialization function can be installed 
     with",
     PRE "new Z of X from Y := (z,y) -> ...",
     "where ", TT "z", " denotes the new hash table of class ", TT "Z", " and parent
     ", TT "x", " provided to the routine by the system."
     }

document {quote OptionsRegistry,
     TT "OptionsRegistry", " -- a hash table used for recording the tables of
     option names and their default values for those functions which accpet
     optional arguments.",
     PARA,
     "If ", TT "f", " is a function which accepts optional arguments, then
     the ", TO "OptionTable", " for ", TT "f", " is stored as ", 
     TT "OptionsRegistry#f", ".",
     PARA,
     "The function ", TO "method", ", when given a table of options, will
     record them here."
     }

document { quote SingleArgumentDispatch,
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

document { quote method,
     TT "f = method()", " -- creates a method function",
     PARA,
     "Optional arguments:",
     MENU {
	  TO "Associative",
	  -- TO "ClassArgument",
	  TO "SingleArgumentDispatch",
	  TO "Options"
	  },
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
     SEEALSO {"Options", "methods", "OptionsRegistry"}
     }

document { quote Associative,
     TT "Associative", " -- an option name for ", TO "method", " which
     allows associative methods to be created.",
     PARA,
     NOINDENT,
     TT "f = method(Associative=>true)", " -- creates an associative
     method which will call upon the appropriate binary methods for its arguments 
     two at a time.",
     PARA,
     "In the following example we install a method which isn't associative
     to illustrate the order of evaluation.",
     EXAMPLE {
	  "f = method(Associative => true)",
	  ///f(String,String) := (i,j) -> "(" | i | ")," | j;///,
      	  ///f("a","b","c","d")///,
	  },
     SEEALSO "method"
     }

document { quote size,
     TT "size x", " -- returns the size of ", TT "x", " which usually gives
     a rough indication of memory space required to store the object ", TT "x", ".",
     PARA,
     "For a polynomial, the size is the number of terms."
     }
document { quote baseName,
     TT "baseName x", " -- returns the variable or symbol upon which a generator of a
     monoid or polynomial ring is based."
     }
document { quote degree,
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

document { quote degreeLength,
     TT "degreeLength x", " -- returns the number of degrees of x.",
     PARA,
     "Here x may be a ring, in which case it returns the number of degrees
     (the length of the degree vector) used in grading the ring.",
     SEEALSO "degree"
     }

document { quote coefficients,
     TT "coefficients({i,j,...},p)", " -- yields the coefficients and
     monomials of the polynomial or matrix p with respect to variables 
     numbered i, j, ... .  This has to
     be completely redone, so I don't document it further, but it is used in
     the factoring code.",
     BR,NOINDENT,
     TT "coefficients(p)", " -- yields the coefficients and monomials of
     the polynomial or matrix p with respect to all of the variables."
     }

document { quote isIsomorphism,
     TT "isIsomorphism f", " -- whether the map f of modules is an isomorphism."
     }
document { quote isHomogeneous,
     TT "isHomogeneous x", " -- whether the polynomial or ideal x is homogeneous."
     }
document { quote vars, 
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

document { quote leadCoefficient,
     TT "leadCoefficient f", " -- return the leading coefficient of the polynomial
     or vector f.",
     PARA,
     SEEALSO {"leadTerm", "leadMonomial", "leadComponent"}
     }

document { quote leadComponent,
     TT "leadComponent f", " -- return the leading component of the vector f,
     i.e., the integer i so that f_i is the first nonzero component of f.",
     PARA,
     SEEALSO {"leadTerm", "leadCoefficient", "leadMonomial"}
     }

document { quote leadMonomial,
     TT "leadMonomial f", " -- return the leading monomial of the polynomial
     or vector f.",
     PARA,
     SEEALSO {"leadTerm", "leadCoefficient", "leadCoefficient"}
     }

document { quote flatten,
     TT "flatten m", " -- produces a new list from m by effectively removing the braces
     surrounding the elements of any elements of m which happen to be
     lists.  Also works for matrices.",
     PARA,
     EXAMPLE "flatten {{2,3,4},{{5}},6}"
     }
document { quote coker,
     "An abbreviation for ", TO "cokernel", "."
     }
document { quote cokernel,
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


document { quote image,
     TT "image h", " -- yields the image of the homomorphism h.",
     PARA,
     "The result will be a submodule of the target of h",
     PARA,
     "If h is a ring element, it is interpreted as a one by one matrix."
     }

document { quote source,
     TT "source h", " -- the source of a morphism h.",
     }

document { quote target,
     TT "target h", " -- the target of a morphism or Groebner basis.",
     }

document { quote ambient,
     TT "ambient M", " -- yields the ambient free module for the module M.",
     BR,
     NOINDENT,
     TT "ambient R", " -- yields the ambient ring of the quotient ring ", TT "R", ".
     For a Galois field it yields the ring it was constructed from.",
     PARA,
     EXAMPLE "ambient(ZZ/101[a,b]/b^3/a^3)",
     SEEALSO {"cover", "super"}
     }
     
document { quote Hom,
     TT "Hom(M,N)", " -- constructs the module of homomorphisms from M to N.",
     PARA,
     "Implemented with a method of the same name.",
     PARA,
     "Use ", TO "homomorphism", " to convert an element of the module of
     homomorphisms to a matrix."
     }
document { quote gens,
     "See ", TO "generators", "."
     }
document { quote generators,
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

document { quote someTerms,
     TT "someTerms(f,i,n)", " -- selects n terms from the polynomial f, starting
     with the i-th one, and returns the resulting polynomial."
     }

document { quote scanKeys,
     TT "scanKeys(x,f)", " -- apply the function f to each key used in the
     hash table or database x.",
     PARA,
     "This function requires an immutable hash table.  To scan the pairs in
     a mutable hash table, use ", TT "scan(keys x, f)", "."
     }

document { quote GlobalAssignHook,
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
	  ///GlobalAssignHook RR := (sym,val) -> << concatenate (
     "assigning ", name val, " to ", name sym
     ) << endl///,
          "a=4.5",
      	  "a=5.4",
	  },
     SEEALSO "GlobalReleaseHook"
     }

document { quote GlobalReleaseHook,
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
     "assigning ", name val, " to ", name sym
     ) << endl///,
          "a=4.5",
      	  "a=5.4",
	  },
     SEEALSO "GlobalAssignHook"
     }

document { quote stats,
     TT "stats g", " -- describe the status of a Groebner basis computation
     or of a resolution computation.",
     PARA,
     EXAMPLE {
	  "ZZ/101[a..f]",
      	  "stats gb matrix {{a*b, b*c},{a^3*f, b^3*e}}",
	  },
     SEEALSO { "GroebnerBasis", "Resolution" }
     }

document { quote complete,
     TT "complete C", " -- completely fills out the chain complex C by
     calling upon the engine to provide the maps and modules computed
     by ", TO "resolution", ".",
     PARA,
     "This is mainly intended for developers of new routines for chain
     complexes which have to make use of their internal structure.
     Before running this routine, it is not possible to determine which
     spots in a chain complex are actually occupied by modules or maps."
     }

document { quote drop,
     TT "drop(v,n) ", " -- yields the list obtained from the list v by
     dropping the first n elements.  Also works for sequences.",
     BR, NOINDENT,
     TT "drop(v,-n)", " -- yields the list obtained from the list v by dropping the 
     last n elements.",
     BR, NOINDENT,
     "drop(v,{m,n})", " -- yields the list obtained from the list v by dropping the
     elements at positions m through n.",
     PARA,
     EXAMPLE {
	  "drop({a,b,c,d,e},2)",
	  "drop({a,b,c,d,e},-2)",
	  "drop({a,b,c,d,e},{2,3})",
	  },
     SEEALSO{ "take"}
     }

document { quote options,
     TT "options f", " -- returns the table of option names and default values
     provided for the function ", TT "f", ", if one has been registered.",
     BR,NOINDENT,
     TT "options X", " -- returns the options used when the monoid or polynomial
     ring X was created.",
     BR,NOINDENT,
     TT "options S", " -- returns a list of those functions which have an
     optional argument named ", TT "S", ".  Here ", TT "S", " is a symbol.",
     PARA,
     SEEALSO {"method", "OptionsRegistry"}
     }
document { (quote <<, Nothing, Thing),
     "null << x", " -- does nothing and returns ", TT "null", ".",
     PARA,
     "The intention here is that you can use ", TT "null", " as a dummy
     output file."
     }

document { (quote =>, Thing, Thing),
     TT "x => y", " -- an ", TO "Option", ", used as an optional argument with 
     some functions."
     }

document { quote mathML,
     TT "mathML x", " -- converts ", TT "x", " to MathML form.",
     PARA,
     EXAMPLE {
	  "R = ZZ[x,y];",
	  "mathML matrix {{x,y},{x^2+2,0}}"
	  },
     SEEALSO "hypertext"
     }
