--		Copyright 1994 by Daniel R. Grayson

document { "using methods",
     "The method to be used for computing an expression such as -x depends 
     on the type of x.  For example, the method for negating a polynomial
     differs from the method for negating an integer modulo 111.  Each
     method is a function of one variable, and is stored in the class 
     of x under a key which is referred to as the name of the method.
     For some built-in methods the method name is a ", TO "Symbol", ", but for
     methods created with ", TO "method", ", the method name is the same
     as the function used for calling it up.",
     PARA,
     "Let's assume that X is the class of x.  The way to install a method
     for the negation of an instance x of X is with a statement of the 
     following form.",
     PRE "          - X := x -> ...",
     "Here '...' represents the body of the function, consisting of
     suitable code for the operation at hand.",
     PARA,
     "The method installed by the code above is automatically inherited by
     ", TO "subclass", "es of X.  Here is a brief description of the way 
     this works.  Suppose X is the ", TO "parent", " of P.  When an expression
     -p is to be evaluated, where the class of p is P, then the method for
     -P is applied, unless there isn't one, in which case the method for
     -X is applied, and so on, all the way up the chain of parents to the
     topmost ancestor of everything, which is called ", TO "Thing", ".",
     PARA,
     "As an extreme example of inheritance, the code ", 
     PRE "          - Thing := x -> ...",
     "will install a method for negating anything, which will take
     effect as a last resort whenever more a specifically defined method
     isn't found.",
     PARA,
     "The user may introduce new methods as well as new method names.  So it
     is important to understand how methods are installed and consulted.",
     PARA,
     "Applying a method named C to a thing x whose class is X means that",
     PRE "          (lookup(C,X)) x",
     "is evaluated.  In other words, C is used as a key
     to obtain a function from X (or its parent, grandparent,
     and so on), and the function is applied to x.  See ", TT "lookup", ".",
     PARA,
     "Installing a method named C for the class X is done with code such
     as ",
     PRE "          X#C = (x) -> ...",
     "where '...' represents suitable code for the operation at hand.",
     PARA,
     "Here is the routine for making new methods.",
     MENU {
	  TO "method"
	  },
     SEEALSO( ".", "binary method", "classes")
     }

document { "binary method",
     "The method for computing a sum x+y depends on the types of x and y.
     For example, the method for adding an integer x and a polynomial 
     y differs from the method for adding two integers modulo 111.  Because
     both the type of x and the type of y must enter into the selection of
     the method, we refer to these methods as binary methods.  Each binary
     method is a function of two variables, and is stored either in the class
     of x or in the class of y.  See also ", TO "lookup", ".",
     PARA,
     "Let's assume that X is the class (or type) of x, and that Y is the
     class of y.  The way to install a method for the addition of an
     instance x of class X to an instance y of class Y is with a statement
     of the form ",
     PRE "          X + Y := (x,y) -> ...",
     "where ... represents the body of the function, consisting of suitable
     code for the operation at hand.",
     PARA,
     "The method installed by the code above is automatically inherited by
     ", TO "subclass", "es of X and Y.  Here is a brief description of the way 
     this works.  Suppose X is the ", TO "parent", " of P and Y is the parent
     of X.  When a sum p+q is evaluated where the class of p is P and the class
     of q is Q, then the binary method for P+Q is applied, unless there isn't one,
     in which case the binary method for P+Y is applied, unless there isn't one,
     in which case the binary method for X+Q is applied, unless there isn't one,
     in which case the binary method for P+Q is applied.  In general this search 
     for a binary method continues all the way up the chain of parents to the
     topmost ancestor of everything, which is called ", TO "Thing", ".",
     PARA,
     "As an extreme example of inheritance, the code ", 
     PRE "          Thing + Thing := (x,y) -> ...",
     "will install a binary method for adding any two things, which will take
     effect as a last resort whenever more a specifically defined method
     isn't found.",
     PARA,
     "The ", TO "new", " function also uses a ternary lookup table to
     find the initialization function for the new thing, and should
     be thought of as a ternary operator.  The initialization function
     for a new expression created by",
     PRE "         new Z of x from y", "is",
     PRE "         lookup(NewMethod,Z,X,Y)",
     "and can be installed with ",
     PRE "         new Z of X from Y := (z,y) -> ...",
     "where z denotes the new hash table of class Z and parent x provided
     to the routine by the system."
     }

noapp := (f,x) -> error(
     "no method for applying item of class ", name class f, 
     " to item of class ", name class x
     )

Symbol Thing := (f,x) -> (
     m := lookup(f,class x);
     if m =!= null then (
	  if class m === Function then m x
	  else noapp(f,x)
	  )
     else noapp(f,x))

-----------------------------------------------------------------------------

protect Options
OptionsRegistry = new MutableHashTable
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
noMethod := args -> (
     if class args === Sequence 
     then if 0 < #args and #args <= 3 
     then error("no method found for items of classes ",name apply(args, class))
     else error("no method found for item of class Sequence and length ",string(#args))
     else error("no method found for item of class ", name class args)
     )

methodDefaults := new OptionTable from {
     SingleArgumentDispatch => false,
     -- ClassArgument => {},
     Associative => false,
     Options => null
     }

method = args -> processArgs(
  args,
  methodDefaults,
  (args,options) -> (
    () -> (
      if options.Options === null then (
	if options.Associative then (
	  methodFunction := newmethod1 noMethod;
	  sequenceMethod := methodFunction(Sequence) :=
	  args -> (
	    -- Common code for every associative method without options
	    if #args === 2 
	    then ((x,y) -> (
		f := lookup(methodFunction,class x,class y);
		if f === null then noMethod args
		else f(x,y))
	      ) args
	    else if #args >= 3 
	    then sequenceMethod prepend(sequenceMethod(args#0,args#1),drop(args,2))
	    else if #args === 1 then args#0
	    else if #args === 0 then noMethod args
	    else error "wrong number of arguments"
	    ))
	else if options.SingleArgumentDispatch
	then methodFunction = newmethod1 noMethod
	else (
	  if false -- options.FirstArgumentDispatch
	  then (
	    methodFunction = newmethod1 noMethod;
	    methodFunction(Sequence) :=
	    args -> (
	      -- Common code for methods that dispatch on first argument
	      -- and receive a sequence of arguments.
	      -- Using browse?  Try looking at the METHODS.
	      f := lookup(methodFunction, class args#0);
	      if f === null then noMethod args else f args
	      )
	    )
	  else (
	    methodFunction = newmethod123c(,noMethod,
		 {} -- options.ClassArgument
		 );
	    methodFunction(Sequence) := newmethod123c(
	      methodFunction, noMethod, 
	      {} -- options.ClassArgument
	      ))))
      else (
	opts := new OptionTable from options.Options;
	methodFunction = 
	args -> processArgs(args,opts,
	  -- Common code for every method with options.
	  -- Using browse?  Try looking at the METHODS.
	  (args,options) -> (
	    if #args === 1 then args = args#0;
	    f := lookup(methodFunction, class args);
	    if f === null then noMethod args
	    else f(args,options)));
	OptionsRegistry#methodFunction = opts;
	methodFunction(Sequence) := 
	(args,options) -> (
	  -- Common code for every method with options
	  if #args === 2 
	  then ((x,y) -> (
	      f := lookup(methodFunction,class x,class y);
	      if f === null then noMethod args
	      else f(x,y,options))
	    ) args
	  else if #args === 3 
	  then ((x,y,z) -> (
	      f := lookup(methodFunction,class x,class y,class z);
	      if f === null then noMethod args
	      else f(x,y,z,options))
	    ) args
	  else if #args === 1 
	  then ((x) -> (
	      f := lookup(methodFunction,class x);
	      if f === null then noMethod args
	      else f(x,options))
	    ) args
	  else if #args === 0
	  then noMethod args
	  else error "wrong number of arguments"
	  )
	);
      methodFunction
      )
    ) args
  )

OptionsRegistry#method = methodDefaults

document { quote SingleArgumentDispatch,
     TT "SingleArgumentDispatch=>true", " -- an option to ", TO "method", "
     which specifies whether the method function should treat several
     arguments as a single argument, i.e., as a sequence.",
     PARA,
     "This allows the user to install a method for handling sequences, whereas
     normally, the types of up to the three arguments are considered.",
     EXAMPLE "f = method ( SingleArgumentDispatch => true )",
     EXAMPLE "f Sequence := print",
     EXAMPLE "f (1,2,3)"
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
     SEEALSO ("Options", "methods", "OptionsRegistry")
     }

-- document { quote ClassArgument,
--      TT "ClassArgument", " -- an option name for ", TO "method", " which
--      allows one argument to the method function to be interpreted as a
--      class in its own right.",
--      PARA,
--      NOINDENT,
--      TT "f = method(ClassArgument => {false,true})", " -- provides a list
--      of boolean values, the n-th of which specifies whether the n-th
--      argument presented to the the method function, rather than its class,
--      will participate in the search for a method function.",
--      PARA,
--      "The code above creates a function named 'f' which takes up to three 
--      arguments, looking up the appropriate method according to its arguments,
--      with inheritance.  To install a method for two arguments,
--      (x,Y), where x is of class X, use code like this:",
--      PRE "     f(X,Y) := (x,Y) -> ...",
--      "where '...' represents the body of the function you wish to install.
--      The syntax for one or three arguments is analogous.",
--      PARA,
--      "We give two examples for contrast.",
--      EXAMPLE "f = method ();",
--      EXAMPLE "f(ZZ,QQ) := print;",
--      EXAMPLE "f(3,3/2)",
--      "The method function used above was effectively found as the value of
--      ", TT "lookup(f,class 3, class 3/2)", ".",
--      EXAMPLE "g = method (ClassArgument => {false,true});",
--      EXAMPLE "g(ZZ,QQ) := print;",
--      EXAMPLE "g(3,QQ)",
--      "The method function used above was effectively found as the value of
--      ", TT "lookup(f,class 3, QQ)", ".  Notice that ", TT "QQ", ", the second
--      argument, participated directly, rather than its class.",
--      PARA,
--      "This option is incompatible with the ", TO "Associative", "option,
--      and hasn't yet been implemented in conjunction with the ", TO "Options", "
--      option.",
--      SEEALSO "method"
--      }

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
     EXAMPLE "f = method(Associative => true)",
     EXAMPLE "f(String,String) := (i,j) -> \"(\" | i | \"),\" | j;",
     EXAMPLE "f(\"a\",\"b\",\"c\",\"d\")",
     SEEALSO "method"
     }

document { quote Options,
     TT "Options", " -- an option used with ", TO "method", " to specify
     names of optional arguments and their default values.",
     PARA,
     NOINDENT,
     TT "f = method(Options => w)", " -- creates a method which accepts
     optional arguments.  Here 'w' is a list ", TT "{A=>a,B=>b,...}", " of
     optional argument names A,B,... and corresponding default values a,b,...",
     PARA,
     "When optional arguments are specified, the method functions installed
     should accept an additional argument to which will be passed a
     hash table of type ", TO "OptionTable", " containing the optional 
     argument names and their values.  The table will be stored in the
     ", TO "OptionsRegistry", " and can be recovered with the function
     ", TO "options", ".",
     EXAMPLE "f = method(Options => {Slope => 1, Intercept => 1})",
     EXAMPLE "f RR := (x,options) -> options.Slope * x + options.Intercept",
     EXAMPLE "f(5.,Slope=>100)",
     SEEALSO "method"
     }

scan({ 
	  realPart, conjugate, imaginaryPart, borel, isBorel, codim, 
	  lcmDegree, gcdDegree, prune, euler, genera, gcdCoefficients,
	  isWellDefined, isInjective, isSurjective, singularLocus, isSubset,
	  dim, Hom, diff, contract, exteriorPower, subsets, partitions, member,
	  koszul, symmetricPower, basis, coefficientRing, trace, binomial, subquotient,
	  getchange, poincare, cover, super, poincareN,
	  dual, cokernel, coimage, image, generators, someTerms, scanKeys, stats, options,
	  substitute, rank, complete, ambient, top, transpose, length, baseName,
	  degree, degreeLength, coefficients, isHomogeneous, size,
	  isIsomorphism, exponents, 
	  height, depth, width, regularity, nullhomotopy,
	  hilbertFunction, content,
	  isPrime, leadTerm, leadCoefficient, leadMonomial, isField,
	  leadComponent, expand, degreesRing, degrees, annihilator,
	  chainComplex, assign, cohomology, homology, numgens,
	  autoload, ggPush, char, minprimes, relations, cone, pdim, random,
	  frac, betti, det, ring, presentation, use, degreesMonoid, submatrix,
	  truncate, fraction
	  },
     n -> (
	  f := method();
	  Documentation#f = n;
	  n <- f;
	  )
     )

radical = method( Options=>{
	  Unmixed=>false,
	  CompleteIntersection => null
	  }
     )

sum = method()
product = method()
max = method(SingleArgumentDispatch=>true)
min = method(SingleArgumentDispatch=>true)
ideal = method(SingleArgumentDispatch=>true)
submodule = method(SingleArgumentDispatch=>true)
directSum = method(SingleArgumentDispatch=>true)
intersect = method(SingleArgumentDispatch=>true)
net = method(SingleArgumentDispatch=>true)
vars = method(SingleArgumentDispatch=>true)
expression = method(SingleArgumentDispatch=>true)
factor = method( Options => { } )
trim    = method ( Options => {
	  -- DegreeLimit => {}
	  } )
mingens = method ( Options => { 
	  -- DegreeLimit => {}
	  } )
hilbertPolynomial = method( Options => { Projective => true } )
width File := fileWidth; erase quote fileWidth
width Net := netWidth; erase quote netWidth
height Net := netHeight; erase quote netHeight
depth Net := netDepth; erase quote netDepth
width String := s -> #s
height String := s -> 1
depth String := s -> 0

-----------------------------------------------------------------------------
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
     EXAMPLE "R = ZZ/101[x,y,z]",
     EXAMPLE "degree (x^2+y^2)^5",
     EXAMPLE "F = R^{2,3,4}",
     EXAMPLE "v = F_2",
     EXAMPLE "degree v",
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
     TT "vars(i .. j)", " -- provides a list of ", TO "indeterminates", ", the i-th
     one through the j-th one.",
     PARA,
     EXAMPLE "vars(3 .. 9)",
     EXAMPLE "R = ZZ/101[vars(3 .. 5)]",
     EXAMPLE "vars R",
     EXAMPLE "symmetricPower(2,vars R)"
     }

document { quote leadCoefficient,
     TT "leadCoefficient f", " -- return the leading coefficient of the polynomial
     or vector f.",
     PARA,
     SEEALSO ("leadTerm", "leadMonomial", "leadComponent")
     }

document { quote leadComponent,
     TT "leadComponent f", " -- return the leading component of the vector f,
     i.e., the integer i so that f_i is the first nonzero component of f.",
     PARA,
     SEEALSO ("leadTerm", "leadCoefficient", "leadMonomial")
     }

document { quote leadMonomial,
     TT "leadMonomial f", " -- return the leading monomial of the polynomial
     or vector f.",
     PARA,
     SEEALSO ("leadTerm", "leadCoefficient", "leadCoefficient")
     }

oldflatten := flatten
erase quote flatten
flatten = method(SingleArgumentDispatch=>true)
flatten List := flatten Sequence := oldflatten

document { quote flatten,
     TT "flatten m", " -- produces a new list from m by effectively removing the braces
     surrounding the elements of any elements of m which happen to be
     lists.  Also works for matrices.",
     PARA,
     EXAMPLE "flatten {{2,3,4},{{5}},6}"
     }

coker = cokernel
document { quote coker,
     "An abbreviation for ", TO "cokernel", "."
     }
document { quote cokernel,
     TT "cokernel f", " -- produces the cokernel of the module homomorphism f",
     PARA,
     "The result will be a quotient module of the target of f.  If f is
     a ring element, it is interpreted as a one by one matrix.",
     PARA,
     "For an abbreviation, use ", TO "coker", "."
     }

document { quote image,
     TT "image h", " -- yields the image of the homomorphism h.",
     PARA,
     "The result will be a submodule of the target of h",
     PARA,
     "If h is a ring element, it is interpreted as a one by one matrix."
     }

source = (h) -> (
     if h#?(quote source) then h.source
     else if (class h)#?(quote source) then (class h)#?(quote source)
     else error ( name h, " of class ", name class h, " has no source" ))

document { quote source,
     TT "source h", " -- the source of a morphism h.",
     }

target = (h) -> (
     if h.?target then h.target
     else if (class h)#?(quote target) then (class h)#?(quote target)
     else error (name h | " of class " | name class h | " has no target"))

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
     SEEALSO ("cover", "super")
     }
     
document { quote Hom,
     TT "Hom(M,N)", " -- constructs the module of homomorphisms from M to N.",
     PARA,
     "Implemented with a method of the same name.",
     PARA,
     "Use ", TO "homomorphism", " to convert an element of the module of
     homomorphism to a matrix."
     }

gens = generators
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
     SEEALSO ("Monoid", "GroebnerBasis", "Module", "relations", "subquotient")
     }

document { quote someTerms,
     TT "someTerms(f,i,n)", " -- selects n terms from the polynomial f, starting
     with the i-th one, and returns the resulting polynomial."
     }

-----------------------------------------------------------------------------

scanKeys(HashTable,Function) := (x,f) -> scanPairs(x, (k,v) -> f k)
scanKeys(Database,Function) := (x,f) -> (
     	  s := firstkey x;
     	  while s =!= null do (
	       f s;
	       s = nextkey x;
	       ))
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
     class of the value.",
     PARA,
     "This method is used for instances of ", TO "Type", " and ", TO "Ring", "
     to arrange for the name of the type or ring to be set to the name
     of the global variable to which it is first assigned.",
     PARA,
     EXAMPLE "GlobalAssignHook RR := (sym,val) -> << concatenate (
     	  \"assigning \", name val, \" to \", name sym
	  ) << endl",
     EXAMPLE "a=4.5",
     EXAMPLE "a=5.4",
     SEEALSO "GlobalReleaseHook"
     }

document { quote GlobalReleaseHook,
     TT "GlobalReleaseHook", " -- a method name which is consulted when an
     assignment to a global variable occurs.",
     PARA,
     "The method should be a function of two variables: the symbol to which
     a value is being assigned, and the old value about to be overwritten.  
     The method should be stored under then name ", TT "GlobalReleaseHook", " in the
     class of the old value.",
     PARA,
     EXAMPLE "GlobalReleaseHook RR := (sym,val) -> << concatenate (
     	  \"assigning \", name val, \" to \", name sym
	  ) << endl",
     EXAMPLE "a=4.5",
     EXAMPLE "a=5.4",
     SEEALSO "GlobalAssignHook"
     }

document { quote stats,
     TT "stats g", " -- describe the status of a Groebner basis computation
     or of a resolution computation.",
     PARA,
     EXAMPLE "ZZ/101[a..f]",
     EXAMPLE "stats gb matrix {{a*b, b*c},{a^3*f, b^3*e}}",
     SEEALSO ( "GroebnerBasis", "Resolution" )
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
     TT "drop(v,n)    ", " -- yields the list obtained from the list v by
     dropping the first n elements.  Also works for sequences.",
     PARA,
     "drop(v,-n)    -- yields the list obtained from the list v by dropping the 
     last n elements.",
     PARA,
     "drop(v,{m,n}) -- yields the list obtained from the list v by dropping the
     elements at positions m through n.",
     PARA,
     SEEALSO( "take")
     }

oldnumerator := numerator
erase quote numerator
numerator = method()
numerator QQ := oldnumerator

olddenominator := denominator
erase quote denominator
denominator = method()
denominator QQ := olddenominator

erase quote newmethod1
erase quote newmethod123c

emptyOptionTable := new OptionTable
options Thing := X -> emptyOptionTable
options Function := function -> (
     if OptionsRegistry#?function then OptionsRegistry#function
     else emptyOptionTable
     )
document {quote options,
     TT "options f", " -- returns the table of option names and default values
     provided for the function ", TT "f", ", if one has been registered.",
     BR,NOINDENT,
     TT "options X", " -- returns the options used when the monoid or polynomial
     ring X was created.",
     PARA,
     SEEALSO ("method", "OptionsRegistry")
     }

computeAndCache := (M,options,name,goodEnough,computeIt) -> (
     if not M#?name or not goodEnough(M#name#0,options) 
     then (
	  ret := computeIt(M,options);
	  M#name = {options,ret};
	  ret)
     else M#name#1
     )
