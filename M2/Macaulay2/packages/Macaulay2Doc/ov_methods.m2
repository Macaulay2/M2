document {
     Key => "making a new method function",
     "The function ", TO "method", " can be used to make new functions
     which execute different bits of code depending on the types
     of the arguments presented.  Our system depends heavily on
     such functions.",
     EXAMPLE "f = method()",
     "We can install a method to be used when ", TT "f", " is applied to a string
     as follows.",
     EXAMPLE {
	  "f String := s -> s|s;",
	  ///f ".abcd."///
	  },
     "We can check for the types of up to three arguments, too.",
     EXAMPLE {
	  "f(ZZ,String) := (n,s) -> concatenate (n:s);",
	  ///f(5,".abcd.")///,
	  },
     Subnodes => {
	  TO "method"
	  }
     }

document {
     Key => "specifying typical values",
     "For the purpose of constructing good documentation automatically, it
     is useful to specify the type of value typically returned by a function
     or method.  For example, the function ", TO "isModule", " returns a boolean
     value, and this is specified when creating the method function with the
     option ", TO "TypicalValue", " as follows.",
     PRE ///isModule = method(TypicalValue => Boolean)///,
     PARA{},
     "Other functions, such as ", TO "prune", ", return values of various types,
     depending on the type of the arguments provided.  To install a
     function ", TT "f", " as the handler for ", TT "prune", " applied to a matrix,
     we would normally use the following statement.",
     PRE ///prune Matrix := f///,
     "To specify that the value typically returned is a matrix (of class ", TT "Matrix", "),
     we replace ", TT "f", " by ", TT "Matrix => f", ", as follows.",
     PRE ///prune Matrix := Matrix => f///,
     "Here is the way our code looks.",
     EXAMPLE "code(prune, Matrix)",
     "The information is stored in the hash table ", TO "typicalValues", ", and can
     be recovered like this.",
     EXAMPLE "typicalValues#(prune,Matrix)",
     PARA{},
     "Warning: don't imagine that a definition of the form ",
     PRE "f = t -> (...)",
     "can be replaced with a declaration of the following form.",
     PRE "f = X => t -> (...)",
     "The difference here is that here we are using simple assignment, rather than
     installing a method.  To document the return type is ", TT "X", " in this case, 
     make an entry in ", TT "typicalValues", " directly.",
     PRE "f = t -> (...)\ntypicalValues#f = X",
     Subnodes => { TO "typicalValues", },
     }

document {
     Key => "binary methods",
     "The method for computing a sum ", TT "x+y", " depends on the types of ", TT "x", " and ", TT "y", ".
     For example, the method for adding an integer ", TT "x", " and a polynomial 
     ", TT "y", " differs from the method for adding two integers modulo 111.  Because
     both the type of ", TT "x", " and the type of ", TT "y", " must enter into the selection of
     the method, we refer to these methods as binary methods.  Each binary
     method is a function of two variables, and is stored either in the class
     of ", TT "x", " or in the class of ", TT "y", ".",
     PARA{},
     "Let's assume that ", TT "X", " is the class (or type) of ", TT "x", ", 
     and that ", TT "Y", " is the class of ", TT "y", ".  The way to install a 
     method for the addition of an instance ", TT "x", " of class ", TT "X", " to 
     an instance ", TT "y", " of class ", TT "Y", " is with a statement of the form ",
     PRE "X + Y := (x,y) -> ( ... )",
     "where ", TT "( ... )", " represents the body of the function, consisting of suitable
     code for the operation at hand.",
     PARA{},
     "The method installed by the code above is automatically inherited by 
     subclasses of ", TT "X", " and ", TT "Y", ".  Here is a brief
     description of the way this works.  Suppose ", TT "X", " is the 
     ", TO "parent", " of ", TT "P", " and ", TT "Y", " is the parent of ", TT "Q", ".  When
     a sum ", TT "p+q", " is evaluated where the class of ", TT "p", " is 
     ", TT "P", " and the class of ", TT "q", " is ", TT "Q", ", then the binary
     method for ", TT "P+Q", " is applied, unless there isn't one, in which
     case the binary method for ", TT "P+Y", " is applied, unless there isn't
     one, in which case the binary method for ", TT "X+Q", " is applied,
     unless there isn't one, in which case the binary method for ", TT "P+Q", "
     is applied.  In general this search for a binary method continues all
     the way up the chain of parents to the topmost ancestor of everything,
     which is called ", TO "Thing", ".  (See also ", TO "lookup", ".)",
     PARA{},
     "As an extreme example of inheritance, the code ", 
     PRE "Thing + Thing := (x,y) -> ( ... )",
     "will install a binary method for adding any two things, which will take
     effect as a last resort whenever more a specifically defined method
     isn't found.",
     PARA{},
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
     ", TT "X", " provided to the routine by the system."
     }

document {
     Key => "installing methods",
     "The method to be used for computing an expression such as ", TT "-x", " depends 
     on the type of ", TT "x", ".  For example, the method for negating a polynomial
     differs from the method for negating an integer modulo 111.  Each
     method is a function of one variable, and is stored in the class 
     of ", TT "x", " under a key that is referred to as the name of the method.
     For some built-in methods the method name is a symbol, but for
     methods created with ", TO "method", ", the method name is the same
     as the function used for calling it up.",
     PARA{},
     "Let's assume that ", TT "X", " is the class of ", TT "x", ".  The way to install a method
     for the negation of an instance ", TT "x", " of ", TT "X", " is with a statement of the 
     following form.",
     PRE "- X := x ->( ... )",
     "Here ", TT "( ... )", " represents the body of the function, consisting of
     suitable code for the operation at hand.",
     PARA{},
     "The method installed by the code above is automatically inherited by
     subclasses of X.  Here is a brief description of the way 
     this works.  Suppose ", TT "X", " is the ", TO "parent", " of ", TT "P", ".  When an expression
     ", TT "-p", " is to be evaluated, where the class of ", TT "p", " is ", TT "P", ", then the method for
     ", TT "-P", " is applied, unless there isn't one, in which case the method for
     ", TT "-X", " is applied, and so on, all the way up the chain of parents to the
     topmost ancestor of everything, which is called ", TO "Thing", ".",
     PARA{},
     "As an extreme example of inheritance, code like", 
     PRE "- Thing := x -> ...",
     "will install a method for negating anything, which will take
     effect as a last resort whenever a more specifically defined method
     isn't found.  It probably isn't a good idea to install such a method,
     for usually all it can do is to print an error message.",
     PARA{},
     "The user may introduce new methods as well as new method names.  So it
     is important to understand how methods are installed and consulted.",
     PARA{},
     "Applying a method named ", TT "C", " to a thing ", TT "x", " whose class is ", TT "X", " means that",
     PRE "(lookup(C,X)) x",
     "is evaluated.  In other words, ", TT "C", " is used as a key
     to obtain a function from ", TT "X", " (or its parent, grandparent,
     and so on), and the function is applied to ", TT "x", ".  See ", TO "lookup", ".",
     PARA{},
     "Installing a method named ", TT "C", " for the class ", TT "X", " is done with code such
     as ",
     PRE "C X := (x) -> ( ... )",
     "where ", TT "( ... )", " represents suitable code for the operation at hand.",
     PARA{},
     "The routine for making new methods is ", TO "method", ".",
     SeeAlso =>{"binary methods"},
     Subnodes => {
	 TO "installing assignment methods",
	 TO "installing augmented assignment methods",
         TO installMethod,
	 TO "binary methods",
	 TO "nullaryMethods",
         },
     }

document {
     Key => "inheritance",
     "Each class has a parent class that can be used as a container
     for bits of code that apply to a more general class of objects.
     In this section we show how this mechanism works in detail.",
     PARA{},
     "We begin by creating a new type of basic list.",
     EXAMPLE {
	  "X = new Type of BasicList",
	  "parent X",
	  },
     "The parent of ", TT "X", " is ", TO "BasicList", ", as desired,
     thus methods applicable to basic lists will also apply also
     to instances of ", TT "X", ".  One such method is the method
     for creating a net from a basic list; here is its code:",
     EXAMPLE "code(net,BasicList)",
     "This code is run automatically to display an instance of ", TT "X", ",
     so if we make one, we'll be able to see what it is:",
     EXAMPLE "x = new X from {2,3,4}",
     "Now let's imagine we wish to treat instances of ", TT "X", " as
     vectors, and to negate one by negating its entries.  As it
     happens, no method for this has been installed for basic lists,
     so trying to negate ", TT "x", " results in an error.",
     EXAMPLE {
	 "stopIfError = false;",
	 "- x",
	 },
     "We install and test a new method as described in ", TO "installing methods", ".",
     EXAMPLE {
	  "- X := t -> apply(t,i -> -i);",
	  "- x"
	  },
     "This method will apply automatically to subclasses of ", TT "X", ",
     as we see now.",
     EXAMPLE {
	  "Y = new Type of X;",
	  "y = new Y from {4,5,6}",
	  "- y"
	  },
     "For ", TT "binary methods", ", there is an apparent ambiguity in
     deciding exactly how inheritance will work.  Let's illustrate
     by making a new subclass ", TT "Z", " of ", TT "X", ".",
     EXAMPLE {
	  "Z = new Type of X;",
	  "z = new Z from {7,8,9}",
	  },
     "Now let's install two methods, either of which might conceivably
     be applied to evaluate the expression ", TT "y+z", ", and see
     what happens.",
     EXAMPLE {
	  "Y + X := (a,b) -> \"Y + X\";",
	  "X + Z := (a,b) -> \"X + Z\";",
	  "y + z"
	  },
     "The result is the string ", TT "Y + X", ".  The reason is that
     after finding that no method applies directly for adding
     an instance of ", TT "Y", " to an instance of ", TT "Z", ", the 
     search continues: ", TT "Z", " is replaced by its parent ", TT "X", ", 
     and so on.  (After enough unsuccessful iterations of this, the 
     second type is reset to ", TT "Z", ", the first type is replaced 
     by its parent, and the search continues.)",
     PARA{},
     "The same search order applies to method functions defined with ", TO "method", ".",
    Subnodes => {
	TO lookup,
        },
     }

document { Key => FunctionClosure,
     Headline => "the class of all function closures",
     "Functions created by the operator ", TO "->", " are function closures.",
     EXAMPLE "class (x->x)",
    Subnodes => TO uncurry,
     }
document { Key => CompiledFunction,
     Headline => "the class of all compiled functions",
     "Compiled functions in Macaulay2 are written in a special purpose language, translated to C during compilation and not available to general users.",
     EXAMPLE "class drop"
     }
document { Key => CompiledFunctionClosure,
     Headline => "the class of all compiled function closures",
     "Some compiled functions return compiled function closures as values.",
     EXAMPLE lines ///
	  class depth
	  f = method()
	  class f
     ///
     }

-- TODO: Should this be here?
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
	  },
    Subnodes => {
	TO FunctionClosure,
	TO CompiledFunction,
	TO CompiledFunctionClosure,
	TO (symbol SPACE, Function, Thing),
        TO (symbol @@, Function, Function),
        TO (symbol _, Function, Thing),
        TO (methodOptions, Function),
	TO identity,
	TO notImplemented,
        },
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
     "The class of all functions is ", TO "Function", ".",
     Subnodes => {
	 TO (options, Function),
         },
     }

document {
     Key => lookup,
     Headline => "look up methods",
     TT "lookup(M,A)", " -- provides the unary method named ", TT "M", " for class ", TT "A", ".
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
     PARA{"Most users will use a different way of installing methods."},
     PARA{
	  TT "installMethod(M,f)", "     -- installs a function ", TT "f", " as a nullary method
	  under the name ", TT "M", ".  This is a replacement for the syntax ", "M () := f", ",
	  which hasn't yet been made to work.  As currently implemented, this is also the same
	  as ", TT "nullaryMethods#(1:M) = f", "."
	  },
     PARA{
	  TT "installMethod(M,A,f)", "     -- installs a function ", TT "f", " as a unary method for
	  the class ", TT "A", " under the name ", TT "M", ".  This is the same as ", "M A := f", "
	  if ", TT "M", " is a function.  As currently implemented, this is also the same
	  as ", TT "A#M = f", "."
	  },
     PARA{
	  TT "installMethod(M,A,B,f)", "   -- installs a function ", TT "f", " as a binary method for
	  classes ", TT "A", " and ", TT "B", " under the name ", TT "M", ".  This is the same as
	  ", TT "M(A,B) := f", " if ", TT "M", " is a
	  function, or the same as ", TT "A M B := f", " if ", TT "M", " is a binary operator. As currently
	  implemented, this is also the same as ", TT "Y#(M,A,B) = f", ", where ", TT "Y", " is
	  the younger of ", TT "A", " and ", TT "B", "."
	  },
     PARA{
	  TT "installMethod(M,A,B,C,f)", " -- installs a function ", TT "f", " as a ternary method
	  for classes ", TT "A", ", ", TT "B", ", and ", TT "C", " under the name ", TT "M", ".
	  This is the same as ", TT "M(A,B,C) := f", " if ", TT "f", "
	  is a function.  As currently implemented, this is also the same as
	  ", TT "Y#(M,A,B,C) = f", ", where ", TT "Y", " is the youngest of ", TT "A", ", ", TT "B", ",
	  and ", TT "C", "."
	  },
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
     (NewFromMethod, HREF, List),
     (NewFromMethod, Module, List),
     (NewFromMethod, TO, List),
     (NewFromMethod, TO2, List),
     (NewFromMethod, TOH, List),
     (NewFromMethod, TO2, Sequence),
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
     Subnodes => {
	 TO (symbol >>, OptionTable, Function),
	 TO (symbol ++, OptionTable, OptionTable),
         }
     }
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
     PARA {
	  "The value returned by ", TO2{ (options,Function), "options" }, " is a type of hash table that can be used to obtain default values."
	  },
     PARA {
	  "See also ", TO "making new functions with optional arguments", "."
	  },
     EXAMPLE {
	  "(options gb).Syzygies"
	  }
     }

document{
     Headline => "a type of method function",
     Key => MethodFunctionBinary,
     PARA {
	  "The type of all method functions created with the option ", TO "Binary", " set to ", TO "true", ", such as ", TO "gcd", "."
	  },
     SeeAlso => { "method" }
     }

document{
     Headline => "a type of method function",
     Key => MethodFunctionSingle,
     PARA {
	  "The type of all method functions created with the option ", TO "Dispatch", " set to ", TO "Thing", ", such as ", TO "code", "."
	  },
     SeeAlso => { "method" }
     }

document { Key => MethodFunction,
     Headline => "a type of method function", "Functions of this type are created by ", TO "method", "." }
document { Key => MethodFunctionWithOptions,
     Headline => "a type of method function", "Functions of this type are created by ", TO "method", "." }
undocumented (methodOptions, MethodFunctionWithOptions)
undocumented (methodOptions, MethodFunction)
undocumented (methodOptions, Symbol)
document { Key => {(methodOptions, Function),(methodOptions, Command),(methodOptions, ScriptedFunctor),methodOptions},
     Headline => "recover the options used when a method function was created",
     Usage => "methodOptions f",
     Inputs => { "f" },
     Outputs => {{ "the options used when ", TT "f", " was created by ", TO "method", "" }},
     EXAMPLE lines ///
	  methodOptions source
	  methodOptions res
     ///
     }

document { Key => CompiledFunctionBody,
     Headline => "the class of all compiled function bodies",
     "A compiled function body is the body of a compiled function closure.  It is not a function.",
     EXAMPLE lines ///
	  source
	  functionBody source
     ///
     }

document { Key => "nullaryMethods",
     "This experimental hash table is to contain methods for handling the case where a method function, ", TT "f", ", say, is called with
     0 arguments, i.e., as ", TT "f()", "."
     }
