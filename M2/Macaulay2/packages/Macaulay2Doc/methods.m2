document { Key => FunctionClosure,
     Headline => "the class of all function closures",
     "Functions created by the operator ", TO "->", " are function closures.",
     EXAMPLE "class (x->x)"
     }
document { Key => CompiledFunction,
     Headline => "the class of all compiled functions",
     "Compiled functions in Macaulay2 are written in a special purpose language, translated to C during compilation and not available to general users.",
     EXAMPLE "class sin"
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
     (NewFromMethod, Module, Sequence),
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
    Key => options,
    Headline => "get options",
    SeeAlso => {Option}
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
     SeeAlso => { "=>", options }
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
     Key => {(options, Function),(options, Command),(options, Sequence)},
     Headline => "get optional arguments and default values for a function that accepts them",
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
