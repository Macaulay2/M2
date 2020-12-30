document {
     Key => {method,[method,Binary],[method,TypicalValue],TypicalValue,[method, Dispatch],Dispatch,[method,Options]},
     Headline => "make a new method function",
     Usage => "f = method()",
     Inputs => {
	  TypicalValue => Type => { "the type of the value returned by ", TT "f", ", typically.  This information is used only to build
	       documentation automatically, and is stored in the hash table ", TO "typicalValues", "." },
	  Options => List => { "a list of options ", TT "J => v", ", where ", TT "J", " is the name of an optional argument 
	       for ", TT "f", ", and ", TT "v", " is its default value.
	       The list of options could be also replaced by the corresponding ", TO "OptionTable", ".
	       Specifying ", TT "true", " here indicates that option handling is done by the individual methods,
	       while still allowing dispatching to methods based on types of arguments to be done.
	       The individual methods may have various sets of acceptable option names, possibly empty.
	       See ", TO "making new functions with optional arguments", "." },
	  Binary => Boolean => { "whether the method is to be binary: for three arguments or more the result will be computed by calling binary methods installed for
	       ", TT "f", " with two arguments at a time." },
	  Dispatch => { "the method for getting a list of types from the parameters; the value of this option should be ", TO "Thing", " or ", TO "Type", "
	       to indicate that a sequence should be regarded as a single argument, or, if the elements of a sequence are to be regarded as separate parameters,
	       a list whose elements are ", TO "Thing", " or ", TO "Type", ".  Parameters corresponding to ", TO "Thing", " or to a position beyond the end of the list
	       are dispatched according to their ", TO "class", ", whereas parameters
	       corresponding to ", TO "Type", " are expected to be types (actually, hash tables) and are used directly in the search for methods; see
	       ", TO "inheritance", "."
	       }
	  },
     Outputs => {
	  "f" => { "a new method function, ", ofClass{MethodFunction, MethodFunctionSingle, MethodFunctionBinary, MethodFunctionWithOptions}}
	  },
     SPAN {
	  "The code above creates a method function that takes up to four
	  arguments, looking up the appropriate method according to the classes of 
	  the arguments, with inheritance.  To install a method for two arguments,
	  (x,y), of classes X and Y, use code like this:"
	  },
     PRE "     f(X,Y) := (x,y) -> ...",
     SPAN {
	  "where '...' represents the body of the function you wish to install.
	  The syntax for one or three arguments is analogous.  See ", TO ":=", " for details."
	  },
     EXAMPLE lines ///
     	  f = method()
	  f ZZ := x -> -x;
	  f(ZZ,String) := (n,s) -> concatenate(n:s);
	  f(String,ZZ,String) := (s,n,t) -> concatenate(s," : ",toString n," : ",t);
	  f 44
	  f(5,"abcd ")
	  f("foo",88,"bar")
     ///,
     PARA {
	  "In the following example we install a asymmetric method to illustrate the left-associative order of evaluation for a ", EM "binary", " method function."
	  },
     EXAMPLE lines ///
	  p = method(Binary => true, TypicalValue => List)
	  p(ZZ,ZZ) := p(List,ZZ) := (i,j) -> {i,j}
     	  p(1,2)
      	  p(1,2,3,4,5,6)
     ///,
     "By default, at most four arguments (in a sequence) can be handled by a method function, and the types have to be considered separately when installing methods.
     In this example, we define a method function that treats a sequence as a single argument, and we install a method for handling such arguments.",
     EXAMPLE lines ///
	  g = method(Dispatch => Thing);
	  g ZZ := i -> -i;
	  g Sequence := S -> reverse S;
	  g 44
	  g(3,4,5,6)
     ///,
     "Here we define a method whose first argument is to be a type.  It will convert its second argument to that type.",
     EXAMPLE lines ///
     	  h = method(Dispatch => {Type})
	  h(QQ,ZZ) := (QQ,n) -> n/1;
	  h(RR,ZZ) := (RR,n) -> n + 0.;
	  h(ZZ,ZZ) := (ZZ,n) -> n;
	  h(ZZ,14)
	  h(QQ,14)
	  h(RR,14)
     ///,
     PARA {
     	  "In the next example we make a linear function of a single real variable whose 
     	  coefficients are provided as optional arguments named Slope and Intercept, with default value 1."
	  },
     EXAMPLE lines ///
          r = method(Options => {Slope => 1, Intercept => 1})
     ///,
     PARA {
	  "The methods installed for this method function should be written in
	  the form ", TT "opts -> args -> (...)", ".  The argument ", TT "opts", "
	  will be assigned a hash table of type ", TO "OptionTable", " containing 
	  the optional argument names and their current values.  For example,
	  in the body of the function, the current value for the argument named
	  ", TT "b", " can be recovered with ", TT "opts#b", ", or with ", TT "opts.b", ",
	  in case ", TT "b", " is known to be a global symbol.  Be careful 
	  not to change the value of ", TT "b", ", or the code will stop working; it
	  would be a good idea to protect it."
	  },
     EXAMPLE lines ///
      	  r RR := o -> x -> o.Slope * x + o.Intercept
      	  r(5.)
      	  r(5.,Slope=>100)
     ///,
     PARA {
	  "The default ", TO2{OptionTable,"option table"}, " for ", TT "r", " 
	  can be recovered with the function ", TO "options", ".  The options given to ", TO "method", " can be recovered with ", TO "methodOptions", "."
	  },
     EXAMPLE lines ///
	  options r
	  methodOptions r
     ///,
     PARA {
	  "In the next example we define a method function that leaves option processing to the individual methods."
	  },
     EXAMPLE lines ///
     	  s = method(Options => true)
	  s ZZ := { Slope => 17 } >> o -> x -> o.Slope * x
	  s RR := { Intercept => 11 } >> o -> x -> x + o.Intercept
	  s 100
	  s 1000.
	  options s
	  options(s,ZZ)
	  options(s,RR)
     ///,
     PARA {
	  "For now, one installs a method function for ", TT "s", " with no non-optional arguments using ", TO installMethod, ":"
	  },
     EXAMPLE lines ///
     	  installMethod(s,{ Slope => 1234 } >> o -> () -> o.Slope)
	  s()
	  s(Slope => 4)
     ///,
     Subnodes => {
	 TO "specifying typical values",
	 ":Types of method funcions",
	 TO MethodFunction,
	 TO MethodFunctionSingle,
	 TO MethodFunctionBinary,
	 TO MethodFunctionWithOptions }
     }
