undocumented {
     (symbol SPACE, RingMap, Number)
     }

document {
     Key => (symbol **, RingMap, Module),
     Headline => "tensor product of a module via a ring map",
     Usage => "f ** M",
     Inputs => {
	  "f" => { "a ring map from ", TT "R", " to ", TT "S" },
	  "M" => { "an ", TT "R", "-module" }
	  },
     Outputs => {
	  { "the tensor product of ", TT "M", " with ", TT "S", " over ", TT "R" }
	  },
     EXAMPLE lines ///
	  R = QQ[x,y];
	  S = QQ[t];
	  f = map(S,R,{t^2,t^3})
	  f ** coker vars R
	  f ** image vars R
	  ///,
     SeeAlso => { (symbol SPACE, RingMap, Module) }
     }

document {
     Key => (symbol **, RingMap, Matrix),
     Headline => "tensor product of a module map via a ring map",
     Usage => "f ** g",
     Inputs => {
	  "f" => { "from ", TT "R", " to ", TT "S" },
	  "g" => { "a map of ", TT "R", "-modules" }
	  },
     Outputs => {
	  { "the tensor product of ", TT "g", " with ", TT "S", " over ", TT "R" }
	  },
     EXAMPLE lines ///
	  R = QQ[x,y];
	  S = QQ[t];
	  f = map(S,R,{t^2,t^3})
	  f ** vars R
	  ///,
     SeeAlso => { (symbol SPACE, RingMap, Module) }
     }

document {
     Key => {"powers",
	  (symbol ^,RingMap,ZZ)
	  },
     Usage => "x^n",
     Inputs => { 
	  "x" => { ofClass{RingMap} },
	  "n" => ZZ
	  },
     Outputs => { {"the ", TT "n", "-th power of ", TT "x"} }
     }

document {
     Key => {(symbol SPACE, RingMap, RingElement),
	  (symbol SPACE, RingMap, Ideal),
	  (symbol SPACE, RingMap, Matrix),
	  (symbol SPACE, RingMap, Vector),
	  (symbol SPACE, RingMap, Module),
	  (symbol SPACE, RingMap, ChainComplex)},
     Headline => "apply a ring map",
     Usage => "f X",
     Inputs => {
	  "f" => { "a ring map from ", TT "R", " to ", TT "S", "." },
	  "X" => { ofClass Ideal, ", ",
		   ofClass Matrix, ", ",
		   ofClass Vector, ", ",
		   ofClass Module, ", or ",
		   ofClass ChainComplex}
	  },
     Outputs => {
	  { "the image of X under the ring map f.  The result has the same type as
	       X, except that its ring will be S."}
	  },
     "If ", TT "X", " is a module then it must be either free or a submodule of a free module.
     If ", TT "X", " is a chain complex, then every module of ", TT "X", " must be free or a submodule of a free module.",
     EXAMPLE lines ///
	  R = QQ[x,y];
	  S = QQ[t];
	  f = map(S,R,{t^2,t^3})
	  f (x+y^2)
	  f image vars R
	  f ideal (x^2,y^2)
	  f resolution coker vars R
	  ///,
     Caveat => {"If the rings ", TT "R", " and ", TT "S", " have different degree monoids, then the degrees of the image
        might need to be changed, since Macaulay2 sometimes doesn't have enough information to
	determine the image degrees of elements of a free module."},
     }

document { 
     Key => {substitute,
	  (substitute, Vector, Option),
	  (substitute, Product, Thing),
	  (substitute, Matrix, List),
	  (substitute, Ideal, List),
	  (substitute, Module, Matrix),
	  (substitute, Matrix, Ring),
	  (substitute, Matrix, ZZ),
	  (substitute, Ideal, Ring),
	  (substitute, Module, Option),
	  (substitute,Ideal,RingFamily),
	  (substitute,Matrix,RingFamily),
	  (substitute,Module,RingFamily),
	  (substitute,Number,RingFamily),
	  (substitute,RingElement,RingFamily),
	  (substitute,Vector,RingFamily),
	  (substitute, Vector, List),
	  (substitute, RingElement, Matrix),
	  (substitute, Vector, Ring),
	  (substitute, RingElement, Option),
	  (substitute, Matrix, Matrix),
	  (substitute, Ideal, Matrix),
	  (substitute, Module, List),
	  (substitute, Divide, Thing),
	  (substitute, Module, Ring),
	  (substitute, Matrix, Option),
	  (substitute, Sum, Thing),
	  (substitute, Ideal, Option),
	  (substitute, Vector, Matrix),
	  (substitute, RingElement, List),
	  (substitute, Power, Thing),
	  (substitute, RingElement, Ring),
	  (substitute, Number, Ring)},
     Headline => "substituting values for variables",
     Usage => "substitute(f,v)\nsub(f,v)",
     Inputs => {
	  "f" => {ofClass RingElement, ", ",
	         ofClass Matrix,", ",
		 ofClass Ideal,", ",
		 ofClass Module,", ",
		 ofClass Vector,", or ",
		 ofClass Expression, " over a ring ", TT "R"},
	  "v" => {ofClass Ring, ", ",
	       ofClass Matrix, ", ",
	       ofClass Option, ", or ", ofClass List, " of ", TO2{Option,"options"}}
	  },
     Outputs => {
	  {"An object of the same sort as ", TT "f", ", obtained by substituting values for
	  the variables in the ring ", TT "R", " of ", TT "f", " using ", TT "v", "."}
	  },
     PARA{
	  "A convenient abbreviation for ", TO "substitute", " is ", TT "sub", "."
	  },
     "This function allows you to substitute values for some variables.  There are three ways to describe
     the kind of substitution, depending on the second argument ", TT "v", ".",
     UL {
	  LI {
	       "give specific values for (some of) the variables.  An option ", TT "x=>r", " specifies that a generator
	       ", TT "x", " should be replaced by the ring element ", TT "r", ".  Generators not mentioned will be preserved."
	       },
	  LI {
	       "give a matrix, the entries determines the values of each of the variables"
	       },
	  LI {
	       "give a ring, the effect will be to substitute variables with variables of the same name, into this new ring"
	       }
	  },
     EXAMPLE lines ///
          A = QQ[a..f]; B = QQ[a..d]; C = ZZ/101[x,y];
	  F = 3*a^2-b-d+101*c
	  ///,
     "The following line substitutes values for a and b, and the result is in the same ring ", 
     TT "B", " of ", TT "F", ".",
     EXAMPLE lines ///
     	  sub(F, {a=>1, b=>b^4})
	  ///,
     "Substitute ", TT "a", " by ", TT "x", ", ", TT "b", " by ", TT "y", 
     " and so on.  The result is a polynomial in the ring ", TT "C", ".",     
     EXAMPLE lines ///
	  sub(F, matrix{{x,y,1,0}})
          ///,
     "Using a ring as the second argument substitutes variables with the same name.
     The following produces the polynomial ", TT "F", " in the rings ", TT "A", " and ", TT "D", ".",
     EXAMPLE lines ///
     	  sub(F, A)
	  D = B/(a*b*c*d);
	  sub(F,D)
     	  ///,
     "If the values of all of the variables are in a different ring, then the result will be in that ring.",
     EXAMPLE lines ///
     	  use ring F;
     	  sub(F, {a=>1, b=>3, c=> 1, d=>13})
          ///,
     "If ", TT "f", " is an ideal or a submodule of a free module over ", TT "R", ", then substitution amounts to substitution
     in the matrix of generators of ", TT "f", ".  This is
     not the same as tensor product!",
     EXAMPLE lines ///
     	  use B;
	  M = image(vars B ++ vars B)
	  N = substitute(M, {a=>b+c,c=>1})
	  ///,
     "Although we cannot use substitute directly on modules
     which are not submodules, here is a useful idiom for
     moving a cokernel module to another ring.  One must be careful though:
     the degrees of the generators might not be the desired ones.",
     EXAMPLE lines ///
     	  M' = prune M
	  N' = coker substitute(presentation M', {a=>b+c,c=>1})
     	  ///,
     "Unevaluated expressions (i.e. from ", TO hilbertSeries, ") may also
     have variables substituted in all of the ways mentioned so far.",
     EXAMPLE lines ///
     	  hf = hilbertSeries coker matrix{{a,b^3,d^5}}
	  hf1 = reduceHilbert hf
	  use ring numerator hf;
	  sub(hf1, T => -1)
     	  ///,
     "Of course, we can change the ring too:",
     EXAMPLE lines ///
          sub(hf, T => a)
	  value oo
	  oo == value sub(hf1, T=>a)
     	  ///,
     PARA{
	  "If you plan on using the same substitution over and over, it is
	  wise to create a ring map that will perform the same substitution.",
	  },
     PARA{
	  "For example, in the first example above, we can make ", ofClass RingMap, " ", TT "G", ", and then apply 
	  it to ", TT "F", ".",
	  },
     EXAMPLE lines ///
     	  use B;
     	  G = map(B,B,{a=>1, b=>b^4})
	  G F
	  ///,
     Caveat => "The specified substitution is not checked to see whether
     the corresponding ring homomorphism is well-defined; this may produce
     surprising results, especially if rational coefficients are converted
     to integer coefficients.",
     SeeAlso => {RingMap, hilbertSeries, value, Expression}
     }

-- document {
--      Key => NonLinear,
--      Headline => "use the algorithm which doesn't assume that the ring map is linear",
--      TT "Strategy => NonLinear", " -- an option value for the ", TO "Strategy", "
--      option to ", TO "pushForward1", "."
--      }

-- document {
--      Key => [pushForward1,StopBeforeComputation],
--      Headline => "initialize but do not begin the computation",
--      TT "StopBeforeComputation", " -- keyword for an optional argument used with
--      ", TO "pushForward1", ".",
--      PARA{},
--      "Tells whether to start the computation, with the default value
--      being ", TT "true", "."
--      }
-- 
-- document {
--      Key => [pushForward1,DegreeLimit],
--      Headline => "compute only up to this degree",
--      TT "DegreeLimit => n", " -- keyword for an optional argument used with
--      ", TO "pushForward1", " which specifies that the computation should halt after dealing 
--      with degree ", TT "n", ".",
--      PARA{},
--      "This option is relevant only for homogeneous matrices.",
--      PARA{},
--      "The maximum degree to which to compute is computed in terms of the
--      degrees of the ring map, ", TT "f", ".  For example, if ", TT "f", "
--      consists of cubics, then to find a quadratic relation, this option
--      should be set to at least 6, by specifying, for example, ", 
--      TT "DegreeLimit => 6", ".  The default is ", TT "infinity", ".",
--      SeeAlso => {"pushForward1", "DegreeLimit"}
--      }
-- 
-- 
-- document {
--      Key => [pushForward1,StopWithMinimalGenerators],
--      Headline => "stop when minimal generators have been determined",
--      TT "StopWithMinimalGenerators => true", " -- an option for ", TO "pushForward1", "
--      that specifies that the computation should stop as soon as a
--      complete list of minimal generators for the submodule or ideal has been
--      determined.",
--      PARA{},
--      "The value provided is simply passed on to ", TO "gb", ": see 
--      ", TO [gb,StopWithMinimalGenerators], " for details."
--      }
-- 
-- document {
--      Key => [pushForward1,PairLimit],
--      Headline => "stop when this number of pairs is handled",
--      TT "PairLimit => n", " -- keyword for an optional argument used with
--      ", TO "pushForward1", ", which specifies that the computation should
--      be stopped after a certain number of S-pairs have been reduced."
--      }
-- 
-- document {
--      Key => [pushForward1,MonomialOrder],
--      Headline => "specify the elimination order to use in pushForward1",
--      TT "MonomialOrder => x", " -- a keyword for an optional argument to ", TO "pushForward1", "
--      which tells which monomial order to use for the Groebner basis computation
--      involved.",
--      PARA{},
--      "Possible values:",
--      UL {
-- 	  (TT "MonomialOrder => EliminationOrder", " -- use the natural elimination order (the default)"),
-- 	  (TT "MonomialOrder => ProductOrder", " -- use the product order"),
-- 	  (TT "MonomialOrder => LexOrder", " -- use lexical order"),
-- 	  },
--      SeeAlso => "EliminationOrder"
--      }
-- 
-- document {
--      Key => [pushForward1,UseHilbertFunction],
--      Headline => "whether to use knowledge of the Hilbert function",
--      TT "UseHilbertFunction => true", " -- a keyword for an optional argument to
--      ", TO "pushForward1", " which specifies whether to use the Hilbert function,
--      if one has previously been computed.",
--      PARA{},
--      "The default is to use it if possible."
--      }

-- document {
--      Key => [pushForward1,Strategy],
--      Headline => "specify which algorithm to use in the computation",
--      TT "pushForward1(f,M,Strategy => v)", " -- an option for ", TO pushForward1, " 
--      which can be used to specify the strategy to be used in the computation.",
--      PARA{},
--      "The strategy option value ", TT "v", " should be one of the following.",
--      UL {
-- 	  TO "NonLinear",
--      	  TO "Linear"
-- 	  },
--      PARA{},
--      "The default is for the code to select the best strategy heuristically."
--      }

document {
     Key => PushforwardComputation,
     Headline => "a type of list used internally",
     TT "PushforwardComputation", " -- a type of list used internally by ", TO "pushForward", " and ", TO "kernel", "."
     }

-- document {
--      Key => EliminationOrder,
--      Headline => "use the natural elmination order in a pushForward1 computation",
--      TT "EliminationOrder", " -- a value for the ", TO "MonomialOrder", "
--      option to ", TO "pushForward1", " which specifies the natural elimination
--      order be used."
--      }
