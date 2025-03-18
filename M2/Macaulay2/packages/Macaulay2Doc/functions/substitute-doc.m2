document { 
     Key => {substitute,
	  (substitute, Vector, Option),
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
	  (substitute, Module, Ring),
	  (substitute, Matrix, Option),
	  (substitute, Ideal, Option),
	  (substitute, Vector, Matrix),
	  (substitute, RingElement, List),
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
     Subnodes => TO \ {
	 (symbol SPACE, RingElement, Sequence)
	 },
     SeeAlso => {RingMap, hilbertSeries, value, Expression}
     }
