-- -*- coding: utf-8 -*-
document {
     Key => RingMap,
     Headline => "the class of all ring maps",
     "For an overview of ring maps, substitution of variables, and finding implicit
     equations of a set of polynomial or rational functions, see ", TO "substitution and maps between rings", ".",
     PARA{},     
     TEX "A ring map $F : R \\rightarrow{} S$, where $R$ is a polynomial ring, is specified
     by giving the images in $S$ of the variables of $R$.  For a simple example, 
     consider the following map.  Notice that, as is usual in Macaulay2, the target
     ring is given before the source.",
     EXAMPLE {
	  "R = QQ[a,b,c]; S = QQ[s,t];",
	  "F = map(S,R,{s^3-t^2, s^3-t, s-t})",
	  "target F",
	  "source F",
	  "F.matrix"
	  },
     "There are other ways to define ring maps.  See below.", 
     PARA{},
     "Apply matrices to ring elements, vectors, matrices, and ideals
      using usual function notation.",
      EXAMPLE {
	  "F (a+b)"
	  },
     "The kernel of such ring maps are constructed with the aid of
     Gröbner bases.  Preimages of ideals are constructed using the
     same method. ",
     EXAMPLE {
	  "I = kernel F",
     	  "F I",
	  "J = preimage(F, ideal(s-3))",
	  "isSubset(F J, ideal(s-3))"
	  },
     "Geometrically, the inverse image of this line is a conic.",
     PARA{},
     "Consider the Cremona transform, and its square:",
     EXAMPLE {
	  "G = map(R,R,{a=>b*c,b=>a*c,c=>a*b})",
	  "G*G"
	  },
     "These are injective ring maps",
     EXAMPLE{
	  "ker G == 0",
	  "isInjective G",
	  "coimage G",
	  },
     PARA{},
     BOLD "Common ways to make a ring map:",
     UL {
	  TO (map,Ring,Ring),
	  TO (map,Ring,Ring,List),
	  TO (map,Ring,Matrix),
	  TO (map,Ring,Ring,Matrix),	  
	  },
     BOLD "Common ways to get information about ring maps:",
     UL {
	  TO (isHomogeneous, RingMap),
	  TO (isInjective, RingMap)
	  },
     BOLD "Common operations on ring maps:",
     UL {
	  TO (kernel, RingMap),
	  TO (coimage, RingMap),
	  TO (preimage, RingMap, Ideal)
	  },
     BOLD "Applying ring maps, and composing ring maps:",
     UL {
	  TO (symbol SPACE, RingMap, RingElement),
	  TO (symbol SPACE, RingMap, Matrix),
	  TO (symbol SPACE, RingMap, Ideal),
	  TO (symbol SPACE, RingMap, Module),
	  TO (symbol *, RingMap, RingMap),
	  },
     BOLD "Operations involving modules",
     UL {
	  TO pushForward,
	  TO (symbol **, RingMap, Module)
	  },
     Subnodes => {
	 TO (symbol SPACE, RingMap, RingElement),
        TO (kernel, RingMap),
        TO (source, RingMap),
        TO (target, RingMap),
        TO (symbol ^, RingMap, ZZ),
	TO newCoordinateSystem,
         }
     }

document {
     Key => "substitution and maps between rings",
     HEADER2 "An overview",
     Subnodes => {
	  TO substitute,
	  TO "substituting values for variables",
	  TO "working with multiple rings",
	  TO "RingMap",
	  TO "basic construction, source and target of a ring map",
	  TO "evaluation and composition of ring maps",
	  TO "kernel and coimage of a ring map",
	  -- Mike wanted this: TO "preimage of an ideal",
	  TO "graphIdeal",
	  TO "graphRing"
	  },
     PARA{},
      "For additional common operations and a comprehensive list of all routines
     in Macaulay2 which return or use ring maps, see ", TO "RingMap", "."
     }

document {
     Key => "substituting values for variables",
     "Once a ring is defined that has variables, values can be 
     given to these variables using ", TO "substitute", ".  We give 
     an example.",
     EXAMPLE { 
	  "R = ZZ/101[x,y,z];",
	  "f = x^3+3*y^2*z+2*z^3;",
	  "substitute(f,matrix{{-1,2,5}})",
	  "substitute(f,{x=>-1,y=>2,z=>5})"
	  },
     "The same command works for putting values into ideals or 
     matrices.  Also, it is not required that the values be 
     elements from the coefficient ring, nor do you have to give
     a value for every variable.",
     EXAMPLE {
	  "M = matrix{{x^2,x-y},{x-z,z^2},{y-z,y^2}}",
	  "substitute(M,matrix{{-1,2,x+y}})",
	  "I = ideal M",
	  "substitute(I,{x=>-1,y=>2})"
	  }
     }

-- TODO: mention lift and promote in this node
document {
     Key => "working with multiple rings",   -- DOUBLE CHECK BEING DONE WITH THIS ONE!
     "Working with multiple rings is more subtle than simply
     replacing values of the variables in a ring.  On the other 
     hand it is particularly easy in Macaulay2.  We define a 
     sequence of rings below and move between each to show both 
     the dangers and the convenience.",
     
     	  SUBSECTION "defining multiple rings",
     	       EXAMPLE {
	       	    "R1 = ZZ/101;",
	       	    "R2 = ZZ/101[s,t];",
	       	    "describe R2"
	       	    },
     	       "Notice that Macaulay2 sees the coefficient ring as R1, we could 
     	       just as easily defined ", TT "R2", " as ", TT "R1[s,t]", " .  
     	       Movement and addition between these rings is easy.",
     	       EXAMPLE {
	       	    "I = ideal (s^4+t^2+1);",
	       	    "R3 = R2/I;",
	       	    "describe R3"
	       	    },
     	       "Since ", TT "I", " is defined as an ideal in ", TT "R2", " we
     	       cannot type ", TT "ZZ/101[s,t]/I", " as the computer 
     	       sees ", TT "ZZ/101[s,t]", " as different from ", TT "R2", " and 
     	       so does not see ", TT "I", " as being in this ring.  For more 
     	       about defining rings see ", TO "rings", ".  We now work with 
     	       moving between ", TT "R2", " and ", TT "R3", ".",
	       ,
     	  SUBSECTION "moving between rings using use and substitute",
     	       EXAMPLE {
	       	    "f = s^4+1",
	       	    "g = s^4+t^2+1"
	       	    },
     	       "f and g are elements in ", TT "R3", " now and this is shown by the fact that 
     	       Macaulay2 sees them as ", TT "-t^2", " and ", "0.  To recover 
     	       these elements as polynomials in ", TT "R2", " type ", TT "use R2", " and 
     	       define them again in ", TT "R2", ".  The command substitute 
     	       does not work well here, where as if we want to see the image
     	       of elements of ", TT "R2", " in ", TT "R3", " it does work well 
     	       and without using the command ", TT "use", ".  Macaulay2 always tells you 
     	       which ring an element is in on the line after it prints the 
     	       ring element.",
     	       EXAMPLE {
	       	    "use R2;",
	       	    "substitute(g,R2)",
	       	    "f = s^4+1",
	       	    "g = s^4+t^2+1",
	       	    "substitute(f,R3)"
	       	    },
	       ,
     	  SUBSECTION "subtleties of substitute and describe",
     	       "Now we complicate things further by constructing a fraction 
     	       field and then further constructing polynomial rings and 
     	       quotient rings.  First we see that while ", TO "describe", " helped
     	       us to see how we defined ", TT "R2", " and ", TT "R3", ", the 
     	       same does not hold when 
     	       a fraction field is constructed.  Note that R3 is a domain.",
     	       EXAMPLE {
	       	    "describe R3",
	       	    "R4 = frac R3;",
	       	    "describe R4"
	       	    },
     	       "The command ", TO "substitute", " works well to move elements 
     	       from ", TT "R2", " or ", TT "R3", " to ", TT "R4", ". An alternative to
	       substitute is to form the canonical injection of R3 into R4 (the same can
	       be done for the canonical projection from R2 to R3 above - we do the example
	       here).  ",
	       -- Mike wanted this: "For more on ring maps, see ", TO "basic, construction source and target of a ring map", ".  ",
     	       "To move elements 
      	       from ", TT "R4", " back to ", TT "R3", " an alternate method must 
	       be used.  Also, 
     	       the method of constructing a map does not work well in the reverse 
     	       direction for the same reasons ", TO "substitute", " does not.",
     	       EXAMPLE {
	       	    "use R2;",
	       	    "f = s^4+1;",
	       	    "substitute(f,R4)",
	       	    "use R3;",
	       	    "g = substitute(f,R3);",
	       	    "substitute(g,R4)",
	       	    "F = map(R4,R3)",
	       	    "F(f)"
	       	    },
	       ,
     	  SUBSECTION "non-standard coefficient fields",
	       "We can go through the whole process again using R4 now as the field.",
	       EXAMPLE {
	       	    "R5 = R4[u,v,w];",
	       	    "describe R5",
	       	    "J = ideal(u^3-v^2*w+w^3,v^2+w^2,u*v-v*w+u*w)",
	       	    "R6 = R5/J;",
	       	    "describe R6"
	       	    },
	       "Notice that at each stage Macaulay2 only refers back to the last 
	       ring we defined.  All of the methods above still work here in theory, but 
	       caution is advised.  We give an example below to illustrate.  Also, 
	       note that many other computations will no longer work, because 
	       Gröbner basis computations only work 
	       over ", TO "ZZ", ", ", TT "ZZ/n", " and ", TO "QQ", " at this time. "
	       ,
     	  SUBSECTION "using maps to move between rings",
	       EXAMPLE {
	       	    "map(R6,R2)",
	       	    "substitute(f,R6)"
	       	    },
	       "Macaulay2 claims this is the zero map, and that the image 
	       of ", TT "f", " is 1, but we know better.  By 
	       forming a series of maps and composing them we see the map that 
	       makes sense.  We also contrast the map with 
	       using ", TT "substitute", ".",
	       EXAMPLE {
	       	    "use R2;",
	       	    "f = s^4+1;",
	       	    "F = map(R4,R2);",
	       	    "G = map(R5,R4);",
	       	    "H = map(R6,R5);",
	       	    "H(G(F(f)))",
	       	    "f1 = substitute(f,R4)",
	       	    "f2 = substitute(f1,R5)",
	       	    "substitute(f2,R6)"
	       	    },
	       ,
     	  SUBSECTION "elements versus matrices",
	       "Finally, note that everywhere we used the element ", TT "f", " we 
	       can place a matrix or an ideal and get similar results."
	       ,
     	  SUBSECTION "substitute(J,vars R)",
	       "We close this long example with a brief discussion 
	       of ", TT "substitute(J,vars R)", ".  This command is more 
	       sensitive than ", TT "substitute", " as it will give an error 
	       message when the variables involved do not match up.",
	       EXAMPLE {
	       	    "substitute(f,vars R3)",
	       	    ///try substitute(f,vars R5) else "found error"///
        },
    Subnodes => {
	TO(use, Ring),
	TO lift,
	TO promote,
	TO newRing, -- TODO: move?
	TO commonRing,
        },
    }

document {
     Key => "basic construction, source and target of a ring map",
     
	  SUBSECTION "constructing a ring map", 
	       "Use the function ", TO "map", " to construct a map 
	       between two rings.  The input, in order, is the 
	       target, the source, and the images of the 
	       variables of the source ring.  The images can 
	       be given as a matrix or a list.",
	       EXAMPLE {
		    "S = QQ[x,y,z]/ideal(x^3+y^3+z^3);",
		    "T = QQ[u,v,w]/ideal(u^3+v^3+w^3);",
		    "G = map(T,S,matrix{{u,v,w^2}})",
		    "G(x^3+y^3+z)",
		    },
	       "If the third argument is not given there are two 
	       possibilities.  If a variable 
	       in the source ring also appears in the target ring then that 
	       variable is mapped to itself and if a variable does not appear 
	       in the target ring then it is mapped to zero.",
	       EXAMPLE {
		    "R = QQ[x,y,w];",
		    "F = map(S,R)",
		    "F(x^3)"
		   }
	      ,
	 SUBSECTION "source and target",
	      "Once a ring map is defined the functions ", TO "source", " 
	      and ", TO "target", " can be used to find out what the source 
	      and target of a map are.  These functions are particularly useful 
	      when working with matrices (see the next example). ",
	      EXAMPLE {
		   "U = QQ[s,t,u, Degrees => {{1,2},{1,1},{1,3}}];",
		   "H = map(U,R,matrix{{s^2,t^3,u^4}})",
		   "use R; H(x^2+y^2+w^2)",
		   "source H",
		   "target H",
		   },
	      ,
	 SUBSECTION "obtaining the matrix defining a map",
	      "Use ", TT "F.matrix", " to obtain the matrix defining
	      the map F.",
	      EXAMPLE {
	      	   "H.matrix",
		   "source H.matrix",
		   "target H.matrix",
		   },
	      "For more on matrices from maps see ", TO "inputting a matrix", "."
	      
	 
    }

document {
     Key => "evaluation and composition of ring maps",
     
	  SUBSECTION "evaluating ring maps",
     	       "Once a ring map ", TT "F", " is defined, the image of an 
     	       element ", TT "m", " in the source ring can be found by applying 
     	       the map as ", TT "F(m)", ".",
	       EXAMPLE {
		    "R = ZZ[x,y,z];",
		    "S = ZZ/101[x,y,z,Degrees => {{1,2},{1,3},{1,3}}]/ideal(x+z^3);",
		    "F = map(S,R,{x,y^2,z^3})",
		    "use R; F(107*x+y+z)"
		    }
	       ,
	  SUBSECTION "composition of ring maps",  
	       -- see if you can't do something with galois.
	       "The function ", TO (symbol*,RingMap,RingMap), " performs a 
	       composition of ring maps.  Evaluation of elements in the source 
	       of a ring map ", TT "G"," can also be done using ", TT "F(G(m))", ".",
	       EXAMPLE { 
		    "T = ZZ/5[x,y];",
		    "G = map(T,S);",
		    "G*F",
		    "use R; G(F(107*x+y+z))",
		    },
	       
	  
     }


document {
     Key => "kernel and coimage of a ring map",
     "The kernel and coimage of a ring map can be computed
     using ", TO "coimage", " and ", TO2(kernel, "ker"), " .  The output
     of ", TT "ker", " is an ideal and the output of ", TT "coimage", " is a
     ring or quotient ring.",
     EXAMPLE {
	  "R = QQ[x,y,w]; U = QQ[s,t]/ideal(s^4+t^4);",
	  "H = map(U,R,matrix{{s^2,s*t,t^2}})",
	  "ker H",
	  "coimage H"
	  }
     -- if module and ring map are homogeneous, and Hilbert F is known,
     -- this is used in computing the kernel (or coimage).
     }
undocumented {
     (symbol SPACE, RingMap, Number)
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
     SeeAlso => {(symbol SPACE, RingElement, Sequence)}
     }

undocumented (symbol SPACE, RingElement, Array) -- TODO: eventually deprecate this

doc ///
Node
  Key
    (symbol SPACE, RingElement, Sequence)
    (symbol SPACE, RingElement, Number)
    (symbol SPACE, RingElement, RingElement)
  Headline
    evaluation of polynomials
  Usage
    f(a,b,c)
  Inputs
    f:RingElement
    :Nothing
      @TT "(a,b,c)"@, a sequence of numbers or ring elements
  Outputs
    r:RingElement
      the result of evaluating @TT "f"@ at the provided values
  Description
    Example
      R = QQ[x,y,z];
      f = x^3 - y^2 + 999*z;
      f(10,0,0)
      f(0,1,0)
      f(0,0,1)
      f(10,1,-1)
      x^3 * f(0,0,1)
      f(0,0,x+y+z) / 999
      f(z,y,x)
      f(x,x,x)
    Text
      Note that giving fewer variables results in partial evaluation.
    Example
      f(10)
      f(10,y,z)
  SeeAlso
    (symbol SPACE, RingMap, RingElement)
///

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

-- document {
--      Key => EliminationOrder,
--      Headline => "use the natural elimination order in a pushForward1 computation",
--      TT "EliminationOrder", " -- a value for the ", TO "MonomialOrder", "
--      option to ", TO "pushForward1", " which specifies the natural elimination
--      order be used."
--      }
