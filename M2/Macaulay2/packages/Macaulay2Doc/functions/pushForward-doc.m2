--- -*- coding: utf-8 -*-
--- status: Draft
--- author(s): MES, taken also from original by DE
--- notes: 

document { 
     Key => {(pushForward,RingMap,Module),pushForward,
	  [pushForward,MonomialOrder],
	  [pushForward,UseHilbertFunction],
	  [pushForward,StopBeforeComputation],
	  [pushForward,DegreeLimit],
	  [pushForward,PairLimit],
	  [pushForward,Strategy],
	  },
     Headline => "compute the pushforward of a module along a ring map",
     Usage => "pushForward(F,M)",
     Inputs => {
	  "F" => "a ring map F: R --> S, graded",
	  "M" => "over S, graded",
	  MonomialOrder => {"the type of monomial ordering to use in the computation,
	       as keyword, either ", TO "Eliminate", ", ", TO "ProductOrder", ", or ", TO "Lex"
	       },
	  UseHilbertFunction => Boolean => {"whether to use the Hilbert function as a hint for the 
	       Gröbner basis computation, if ", TT "M", " and ", TT "F", " are homogeneous" },
	  StopBeforeComputation => Boolean => {"see ", TO [gb,StopBeforeComputation]},
	  DegreeLimit => Boolean => {"see ", TO [gb,DegreeLimit]},
	  PairLimit => Boolean => {"see ", TO [gb,PairLimit]}
	  },
     Outputs => {
	  Module => "M, considered as an R-module"
	  },
     "Currently, ", TT "R", " and ", TT "S", " must both be polynomial rings over the same base field.",
     PARA{},
     "This function first checks to see whether M will be a finitely generated
     R-module via F.  If not, an error message describing the codimension of
     M/(vars of S)M is given (this is equal to the dimension of R if and only if M is a finitely
     generated R-module.",
     PARA{},
     "Assuming that it is, the push forward ", TT "F_*(M)", " is computed.  This is done
     by first finding a presentation for ", TT "M", " in terms of a set of elements that generates
     ", TT "M", " as an ", TT "S", "-module, and then applying the routine ", TO "coimage", " to a map whose target
     is ", TT "M", " and whose source is a free module over ", TT "R", ".",
     SUBSECTION "Example: The Auslander-Buchsbaum formula",
     "Let's illustrate the Auslander-Buchsbaum formula.
     First construct
     some rings and make a module of projective dimension 2.",
     EXAMPLE lines ///
     	  R4 = ZZ/32003[a..d];
	  R5 = ZZ/32003[a..e];
     	  R6 = ZZ/32003[a..f];
	  M = coker genericMatrix(R6,a,2,3)
	  pdim M
	  ///,
     "Create ring maps.",
     EXAMPLE lines ///
	  G = map(R6,R5,{a+b+c+d+e+f,b,c,d,e})
	  F = map(R5,R4,random(R5^1, R5^{4:-1}))
	  ///,
     "The module M, when thought of as an R5 or R4 module, has the
     same depth, but since depth M + pdim M = dim ring, the projective
     dimension will drop to 1, respectively 0, for these two rings.",
     EXAMPLE lines ///
	  P = pushForward(G,M)
	  pdim P
	  Q = pushForward(F,P)
	  pdim Q
          ///,
     SUBSECTION "Example: generic projection of a homogeneous coordinate ring",
     "We compute the pushforward N of the homogeneous coordinate ring M of the twisted cubic
     curve in P^3.",
     EXAMPLE lines ///
          P3 = QQ[a..d];
	  M = comodule monomialCurveIdeal(P3,{1,2,3})
	  ///,
     "The result is a module with the same codimension, degree and genus as the twisted
     cubic, but the support is a cubic in the plane, necessarily having one node.",
     EXAMPLE lines ///
	  P2 = QQ[a,b,c];
	  F = map(P3,P2,random(P3^1, P3^{-1,-1,-1}))
	  N = pushForward(F,M)
	  hilbertPolynomial M
	  hilbertPolynomial N
	  ann N
          ///,
     "Note: these examples are from the original Macaulay script by David Eisenbud.",
     Caveat => {"The module M must be homogeneous, as must R, S, and f.  If you need this
	  function in more general situations, please write it and send it to the Macaulay2 authors, 
	  or ask them to write it!"},
     SeeAlso => {
	 "PushForward::PushForward",
         },
     }

-- document { 
--      Key => {(pushForward1,RingMap,Module), pushForward1},
--      Headline => "the main computational component of pushForward",
--      Usage => "pushForward1(F,M)",
--      Inputs => {
-- 	  "F" => "a ring map F : R --> S",
-- 	  "M" => "an S-module"
-- 	  },
--      Outputs => {
-- 	  {"the presentation matrix of the R-submodule of M generated
-- 	  by the given (S-module) generators of M."}
-- 	  },
--      PARA{},
--      "Warning: this function may be removed, and its function incorporated into
--      that of ", TO "image", " and ", TO "prune", ".",
--      PARA{},
--      "This is a very basic operation, and is used by several other functions.  See,
--      for example, ", TO "pushForward", ".  Therefore we intend to eliminate it,
--      and merge its function into ", TO "image", " after introducing
--      generalized module homomorphisms which map an R-module to an S-module.",
--      PARA{},
--      "As an example, the following fragment computes the ideal of the
--      rational normal curve. This could also be done using ", TO "monomialCurveIdeal", ".",
--      EXAMPLE lines ///
-- 	  R = ZZ/101[a..d];
--       	  S = ZZ/101[s,t];
--       	  F = map(S,R,matrix{{s^4, s^3*t, s*t^3, t^4}})
--       	  pushForward1(F,S^1)
-- 	  ///,
--      PARA{},
--      "The following code performs the Gröbner computation using a product order 
--      rather than the default elimination order.",
--      EXAMPLE lines ///
--       	  F = map(S,R,matrix{{s^4, s^3*t, s*t^3, t^4}})     
--           time pushForward1(F,S^1,MonomialOrder=>ProductOrder)
-- 	  ///,
--      PARA{},
--      "The computation is stashed inside the ring map, until the computation has
--      finished completely.  This means that you may interrupt this command, and 
--      later restart it. You may also obtain partial results, as follows.",
--      EXAMPLE lines ///
-- 	  F = map(S,R,matrix{{s^4, s^3*t, s*t^3, t^4}})
--       	  pushForward1(F,S^1,DegreeLimit=>8)
-- 	  ///,
--      "After interrupting a computation (using control-C), you may view the
--      equations so far obtained by using the ", TT "StopBeforeComputation", " option to prevent any
--      further work from being done.",
--      EXAMPLE "pushForward1(F,S^1,StopBeforeComputation => true)",
--      "The type ", TO "PushforwardComputation", " is used internally by our current implementation.",
--      SUBSECTION "Further examples",
--      Caveat => {},
--      SeeAlso => {pushForward, Elimination}
--      }
