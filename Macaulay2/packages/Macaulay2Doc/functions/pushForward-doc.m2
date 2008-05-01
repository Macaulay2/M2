--- status: Draft
--- author(s): MES, taken also from original by DE
--- notes: 

document { 
     Key => {(pushForward,RingMap,Module),pushForward},
     Headline => "",
     Usage => "pushForward(F,M)",
     Inputs => {
	  "F" => "a ring map F: R --> S, graded",
	  "M" => "over S, graded"
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
     by first finding a presentation for ", TT "M", " in terms of a set of elements which generate
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
	  or ask them to write it!"}
     }

TEST ///
  P3 = ZZ/32003[a..i];
  M = comodule monomialCurveIdeal(P3,{1,3,8,9,12,13,17,21})
  P2 = ZZ/32003[a,b,c,d,e,f];
  F = map(P3,P2,random(P3^1, P3^{-1,-1,-1,-1,-1,-1}))
time   N = pushForward(F,M,Strategy=>Linear);
  P2 = ZZ/32003[a,b,c,d,e,f];
  F = map(P3,P2,random(P3^1, P3^{-1,-1,-1,-1,-1,-1}))
time   N = pushForward(F,M,BasisElementLimit=>5);
  hilbertPolynomial M
  hilbertPolynomial N
  ann N
///
end

document {
     Key => pushForward,
     TT "pushForward(f,M)", " -- yields an R-presentation of the S-module ", TT "M", ", where
     ", TT "f:R --> S", " is a ring map, and ", TT "M", " is considered as 
     an ", TT "R", "-module via ", TT "f", ".",
     PARA{},
     "If ", TT "M", " is not finitely generated over ", TT "R", ", then an error is raised.",
     PARA{},

     }


document { 
     Key => [pushForward, DegreeLimit],
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => [pushForward, PairLimit],
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => [pushForward, BasisElementLimit],
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => [pushForward, StopWithMinimalGenerators],
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => [pushForward, UseHilbertFunction],
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => [pushForward, MonomialOrder],
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => [pushForward, Strategy],
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => [pushForward, StopBeforeComputation],
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
