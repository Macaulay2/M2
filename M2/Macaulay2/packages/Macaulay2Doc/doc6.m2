-- -*- coding: utf-8 -*-
--		Copyright 1993-2002 by Daniel R. Grayson

document {
     Key => Engine,
     Headline => "specify whether a ring is handled by the engine",
     TT "Engine", " -- a key for rings that yields the value ", TT "true", " if this
     ring is supported by the ", TO "engine", "."}

document {
     Key => End,
     Headline => "module of endomorphisms",
     TT "End M", " -- constructs the module of endomorphisms of ", TT "M", "."}
document {
     Key => ModuleMap,
     Headline => "the class of all maps between modules",
     "This class is experimental, designed to support graded modules.",
     SeeAlso => {"Matrix"}}

document {
     Key => (symbol ^, Ring, List),
     Headline => "make a free module",
     Usage => "M = R^{i,j,k,...}",
     Inputs => {"R", 
	  Nothing => {TT "{i,j,k, ...}", ", ", ofClass List, ", of integers or lists of integers"}},
     Outputs => {
          Module => {
	       {", a free module over ", TT "R", " whose generators have degrees ", TT "-i", ", ", TT "-j", ", ", TT "-k", ", ..."}}},
     EXAMPLE lines ///
     	  R = QQ[a..d]
	  R^{-1}
	  R^{-1,2:-2,-3}
     	  ///,
     PARA{},
     "If ", TT "i", ", ", TT "j", ", ... are lists of integers, then
     they represent multi-degrees, as in ", TO "graded and multigraded polynomial rings", ".",
     EXAMPLE lines ///
     	  R = QQ[x,y,z,Degrees=>{{1,0},{1,-1},{1,-2}}]
	  R^{{1,2}}
     	  ///,
     SeeAlso => {"degrees", "free modules", "graded and multigraded polynomial rings"}}
document {
     Key => (symbol ^,Module,ZZ),
     Headline => "direct sum",
     Usage => "M^n",
     Inputs => {"M", "n"},
     Outputs => {{"the direct sum of ", TT "n", " copies of ", TT "M"}},
     EXAMPLE lines ///
     	  M = coker matrix{{1,2,3}}
	  M^3
	  directSum(3:M)
     ///,
     SeeAlso => {directSum, symbol++}
     }
document {
     Key => (symbol ^,Ring,ZZ),
     Headline => "make a free module",
     Usage => "R^n",
     Inputs => {"R", "n"},
     Outputs => {{"a new free ", TT "R", "-module of rank ", TT "n", "." }},
     "The new free module has basis elements of degree zero.  To specify the
     degrees explicitly, see ", TO (symbol ^,Ring,List), ".",
     EXAMPLE lines ///
     	  R = ZZ[x,y,z]/(x^2-y*x)
	  F = R^4
	  degrees F
     	  ///,
     SeeAlso => {(degrees,Module), (symbol^,Ring,List), "graded and multigraded polynomial rings"}
     }

document {
     Key => (symbol ^, SheafOfRings, List),
     Headline => "make a graded free coherent sheaf",
     Usage => "M = R^{i,j,k,...}",
     Inputs => {"R", 
	  Nothing => {TT "{i,j,k, ...}", ", ", ofClass List, ", of integers or lists of integers"}},
     Outputs => {
          Module => {
	       {", a graded free coherent sheaf whose generators have degrees ", TT "-i", ", ", TT "-j", ", ", TT "-k", ", ..."}}},
     EXAMPLE lines ///
     	  R = QQ[a..d]/(a*b*c*d)
	  X = Proj R
	  OO_X^{-1,-2,3}
     	  ///,
     PARA{},
     "If ", TT "i", ", ", TT "j", ", ... are lists of integers, then
     they represent multi-degrees, as in ", TO "graded and multigraded polynomial rings", ".",
     EXAMPLE lines ///
     	  Y = Proj (QQ[x,y,z,Degrees=>{{1,0},{1,-1},{1,-2}}])
	  OO_Y^{{1,2},{-1,3}}
	  degrees oo
     	  ///,
     SeeAlso => {OO, Proj, degrees, "graded and multigraded polynomial rings"}}
document {
     Key => {
	  (symbol ^, CoherentSheaf, ZZ),
	  (symbol ^, SheafOfRings, ZZ)},
     Headline => "direct sum",
     Usage => "F^n",
     Inputs => {"F" => {", or a ", ofClass SheafOfRings}, "n"},
     Outputs => {
	  CoherentSheaf => {"the direct sum of ", TT "n", " copies of ", TT "F"},
	  },
     EXAMPLE lines ///
     	  R = QQ[a..d]/(a*d-b*c)
	  Q = Proj R
	  OO_Q^5
	  IL = sheaf module ideal(a,b)
	  IL^3
     	  ///,
     SeeAlso => {Proj, sheaf}
     }
document {
     Key => (symbol ^, RingElement, ZZ),
     Headline => "power",
     Usage => "f^n",
     Inputs => {"f", "n"},
     Outputs => {
     	  RingElement => TT "f^n"
	  },
     EXAMPLE lines ///
     	  R = ZZ/7[x]/(x^46-x-1);
	  (x+4)^(7^100)
     	  ///,
     PARA{},
     "If the ring allows inverses, negative values may be used.",
     EXAMPLE lines ///
     	  S = ZZ[t,Inverses=>true,MonomialOrder=>RevLex];
	  t^-1
	  T = frac(ZZ[a,b,c]);
	  (a+b+c)^-1
     	  ///,
     SeeAlso => {frac, "polynomial rings"}
     }
document {
     Key => (symbol ^, Matrix, ZZ),
     Headline => "power",
     Usage => "f^n",
     Inputs => {"f", "n"},
     Outputs => {
     	  Matrix => TT "f^n"
	  },
     EXAMPLE lines ///
     	  R = ZZ/7[x]/(x^6-3*x-4)
	  f = matrix{{x,x+1},{x-1,2*x}}
	  f^2
	  f^1000
     	  ///,
     PARA{},
     "If the matrix is invertible, then f^-1 is the inverse.",
     EXAMPLE lines ///
     	  M = matrix(QQ,{{1,2,3},{1,5,9},{8,3,1}})
	  det M
	  M^-1
	  M^-1 * M
	  R = QQ[x]
	  N = matrix{{x^3,x+1},{x^2-x+1,1}}
	  det N
	  N^-1
	  N^-1 * N
     	  ///,
     SeeAlso => {det}
     }
document {
     Key => {(symbol ^, ChainComplex, ZZ)},
     Headline => "access member, cohomological degree",
     Usage => "C^n",
     Inputs => {"C", "n"},
     Outputs => {
     	  Module => {"The ", TT "(-n)", "-th component ", TT "C_(-n)", " of ", TT "C"}
	  },
     "Subscripts refer to homological degree, and superscripts refer to cohomological degree.
     It is only a matter of notation: ", TT "C_(-n)", " is always the same as ", TT "C^n", ".",
     EXAMPLE lines ///
     	  R = QQ[x,y,z];
	  C = res coker vars R
	  C = dual C
	  C^2
	  C^2 == C_(-2)
     	  ///,
     SeeAlso => {ChainComplex, (symbol^, ChainComplex, Array)}
     }
document {
     Key => {(symbol ^, ChainComplexMap, ZZ),
	  (symbol ^, GradedModuleMap, ZZ)},
     Headline => "iterated composition",
     Usage => "f^n",
     Inputs => {"f" => {"or a ", ofClass GradedModuleMap}, "n"},
     Outputs => {
     	  ChainComplexMap => {"the composite ", TT "f o f o ... o f", " (", TT "n", " times)"}
	  },
     "If ", TT "f", " is a ", TO GradedModuleMap, ", then so is the result.",
     PARA{},
     "One use of this function is to determine if a chain complex is well-defined.  
     The chain complex will be well-defined if the square of the differential is zero.",
     EXAMPLE lines ///
     	  R = QQ[x,y,z];
	  C = res coker vars R
	  C.dd^2 == 0
     	  ///,
     SeeAlso => {ChainComplex}
     }

document {
     Key => (symbol *, Matrix, Matrix),
     Headline => "matrix multiplication",
     Usage => "f * g",
     Inputs => {"f", "g"},
     Outputs => { Matrix },
     "Multiplication of matrices corresponds to composition of maps, and when
     the target ", TT "Q", "
     of ", TT "g", " equals the source ", TT "P", " of ", TT "f", ", the
     product ", TT "f*g", " is defined, its source is the source of ", 
     TT "g", ", and its target is the target of ", TT "f", ".  ",
     EXAMPLE {
	  "R = QQ[a,b,c,x,y,z];",
	  "f = matrix{{x},{y},{z}}",
	  "g = matrix{{a,b,c}}",
	  "f*g"
	  },
     PARA{},
     "The degree of ",
     TT "f*g", " is the sum of the degrees of ", TT "f", " and of ", TT "g",
     ".",
     PARA{},
     "The product is also defined when ", TT "P", " != ", TT "Q", ",
     provided only that ", TT "P", " and ", TT "Q", " are free modules of the
     same rank.  If the degrees of ", TT "P", " differ from the corresponding
     degrees of ", TT "Q", " by the same degree ", TT "d", ", then the degree
     of ", TT "f*g", " is adjusted by ", TT "d", " so it will have a good
     chance to be homogeneous, and the target and source of ", TT "f*g", "
     are as before.",
     EXAMPLE {
	  "target (f*g) == target f",
	  "source (f*g) == source g",
	  "isHomogeneous (f*g)",
	  "degree(f*g)",
	  },
     "Sometimes, it is useful to
     make this a map of degree zero.  Use ", TO (map,Matrix), " for this purpose.",
     EXAMPLE {
	  "h = map(f*g,Degree=>0)",
	  "degree h",
	  "degrees source h"
	  },
     SeeAlso => {(degree,Matrix),degrees}
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
