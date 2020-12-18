-- -*- coding: utf-8 -*-
--		Copyright 1993-2002 by Daniel R. Grayson

document {
     Key => Resolution,
     Headline => "the class of all resolution computations",
     PARA{},
     "These resolutions are internal engine objects not meant to be examined
     by the user.",
     PARA{},
     "The symbol ", TT "Resolution", " is also used in a ", TO "ChainComplex", " to
     store the resolution it comes from."
     }

document {  -- This node is used as an example in the node: Key
     Key => resolution,
     Headline => "projective resolution"
     }
document {
     Key => [resolution,DegreeLimit],
     Headline => "compute only up to this degree",
     TT "DegreeLimit => n", " -- keyword for an optional argument used with
     ", TO "resolution", " which specifies that the computation should halt
     after dealing with degree n.",
     PARA{},
     "This option is relevant only for homogeneous modules.",
     PARA{},
     Caveat => "One might get some matrix entries of slightly higher degree than requested.",
     EXAMPLE {
	  "R = ZZ/101[x,y,z,w];",
      	  "M = cokernel matrix {{x*y-z^2,y^2-w^2}}",
      	  "res(M,DegreeLimit => 1)",
      	  "res(M,DegreeLimit => 2)"
	  },
     }

document { -- This node is used as an example in the node: Key
     Key => [resolution,SyzygyLimit],
     Headline => "stop when this number of syzygies is reached",
     TT "SyzygyLimit", " -- keyword for an optional argument used with
     ", TO "resolution", ", which specifies that the computation should
     stop after a certain number of syzygies have computed.",
     PARA{},
     EXAMPLE {
	  "R = ZZ/101[x,y,z,w];",
      	  "M = cokernel matrix {{x*y-z^2,y^2-w^2,w^4}}",
      	  "res(M,SyzygyLimit => 1)",
      	  "res(M,SyzygyLimit => 2)",
      	  "res(M,SyzygyLimit => infinity)"
	  }
     }

document {
     Key => [resolution,PairLimit],
     Headline => "stop when this number of pairs has been handled",
     TT "PairLimit", " -- keyword for an optional argument used with
     ", TO "resolution", ", which specifies that the computation should
     be stopped after a certain number of S-pairs have been reduced.",
     EXAMPLE {
	  "R = QQ[x,y,z,w]",
      	  "M = cokernel matrix {{x*y-z,y^2-w-1,w^4-3}}",
      	  "res(M, PairLimit => 1)",
      	  "res(M, PairLimit => 10)",
      	  "res(M, PairLimit => 20)"
	  }
     }

document {
     Key => [resolution,StopBeforeComputation],
     Headline => "whether to stop the computation immediately",
     TT "StopBeforeComputation", " -- keyword for an optional argument used with
     ", TO "resolution", ".",
     PARA{},
     "Tells whether to start the computation, with the default value
     being ", TT "true", ".  This can be useful when you want to obtain
     the partially computed resolution contained in an interrupted computation."
     }

document {
     Key => LengthLimit,
     Headline => "stop when the resolution reaches this length",
     }

document {
     Key => [resolution,LengthLimit],
     Headline => "stop when the resolution reaches this length",
     TT "LengthLimit", " -- keyword for an optional argument used with
     ", TO "resolution", " which indicates how long a resolution to make.",
     PARA{},
     "For polynomial rings over a field or over the integers, the length
     is taken to be the dimension of the ring, so the complete resolution will
     be obtained.  For quotient rings of such rings, the same number is used,
     so the complete resolution may not be obtained.",
     PARA{},
     "In the current version, asking for a second and longer resolution of the
     same module involves recomputing the resolution from scratch.  Eventually
     the previous work will be used and the recomputation will go quicker.",
     PARA{},
     "The resolution returned may actually be one step longer than requested.
     The extra differential is not guaranteed to be minimal."
     }

document {
     Key => HardDegreeLimit,
     Headline => "compute only up to this degree",
     TT "HardDegreeLimit", " -- keyword for an optional argument that specifies
     that information above a specified degree is to be discarded."
     }

document {
     Key => [resolution,HardDegreeLimit],
     TT "HardDegreeLimit", " -- keyword for an optional argument used with
     ", TO "resolution", ".",
     PARA{},
     "The default value is ", TT "{}", ".",
     PARA{},
     "Information above the specified degree is discarded."
     }

document {
     Key => [resolution,Strategy],
     TT "Strategy => n", " -- an option for ", TO "resolution", " which specifies
     which algorithm to use.  Strategies are specified by number and the
     algorithms available are",
     UL {
	  SPAN (TT "Strategy => 0", " -- Compute syzygies on the Gröbner bases of each syzygy
	       module.  The algorithm uses important speedups due to R. La Scala.
	       This algorithm appears to be on the average the fastest."),
	  SPAN (TT "Strategy => 1", " -- An older version of algorithm 0, which doesn't allow as
	       much experimentation, but can sometimes be marginally faster."),
	  SPAN (TT "Strategy => 2", " -- Compute syzygies on the minimal generators of each
	       matrix in the resolution.  Over quotient rings, it's preferred."),
	  SPAN (TT "Strategy => 3", " -- Same as algorithm 2, but compute those Hilbert functions
	       which allow removal of S-pairs (a la Robbiano, et al.). Sometimes this
	       improvement can be very dramatic.")
	  },
     "All algorithms use induced monomial orders (Schreyer orders), since
     this makes an enormous improvement to the efficiency of the algorithm."
     }

document {
     Key => SortStrategy,
     Headline => "specify a strategy for sorting S-pairs",
     TT "SortStrategy", " -- a keyword for an optional argument that
     specifies the strategy to be used for sorting S-pairs."
     }

document {
     Key => [resolution,SortStrategy],
     TT "SortStrategy => n", " -- an option for ", TO "resolution", " which
     specifies the strategy to be used for sorting S-pairs.",
     PARA{},
     "Not implemented yet."
     }

document {   -- This node is used as an example for the documentation node: Key, Usage
     Key => (resolution,Module),
     Headline => "compute a free resolution of a module",
     Usage => "resolution M\nres M",
     Inputs => { "M" },
     Outputs => { {"a free resolution of ", TT "M"} },
     PARA {
     	  "The given generators and relations are used to determine a ", TO "presentation", " of ", TT "M", " to serve as the first matrix of the free
     	  resolution; if the presentation is not minimal, and a minimal resolution is desired, use ", 
     	  TT "resolution minimalPresentation M", " instead."},
     PARA {"Warning: the resolution can have free modules with unexpected ranks
	  when the module ", TT "M", " is not homogeneous.  Here is an example
	  where even the lengths of the resolutions differ.  We compute
	  a resolution of the kernel of a ring map in two ways.
	  The ring ", TT "R", " is constructed naively, but the ring
	  ", TT "S", " is constructed with variables of the right degrees
	  so the ring map ", TT "g", " will turn out to be homogeneous."},
     EXAMPLE {
	  "k = ZZ/101; T = k[v..z];",
	  "m = matrix {{x,y,z,x^2*v,x*y*v,y^2*v,z*v,x*w,y^3*w,z*w}}",
	  "n = rank source m",
	  "R = k[u_1 .. u_n]",
	  "S = k[u_1 .. u_n,Degrees => degrees source m]",
	  "f = map(T,R,m)",
	  "g = map(T,S,m)",
	  "res ker f",
	  "res ker g",
	  "isHomogeneous f",
	  "isHomogeneous g"
	  },
     EXAMPLE {
	  "R = ZZ/32003[a..d]/(a^2+b^2+c^2+d^2);",
	  "M = coker vars R",
	  "C = resolution(M, LengthLimit=>6)"
	  },
     PARA {
	  "A manually constructed resolution can be installed as the resolution of
	  a module, bypassing the call to the engine when a resolution is requested,
	  as follows."
	  },
     EXAMPLE {
     	  "A = QQ[x,y]",
	  "C = chainComplex(
	       map(A^1,A^{3:-2},{{x^2,x*y,y^2}}),
	       map(A^{3:-2},A^{2:-3},{{y,0},{ -x,y},{0,-x}}),
	       map(A^{2:-3},0,0))",
	  "M = HH_0 C",
	  "res M = C;",
	  "res M"
	  },
     PARA {
     	  "For an overview of resolutions, in order of increasing detail, see:"
	  },
     UL {
	  SPAN (TO "Hilbert functions and free resolutions"),
	  TO "free resolutions of modules",
	  SPAN (TO "computing resolutions", " -- most detailed")
	  },
     "Some useful related functions:",
     UL {
	  TO (betti,GradedModule),
	  TO (status,Resolution),
	  }
     }

document {
     Key => (resolution, Matrix),
     Headline => "given a module map represented by a matrix, produce a comparison map between resolutions of its source and target",
     Usage => "resolution f",
     Inputs => { "f" => {"a module homomorphism ", TT "N <--- M"} },
     Outputs => { {"a chain map from a projective resolution of the source of ", TT "f", " to a resolution of the target of ", TT "f" } },
     EXAMPLE {
	  "R = ZZ[x,y,z]",
	  "N = R^1/(x,y,z)",
	  "M = R^1/(x^2,y^2,x*y*z,z^2)",
	  "f = map(N,M,1)",
	  "res f"
	  },
     SeeAlso => { "free resolutions of modules" }
     }

document { -- This node is used as an example in the documentation nodes: Inputs, Outputs
     Key => {(resolution, Ideal),(resolution, MonomialIdeal)},
     Headline => "compute a projective resolution of (the quotient ring corresponding to) an ideal",
     Usage => "resolution I",
     Inputs => {
	  "I" => { "an ideal in a ring ", TT "R", ", say" }
	  },
     Outputs => {
	  {"a resolution of ", TT "R/I", " by projective ", TT "R", "-modules"}
	  },
     EXAMPLE {
	  "R = ZZ[a..d]",
	  "I = ideal(a,b,c,d)",
	  "C = res I",
	  "C_2",
	  "C.dd_2"
	  },
     SeeAlso => { (symbol _, ChainComplex, ZZ), dd, res, ideal }
     }

document {
     Key => {status,(status, GroebnerBasis),(status, Resolution),(status, ChainComplex),
	  [status, TotalPairs],[status, Monomials],[status, PairsRemaining]},
     Headline => "status of a resolution computation",
     TT "status C", " -- displays the status of the computation of a
     chain complex ", TT "C", " constructed by application of ", TO "resolution", " to
     a module, provided the resolution has been constructed in the engine;
     in particular, the module should be homogeneous and the ultimate coefficient ring of its
     ring should be a field.  The display has
     the same shape as the display produced by ", TO "betti", ", but
     the number(s) displayed in each degree differ.",
     PARA{},
     "Options:",
     UL {
	  {TO TotalPairs, " -- display the total number of S-pairs, default value ",
	       toString (options status).TotalPairs },
	  {TO PairsRemaining, " -- display the number of S-pairs remaining, default value ",
	       toString (options status).PairsRemaining},
	  {TO Monomials, " -- display the number of monomials, default value ",
	       toString (options status).Monomials}
	  },
     EXAMPLE lines ///
          R = QQ[a..d]
	  D = res coker random(R^2,R^{4:-2})
	  status(D, TotalPairs => true, PairsRemaining => true, Monomials => true)
     ///
     }

document {
     Key => TotalPairs,
     Headline => "whether to display the total number of S-pairs",
     TT "TotalPairs", " -- an option for ", TO "status", " which specifies
     whether to display the total number of S-pairs."
     }

document {
     Key => PairsRemaining,
     Headline => "whether to display the number of S-pairs remaining",
     TT "PairsRemaining", " -- an option for ", TO "status", " which specifies
     whether to display number of S-pairs remaining."
     }

document {
     Key => Monomials,
     Headline => "whether to display the number of monomial",
     TT "Monomials", " -- an option for ", TO "status", " which specifies
     whether to display the number of monomials."
     }

document {
    Key => Precision,
    Headline => "name of an optional argument.",
}

document {
    Key => Unique,
    Headline => "do not return repeated polynomial roots",
    "A boolean", TO Boolean, ", to select whether to return repeated roots or not.",
}


document {
     Key => Variety,
     Headline => "the class of all algebraic varieties",
     SeeAlso => "varieties"
     }
document { Key => AffineVariety, Headline => "the class of all affine varieties" }
document { Key => ProjectiveVariety, Headline => "the class of all projective varieties" }
document {
     Key => {(Spec, Ring),Spec},
     Headline => "make an affine variety",
     Usage => "Spec R",
     Inputs => {"R"},
     Outputs => {{ "the affine variety (or scheme) formed from the ring ", TT "R" }},
     EXAMPLE lines ///
     R = QQ[x,y];
     Spec R
     ///
     }

document {
     Key => {(symbol >=, ZZ),(symbol >=,InfiniteNumber)},
     Usage => "(>= d)",
     Inputs => { "d" },
     Outputs => {{"a special object of class ", TT "LowerBound", " used to represent the set of natural numbers at least as large as ", TT "d"}}
     }

document {
     Key => {(symbol >, ZZ),(symbol >,InfiniteNumber)},
     Usage => "(> d)",
     Inputs => { "d" },
     Outputs => { { "a special object of class ", TT "LowerBound", " used to represent the set of natural numbers larger than ", TT "d" } }
     }

document {
     Key => {(symbol _,OO,Variety), OO},
     Headline => "the structure sheaf",
     Usage => "OO_X",
     Inputs => { "X" => "a variety" },
     Outputs => { { "the structure sheaf of ", TT "X", "." } },
     EXAMPLE lines ///
     R = QQ[x,y,z]/(y^2*z-x*(x-z)*(x-37*z));
     X = Proj R
     OO_X
     HH^1(OO_X)
     HH^0(OO_X(3))
     ///,
     SeeAlso => {CoherentSheaf, cohomology}
     }

document { Key => toRR,
     Headline => "convert to high-precision real number",
     Usage => "toRR(prec,x)",
     Inputs => {
	  "prec" => ZZ => {"the number of bits of precision desired"},
	  "x" => {ofClass{RR,ZZ,QQ}}
	  },
     Outputs => {RR => {"the result of converting ", TT "x", " to a high-precision real number"}},
     EXAMPLE lines ///
     toRR(200,1/7)
     precision oo
     ///
     }

document {
     Key => {toCC,
 	  (toCC, ZZ, ZZ), (toCC, ZZ, QQ), (toCC, ZZ, RR), (toCC, ZZ, CC),
 	  (toCC, RR, RR), (toCC, ZZ, ZZ, ZZ), (toCC, ZZ, ZZ, QQ),
	  (toCC, ZZ, QQ, ZZ), (toCC, ZZ), (toCC, ZZ, QQ, QQ), (toCC, QQ),
	  (toCC, ZZ, RR, ZZ), (toCC, ZZ, ZZ, RR), (toCC, ZZ, RR, QQ),
	  (toCC, ZZ, QQ, RR), (toCC, RR), (toCC, CC), (toCC, ZZ, RR, RR)
	  },
     Headline => "convert to high-precision complex number",
     SYNOPSIS (
	  Usage => "toCC(prec,x,y)\ntoCC(prec,x)",
	  Inputs => {
	       "prec" => ZZ => {"the number of bits of precision desired"},
	       "x" => {ofClass{ZZ,QQ,RR}},
	       "y" => {ofClass{ZZ,QQ,RR}}
	       },
	  Outputs => {CC => {"the complex number with real part ", TT "x", " and complex part ", TT "y", ".  If
		    ", TT "y", " is omitted, the imaginary part is zero."}},
	  EXAMPLE lines ///
	  toCC(200,7)
	  toCC(100,7,3.)
	  ///
	  ),
     SYNOPSIS (
	  Usage => "toCC(x,y)\ntoCC x",
	  Inputs => { "x" => RR, "y" => RR },
	  Outputs => {CC => {"the complex number with real part ", TT "x", " and complex part ", TT "y", ".  If
		    ", TT "y", " is omitted, the imaginary part is zero.  The precision of the result is
		    the minimum precision of the arguments."}},
	  EXAMPLE lines ///
	  toCC(3.,4.)
	  toCC(3.p100,4.p200)
	  ///
	  )
     }

document { Key => InexactNumber,
     PARA {
	  "This type of number is intended to serve as a parent class for those types of numbers
	  that are inexactly represented in the computer."
	  }
     }

document {
     Key => { Constant,
	  (symbol /,Constant,Constant),
	  (symbol /,Constant,InexactNumber),
	  (symbol /,Constant,Number),
	  (symbol /,InexactNumber,Constant),
	  (symbol /,Number,Constant),
	  (symbol ==,Constant,Constant),
	  (symbol ==,Constant,InexactNumber),
	  (symbol ==,InexactNumber,Constant),
	  (symbol ^,Constant,Constant),
	  (symbol ^,Constant,InexactNumber),
	  (symbol ^,Constant,Number),
	  (symbol ^,InexactNumber,Constant),
	  (symbol ^,Number,Constant),
	  (symbol +,Constant,RingElement),
	  (symbol +,RingElement,Constant),
	  (symbol -,Constant,RingElement),
	  (symbol -,RingElement,Constant),
	  (symbol *,Constant,RingElement),
	  (symbol *,RingElement,Constant),
	  (symbol /,Constant,RingElement),
	  (symbol /,Holder,OneExpression),
	  (symbol /,RingElement,Constant)
     	  },
     PARA {
	  "A constant is a symbolic entity that can be approximated by a real or complex
	  number to any desired accuracy.  It is converted to a numeric value of the
	  correct precision, when necessary."
	  },
     EXAMPLE lines ///
     pi
     +pi
     numeric_100 pi
     2. * pi
     2p100 * pi
     exp(2*pi*ii/17)
     ///,
     SeeAlso => { numeric, "defaultPrecision" }
     }

document { Key => InexactField,
     Headline => "the class of inexact fields",
     PARA {
	  "An inexact field is one whose elements are real or complex numbers,
	  represented floating point approximations of varying accuracy or precision."
	  },
     EXAMPLE lines ///
     numeric_100 pi
     ring oo
     class oo
     parent oo
     ///
     }

document { Key => InexactFieldFamily,
     Headline => "the class of all families of inexact fields",
     PARA {
	  "All real numbers have the same class, ", TO "RR", ", but the rings they
	  belong to depends on the number of binary digits of precision used
	  to represent them.  Similarly for complex numbers, which all belong
	  to the class ", TO "CC", ".  Thus ", TO "RR", " and ", TO "CC", " are regarded not as inexact
	  fields, but as families of inexact fields."
	  },
     EXAMPLE lines ///
     x = 1/3.
     class x
     ring x
     x = 1/3.p200
     class x
     ring x
     ///,
     SeeAlso => { InexactField }
     }

document { Key => RealField,
     Headline => "the class of all real fields",
     PARA { "A real number ring is a ring whose elements are real numbers of variable precision." }
     }

undocumented {
     (NewOfFromMethod,ComplexField,Nothing,ZZ),
     (NewOfFromMethod,RealField,Nothing,ZZ)
     }

document { Key => ComplexField,
     Headline => "the class of all complex fields",
     PARA { "A complex number ring is a ring whose elements are complex numbers of variable precision." }
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
