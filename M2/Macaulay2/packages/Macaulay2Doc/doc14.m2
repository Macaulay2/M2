--		Copyright 2008 by Daniel R. Grayson

document { Key => isReal,
     Usage => "isReal x",
     Headline => "whether a number is real",
     Inputs => { "x" },
     Outputs => { Boolean => {"whether ", TT "x", " is real"} },
     EXAMPLE lines ///
     isReal 2.
     isReal sqrt(-1)
     ///

document { Key => {commonRing, (commonRing,List)},
     Usage => "commonRing v",
     Headline => "find smallest containing ring",
     Inputs => { "v" => "a list of numbers, ring elements, matrices, or things with rings, see ", TO "ring" },
     Outputs => { Ring => "the smallest ring to which all members of the list can be promoted" },
     EXAMPLE lines ///
     	  commonRing {1,2/3}
     	  commonRing {1,2.}
     	  commonRing {1,2p100,matrix{{3p200}}}
	  R = RR[x];
     	  commonRing {1,2p100,matrix{{3p200}},0_R}
     	  ///
     }

document { Key => {(Wikipedia,String),Wikipedia},
     Headline => "link to a Wikipedia entry",
     Usage => "Wikipedia s",
     Inputs => { "s" },
     Outputs => { {"a paragraph with a link to the Wikipedia entry with title ", TT "s",
	       ", usable inside a documentation node"} },
     EXAMPLE lines ///
          Wikipedia "Bessel_function"
	  html oo
	  ///,
     SeeAlso => { document }
     }

document { Key => {log1p,(log1p, QQ),(log1p, ZZ),(log1p, RR)},
     Usage => "log1p x",
     Headline => "logarithm of 1+x",
     Inputs => { "x" },
     Outputs => { RR => { "the logarithm of ", TT "1+x" }},
     EXAMPLE lines ///
log1p 1p100e-10
log(1 + 1p100e-10)
     ///
     }
document { Key => {expm1,(expm1, ZZ),(expm1, RR),(expm1, QQ)},
     Usage => "expm1 x",
     Headline => "exponential minus 1",
     Inputs => { "x" },
     Outputs => { RR => { "the quantity ", TT "exp(x)-1" }},
     EXAMPLE lines ///
     	  expm1 1p100e-10
     	  exp(1p100e-10)-1
     ///
     }
document { Key => {eint,(eint, QQ),(eint, ZZ),(eint, RR)},
     Usage => "eint x",
     Headline => "exponential integral",
     Inputs => { "x" },
     Outputs => { RR => { "the exponential integral of ", TT "x" }},
     EXAMPLE lines ///
     	  eint 2
     ///,
     Wikipedia "Exponential_integral"
     }
document { Key => {Gamma,(Gamma, ZZ),(Gamma, RR),(Gamma, QQ)},
     Usage => "Gamma x",
     Headline => "Gamma function",
     Inputs => { "x" },
     Outputs => { RR => { "the gamma function of ", TT "x" }},
     EXAMPLE lines ///
     	  Gamma 6
     ///,
     Wikipedia "Gamma_function"
     }
document { Key => {lngamma,(lngamma, QQ),(lngamma, ZZ),(lngamma, RR)},
     Usage => "lngamma x",
     Headline => "logarithm of the Gamma function",
     Inputs => { "x" },
     Outputs => {{ "the logarithm of the gamma function of ", TT "x", " as a real or imaginary number" }},
     EXAMPLE lines ///
     	  lngamma 2.1
	  lngamma(-1.1)
	  lngamma(-2.1)
	  lngamma (-2.000000000000000000000000000000001p120)
     ///
     }
document { Key => {zeta,(zeta, QQ),(zeta, ZZ),(zeta, RR)},
     Usage => "zeta x",
     Headline => "Riemann zeta function",
     Inputs => { "x" },
     Outputs => { RR => { "the zeta function of ", TT "x" }},
     EXAMPLE lines ///
     	  zeta 2
     ///,
     Wikipedia "Riemann_zeta_function"
     }
document { Key => {erf,(erf, ZZ),(erf, RR),(erf, QQ)},
     Usage => "erf x",
     Headline => "error function",
     Inputs => { "x" },
     Outputs => { RR => { "the error function of ", TT "x" }},
     EXAMPLE lines ///
     	  erf 2
     ///,
     Wikipedia "Error_function"
     }
document { Key => {erfc,(erfc, QQ),(erfc, ZZ),(erfc, RR)},
     Usage => "erfc x",
     Headline => "complementary error function",
     Inputs => { "x" },
     Outputs => { RR => { "the complementary error function of ", TT "x" }},
     EXAMPLE lines ///
     	  erfc 2
     ///,
     Wikipedia "Error_function"
     }
document { Key => {sec,(sec, ZZ),(sec, RR),(sec, QQ)},
     Usage => "sec x",
     Headline => "secant",
     Inputs => { "x" },
     Outputs => { RR => { "the secant of ", TT "x" }},
     EXAMPLE lines ///
     	  sec(pi/3)
     ///,
     Wikipedia "Trigonometric_function"
     }
document { Key => {csc,(csc, QQ),(csc, ZZ),(csc, RR)},
     Usage => "csc x",
     Headline => "cosecant",
     Inputs => { "x" },
     Outputs => { RR => { "the cosecant of ", TT "x" }},
     EXAMPLE lines ///
     	  csc(pi/3)
     ///,
     Wikipedia "Trigonometric_function"

     }
document { Key => {cot,(cot, ZZ),(cot, RR),(cot, QQ)},
     Usage => "cot x",
     Headline => "cotangent",
     Inputs => { "x" },
     Outputs => { RR => { "the cotangent of ", TT "x" }},
     EXAMPLE lines ///
     	  cot(pi/3)
     ///,
     Wikipedia "Trigonometric_function"

     }
document { Key => {sech,(sech, QQ),(sech, ZZ),(sech, RR)},
     Usage => "sech x",
     Headline => "hyperbolic secant",
     Inputs => { "x" },
     Outputs => { RR => { "the hyperbolic secant of ", TT "x" }},
     EXAMPLE lines ///
     	  sech(pi/3)
     ///,
     Wikipedia "Hyperbolic_function"
     }
document { Key => {csch,(csch, ZZ),(csch, RR),(csch, QQ)},
     Usage => "csch x",
     Headline => "hyperbolic cosecant",
     Inputs => { "x" },
     Outputs => { RR => { "the hyperbolic cosecant of ", TT "x" }},
     EXAMPLE lines ///
     	  csch(pi/3)
     ///,
     Wikipedia "Hyperbolic_function"
     }
document { Key => {coth,(coth, QQ),(coth, ZZ),(coth, RR)},
     Usage => "coth x",
     Headline => "hyperbolic cotangent",
     Inputs => { "x" },
     Outputs => { RR => { "the hyperbolic cotangent of ", TT "x" }},
     EXAMPLE lines ///
     	  coth(pi/3)
     ///,
     Wikipedia "Hyperbolic_function"
     }
     
document {
     Key => ii,
     Headline => "the square root of -1",
     Usage => "ii",
     Inputs => { },
     Outputs => { CC => { "the square root of -1, converted to a numeric value of the correct precision, when necessary." }},
     EXAMPLE lines ///
     	  ii
     	  ii+1p100
	  toCC ii
	  numeric ii
	  numeric_100 ii
     ///,
     "Here is the simplest way to convert ii to a numeric value:",
     EXAMPLE "+ii",
     SeeAlso => {numeric}
     }

document { Key => pi,
     PARA {
	  TEX "This constant represents the mathematical constant $\\pi$, symbolically."
	  },
     EXAMPLE lines ///
     	  pi
     	  toRR_100 pi
	  2. * pi
     ///
     }

document { Key => {EulerConstant},
     Usage => "EulerConstant",
     Headline => "the Euler-Mascheroni constant",
     Inputs => { },
     Outputs => { RR => { "the Euler-Mascheroni constant, converted to a numeric value of the correct precision, when necessary." }},
     EXAMPLE lines ///
     	  EulerConstant
     	  +EulerConstant
     	  EulerConstant+100p100
     ///,
     Wikipedia "Euler-Mascheroni_constant",
     }
document { Key => {InexactNumber'},
     "This class is the common parent of the classes of complex fields and real fields."
     }
document { Key => {RingFamily},
     "This family is used to contain classes that correspond to a family of similar rings with a default member."
     }
document { Key => {BesselJ,(BesselJ, ZZ, QQ),(BesselJ, ZZ, ZZ),(BesselJ, ZZ, RR)},
     Usage => "BesselJ(n,x)\nBesselJ_n x",
     Headline => "Bessel function of the first kind",
     Inputs => {
	  "n" => ZZ => { "the order" },
	  "x" => { ofClass{ZZ,QQ,RR} }
	  },
     Outputs => {
	  {"the Bessel function of the first kind of order ", TT "n", " at ", TT "x"}
	  },
     EXAMPLE lines ///
          BesselJ_0 .5
     	  BesselJ_2 3p200
     ///,
     Wikipedia "Bessel_function",
     SeeAlso => { BesselY }
     }
document { Key => {BesselY,(BesselY, ZZ, ZZ),(BesselY, ZZ, RR),(BesselY, ZZ, QQ)},
     Usage => "BesselY(n,x)\nBesselY_n x",
     Headline => "Bessel function of the second kind",
     Inputs => {
	  "n" => ZZ => { "the order" },
	  "x" => { ofClass{ZZ,QQ,RR} }
	  },
     Outputs => {
	  {"the Bessel function of the second kind of order ", TT "n", " at ", TT "x"}
	  },
     EXAMPLE lines ///
          BesselY_0 .5
     	  BesselY_2 3p200
     ///,
     Wikipedia "Bessel_function",
     SeeAlso => { BesselJ }
     }
document { Key => {agm,(agm, ZZ, ZZ),(agm, QQ, ZZ),(agm, ZZ, QQ),(agm, QQ, QQ),(agm, ZZ, RR),(agm, RR, ZZ),(agm, QQ, RR),(agm, RR, QQ),(agm, RR, RR)},
     Usage => "agm(x,y)",
     Inputs => { "x" => "a number", "y" => "a number" },
     Outputs => { {"the arithmetic-geometric mean of ", TT "x", " and ", TT "y"}},
     Headline => "arithmetic-geometric mean",
     EXAMPLE lines ///
     	  agm(1,2p200)
     ///,
     Wikipedia "Arithmetic-geometric_mean"
     }

document {
     Key => {default,(default, InexactFieldFamily)},
     Headline => "default member of a family",
     Usage => "default T",
     Inputs => { "T" },
     Outputs => {{ "the default member of the family shadowed by ", TT "T" }},
     EXAMPLE lines ///
     	  default RR
	  defaultPrecision = 100
	  default CC
     ///,
     "This function is used internally, and its implementation may change.",
     SeeAlso => { "defaultPrecision" }
     }

document {
     Headline => "fill a mutable matrix with random numbers",
     Key => {fillMatrix,(fillMatrix, MutableMatrix),(fillMatrix, MutableMatrix, ZZ),[fillMatrix,Density],[fillMatrix,UpperTriangular]},
     Usage => "fillMatrix M\nfillMatrix(M,n)",
     BaseFunction => fillMatrix,
     Inputs => {
	  "M"=>MutableMatrix,
	  "n" => ZZ => {"if specified, the maximum number of entries to replace"},
	  Density => RR => {"the fraction of entries of ", TT "M", " to be replaced, if ", TT "n", " is
	       not specified"},
	  UpperTriangular => Boolean => "whether to fill entries only above the diagonal"
	  },
     Outputs => {"M"},
     Consequences => {{ "some entries of M are replaced with randomly generated numbers" }},
     EXAMPLE lines ///
	  printingPrecision = 2
	  fillMatrix(mutableMatrix(RR,5,10))
	  fillMatrix(mutableMatrix(ZZ,5,10),UpperTriangular=>true)
	  randomHeight = 1000
	  fillMatrix(mutableMatrix(QQ,5,10),Density=>.2)
	  fillMatrix(mutableMatrix(ZZ,5,10),25)
	  ///,
     SeeAlso => { "randomHeight" }     
     }

document { 
     Key => {norm,
	  (norm, InfiniteNumber, Matrix),(norm, Matrix),(norm, RR, Matrix),(norm, InfiniteNumber, RingElement),(norm, MutableMatrix),
	  (norm, RingElement),(norm, InexactField, MutableMatrix),(norm, RR, MutableMatrix),(norm, RR, RingElement),
	  (norm,Number),(norm,RR,Number),(norm,InfiniteNumber,Number)
	  },
     Usage => "norm M\nnorm(p,M)",
     Inputs => {
	  "M"=>{ofClass{MutableMatrix,Matrix,RingElement,Number}},
	  "p"=>{ofClass{RR,InfiniteNumber}, ", specifying which norm to compute.  Currently, only ", TT "p=infinity", " is accepted."}
	  },
     Outputs => {TEX{"the $L^p$-norm of ", TT "M", " computed to the minimum of the precisions of ", TT "M", " and of ", TT "p",
	       "."}},
     EXAMPLE lines ///
	  printingPrecision = 2
	  R = RR_100
	  M = 10*random(R^3,R^10)
	  norm M
	  norm_(numeric_20 infinity) M
	  ///,
     "The norm of a polynomial is the norm of the vector of its coefficients.",
     EXAMPLE lines ///
	  RR[x]
	  (1+x)^5
	  norm oo
	  ///
     }

document { Key => "defaultPrecision",
     Headline => "default precision of numbers",
     Usage => "defaultPrecision = n",
     Inputs => { "n" => ZZ },
     Consequences => {{"henceforth, when a real or complex number is to be created,
	       and no precision is otherwise specified, the precision will be ", TT "n"}},
     EXAMPLE lines ///
     defaultPrecision
     1/3.
     defaultPrecision=100     
     1/3.	       
     RR[x]
     numeric pi
     ///
     }

end

[cotangentSheaf, Minimize],
[gb, MaxReductionCount],
[hermite, Strategy],
(lift,Matrix,InexactNumber'),
[lift, Verify],
(map,Module,Module,Number),
[markedGB, ChangeMatrix],
[markedGB, MinimalMatrix],
[markedGB, SyzygyMatrix],
[mingens, Strategy],
[minimalPresentation, Strategy],
(NewOfFromMethod,ComplexField,Nothing,ZZ),
(NewOfFromMethod,RealField,Nothing,ZZ),
(precision,Number),
(promote,Matrix,InexactNumber'),
[pushForward1, BasisElementLimit],
[pushForward, BasisElementLimit],
[pushForward, DegreeLimit],
[pushForward, MonomialOrder],
[pushForward, PairLimit],
[pushForward, UseHilbertFunction],
[quotient, BasisElementLimit],
[quotient, DegreeLimit],
[quotient, PairLimit],
[random, Density],
[random, Height],
randomHeight,
(random,List,Ring),
[random, Norm],
[random, UpperTriangular],
(ring,CC),
(ring,RR),
(round,QQ),
(round,RR),
(round,ZZ,RR),
[saturate, BasisElementLimit],
[saturate, PairLimit],
[solve, ClosestFit],
[solve, MaximalRank],
(substitute,Ideal,RingFamily),
(substitute,Matrix,RingFamily),
(substitute,Module,RingFamily),
(substitute,Number,RingFamily),
(substitute,RingElement,RingFamily),
(substitute,Vector,RingFamily),
(symbol ==,Boolean,Boolean),
(symbol -,CC),
(symbol +,CC),
(symbol ==,CC,CC),
(symbol -,CC,CC),
(symbol /,CC,CC),
(symbol *,CC,CC),
(symbol +,CC,CC),
(symbol _,CC,ComplexField),
(symbol ==,CC,QQ),
(symbol -,CC,QQ),
(symbol /,CC,QQ),
(symbol *,CC,QQ),
(symbol +,CC,QQ),
(symbol ==,CC,RR),
(symbol -,CC,RR),
(symbol /,CC,RR),
(symbol *,CC,RR),
(symbol +,CC,RR),
(symbol ^,CC,ZZ),
(symbol ==,CC,ZZ),
(symbol -,CC,ZZ),
(symbol /,CC,ZZ),
(symbol *,CC,ZZ),
(symbol +,CC,ZZ),
(symbol -,Constant),
(symbol +,Constant),
(symbol ^,Constant,Constant),
(symbol ==,Constant,Constant),
(symbol -,Constant,Constant),
(symbol /,Constant,Constant),
(symbol *,Constant,Constant),
(symbol +,Constant,Constant),
(symbol _,Constant,InexactFieldFamily),
(symbol ^,Constant,InexactNumber),
(symbol ==,Constant,InexactNumber),
(symbol -,Constant,InexactNumber),
(symbol /,Constant,InexactNumber),
(symbol *,Constant,InexactNumber),
(symbol +,Constant,InexactNumber),
(symbol ^,Constant,Number),
(symbol -,Constant,Number),
(symbol /,Constant,Number),
(symbol *,Constant,Number),
(symbol +,Constant,Number),
(symbol _,Constant,Ring),
(symbol _*,Ideal),
(symbol ^,InexactFieldFamily,ZZ),
(symbol _,InexactFieldFamily,ZZ),
(symbol ^,InexactNumber,Constant),
(symbol ==,InexactNumber,Constant),
(symbol -,InexactNumber,Constant),
(symbol /,InexactNumber,Constant),
(symbol *,InexactNumber,Constant),
(symbol +,InexactNumber,Constant),
(symbol ==,Matrix,Number),
(symbol *,Matrix,Number),
(symbol *,Matrix,ZZ),
(symbol ^,Number,Constant),
(symbol -,Number,Constant),
(symbol /,Number,Constant),
(symbol *,Number,Constant),
(symbol +,Number,Constant),
(symbol _,Number,InexactFieldFamily),
(symbol ==,Number,Matrix),
(symbol *,Number,Matrix),
(symbol **,Number,RingElement),
(symbol -,QQ),
(symbol +,QQ),
(symbol ==,QQ,CC),
(symbol -,QQ,CC),
(symbol /,QQ,CC),
(symbol *,QQ,CC),
(symbol +,QQ,CC),
(symbol _,QQ,ComplexField),
(symbol ==,QQ,QQ),
(symbol -,QQ,QQ),
(symbol /,QQ,QQ),
(symbol *,QQ,QQ),
(symbol +,QQ,QQ),
(symbol _,QQ,RealField),
(symbol ==,QQ,RR),
(symbol -,QQ,RR),
(symbol /,QQ,RR),
(symbol *,QQ,RR),
(symbol +,QQ,RR),
(symbol -,QQ,ZZ),
(symbol /,QQ,ZZ),
(symbol *,QQ,ZZ),
(symbol +,QQ,ZZ),
(symbol ==,RingElement,ZZ),
(symbol _*,RingFamily),
(symbol -,RR),
(symbol +,RR),
(symbol ==,RR,CC),
(symbol -,RR,CC),
(symbol /,RR,CC),
(symbol *,RR,CC),
(symbol +,RR,CC),
(symbol _,RR,ComplexField),
(symbol ==,RR,QQ),
(symbol -,RR,QQ),
(symbol /,RR,QQ),
(symbol *,RR,QQ),
(symbol +,RR,QQ),
(symbol _,RR,RealField),
(symbol ==,RR,RR),
(symbol -,RR,RR),
(symbol /,RR,RR),
(symbol *,RR,RR),
(symbol +,RR,RR),
(symbol ==,RR,ZZ),
(symbol -,RR,ZZ),
(symbol /,RR,ZZ),
(symbol *,RR,ZZ),
(symbol +,RR,ZZ),
(symbol ==,Sequence,Sequence),
(symbol SPACE,InexactFieldFamily,Array),
(symbol ==,String,String),
((symbol _*,symbol =),RingFamily),
(symbol ==,Symbol,Symbol),
(symbol **,Thing,InexactFieldFamily),
(symbol -,ZZ),
(symbol +,ZZ),
(symbol ==,ZZ,CC),
(symbol -,ZZ,CC),
(symbol /,ZZ,CC),
(symbol *,ZZ,CC),
(symbol +,ZZ,CC),
(symbol _,ZZ,ComplexField),
(symbol -,ZZ,QQ),
(symbol /,ZZ,QQ),
(symbol *,ZZ,QQ),
(symbol +,ZZ,QQ),
(symbol _,ZZ,RealField),
(symbol ==,ZZ,RingElement),
(symbol ==,ZZ,RR),
(symbol -,ZZ,RR),
(symbol /,ZZ,RR),
(symbol *,ZZ,RR),
(symbol +,ZZ,RR),
(symbol ==,ZZ,ZZ),
(symbol -,ZZ,ZZ),
(symbol /,ZZ,ZZ),
(symbol //,ZZ,ZZ),
(symbol *,ZZ,ZZ),
(symbol +,ZZ,ZZ),
[syz, MaxReductionCount],
[tangentSheaf, Minimize],
(toCC,Constant),
(toCC,Number),
(toCC,ZZ,Constant),
(toCC,ZZ,Number),
(toCC,ZZ,Number,Number),
(toExternalString,Constant),
(toExternalString,RR),
(toRR,Constant),
(toRR,InfiniteNumber),
(toRR,QQ),
(toRR,RR),
(toRR,ZZ),
(toRR,ZZ,Constant),
(toRR,ZZ,InfiniteNumber),
(toRR,ZZ,QQ),
(toRR,ZZ,RR),
(toRR,ZZ,ZZ),
(toString,ComplexField),
(toString,Constant),
(toString,Expression),
(toString,IndexedVariableTable),
(toString,RealField),

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
