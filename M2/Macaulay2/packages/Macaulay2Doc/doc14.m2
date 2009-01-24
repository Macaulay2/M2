--		Copyright 2008 by Daniel R. Grayson

document { Key => {size2, (size2,CC), (size2,RR), (size2,ZZ)},
     Usage => "size2 x",
     Headline => "number of binary digits to the left of the point",
     Inputs => {"x" => Number},
     Outputs => {{"the smallest number ", TT "n", " such that ", TT "2^n", " is strictly greater than the absolute value of ", TT "x"}},
     "This function is not implemented for rational numbers.",
     EXAMPLE lines ///
     size2 4
     size2 3
     size2 4.
     size2 3.99999999
     size2 0
     size2 0.
     size2 (1/0.)
     size2 (1/0.-1/0.)
     ///}

document { Key => {isReal,(isReal,CC),(isReal,QQ),(isReal,RR),(isReal,ZZ)},
     Usage => "isReal x",
     Headline => "whether a number is real",
     Inputs => { "x" => Number },
     Outputs => { Boolean => {"whether ", TT "x", " is real"} },
     EXAMPLE lines ///
     isReal 2.
     isReal sqrt(-1)
     ///
     }


document { Key => isFinite,
     Usage => "isFinite x",
     Headline => "whether a number is finite",
     Inputs => { "x" },
     Outputs => { Boolean => {"whether ", TT "x", " is finite, i.e., is not an infinite real number, nor
	       ", EM "not a number", "."} },
     EXAMPLE lines ///
     sqrt 2.
     isFinite oo
     i = 1/0.
     isFinite oo
     i-i
     isFinite oo
     ///
     }

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
document { 
     --- author(s): L. Gold, Dan Grayson
     Key => {acos,(acos,ZZ), (acos,RR),(acos,CC),(acos, QQ)},
     Headline => "arccosine", 
     Usage => "acos x",
     Inputs => { "x" },
     Outputs => { Number => { "the arccosine (in radians) of ", TT "x"} },
     EXAMPLE lines ///
     acos 0.5
     ///,
     Wikipedia "Trigonometric_function"
     }     
document { 
     --- author(s): L. Gold, Dan Grayson
     Key => {asin,(asin,ZZ),(asin,RR),(asin,CC),(asin, QQ)},
     Headline => "arcsine",
     Usage => "asin x",
     Inputs => { "x" },
     Outputs => {
	  Number => {"the arcsine (in radians) of ", TT "x"}
	  },
     EXAMPLE {
	  "asin 1"
	  },
     Wikipedia "Trigonometric_function"
     }
document { 
     --- author(s): L. Gold
     Key => {cosh, (cosh,ZZ),(cosh,RR),(cosh, QQ),(cosh,CC)},
     Headline => "compute the hyperbolic cosine",
     Usage => "cosh x",
     Inputs => { "x" },
     Outputs => { Number => { "the hyperbolic cosine of ", TT "x" } },
     EXAMPLE lines ///
     cosh .2
     ///,
     }
document { 
     Key => {acosh,(acosh,Number)},
     Headline => "inverse hyperbolic cosine", 
     Usage => "acosh x",
     Inputs => { "x" },
     Outputs => { Number => { "the inverse hyperbolic cosine of ", TT "x"} },
     EXAMPLE lines ///
     acosh .2
     cosh oo
     ///,
     Wikipedia "Hyperbolic_function"
     }     
document { 
     Key => {asinh,(asinh,Number)},
     Headline => "inverse hyperbolic sine",
     Usage => "asinh x",
     Inputs => { "x" },
     Outputs => { Number => {"the inverse hyperbolic sine of ", TT "x"} },
     EXAMPLE lines ///
     asinh .2
     ///,
     Wikipedia "Hyperbolic_function"
     }
document { Key => {sec,(sec, ZZ),(sec,CC),(sec, RR),(sec, QQ)},
     Usage => "sec x",
     Headline => "secant",
     Inputs => { "x" },
     Outputs => { RR => { "the secant of ", TT "x" }},
     EXAMPLE lines ///
     	  sec(pi/3)
     ///,
     Wikipedia "Trigonometric_function"
     }
document { Key => {csc,(csc,CC),(csc, QQ),(csc, ZZ),(csc, RR)},
     Usage => "csc x",
     Headline => "cosecant",
     Inputs => { "x" },
     Outputs => { RR => { "the cosecant of ", TT "x" }},
     EXAMPLE lines ///
     	  csc(pi/3)
     ///,
     Wikipedia "Trigonometric_function"

     }
document { Key => {cot,(cot, ZZ),(cot, RR),(cot,CC),(cot, QQ)},
     Usage => "cot x",
     Headline => "cotangent",
     Inputs => { "x" },
     Outputs => { RR => { "the cotangent of ", TT "x" }},
     EXAMPLE lines ///
     	  cot(pi/3)
     ///,
     Wikipedia "Trigonometric_function"

     }
document { Key => {sech,(sech,CC),(sech, QQ),(sech, ZZ),(sech, RR)},
     Usage => "sech x",
     Headline => "hyperbolic secant",
     Inputs => { "x" },
     Outputs => { RR => { "the hyperbolic secant of ", TT "x" }},
     EXAMPLE lines ///
     	  sech(pi/3)
     ///,
     Wikipedia "Hyperbolic_function"
     }
document { Key => {csch,(csch,CC),(csch, ZZ),(csch, RR),(csch, QQ)},
     Usage => "csch x",
     Headline => "hyperbolic cosecant",
     Inputs => { "x" },
     Outputs => { RR => { "the hyperbolic cosecant of ", TT "x" }},
     EXAMPLE lines ///
     	  csch(pi/3)
     ///,
     Wikipedia "Hyperbolic_function"
     }
document { Key => {coth,(coth,CC),(coth, QQ),(coth, ZZ),(coth, RR)},
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
     	  +pi
     	  numeric_100 pi
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
document { Key => {agm,(agm, ZZ, ZZ),(agm, QQ, ZZ),(agm, ZZ, QQ),(agm, QQ, QQ),(agm, ZZ, RR),(agm, RR, ZZ),(agm, QQ, RR),(agm, RR, QQ),(agm, RR, RR),
	  (agm,CC,CC),(agm,CC,QQ),(agm,CC,RR),(agm,CC,ZZ),(agm,QQ,CC),(agm,RR,CC),(agm,ZZ,CC)},
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
     Key => {fillMatrix,(fillMatrix, MutableMatrix),(fillMatrix, MutableMatrix, ZZ),
	  [fillMatrix, Height],[fillMatrix,Density],[fillMatrix,UpperTriangular]},
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
     Consequences => {{ "some entries of M are replaced with randomly generated numbers, whose
	       size depends on the value of the global variable ", TT "randomHeight" }},
     EXAMPLE lines ///
	  printingPrecision = 2
	  fillMatrix(mutableMatrix(RR,5,10))
	  fillMatrix(mutableMatrix(ZZ,5,10),UpperTriangular=>true)
	  fillMatrix(mutableMatrix(QQ,5,10),Density=>.2,Height=>1000)
	  fillMatrix(mutableMatrix(ZZ,5,10),25,Height=>1000)
	  ///
     }

document { 
     Key => {norm,
	  (norm, InfiniteNumber, Matrix),(norm, Matrix),(norm, RR, Matrix),(norm, InfiniteNumber, RingElement),(norm, MutableMatrix),
	  (norm, RingElement),(norm, InexactField, MutableMatrix),(norm, RR, MutableMatrix),(norm, RR, RingElement),
	  (norm,List),(norm,Vector),(norm,Number),(norm,RR,Number),(norm,InfiniteNumber,Number)
	  },
     Usage => "norm M\nnorm(p,M)",
     Inputs => {
	  "M"=>{ofClass{MutableMatrix,Matrix,RingElement,Number,Vector,List}},
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
	  norm {3/2,4,-5}
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

document { Key => isANumber,
     Usage => "isANumber x",
     Headline => "whether a number is a  number",
     Inputs => { "x" },
     Outputs => { Boolean => {"whether ", TT "x", " is a number, i.e., is not ", EM "not a number", "."} },
     EXAMPLE lines ///
     isANumber 3.
     inf = 1/0.
     isANumber inf
     nan = inf - inf
     isANumber nan
     ///
     }

document { Key => isInfinite,
     Usage => "isInfinite x",
     Headline => "whether a number is infinite",
     Inputs => { "x" },
     Outputs => { Boolean => {"whether ", TT "x", " is an infinite number"} },
     EXAMPLE lines ///
     isInfinite 3.
     inf = 1/0.
     isInfinite inf
     nan = inf - inf
     isInfinite nan
     ///
     }

document {
     Key => associatedPrimes,
     Headline => "find the associated primes of an ideal",
     SeeAlso => { "PrimaryDecomposition :: PrimaryDecomposition"}     
     }

document {
     Key => (symbol _*,RingFamily),
     Usage => "R_*",
     Inputs => {"R"},
     Outputs => {{"the common parent for the rings in the family ", TT "R"}},
     EXAMPLE lines ///
     4.
     ring 4.
     parent ring 4.
     RR_*
     ///
     }

document {
     Key => {
	  (map,Module,Module,RingMap,Matrix),
	  (map,Module,Module,RingMap,List),
	  (map,Module,Nothing,RingMap,Matrix),
	  (map,Module,Nothing,RingMap,List),
	  (map,Module,RingMap)},
     Headline => "homomorphism of modules over different rings",
     Usage => "g = map(M,N,p,f)\ng = map(M,,p,f)\ng = map(M,p)",
     Inputs => { "M", 
	  "N" => {"or ", TO "null", ""},
	  "p" => {"from the ring of ", TT "N", " to the ring of ", TT "M"}, 
	  "f" => {"to the ring of ", TT "M", ", from the cover of ", TT "N", " tensored with the ring of ", TT "M", " along ", TT "p", ".
	       Alternatively, ", TT "f", " can be represented by its doubly nested list of entries."},
	  Degree => List => {
	       "a list of integers of length equal to the degree length of the ring of ", TT "M", ", providing the degree of ", TT "g", ".
	       By default, the degree of ", TT "g", " is zero."
	       }
	  },
     Outputs => {
	  "g" => Matrix => {"the homomorphism to M from N defined by f"}
	  },
     EXAMPLE lines ///
     R = QQ[x,y]
     p = map(R,QQ)
     f = matrix {{x-y, x+2*y, 3*x-y}};
     kernel f
     g = map(R^1,QQ^3,p,f)
     g === map(R^1,QQ^3,p,{{x-y, x+2*y, 3*x-y}})
     isHomogeneous g
     kernel g
     coimage g
     rank oo
     ///,
     PARA { "If the module ", TT "N", " is replaced by ", TO "null", ",
	  which is entered automatically between consecutive commas,
	  then a free module will be used for ", TT "N", ",
	  whose degrees are obtained by lifting
	  the degrees of the cover of the source of ", TT "g", ", minus the degree of ", TT "g", ", along the degree map of ", TT "p" },
     EXAMPLE lines ///
     g2 = map(R^1,,p,f,Degree => {1})
     g === g2
     ///,
     PARA {
	  "If N and f are both omitted, along with their commas, then for ", TT "f", " the matrix of generators of M is used."
	  },
     EXAMPLE lines ///
     M' = image f
     g3 = map(M',p,Degree => {1})
     isHomogeneous g3
     kernel g3
     oo == kernel g
     ///,
     PARA { "The degree of the homomorphism enters into the determination of its homogeneity." },
     EXAMPLE lines ///
     R = QQ[x, Degrees => {{2:1}}];
     M = R^1
     S = QQ[z];
     N = S^1
     p = map(R,S,{x},DegreeMap => x -> join(x,x))
     isHomogeneous p
     f = matrix {{x^3}}
     g = map(M,N,p,f,Degree => {3,3})
     isHomogeneous g
     kernel g
     coimage g
     ///,
     SeeAlso => { (map,Ring,Ring,List), isHomogeneous, (kernel,Matrix), (coimage,Matrix) }
     }

document {
     Key => {(quotientRemainder,RingElement,RingElement),
	  (quotientRemainder,Number,RingElement), (quotientRemainder,RingElement,Number)},
     Headline => "quotient and remainder",
     Usage => "(q,r) = quotientRemainder(f,g)",
     Inputs => {"f","g"},
     Outputs => {
	  "q" => RingElement => {"the quotient for the division of ", TT "f", " by ", TT "g"},
	  "r" => RingElement => {"the remainder for the division of ", TT "f", " by ", TT "g"}
	  },
     EXAMPLE lines ///
     R = QQ[x,y];
     (q,r) = quotientRemainder(x^10+5,x-2);
     q
     r
     q*(x-2)+r
     ///
     }

document {
     Key => (max,GradedModule),
     Usage => "max C",
     Inputs => { "C" },
     Outputs => {
	  ZZ => {"the maximum index of a component, possibly zero, of the graded module ", TT "C" }
	  },
     EXAMPLE lines ///
     R = QQ[a..e]
     C = res coker vars R
     max C
     dual C
     max dual C
     ///,
     SeeAlso => {(min,GradedModule)}
     }

document {
     Key => (min,GradedModule),
     Usage => "max C",
     Inputs => { "C" },
     Outputs => {
	  ZZ => {"the minimum index of a component, possibly zero, of the graded module ", TT "C" }
	  },
     EXAMPLE lines ///
     R = QQ[a..e]
     C = res coker vars R
     min C
     dual C
     min dual C
     ///,
     SeeAlso => {(max,GradedModule)}
     }

document { Key => {(symbol **,AffineVariety,Ring)},
     Usage => "X ** R",
     Inputs => {"X","R"},
     Outputs => {{"the tensor product of ", TT "X", " with ", TT "R"}},
     EXAMPLE lines ///
     X = Spec(QQ[x,y])
     Y = X ** (QQ[t])
     describe Y
     ///
     }

document { Key => (symbol **,GradedModule,GradedModule),
     Usage => "C ** D",
     Inputs => {"C","D"},
     Outputs => {{"the tensor product of ", TT "C", " with ", TT "D"}},
     EXAMPLE lines ///
     C = gradedModule(ZZ^1,ZZ^6,ZZ^2)
     C ** C
     betti oo
     ///
     }

document { Key => {(symbol **,GradedModule,Module),(symbol **,Module,GradedModule)},
     Usage => "C ** M",
     Inputs => {"C","M"},
     Outputs => {{"the tensor product of ", TT "C", " with ", TT "M"}},
     EXAMPLE lines ///
     C = gradedModule(ZZ^1,ZZ^6,ZZ^2)
     C ** ZZ^3
     betti oo
     ///,
     PARA {"It also works the other way around."},
     EXAMPLE lines ///
     ZZ^3 ** C
     ///
     }

document { Key => (symbol **,Matrix,RingElement),
     Usage => "f ** r",
     Inputs => {"f","r"},
     Outputs => {{"the tensor product of ", TT "f", " with ", TT "r"}},
     EXAMPLE lines ///
     f = matrix "2,3,4;5,6,7"
     f ** 10
     ///,
     PARA { "When the ring element is homogeneous, the degrees of the source module can change, which is
	  what makes this operation different from scalar multiplication." },
     EXAMPLE lines ///
     QQ[x,y]
     f = matrix "x,y"
     g = f ** y^7
     h = f * y^7
     degrees g
     degrees h
     ///
     }

document { Key => (symbol |,GradedModuleMap,GradedModuleMap),
     Usage => "f|g",
     Inputs => {"f","g"},
     Outputs => {{"the map of graded modules whose component in degree ", TT "i", " is ", TT "f_i|g_i", " see ", TO (symbol |, Matrix, Matrix)}},
     EXAMPLE lines ///
     f = gradedModuleMap( matrix "1;2", matrix "2,3" )
     f|f
     ///
     }

document { Key => (symbol ||,GradedModuleMap,GradedModuleMap),
     Usage => "f||g",
     Inputs => {"f","g"},
     Outputs => {{"the map of graded modules whose component in degree ", TT "i", " is ", TT "f_i||g_i", " see ", TO (symbol ||, Matrix, Matrix)}},
     EXAMPLE lines ///
     f = gradedModuleMap( matrix "1;2", matrix "2,3" )
     f||f
     ///
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
