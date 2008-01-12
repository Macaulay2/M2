--		Copyright 2008 by Daniel R. Grayson

document { Key => {log1p,(log1p, QQ),(log1p, ZZ),(log1p, RR)},
     Usage => "log1p x",
     Inputs => { "x" },
     Outputs => {{ "the logarithm of ", TT "1+x" }},
     EXAMPLE lines ///
log1p 1p100e-10
log(1 + 1p100e-10)
     ///
     }
document { Key => {expm1,(expm1, ZZ),(expm1, RR),(expm1, QQ)},
     Usage => "expm1 x",
     Inputs => { "x" },
     Outputs => {{ "the quantity ", TT "exp(x)-1" }},
     EXAMPLE lines ///
     	  expm1 1p100e-10
     	  exp(1p100e-10)-1
     ///
     }
document { Key => {eint,(eint, QQ),(eint, ZZ),(eint, RR)},
     Usage => "eint x",
     Inputs => { "x" },
     Outputs => {{ "the exponential integral of ", TT "x" }},
     EXAMPLE lines ///
     	  eint 2
     ///
     }
document { Key => {Gamma,(Gamma, ZZ),(Gamma, RR),(Gamma, QQ)},
     Usage => "Gamma x",
     Inputs => { "x" },
     Outputs => {{ "the gamma function of ", TT "x" }},
     EXAMPLE lines ///
     	  Gamma 6
     ///
     }
document { Key => {lngamma,(lngamma, QQ),(lngamma, ZZ),(lngamma, RR)},
     Usage => "lngamma x",
     Inputs => { "x" },
     Outputs => {{ "the logarithm of the gamma function of ", TT "x" }},
     EXAMPLE lines ///
     	  lngamma 2.1
	  lngamma(-1.1)
	  lngamma(-.1)
     ///,
     SeeAlso => { "lgamma" }
     }
document { Key => {lgamma,(lgamma, ZZ),(lgamma, RR),(lgamma, QQ)},
     Usage => "(y,s) = lgamma x",
     Inputs => { "x" },
     Outputs => {
	  "y" => { "the logarithm of the absolute value of the gamma function of ", TT "x" },
	  "s" => { "the sign of the gamma function of ", TT "x" }
	  },
     EXAMPLE lines ///
     	  lgamma 2.1
	  lgamma(-1.1)
	  lgamma(-.1)
     ///,
     SeeAlso => { "lngamma" }
     }
document { Key => {zeta,(zeta, QQ),(zeta, ZZ),(zeta, RR)},
     Usage => "zeta x",
     Inputs => { "x" },
     Outputs => {{ "the zeta function of ", TT "x" }},
     EXAMPLE lines ///
     	  zeta 2
     ///
     }
document { Key => {erf,(erf, ZZ),(erf, RR),(erf, QQ)},
     Usage => "erf x",
     Inputs => { "x" },
     Outputs => {{ "the error function of ", TT "x" }},
     EXAMPLE lines ///
     	  erf 2
     ///
     }
document { Key => {erfc,(erfc, QQ),(erfc, ZZ),(erfc, RR)},
     Usage => "erfc x",
     Inputs => { "x" },
     Outputs => {{ "the complementary error function of ", TT "x" }},
     EXAMPLE lines ///
     	  erfc 2
     ///
     }
document { Key => {sec,(sec, ZZ),(sec, RR),(sec, QQ)},
     Usage => "sec x",
     Inputs => { "x" },
     Outputs => {{ "the secant of ", TT "x" }},
     EXAMPLE lines ///
     	  sec(pi/3)
     ///
     }
document { Key => {csc,(csc, QQ),(csc, ZZ),(csc, RR)},
     Usage => "csc x",
     Inputs => { "x" },
     Outputs => {{ "the cosecant of ", TT "x" }},
     EXAMPLE lines ///
     	  csc(pi/3)
     ///
     }
document { Key => {cot,(cot, ZZ),(cot, RR),(cot, QQ)},
     Usage => "cot x",
     Inputs => { "x" },
     Outputs => {{ "the cotangent of ", TT "x" }},
     EXAMPLE lines ///
     	  cot(pi/3)
     ///
     }
document { Key => {sech,(sech, QQ),(sech, ZZ),(sech, RR)},
     Usage => "sech x",
     Inputs => { "x" },
     Outputs => {{ "the hyperbolic secant of ", TT "x" }},
     EXAMPLE lines ///
     	  sech(pi/3)
     ///
     }
document { Key => {csch,(csch, ZZ),(csch, RR),(csch, QQ)},
     Usage => "csch x",
     Inputs => { "x" },
     Outputs => {{ "the hyperbolic cosecant of ", TT "x" }},
     EXAMPLE lines ///
     	  csch(pi/3)
     ///
     }
document { Key => {coth,(coth, QQ),(coth, ZZ),(coth, RR)},
     Usage => "coth x",
     Inputs => { "x" },
     Outputs => {{ "the hyperbolic cotangent of ", TT "x" }},
     EXAMPLE lines ///
     	  coth(pi/3)
     ///
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
     Inputs => { },
     Outputs => {{ "the Euler constant, converted to a numeric value of the correct precision, when necessary." }},
     EXAMPLE lines ///
     	  EulerConstant
     	  +EulerConstant
     	  EulerConstant+100p100
     ///
     }
document { Key => {InexactNumber'},
     "This class is the common parent of the classes of complex fields and real fields."
     }
document { Key => {RingFamily},
     "This family is used to contain classes that correspond to a family of similar rings with a default member."
     }
end
document { Key => {BesselJ,(BesselJ, ZZ, QQ),(BesselJ, ZZ, ZZ),(BesselJ, ZZ, RR)},
     EXAMPLE lines ///
     ///
     }
document { Key => {BesselY,(BesselY, ZZ, ZZ),(BesselY, ZZ, RR),(BesselY, ZZ, QQ)},
     EXAMPLE lines ///
     ///
     }
document { Key => {agm,(agm, ZZ, ZZ),(agm, QQ, ZZ),(agm, ZZ, QQ),(agm, QQ, QQ),(agm, ZZ, RR),(agm, RR, ZZ),(agm, QQ, RR),(agm, RR, QQ),(agm, RR, RR)},
     EXAMPLE lines ///
     ///
     }

(char, InexactField)
clean,(clean, RR, MutableMatrix),(clean, RR, RingElement),(clean, RR, Matrix)
default,(default, Type),(default, InexactFieldFamily)
(degreeLength, InexactField)
(dim, InexactField)
(expression, Constant)
(expression, RealField)
(expression, ComplexField)
fillMatrix,(fillMatrix, MutableMatrix),(fillMatrix, MutableMatrix, ZZ)
(isField, RingFamily)
(lift, Matrix, Nothing)
(lift, Matrix, InexactNumber)
(lift, Number, InexactNumber)
(liftable, Number, InexactNumber)
(log, ZZ, ZZ),(log, QQ, ZZ),(log, ZZ, QQ),(log, QQ, QQ),(log, RR, ZZ),(log, ZZ, RR),(log, QQ, RR),(log, RR, QQ),(log, RR, RR)
(map, RingFamily, Thing, Thing)
(map, Thing, RingFamily, Thing)
(mutableIdentity, RingFamily, ZZ)
(mutableMatrix, RingFamily, ZZ, ZZ)
(net, InexactField)
(net, Constant)
norm,(norm, InfiniteNumber, Matrix),(norm, Matrix),(norm, RR, Matrix),(norm, InfiniteNumber, RingElement),(norm, MutableMatrix),(norm, RingElement),(norm, InexactField, MutableMatrix),(norm, RR, MutableMatrix),(norm, RR, RingElement)
(numeric, ZZ, InfiniteNumber)
(numeric, InfiniteNumber)
(numgens, InexactField)
(precision, QuotientRing),(precision, MutableMatrix),(precision, RingElement),(precision, PolynomialRing),(precision, InexactNumber),(precision, InexactField),(precision, Matrix)
(promote, Matrix, InexactNumber),(promote, Number, InexactNumber),(promote, Matrix, Nothing)
(random, RingFamily)
(ring, RR),(ring, CC)
(round, QQ),(round, ZZ, RR),(round, RR)
(substitute, Ideal, RingFamily),(substitute, Vector, RingFamily),(substitute, Module, RingFamily),(substitute, RingElement, RingFamily),(substitute, Matrix, RingFamily),(substitute, Number, RingFamily)
(toCC, Constant),(toCC, ZZ, Constant)
(toExternalString, RR),(toExternalString, Constant)
(toRR, ZZ, Constant),(toRR, Constant)
(toString, RealField),(toString, ComplexField),(toString, Constant)
defaultPrecision
randomHeight
[pushForward, MonomialOrder]
(symbol ==,String,String),(symbol ==,Sequence,Sequence)
(symbol SPACE,InexactFieldFamily,Array)
((symbol _*,symbol =),RingFamily)
(symbol ==,Boolean,Boolean),(symbol ==,CC,CC),(symbol ==,CC,QQ),(symbol ==,CC,RR),(symbol ==,CC,ZZ),(symbol ==,Constant,Constant),(symbol ==,Constant,InexactNumber),(symbol ==,InexactNumber,Constant),(symbol ==,QQ,CC),(symbol ==,QQ,QQ),(symbol ==,QQ,RR),(symbol ==,RingElement,ZZ),(symbol ==,RR,CC),(symbol ==,RR,QQ),(symbol ==,RR,RR),(symbol ==,RR,ZZ),(symbol ==,Symbol,Symbol),(symbol ==,ZZ,CC),(symbol ==,ZZ,RingElement),(symbol ==,ZZ,RR),(symbol ==,ZZ,ZZ)
(symbol _,CC,ComplexField),(symbol _,Constant,InexactFieldFamily),(symbol _,Constant,Ring),(symbol _,InexactFieldFamily,ZZ),(symbol _,Number,InexactFieldFamily),(symbol _,QQ,ComplexField),(symbol _,QQ,RealField),(symbol _,RR,ComplexField),(symbol _,RR,RealField),(symbol _,ZZ,ComplexField),(symbol _,ZZ,RealField)
(symbol +,CC),(symbol +,Constant),(symbol +,QQ),(symbol +,RR),(symbol +,ZZ),(symbol +,CC,CC),(symbol +,CC,QQ),(symbol +,CC,RR),(symbol +,CC,ZZ),(symbol +,Constant,Constant),(symbol +,Constant,InexactNumber),(symbol +,Constant,Number),(symbol +,InexactNumber,Constant),(symbol +,Number,Constant),(symbol +,QQ,CC),(symbol +,QQ,QQ),(symbol +,QQ,RR),(symbol +,QQ,ZZ),(symbol +,RR,CC),(symbol +,RR,QQ),(symbol +,RR,RR),(symbol +,RR,ZZ),(symbol +,ZZ,CC),(symbol +,ZZ,QQ),(symbol +,ZZ,RR),(symbol +,ZZ,ZZ)
(symbol -,CC),(symbol -,Constant),(symbol -,QQ),(symbol -,RR),(symbol -,ZZ),(symbol -,CC,CC),(symbol -,CC,QQ),(symbol -,CC,RR),(symbol -,CC,ZZ),(symbol -,Constant,Constant),(symbol -,Constant,InexactNumber),(symbol -,Constant,Number),(symbol -,InexactNumber,Constant),(symbol -,Number,Constant),(symbol -,QQ,CC),(symbol -,QQ,QQ),(symbol -,QQ,RR),(symbol -,QQ,ZZ),(symbol -,RR,CC),(symbol -,RR,QQ),(symbol -,RR,RR),(symbol -,RR,ZZ),(symbol -,ZZ,CC),(symbol -,ZZ,QQ),(symbol -,ZZ,RR),(symbol -,ZZ,ZZ)
(symbol *,CC,CC),(symbol *,CC,QQ),(symbol *,CC,RR),(symbol *,CC,ZZ),(symbol *,Constant,Constant),(symbol *,Constant,InexactNumber),(symbol *,Constant,Number),(symbol *,InexactNumber,Constant),(symbol *,Matrix,Number),(symbol *,Matrix,ZZ),(symbol *,Number,Constant),(symbol *,Number,Matrix),(symbol *,QQ,CC),(symbol *,QQ,QQ),(symbol *,QQ,RR),(symbol *,QQ,ZZ),(symbol *,RR,CC),(symbol *,RR,QQ),(symbol *,RR,RR),(symbol *,RR,ZZ),(symbol *,ZZ,CC),(symbol *,ZZ,QQ),(symbol *,ZZ,RR),(symbol *,ZZ,ZZ)
(symbol /,CC,CC),(symbol /,CC,QQ),(symbol /,CC,RR),(symbol /,CC,ZZ),(symbol /,Constant,Constant),(symbol /,Constant,InexactNumber),(symbol /,Constant,Number),(symbol /,InexactNumber,Constant),(symbol /,Number,Constant),(symbol /,QQ,CC),(symbol /,QQ,QQ),(symbol /,QQ,RR),(symbol /,QQ,ZZ),(symbol /,RR,CC),(symbol /,RR,QQ),(symbol /,RR,RR),(symbol /,RR,ZZ),(symbol /,ZZ,CC),(symbol /,ZZ,QQ),(symbol /,ZZ,RR)
(symbol /,ZZ,ZZ)
(symbol //,ZZ,ZZ)
(symbol ^,CC,ZZ),(symbol ^,Constant,Constant),(symbol ^,Constant,InexactNumber),(symbol ^,Constant,Number),(symbol ^,InexactFieldFamily,ZZ),(symbol ^,InexactNumber,Constant),(symbol ^,Number,Constant)
(symbol **,Number,RingElement),(symbol **,Thing,InexactFieldFamily)
(symbol _*,RingFamily)
[cotangentSheaf, Minimize]
[gb, MaxReductionCount]
[hermite, Strategy]
[lift, Verify]
[markedGB, ChangeMatrix],[markedGB, MinimalMatrix],[markedGB, SyzygyMatrix]
[mingens, Strategy]
[minimalPresentation, Strategy]
(NewOfFromMethod,ComplexField,Nothing,ZZ),(NewOfFromMethod,RealField,Nothing,ZZ)
[pushForward1, BasisElementLimit],[pushForward, BasisElementLimit],[pushForward, DegreeLimit],[pushForward, PairLimit],[pushForward, UseHilbertFunction]
[quotient, BasisElementLimit],[quotient, DegreeLimit],[quotient, PairLimit]
[saturate, BasisElementLimit],[saturate, PairLimit]
[solve, MaximalRank]
[syz, MaxReductionCount]
[tangentSheaf, Minimize]
