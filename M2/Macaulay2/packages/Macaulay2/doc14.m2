--		Copyright 2008 by Daniel R. Grayson

log1p,(log1p, QQ),(log1p, ZZ),(log1p, RR)


i16 : log1p 1p100e-10

o16 = 9.99999999950000000003333333333e-11

i17 : log(1 + 1p100e-10)

o17 = 9.99999999950000000005934045724e-11


expm1,(expm1, ZZ),(expm1, RR),(expm1, QQ)
eint,(eint, QQ),(eint, ZZ),(eint, RR)
Gamma,(Gamma, ZZ),(Gamma, RR),(Gamma, QQ)
lngamma,(lngamma, QQ),(lngamma, ZZ),(lngamma, RR)
lgamma,(lgamma, ZZ),(lgamma, RR),(lgamma, QQ)
zeta,(zeta, QQ),(zeta, ZZ),(zeta, RR)
erf,(erf, ZZ),(erf, RR),(erf, QQ)
erfc,(erfc, QQ),(erfc, ZZ),(erfc, RR)
BesselJ,(BesselJ, ZZ, QQ),(BesselJ, ZZ, ZZ),(BesselJ, ZZ, RR)
sec,(sec, ZZ),(sec, RR),(sec, QQ)
csc,(csc, QQ),(csc, ZZ),(csc, RR)
cot,(cot, ZZ),(cot, RR),(cot, QQ)
sech,(sech, QQ),(sech, ZZ),(sech, RR)
csch,(csch, ZZ),(csch, RR),(csch, QQ)
coth,(coth, QQ),(coth, ZZ),(coth, RR)
BesselY,(BesselY, ZZ, ZZ),(BesselY, ZZ, RR),(BesselY, ZZ, QQ)
agm,(agm, ZZ, ZZ),(agm, QQ, ZZ),(agm, ZZ, QQ),(agm, QQ, QQ),(agm, ZZ, RR),(agm, RR, ZZ),(agm, QQ, RR),(agm, RR, QQ),(agm, RR, RR)
EulerConstant
InexactNumber'
RingFamily
UpperTriangular
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
(symbol !,Constant),(symbol !,RR)
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
