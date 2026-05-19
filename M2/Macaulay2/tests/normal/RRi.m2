-- tests basic methods (e.g., arithmetic operations) for RRi implementation

FF := RRi_1000
assert(precision FF===1000)
I := interval(1,3)
assert not (I===interval(3,1))
assert isEmpty interval(3,1)
assert (I===span(3,1))
assert (I===interval[1,3])
assert (precision ring I === 53)
assert (precision interval(1,3_(RR_1000))==1000)
assert (precision interval(Precision=>1000,1,3) == 1000)
assert (precision interval(1_(RR_1000),2_(RR_2000)) == 1000)
assert (precision (interval(Precision=>100,1,3)+interval(Precision=>200,2,4)) == 100)
assert (precision toRRi(300,2,2) == 300)
assert (precision toRRi(30) == 53)
assert (precision toRRi(10_(RR_100)) == 100)
assert (toRRi(4) == interval(4))
assert(toString FF === "RRi_1000")
assert(expression FF === new Subscript from {RRi,1000})
assert(left I === 1_(RR_53))
assert(right I === 3_(RR_53))
assert(midpoint I === 2_(RR_53))
J := toRRi 2
assert not(I<J or I>J or I==J or I>=J or I<=J)
assert isSubset(J,I)
assert (J <= J)
assert not(I <= I)
assert (I==I)
assert (J==J)
assert (I===I)
assert (I==interval(1,3))
assert (0<I)
assert (I<=interval(3,5))
assert (J<interval(3,5))
assert(left sin J <= sin 2.0)
assert(right sin J >= sin 2.0)
assert(left sin J != right sin J)
assert(I+J===J+I)
assert(I*J===J*I)
K := interval(-2,3)
assert(not (K*K===K^2))
assert(diameter sqrt I < 1)
assert(right sqrt I > sqrt 3) -- this has to do with different rounding directions (mpfi vs mpfr)
assert(right log exp interval 1 > 1)
assert(left log exp interval 1 < 1)
assert (abs(K)===interval(0,3))
assert (midpoint(I) == J)
assert (diameter(I) == 2)
assert isMember(numeric_100 pi,numericInterval_100 pi)
L := interval(-2/3,1.5_(RR_53))
M := interval(-4,2.7)
assert (L+M===interval(-14/3,4.2))
assert isSubset(interval(-10.1/3,5.5),L-M)
assert isSubset(interval(-6,4.05),L*M)
assert isSubset(K^2,K*K)
assert not isSubset(K*K,K^2)
assert (L/I===L)
assert (L^2===interval(0,2.25))
assert isSubset(interval(-8/27,3.375),L^3)
assert isSubset(sin(L),interval(-1,1))
assert isFinite(I)
assert not isFinite(I/K)
assert isANumber(I)
assert isANumber(I/K)
assert not isInfinite(I)
assert isInfinite(I/K)
assert isMember(numeric_100 pi,acos cos(numericInterval_100 pi))
assert (left atan I == atan 1)
assert (right atan I == atan 3)
assert (left sinh I == sinh 1)
assert (acosh(I) >= 0)

assert (interval(1+2*ii,2+3*ii)*interval (1+3*ii,2+4*ii) == interval(-11 + 5*ii , -2+14*ii))

FF := CCi_1000
assert(precision FF===1000)
I := interval(1+2*ii,3+4*ii)
assert not (I===interval(3+2*ii,1+4*ii))
assert isEmpty interval(3+2*ii,1+4*ii)
assert (I===span(3+2*ii,1+4*ii))
assert (I===interval[1+2*ii,3+4*ii])
assert (precision ring I === 53)
assert (precision interval(1_(RR_100)+2_(RR_300)*ii,3_(RR_1000)+4_(RR_200)*ii)==100)
assert (precision (interval(Precision=>100,1+2*ii,3+4*ii)+interval(Precision=>200,5+6*ii,7+8*ii)) == 100)
assert (precision toCCi(100,1,3) == 100)
assert (precision toCCi(2,3) == 53)
assert (precision toCCi(2_(RR_100),3)==100)
assert (toCCi(interval(1,3),interval(2,4)) == interval(1+2*ii,3+4*ii))
assert(toString FF === "CCi_1000")
assert(expression FF === new Subscript from {CCi,1000})
assert(realPart I === interval(1,3))
assert(imaginaryPart I === interval(2,4))
assert(midpoint I === 2+3*ii)
J := toCCi(1+4*ii)
assert(isSubset(J,I))
assert(J==J)
assert(I===I)
assert(I==interval(1+2*ii,3+4*ii))
assert(I+J===J+I)
assert(I*J===J*I)
K := interval(-1-2*ii,3+4*ii)
assert(not (K*K === K^2))
assert(diameter I < 3)
assert(isSubset(interval(1,5),abs I))
assert(J == 1+4*ii)
assert(isFinite I)
assert(not isFinite (I/K))
assert(not isInfinite I)
assert(isInfinite (I/K))
assert isANumber(I)
assert isANumber(I/K)
assert isMember(sqrt(midpoint I),sqrt I)
L := interval(-2/3+1.5_(RR_53)*ii,4+2.7*ii)
M := toCCi(interval((-2/3)_(RR_53)+1,7),interval(3.5,6.7))
assert(I+L == M)
assert isSubset(toCCi(interval(-23/3,10/3),interval(-5,-.8)),L-M)
assert isSubset(K^2,K*K)
assert not isSubset(K*K,K^2)
assert isSubset(toCCi(interval(-14,17),interval(-10,24)),L*K)

A = matrix{{interval(1,3), interval(3,5)}, {interval(5,7), interval(7,9)}}
assert(midpoint A == matrix(RR, {{2, 4}, {6, 8}}))

R = RRi[x,y,z]
f = interval(1,3)*x + interval(3,5)*y + interval(5,7)*z
use ring midpoint f
assert(midpoint f == 2*x + 4*y + 6*z)

R = RRi[x]/x^2
assert(promote(toRRi 2, R) == 2)

R = CCi[x]/x^2
assert(promote(toRRi 2, R) == 2)
assert(promote(toCCi 2, R) == 2)

f = map(CCi_53, RRi_53)
assert Equation(f 2, toCCi 2)

--- TO FIX: ---------------------------------------------

-*
sqrt K -- produce a different error?
*-

-*
Ok to divide by 0, gives infinity.
Ok to take log of 0 (-infinity), just not negative values.
Can't take arc-trig of inappropriate intervals.
Some of these functions (like contains) should be isSubset
*-

-*
debug Core
errorDepth = 0 
FF[x]
-- start with... rawToRRi in d/interface.dd 
*-



