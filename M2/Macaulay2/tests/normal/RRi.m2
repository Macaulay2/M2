-- tests basic methods (e.g., arithmetic operations) for RRi implementation

restart
FF := RRi_1000
assert(precision FF===1000)
I = interval(1,3)
assert sameinterval(I,interval(3,1))
assert (precision ring I === 53)
assert(toString FF === "RRi_1000")
assert(expression FF === new Subscript from {symbol RRi,1000})
assert(leftRR I === 1_(RR_53))
assert(rightRR I === 3_(RR_53))
assert(midpointRR I === 2_(RR_53))
J = toRRi 2
assert not(I<J or I>J or I==J or I>=J or I<=J)
assert contains(I,J)
assert (J <= J)
assert not(I <= I)
assert(leftRR sin J <= sin 2.0)
assert(rightRR sin J >= sin 2.0)
assert(leftRR sin J != rightRR sin J)
assert(sameinterval(I+J,J+I))
assert(sameinterval(I*J,J*I))
K = interval(-2,3)
assert(not sameinterval(K*K,K^2))
assert(widthRR sqrt I < 1)
assert(rightRR sqrt I > sqrt 3) -- this has to do with different rounding (mpfi vs mpfr)
assert(rightRR log exp toRRi 1 > 1)
assert(leftRR log exp toRRi 1 < 1)
assert sameinterval(abs(K),interval(0,3))
assert (midpointRR(I) == J)
assert (widthRR(I) == 2)
assert contains(numeric_100 piRRi,numeric_100 pi)
L = interval(-2/3,1.5_(RR_53))
M = interval(-4,2.7)
assert sameinterval(L+M,interval(-14/3,4.2))
assert contains(L-M,interval(-10.1/3,5.5))
assert contains(L*M,interval(-6,4.05))
assert sameinterval(L/I,L)
assert sameinterval(L^2,interval(0,2.25))
assert contains(L^3,interval(-8/27,3.375))
assert contains(interval(-1,1),sin(L))

--- TO FIX: ---------------------------------------------

-*
sqrt K -- produce a different error?
*-

-*
debug Core
errorDepth = 0 
FF[x]
-- start with... rawToRRi in d/interface.dd 
*-
