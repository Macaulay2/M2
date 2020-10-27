-- tests basic methods (e.g., arithmetic operations) for RRi implementation

restart
FF := RRi_1000
assert(precision FF===1000)
I = interval(1,3)
assert (precision ring I === 53)
assert(toString FF === "RRi_1000")
assert(expression FF === new Subscript from {symbol RRi,1000})
assert(leftRR I === 1_(RR_53))
assert(rightRR I === 3_(RR_53))
assert(midpointRR I === 2_(RR_53))
J = toRRi 2
assert not(I<J or I>J or I==J or I>=J or I<=J)
assert(leftRR sin J <= sin 2.0)
assert(rightRR sin J >= sin 2.0)
assert(leftRR sin J != rightRR sin J)
assert(I+J == J+I)
assert(I*J == J*I)
K = interval(-2,3)
assert(K*K != K^2)
assert(widthRR sqrt I < 1)
assert(rightRR sqrt I > sqrt 3) -- this has to do with different rounding (mpfi vs mpfr)

--- TO FIX: ---------------------------------------------

log K -- should produce an error

-*
sqrt K -- produce a different error?
*-

-*
debug Core
errorDepth = 0 
FF[x]
-- start with... rawToRRi in d/interface.dd 
*-