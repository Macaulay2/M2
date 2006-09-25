R = QQ[x]
x-x ? x-x
m = matrix{{x^3, x^2-1, 0_R, x, 0_R, 1_R, 2_R}}
L1 = sort first entries m
m1 = sort m
assert( L1 === first entries m1 )			    -- the two sorting routines should be compatible

R = ZZ[x,y,z]
L = {12*x, 3*x-4, 3*x-5, x^2+1, -x^3+3}
sort L
L = {12_R, 0_R, 14_R, -3_R}
sort L

Q = frac R
L = {1/x, 1/y, 14*x^3/y^2, x^2/y, x^3/y, -14_Q}
assert(sort L === {-14_Q, 1/(y), 1/(x), (x^2)/(y), (x^3)/(y), (14*x^3)/(y^2)})
