R = QQ[x]
x-x ? x-x
m = matrix{{x^3, x^2-1, 0_R, x, 0_R, 1_R, 2_R}}
L1 = sort first entries m
m1 = sort m
assert(L1#5 == 0 and L1#6 == 0)
assert (m1_(0,5) == 0)
assert (m1_(0,6) == 0)
