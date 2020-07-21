Q = QQ[x,y]
K = Q^1/(x,y)
FF = res K
(xi1,xi2) = apply((x,y), z -> HH Hom(nullhomotopy(z^2*id_FF),K))
M = ker(xi1*xi2)  -- M should be all of source xi2
assert(numgens M_0 > 0)

