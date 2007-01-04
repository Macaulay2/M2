-- 3 by 3 nilpotent matrices
R = ZZ/101[vars(0..8)]
M = genericMatrix(R,a,3,3)
I = ideal (M^3)
time J = radical(I, CompleteIntersection=>I1)
assert(J == ideal(trace(M), trace(M^2), trace(M^3)))

time decompose I; -- not good

debug PrimaryDecomposition
independentSets(I, Limit=>1)
flatt(I,oo_0)
intersect values oo
saturate(I,oo)

top I -- not good

-- Local Variables:
-- M2-send-to-buffer: "*gud*"
-- End:
