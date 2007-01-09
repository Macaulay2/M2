A = ZZ[a]/(a^2-3)
B = A[x,y,z]/(a*x^2-y^2-z^2, y^3, z^3)
(D,F,G) = flattenRing B
describe D      
K = frac(ZZ[a])
B = K[x,y,z]/(a*x^2-y^2-z^2, y^3, z^3)
(D,F,G) = flattenRing B
describe D      
L = toField A
B = L[x,y,z]/(a*x^2-y^2-z^2, y^3, z^3)
(D,F,G) = flattenRing(B[s,t])
describe D      
use L
C1 = L[s,t];
C2 = C1/(a*s-t^2);
C3 = C2[p_0..p_4]/(a*s*p_0)[q]/(q^2-a*p_1);
(D,F,G) = flattenRing(C3, CoefficientRing=>C2)
describe D
(D,F,G) = flattenRing(C3, CoefficientRing=>ZZ)
describe D
