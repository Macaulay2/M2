-- Smoke test: the documented first example of the package.
R = QQ[x,y,z];
I = ideal(x*y-z, x*z-y^2);
bpd = binomialPrimaryDecomposition I;
assert(intersect bpd == I);
