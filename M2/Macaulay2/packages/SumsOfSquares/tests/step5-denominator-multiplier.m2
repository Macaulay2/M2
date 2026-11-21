-- LOG.md 5.1 decision 1 (step 5): a positive denominator that is not a sum
-- of squares is certified by (1 + x_1^2 + ... + x_n^2) q SOS, so that the
-- relaxation p - t q is used, not the fallback p q - t q^2.  m + 1 (m the
-- dehomogenised Motzkin form) is positive but not SOS; (1+x^2+y^2)(m+1) is
-- SOS (dehomogenise (x^2+y^2+z^2) M SOS).  The infimum of (x^2+y^2)/(m+1) is 0.
R = QQ[x,y];
m = library("Motzkin", {x, y, 1_R});
assert((solveSOS(m + 1))#GramMatrix === null)
(b, sol) = lowerBound((x^2 + y^2)/(m + 1));
assert(b == 0 and ring b === QQ)
-- with an equation: the same certificate modulo h (here x = y), infimum 0
(b', sol', mult') = lowerBound((x^2 + y^2)/(m + 1), matrix{{x - y}}, 6);
assert(b' =!= null and b' <= 0 and b' >= -2^-3)
-- LOG.md 5.1 decision 6 (after the review of step 5): M + 1 (M the Motzkin
-- form) and (1 + |x|^2)(M + 1) are not SOS, |x|^2 (M + 1) is (numerically):
-- a bound again (real, since the certificate of the denominator is real; the
-- step-3 version and the version before this decision gave none)
R = QQ[x,y,z];
M = library("Motzkin", R);
(b, sol) = lowerBound((x^2 + y^2 + z^2)/(M + 1));
assert(b =!= null and b <= 0 and b > -1e-6)
