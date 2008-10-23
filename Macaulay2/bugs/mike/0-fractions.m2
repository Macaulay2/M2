R = frac (ZZ/101[a,b]) [x]   
assert("matrix {{1}}" == toString gens gb ideal (a*x^3-13, b*x^2-12))

R = frac (ZZ[a,b]) [x]
assert("matrix {{1}}" == toString gens gb ideal (a*x^3-13, b*x^2-12))

R = frac (QQ[a,b]) [x]
assert("matrix {{1}}" == toString gens gb ideal (a*x^3-13, b*x^2-12))

R = frac (ZZ/101[a,b,c]/(c)) [x]
assert("matrix {{1}}" == toString gens gb ideal (a*x^3-13, b*x^2-12))


debug Core
A = QQ[a,b]
B = frac A
R = A [x]
use A
f = new B from rawFraction(raw B, raw(12*a), raw(12*b-1/2*a))
assert (numerator f == -24*a)
assert (denominator f == a-24*b)

end
-- fixed 10/22/08 MES (obtained 13/-13 in GB's)





