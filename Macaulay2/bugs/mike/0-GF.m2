-- Current problems with galois fields
-- (1) inclusion maps from one GF to another are difficult
-- (2) ring maps display with internal confusing information FIXED
-- (3) no factorization
-- (4) no gcd's FIXED
-- (5) module structure of one GF wrt a smaller one

restart
loadPackage "ConwayPolynomials"
setRandomSeed 0
R = GF 729
a
sub(a,{a=>a+1})

F = map(R,R,{a^3})
F a
F (a+1)
debug Core
raw F

S = R[x]
F = map(S,S,{x-a,a^3})
F a
F (x-a)
isWellDefined map(R,R,{a+1})
isWellDefined map(R,R,{a^2})
isWellDefined map(R,R,{a^3})
factor 729


R = GF(729, Variable=>a)
S = GF(27, Variable=>b)
F = map(R,S,{a^28})
isWellDefined F
F a  -- gives error
a^3

R = ZZ/3[a]/(a^6+2*a^4+a^2+2*a+2)
S = ZZ/3[b]/(b^3+2*b+1)
F = map(R,S,{a^(28)})
isWellDefined F
a^2
a^3
a^(2*2*7*13)
a^(2*2*2*7)
3^6-1
3^3-1
728/26
