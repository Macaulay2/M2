R = QQ[a..h]
rows = {0,1,2}
cols = {0,3}
result = map(R^3, 2, (i,j) -> R_(rows_i + cols_j))
R = ZZ/101[a..d];
m = matrix{{a^2+a^2*c+a*b+3*d}}
result = coefficients(m, Variables => {a})
result_0
result_1
R = QQ[a,b,Degrees=>{{1,0},{1,-1}}];
m = matrix{{a*b, b^2}}
(degrees source m)_0
R = ZZ/101[a..d]
m = matrix{{a,b},{c,d}}
copym = map(target m, source m, entries m)
R = ZZ[a..d];
m = matrix{{a^2,b^3,c^4,d^5}}
map(R^(numgens source m), source m, 
                 (i,j) -> if i === j then m_(0,i) else 0)
R = ZZ[a..d];
m = matrix{{a,b^2},{c^2,d^3}}
betti m
n = m ** R^{-1}
betti n
R = QQ[a..d]
S = QQ[s,t]
m = matrix{{a^2-d, b*c}}
f = matrix{{s^4,s^3*t,s*t^3,t^4}}
substitute(m,f)
F = map(R,R,{b,c,d,a})
m + F m + F F m + F F F m
substitute(m, {a=>1, b=>3})
R = ZZ[s,t]
m = s^2+t^2
S = R[a..d]
substitute(m,S)
R = ZZ[a..d]
f = matrix{{a^2-b*c,3*b*c^4-1}}
J = ideal f
generators J
image f
cokernel f
id_(R^4)
myanswer = 2*(numgens R) - 1
R = ZZ/31991[a..d]
a
a = 43
a
use R
a
I = ideal(a^2-b,c-1,d^2-a*b)
J = ideal(a*b-1, c*d-2)
intersect(I,J)
