KK = ZZ/32003
R = KK[x,y,z,w]
I = ideal(x^2*y,x*y^2+x^3)
J = gens gb I
R = KK[x,y,z,w,MonomialOrder=>Lex]
I = substitute(I,R)
gens gb I
R = KK[x,y,z]
F = random(R^1, R^{-2,-3})
GB = gens gb F
LT = leadTerm GB
betti LT
R = KK[x,y,z, MonomialOrder => Lex]
F = random(R^1, R^{-2,-3})
GB = gens gb F
LT = leadTerm GB
betti LT
R = KK[a..i]
M = genericMatrix(R,a,3,3)
N = M^3
I = flatten N
Tr = trace M 
Tr //I  -- the quotient, which is 0
Tr % I  -- the remainder, which is Tr again
Tr^2 % I
Tr^3 % I
Tr^4 % I
Tr^5 % I
Tr^6 % I
Tr^7 % I
Tr^6 // I
Tr^6 == I * (Tr^6 // I) + (Tr^6 % I)
R = KK[t,y,z,MonomialOrder=>Lex]
I = ideal(y-(t^2+t+1), z-(t^3+1))
GB = gens gb I
F = GB_(0,0)
substitute(F, {y =>t^2+t+1, z=>t^3+1})
R = KK[y,z,t]
I = substitute(I,R)
eliminate(I,t)
A = KK[t]
B = KK[y,z]
G = map(A,B,{t^2+t+1, t^3+1})
kernel G
R = KK[t,x,y,z]
I = ideal(x^3,y^3,z^3)
F = x+y+z
L = t*I + (1-t)*ideal(F)
L1 = eliminate(L,t)
gens gb L1
(gens L1) % F
J = ideal ((gens L1)//F)
mingens J
betti oo
R = KK[x,y,z]
I = ideal(x^3,y^3,z^3)
F = x+y+z
J = I : F
betti J
transpose gens J
transpose gens gb J
R = KK[t,a..f]
I = ideal(a*b*c-d*e*f, a^2*b-c^2*d, a*f^2-d*b*c)
F = a*b*c*d*e*f
J = eliminate(I + ideal(t*F-1), t)
transpose gens J
R = KK[a..f]
I = substitute(I,R)
F = product gens R
J' = saturate(I,F)
transpose gens J'
