KK = ZZ/31991
R = KK[x,y,z,w]
I = ideal(x^2*y,x*y^2+x^3)
J = gens gb I
R = KK[a..d]
I = monomialCurveIdeal(R,{1,3,4})
codim I
dim I
codim (R^1/(I*R^1))
M = coker gens I
codim M
dim M
degree I
degree M
hilbertPolynomial M
hilbertSeries M
Mres = res M
betti Mres
R = KK[x,y,z]
F = random(R^1, R^{-2,-3})
GB = gens gb F
LT = leadTerm gens gb F
betti LT
R = KK[x,y,z, MonomialOrder => Lex]
F = random(R^1, R^{-2,-3})
GB = gens gb F
LT = leadTerm gens gb F
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
x = global x
R = KK[x_0..x_3] 
M = map(R^2, 3, (i,j)->x_(i+j))
I = gens minors(2,M)
pideal = ideal(x_0+x_3, x_1, x_2)
y = global y
S = KK[y_0..y_3,MonomialOrder=> Eliminate 1]
I1 = substitute(I, matrix{{y_0,y_1,y_2,y_3-y_0}})
J = selectInSubring(1,gens gb I1)
S1 = KK[y_1..y_3]
J1 = substitute(J, S1)
Rbar = R/(ideal I)
f = map(Rbar, S1, matrix(Rbar,{{x_0+x_3, x_1,x_2}}))
J1 = ker f
R = KK[a,b,c,d]
I1 = ideal(d*b-a^2, d^2*c-a^3)
I1aug = (gens I1) | matrix{{d}}
augrelations = gens ker I1aug
I21 = submatrix(augrelations, {2}, {0,1})
I21 = ideal I21
I22 = I21 : d
I23 = I22 : d
(gens I23) % (gens I22)
gens gb I1
I2 = divideByVariable(gens gb I1,d)
saturate(I1, d)
