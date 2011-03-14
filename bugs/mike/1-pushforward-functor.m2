-- We would like to make pushforward into a functor:
-- Also, we would like to implement other aspects too (adjointness, module to ring).

S = QQ[symbol a]/(a)
R = QQ[h,symbol a]/(h^3,a)
F = map(S,R,{0_S,0_S})
basis(S)
pushForward(F,S^1)
viewHelp pushForward

R = QQ[a]/(a^6-a-1)
f = map(R^1, R^1, a^2-a-1)
B = basis R^1
last coefficients(f * B, Monomials => B)

R = QQ[a]/(a^12000-a-1)


pushForward Matrix := opts -> (F) -> (
     -- push forward to the coefficient ring
     R := coefficientRing ring F;
     time B1 := basis source F;
     time B2 := basis target F;
     time sub(last time coefficients(time F * B1, Monomials => B2),R)
     )

pushForward(map(R^1, R^2, {{a,a^2}}))

time pushForward(map(R^1, R^1, {{a}}));

A = QQ[a,b]
B = A[x,y]/(a*x,x^2,y^3)
basis B
basis(1,B)
basis(2,B)
R = A[x]/(x^6-a*x^3-b*x-a^2-1)
time pushForward(map(R^1, R^1, {{x}}));

R = QQ[a]/(a^6-a-1)