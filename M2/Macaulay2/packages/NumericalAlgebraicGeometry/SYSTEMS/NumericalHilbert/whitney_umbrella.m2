restart
loadPackage "NumericalHilbert"

K = QQ 
K = CC
R = K[x,y,z, MonomialOrder => {Weights=>3:-1}, Global => false]
noise = eps -> random(-eps,eps)+ii*random(-eps,eps)
eps = 1e-16

P = {noise(eps),noise(eps),random CC } -- (0,0,z)
L = (vars R - matrix{P}) * random(CC^3,CC^1) -- random plane through P
M = matrix{{x^2-y^2*z, L}}

dualInfo(M, Point=>P)
E3 = eliminatingDual(M,3,{R_0},Point=>P)
E2 = eliminatingDual(M,2,{R_0},Point=>P)
dualCompare(E2, colonDual(E3,{R_0}))


P = {0,0,1}
P = {0,0,0}
P = {noise(eps),noise(eps),noise(eps)} -- (0,0,0)
M = matrix{{x^2-y^2*z}}
dualInfo(M, Point=>P)

help dualInfo
