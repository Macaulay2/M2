-- Test of some simple GB things
-- over quotient rings


R = ZZ/101[x]/(x^3-x-1)
J = ideal(x)
gbTrace = 3 
g = gb(J,ChangeMatrix=>true)
gens g
getChangeMatrix g
m = matrix{{1_R}}
m // (gens J) == matrix{{x^2-1}}
(1_R) // (gens J) -- WRONG
syz gens J




syz gens J -- WRONG
gens J

S = ZZ/101[x]
L = ideal(x, x^3-x-1)
syz gens L
(1_S) // (gens L)
gens gb L
