R = QQ[x,y];
f = x^4
g = x^2*y + 13*x^2*y^4 +x*y^2-3*x - 1
(lg, cg) = topCoefficients g
h = pseudoRemainder(f,g)
(cg^3 * f - h) % g
q = (cg^3 * f - h) // g
cg^3*f == h + q*g
