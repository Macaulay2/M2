R = QQ[x,y,a,b,c,d,e,f];
F = a*x^2+b*x*y+c*y^2
(M,C) = coefficients(F, Variables=>{x,y})
M*C == matrix{{F}}
G = d*x^2+e*x*y+f*y^2
P = matrix{{x*F,y*F,x*G,y*G}}
(M,C) = coefficients(P, Variables=>{x,y})
M*C == P
(M,C) = coefficients(P, Variables=>{x,y}, Monomials=>{x^3,y^3,x^2*y,x*y^2})
(M,C) = coefficients(P, Variables=>{x,y}, Monomials=>{x^3,y^3})
M*C == P
