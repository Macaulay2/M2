R = QQ[a..f,MonomialOrder=>Weights=>{1,1,1,1,0,0}]
I = ideal(a*b*c-d*e*f,a*c*e-b*d*f,a*d*f-b*c*e)
gens gb I
leadTerm I
leadTerm(1,I)
R = ZZ[x,y]
F = y^2-(x^3+3*x+5)
I = ideal(F, diff(x,F), diff(y,F))
gens gb I
leadTerm I
factor 174
R = QQ[a..d]/(a^2+b^2+c^2+d^2-1)
I = ideal(a*b*c*d)
gens gb I
R = QQ[a..d,SkewCommutative=>true]
I = ideal(a*b-c*d)
gens gb I
A = QQ[s,c]/(s^2+c^2-1)
B = A[x,y,z]
I = ideal(c*x^2, s*y^2, c*y-s*x)
gens gb I
leadTerm oo
